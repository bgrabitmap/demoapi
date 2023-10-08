unit uhttp;

{$mode objfpc}{$H+}{$codepage UTF8}

interface

uses
  Classes, Forms, blcksock, sockets, Synautil, SysUtils, StrUtils,
  Dialogs, URIParser, JSONTools,opensslsockets;

const
  POSTENDPOINTS: array[0..0] of string =
    ('lazpaint');

type
  TTCPHttpDaemon = class(TThread)
  private
    Sock: TTCPBlockSocket;
    sRootDirectory: string;
    fActive: boolean;
  public
    constructor Create(asRootDirectory: string);
    destructor Destroy; override;
    procedure Execute; override;
    property Active: boolean read fActive write fActive;
  end;

  { TTCPHttpThrd }

  TTCPHttpThrd = class(TThread)
  private
    num: integer;
  private
    Sock: TTCPBlockSocket;
  public
    Headers: TStringList;
    InputData, OutputData: TMemoryStream;
    sRootDirectory: string;
    constructor Create(hsock: tSocket; asRootDirectory: string);
    destructor Destroy; override;
    procedure Execute; override;
    function ProcessHttpRequest(Request, URI, RequestData: string): integer;
  public
    procedure SendToMainNum;
  end;

implementation

uses
  uMain;

function ObtenerParametros(const parametros: string): TStringList;
begin
  Result := TStringList.Create;
  Result.Delimiter := '&';
  Result.StrictDelimiter := True;
  Result.DelimitedText := parametros;
end;

{ TTCPHttpDaemon }

constructor TTCPHttpDaemon.Create(asRootDirectory: string);
begin
  inherited Create(False);
  sRootDirectory := asRootDirectory;
  sock := TTCPBlockSocket.Create;
  fActive := False;
  FreeOnTerminate := True;

end;


destructor TTCPHttpDaemon.Destroy;
begin
  FreeAndNil(Sock);
  inherited Destroy;
end;

procedure TTCPHttpDaemon.Execute;
var
  ClientSock: TSocket;
begin
  try
    with sock do
    begin
      CreateSocket;
      fActive := False;
      repeat
        if (terminated) then
          break;
        try
          setLinger(True, 10000);
          bind('0.0.0.0', '8000');
          fActive := lastError = 0;
          if fActive then
            listen;

        except
          fActive := False;
        end;
        if not fActive then
          sleep(1000);
      until fActive;

      repeat
        if (terminated) then
          break;
        if canread(1000) then
        begin
          ClientSock := accept;
          fActive := True;
          if lastError = 0 then
            TTCPHttpThrd.Create(ClientSock, sRootDirectory);
        end;
      until False;
    end;
  finally
    if Assigned(Sock) then
      Sock.CloseSocket;
  end;
end;

{ TTCPHttpThrd }

constructor TTCPHttpThrd.Create(hsock: tSocket; asRootDirectory: string);
begin
  sock := TTCPBlockSocket.Create;
  Headers := TStringList.Create;
  InputData := TMemoryStream.Create;
  OutputData := TMemoryStream.Create;
  Sock.socket := HSock;
  FreeOnTerminate := True;
  Priority := tpNormal;
  sRootDirectory := asRootDirectory;
  inherited Create(False);
end;

destructor TTCPHttpThrd.Destroy;
begin
  FreeAndNil(Sock);
  FreeAndNil(Headers);
  FreeAndNil(InputData);
  FreeAndNil(OutputData);
  inherited Destroy;
end;

procedure TTCPHttpThrd.Execute;
var
  timeout: integer;
  s: string;
  method, uri, protocol: string;
  size: integer;
  x, n: integer;
  resultcode: integer;
  Close: boolean;
  slData: TStringList;
begin
  timeout := 120000;
  repeat
    //read request line
    s := sock.RecvString(timeout);
    if sock.lasterror <> 0 then
      Exit;
    if s = '' then
      Exit;
    method := fetch(s, ' ');
    if (s = '') or (method = '') then
      Exit;
    uri := fetch(s, ' ');
    if uri = '' then
      Exit;
    protocol := fetch(s, ' ');
    headers.Clear;
    size := -1;
    Close := False;
    //read request headers
    if protocol <> '' then
    begin
      if pos('HTTP/', protocol) <> 1 then
        Exit;
      if pos('HTTP/1.1', protocol) <> 1 then
        Close := True;
      repeat
        s := sock.RecvString(Timeout);
        if sock.lasterror <> 0 then
          Exit;
        if s <> '' then
          Headers.add(s);
        if Pos('CONTENT-LENGTH:', Uppercase(s)) = 1 then
          Size := StrToIntDef(SeparateRight(s, ' '), -1);
        if Pos('CONNECTION: CLOSE', Uppercase(s)) = 1 then
          Close := True;
      until s = '';
    end;
    //recv document...
    InputData.Clear;
    if size >= 0 then
    begin
      InputData.SetSize(Size);
      x := Sock.RecvBufferEx(InputData.Memory, Size, Timeout);
      InputData.SetSize(x);
      if sock.lasterror <> 0 then
        Exit;
    end;

    slData := TStringList.Create;
    try
      slData.Clear;
      slData.LoadFromStream(InputData);
      OutputData.Clear;
      ResultCode := ProcessHttpRequest(method, uri, slData.Text);
      sock.SendString(protocol + ' ' + IntToStr(ResultCode) + CRLF);
    finally
      FreeAndNil(slData);
    end;
    if protocol <> '' then
    begin
      headers.Add('Access-Control-Allow-Origin: *');
      headers.Add('Access-Control-Allow-Methods: GET, POST, PATCH, PUT, DELETE, OPTIONS');
      headers.Add('Access-Control-Allow-Headers: Origin, Content-Type, X-Auth-Token');
      headers.Add('Content-length: ' + IntToStr(OutputData.Size));
      if Close then
        headers.Add('Connection: close');
      headers.Add('Date: ' + Rfc822DateTime(now));
      headers.Add('Server: POSBerry');
      headers.Add('');
      for n := 0 to headers.Count - 1 do
        sock.sendstring(headers[n] + CRLF);
    end;
    if sock.lasterror <> 0 then
      Exit;
    Sock.SendBuffer(OutputData.Memory, OutputData.Size);
    if Close then
      Break;
  until Sock.LastError <> 0;
end;

function TTCPHttpThrd.ProcessHttpRequest(Request, URI, RequestData: string): integer;
var
  l: TStringList;
  f: TMemoryStream;
  sFileExt, sFilePath, sContentType, sFileName, sDireccionIPv4,
  sDireccionRemota, sUri: string;
  pURI: TURI;
  slParametros: TStringList;
  jObj: TJSONNode;
begin
  Result := 200;
  //DebugToIPC(DateTimeToISO(now) + ' Solictud HTTP: ' + URI);
  if request = 'GET' then
  begin
    headers.Clear;
    l := TStringList.Create;
    f := TMemoryStream.Create;
    pURI := ParseURI(Uri);
    slParametros := ObtenerParametros(pURI.Params);
    try
      try
        sDireccionRemota := sock.GetRemoteSinIP;
      except
        sDireccionRemota := 'ERROR';
      end;
      if uri = '/' then
        uri := '/index.html';
      sUri := StringReplace(uri, '/', DirectorySeparator, [rfReplaceAll]);
      if (pos('?', sUri) > 0) then
        sUri := copy(sUri, 1, pos('?', sUri)-1);
      sFilePath := ExcludeTrailingBackslash(sRootDirectory) + sUri;
      sFileExt := ExtractFileExt(sFilePath);
      sFileName := ExtractFileName(sFilePath);
      //si no solicita un archivo
      if (IndexStr(sFileExt, ['.js', '.html', '.htm', '.json',
        '.css', '.svg', '.map', '.woff', '.woff2']) >= 0) and
        (not (FileExists(ExcludeTrailingBackslash(sRootDirectory) + sUri))) then
      begin
        l.Add('<html>');
        l.Add('<head></head>');
        l.Add('<body>');
        //l.Add('Root directory: ' + sRootDirectory);
        l.Add('<br>');
        l.Add('Request sUri: ' + sUri);
        l.Add('<br>');
        l.Add('File Type: ' + sFileExt);
        l.Add('<br>');
        //l.Add('FILE NOT EXISTS: '+sFilePath);
        l.Add('</body>');
        l.Add('</html>');
        headers.Add('Content-type: Text/Html');
        l.SaveToStream(OutputData);
        Result := 404;
        //controlar no puedan acceder a cualquier archivo
      end;
      if (IndexStr(sFileExt, ['.js', '.html', '.htm', '.json',
        '.css', '.svg', '.map', '.woff', '.woff2', '.jpg', '.png', '.ttf']) >= 0) and
        ((FileExists(ExcludeTrailingBackslash(sRootDirectory) + sUri))) then
      begin
        case IndexStr(sFileExt, ['.js', '.html', '.htm', '.json',
            '.css', '.svg', '.map', '.woff', '.woff2', '.jpg', '.png', '.ttf']) of
          0:
          begin
            sContentType := 'text/javascript';
            headers.Add('Content-type: ' + sContentType);
            l.Clear;
            l.LoadFromFile(ExcludeTrailingBackslash(sRootDirectory) + sUri);
            sDireccionIPv4 := sock.GetLocalSinIP;
            if (sFileName = 'app.js') or (sFileName = 'appventas.js') or
              (sFileName = 'order.js') then
            begin
              l.Text :=
                ReplaceText(l.Text, '"ws://127.0.0.1:8080/"', '"ws://' +
                sDireccionIPv4 + ':8080/"');
              l.Text :=
                ReplaceText(l.Text, '"ws://localhost:8080/"', '"ws://' +
                sDireccionIPv4 + ':8080/"');
              l.Text :=
                ReplaceText(l.Text, 'http://localhost:8000', 'http://' +
                sDireccionIPv4 + ':8000');
              l.Text :=
                ReplaceText(l.Text, 'http://127.0.0.1:8000', 'http://' +
                sDireccionIPv4 + ':8000');
              //l.Text:=ReplaceText(l.Text,'http://localhost:8000/precioproductos','http://'+sDireccionIPv4+':8000/precioproductos') ;
              //l.Text:=ReplaceText(l.Text,'"http://localhost:8000/pedidoskvs"','"http://'+sDireccionIPv4+':8000/pedidoskvs"') ;
            end;
          end;
          1:
          begin
            sContentType := 'text/html';
            headers.Add('Content-type: ' + sContentType);
            l.Clear;
            l.LoadFromFile(ExcludeTrailingBackslash(sRootDirectory) + sUri);
          end;
          2:
          begin
            sContentType := 'text/html';
            headers.Add('Content-type: ' + sContentType);
            l.Clear;
            l.LoadFromFile(ExcludeTrailingBackslash(sRootDirectory) + sUri);
          end;
          3:
          begin
            sContentType := 'application/json';
            headers.Add('Content-type: ' + sContentType);
            l.Clear;
            l.LoadFromFile(ExcludeTrailingBackslash(sRootDirectory) + sUri);
          end;
          4:
          begin
            sContentType := 'text/css';
            headers.Add('Content-type: ' + sContentType);
            l.Clear;
            l.LoadFromFile(ExcludeTrailingBackslash(sRootDirectory) + sUri);
          end;
          5:
          begin
            sContentType := 'image/svg+xml';
            headers.Add('Content-type: ' + sContentType);
            l.Clear;
            l.LoadFromFile(ExcludeTrailingBackslash(sRootDirectory) + sUri);
          end;
          6:
          begin
            sContentType := 'application/json';
            headers.Add('Content-type: ' + sContentType);
            l.Clear;
            l.LoadFromFile(ExcludeTrailingBackslash(sRootDirectory) + sUri);
          end;
          7:
          begin
            sContentType := 'font/woff';
            headers.Add('Content-type: ' + sContentType);
            f.Clear;
            f.LoadFromFile(ExcludeTrailingBackslash(sRootDirectory) + sUri);
          end;
          8:
          begin
            sContentType := 'font/woff2';
            headers.Add('Content-type: ' + sContentType);
            f.Clear;
            f.LoadFromFile(ExcludeTrailingBackslash(sRootDirectory) + sUri);
          end;
          9:
          begin
            sContentType := 'image/jpeg';
            headers.Add('Content-type: ' + sContentType);
            f.Clear;
            f.LoadFromFile(ExcludeTrailingBackslash(sRootDirectory) + sUri);
          end;
          10:
          begin
            sContentType := 'image/png';
            headers.Add('Content-type: ' + sContentType);
            f.Clear;
            f.LoadFromFile(ExcludeTrailingBackslash(sRootDirectory) + sUri);
          end;
          11:
          begin
            sContentType := 'font/ttf';
            headers.Add('Content-type: ' + sContentType);
            f.Clear;
            f.LoadFromFile(ExcludeTrailingBackslash(sRootDirectory) + sUri);
          end;
          else
            sContentType := 'text/html';
            headers.Add('Content-type: ' + sContentType);
            l.Clear;
            l.Add('<html>');
            l.Add('<head></head>');
            l.Add('<body>');
            l.Add('FILE TYPE NOT ALLOWED ' + sFileExt);
            l.Add('</body>');
            l.Add('</html>');
            l.SaveToStream(OutputData);
        end;
        if f.Size > 0 then  //si es un archivo binario
          f.SaveToStream(OutputData)
        else
          l.SaveToStream(OutputData);
        Result := 200;
      end;

    finally
    end
  end;
  if request = 'POST' then
  begin
    headers.Clear;
    l := TStringList.Create;
    pURI := ParseURI(Uri);

    try
      if (pUri.Document in POSTENDPOINTS) then
      begin
        Result := 200;

        if (pUri.Document = 'lazpaint') then
        begin
          jObj := TJSONNode.Create;
          jObj.Parse(RequestData);
          num := trunc(jObj.Find('num').AsNumber);
          Synchronize(@SendToMainNum);
          jObj.Free;
          l.Text := RequestData;
        end;
        l.SaveToStream(OutputData);
      end;
    finally
      FreeAndNil(l);
    end;
  end;
end;

procedure TTCPHttpThrd.SendToMainNum;
begin
  uMain.frmLazPaint.BGRAShape1.SideCount := num;
end;

end.

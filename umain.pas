unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  uhttp, BGRAShape, LCLIntF;

type

  { TfrmLazPaint }

  TfrmLazPaint = class(TForm)
    BGRAShape1: TBGRAShape;
    btnOpenURL: TButton;
    procedure btnOpenURLClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fTCPHttpDaemon: TTCPHttpDaemon;
  public

  end;

var
  frmLazPaint: TfrmLazPaint;

implementation

{$R *.lfm}

{ TfrmLazPaint }

procedure TfrmLazPaint.FormCreate(Sender: TObject);
begin
  fTCPHttpDaemon := TTCPHttpDaemon.Create(Application.Location + 'htdocs');
end;

procedure TfrmLazPaint.btnOpenURLClick(Sender: TObject);
begin
  openurl('http://localhost:8000/');
end;

end.


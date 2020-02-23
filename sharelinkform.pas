unit ShareLinkForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Profile,
  UBarCodes;

type

  { TFormShareLink }

  TFormShareLink = class(TForm)
    MemoLinkText: TMemo;
    ShapeQRCodeLocator: TShape;
    procedure ApplyProfile(Profile: TProfile);
    procedure FormCreate(Sender: TObject);
  private
    QR: TBarCodeQR;
  public
  end;

var
  FormShareLink: TFormShareLink;

implementation
{$R *.lfm}

procedure TFormShareLink.ApplyProfile(Profile: TProfile);
var
  L: string;
begin
  L := Profile.GenerateLink;
  MemoLinkText.Lines.Clear;
  MemoLinkText.Lines.Add(L);
  QR.Text := L;
end;

procedure TFormShareLink.FormCreate(Sender: TObject);
begin
  QR := TBarCodeQR.Create(FormShareLink);
  with QR do
  begin
    Parent := FormShareLink;
    Left := ShapeQRCodeLocator.Left + 4;
    Top := ShapeQRCodeLocator.Top + 4;
    Width := ShapeQRCodeLocator.Width - 5;
    Height := ShapeQRCodeLocator.Height - 5;
    Visible := True;
    Text := '';
  end;
end;

end.


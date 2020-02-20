unit ShareLinkForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Profile;

type
  TFormShareLink = class(TForm)
    MemoLinkText: TMemo;
    procedure ApplyProfile(Profile: TProfile);
  private
  public
  end;

var
  FormShareLink: TFormShareLink;

implementation
{$R *.lfm}

procedure TFormShareLink.ApplyProfile(Profile: TProfile);
begin
  MemoLinkText.Lines.Clear;
  MemoLinkText.Lines.Add(Profile.GenerateLink);
end;

end.


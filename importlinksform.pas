unit ImportLinksForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Profile, V2rayJsonConfig;

type

  { TFormImportLinks }

  TFormImportLinks = class(TForm)
    ButtonSubmit: TButton;
    ButtonRemove: TButton;
    ButtonEdit: TButton;
    ButtonGotoStep2: TButton;
    ButtonClearLinks: TButton;
    LabelInfo: TLabel;
    LabelDetail: TLabel;
    LabelReviewLinks: TLabel;
    LabelLinks: TLabel;
    ListBoxReviewLinks: TListBox;
    MemoLinkDetail: TMemo;
    MemoLinks: TMemo;
    PageControlImportSteps: TPageControl;
    TabSheetImportStep1: TTabSheet;
    TabSheetImportStep2: TTabSheet;
    procedure ButtonClearLinksClick(Sender: TObject);
    procedure ButtonEditClick(Sender: TObject);
    procedure ButtonGotoStep2Click(Sender: TObject);
    procedure ButtonRemoveClick(Sender: TObject);
    procedure ButtonSubmitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBoxReviewLinksSelectionChange(Sender: TObject; User: boolean);
    procedure Refresh;
  private
  public
    SaveAfterExit: boolean;
    ReadyProfileList: TList;
  end;

var
  FormImportLinks: TFormImportLinks;

implementation

uses ProfileEditor;
{$R *.lfm}

procedure TFormImportLinks.ButtonClearLinksClick(Sender: TObject);
begin
  MemoLinks.Lines.Clear;
end;

procedure TFormImportLinks.ButtonEditClick(Sender: TObject);
var
  P: TProfile;
  I: integer;
begin
  I := ListBoxReviewLinks.ItemIndex;
  if I <> -1 then
  begin
    P := TProfile(ReadyProfileList[I]);
    FormEditProfile.ApplyProfile(P);
    FormEditProfile.ShowModal;
    if FormEditProfile.SaveAfterExit then
    begin
      ListBoxReviewLinks.Items[I] := P.Name;
      ListBoxReviewLinksSelectionChange(nil, False);
    end;
  end;
end;

procedure TFormImportLinks.ButtonGotoStep2Click(Sender: TObject);
var
  L: string;
  L2: string;
  P: TProfile;
  Suc: boolean;
  Lines: TStringArray;
begin
  ReadyProfileList.Clear;
  ListBoxReviewLinks.Items.Clear;
  Lines := string(MemoLinks.Text).Split(#10);
  for L in Lines do
  begin
    L2 := L.Trim;
    if L2 = '' then continue;
    P := TProfile.Create;
    Suc := ParseLinkToProfile(L2, P);
    if Suc then
    begin
      ReadyProfileList.Add(P);
      ListBoxReviewLinks.Items.Add(P.Name);
    end;
  end;
  if ReadyProfileList.Count = 0 then
    LabelInfo.Caption := 'No vaild links!'
  else
  begin
    ListBoxReviewLinksSelectionChange(nil, False);
    PageControlImportSteps.PageIndex := 1;
  end;
end;

procedure TFormImportLinks.ButtonRemoveClick(Sender: TObject);
var
  I: integer;
begin
  I := ListBoxReviewLinks.ItemIndex;
  if I <> -1 then
  begin
    ListBoxReviewLinks.DeleteSelected;
    TProfile(ReadyProfileList[I]).Free;
    ReadyProfileList.Delete(I);
    ListBoxReviewLinksSelectionChange(nil, False);
  end;
end;

procedure TFormImportLinks.ButtonSubmitClick(Sender: TObject);
begin
  if ReadyProfileList.Count > 0 then SaveAfterExit := True;
  Close;
end;

procedure TFormImportLinks.FormCreate(Sender: TObject);
begin
  ReadyProfileList := TList.Create;
end;

procedure TFormImportLinks.ListBoxReviewLinksSelectionChange(Sender: TObject;
  User: boolean);
var
  P: TProfile;
  I: integer;
begin
  I := ListBoxReviewLinks.ItemIndex;
  if I <> -1 then
  begin
    P := TProfile(ReadyProfileList[I]);
    ButtonEdit.Enabled := True;
    ButtonRemove.Enabled := True;
    MemoLinkDetail.Lines.Clear;
    MemoLinkDetail.Lines.Add(Format('Remote: %s:%d', [P.Address, P.Port]));
    MemoLinkDetail.Lines.Add(Format('Protocol: %s', [RemoteProtocolToString(P.Protocol)]));
    MemoLinkDetail.Lines.Add(Format('Network: %s', [TransportToString(P.Network)]));
  end
  else
  begin
    ButtonEdit.Enabled := False;
    ButtonRemove.Enabled := False;
    MemoLinkDetail.Lines.Clear;
  end;
end;

procedure TFormImportLinks.Refresh;
begin
  SaveAfterExit := False;
  MemoLinks.Lines.Clear;
  ReadyProfileList.Clear;
  ListBoxReviewLinks.Items.Clear;
  PageControlImportSteps.PageIndex := 0;
  LabelInfo.Caption := '';
end;

end.


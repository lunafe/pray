unit ProfileEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, Menus, V2rayJsonConfig, Profile;

type

  { TFormEditProfile }

  TFormEditProfile = class(TForm)
    ButtonSave: TButton;
    CheckBoxEnableTLS: TCheckBox;
    ComboBoxNetwork: TComboBox;
    EditAddress: TEdit;
    EditPath: TEdit;
    EditHostname: TEdit;
    EditUUID: TEdit;
    EditProfileName: TEdit;
    GroupBoxGeneral: TGroupBox;
    GroupBoxStream: TGroupBox;
    GroupBoxUser: TGroupBox;
    LabelPort: TLabel;
    LabelAddress: TLabel;
    LabelPath: TLabel;
    LabelHostname: TLabel;
    LabelNetwork: TLabel;
    LabelAlterID: TLabel;
    LabelUUID: TLabel;
    LabelProfileName: TLabel;
    SpinEditPort: TSpinEdit;
    SpinEditAlterID: TSpinEdit;
    procedure ButtonSaveClick(Sender: TObject);
    procedure ApplyProfile(Profile: TProfile);
    procedure ComboBoxNetworkChange(Sender: TObject);
  public
    SaveAfterExit: boolean;
  private
    ProfileObj: TProfile;
  end;

var
  FormEditProfile: TFormEditProfile;

implementation

{$R *.lfm}

{ TFormEditProfile }

procedure TFormEditProfile.ButtonSaveClick(Sender: TObject);
begin
  ProfileObj.Name := EditProfileName.Text;
  ProfileObj.Address := EditAddress.Text;
  ProfileObj.Port := SpinEditPort.Value;
  ProfileObj.UUID := EditUUID.Text;
  ProfileObj.AlterID := SpinEditAlterID.Value;
  ProfileObj.Network := TRemoteTransport(ComboBoxNetwork.ItemIndex);
  ProfileObj.EnableTLS := CheckBoxEnableTLS.Checked;
  ProfileObj.Hostname := EditHostname.Text;
  ProfileObj.Path := EditPath.Text;
  SaveAfterExit := True;
  FormEditProfile.Close;
end;

procedure TFormEditProfile.ApplyProfile(Profile: TProfile);
begin
  EditProfileName.Text := Profile.Name;
  EditAddress.Text := Profile.Address;
  SpinEditPort.Value := Profile.Port;
  EditUUID.Text := Profile.UUID;
  SpinEditAlterID.Value := Profile.AlterID;
  ComboBoxNetwork.ItemIndex := integer(Profile.Network);
  CheckBoxEnableTLS.Checked := Profile.EnableTLS;
  EditHostname.Text := Profile.Hostname;
  EditPath.Text := Profile.Path;
  ProfileObj := Profile;
  SaveAfterExit := False;
end;

procedure TFormEditProfile.ComboBoxNetworkChange(Sender: TObject);
begin
  if TRemoteTransport(ComboBoxNetwork.ItemIndex) = rtKCP then
  begin
    CheckBoxEnableTLS.Enabled := False;
    CheckBoxEnableTLS.Checked := False;
  end
  else
    CheckBoxEnableTLS.Enabled := True;
end;

end.

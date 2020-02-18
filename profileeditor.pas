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
  ProfileObj.Hostname := EditHostname.Text;
  ProfileObj.Path := EditPath.Text;
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
  EditHostname.Text := Profile.Hostname;
  EditPath.Text := Profile.Path;
  ProfileObj := Profile;
end;

end.

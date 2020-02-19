unit ProfileEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, Menus, ExtCtrls, ComCtrls, V2rayJsonConfig, Profile;

type

  { TFormEditProfile }

  TFormEditProfile = class(TForm)
    ButtonSave: TButton;
    CheckBoxEnableTLS: TCheckBox;
    ComboBoxQUICSecurity: TComboBox;
    ComboBoxNetwork: TComboBox;
    ComboBoxUDPHeaderType: TComboBox;
    EditQUICKey: TEdit;
    EditAddress: TEdit;
    EditPath: TEdit;
    EditHostname: TEdit;
    EditUUID: TEdit;
    EditProfileName: TEdit;
    GroupBoxGeneral: TGroupBox;
    GroupBoxStream: TGroupBox;
    GroupBoxUser: TGroupBox;
    LabelQUICKey: TLabel;
    LabelQUICSecurity: TLabel;
    LabelUDPHeaderType: TLabel;
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
    procedure ComboBoxQUICSecurityChange(Sender: TObject);

  public
    SaveAfterExit: boolean;
  private
    ProfileObj: TProfile;
    procedure TiggerUDP(FieldsEnabled: boolean);
    procedure TiggerQUIC(FieldsEnabled: boolean);
    procedure TiggerHostPath(FieldsEnabled: boolean);
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
  ProfileObj.EnableTLS := (CheckBoxEnableTLS.Enabled and CheckBoxEnableTLS.Checked);
  ProfileObj.Hostname := EditHostname.Text;
  ProfileObj.Path := EditPath.Text;
  ProfileObj.UDPHeaderType := TUDPHeaderType(ComboBoxUDPHeaderType.ItemIndex);
  ProfileObj.QUICSecurity := TQUICSecurity(ComboBoxQUICSecurity.ItemIndex);
  ProfileObj.QUICKey := EditQUICKey.Text;
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
  ComboBoxUDPHeaderType.ItemIndex := integer(Profile.UDPHeaderType);
  ComboBoxQUICSecurity.ItemIndex := integer(Profile.QUICSecurity);
  EditQUICKey.Text := Profile.QUICKey;
  ProfileObj := Profile;
  ComboBoxNetworkChange(nil);
  SaveAfterExit := False;
end;

procedure TFormEditProfile.TiggerUDP(FieldsEnabled: boolean);
begin
  LabelUDPHeaderType.Enabled := FieldsEnabled;
  ComboBoxUDPHeaderType.Enabled := FieldsEnabled;
  CheckBoxEnableTLS.Enabled := not FieldsEnabled;
end;

procedure TFormEditProfile.TiggerQUIC(FieldsEnabled: boolean);
begin
  ComboBoxQUICSecurity.Enabled := FieldsEnabled;
  EditQUICKey.Enabled := FieldsEnabled;
  LabelQUICSecurity.Enabled := FieldsEnabled;
  LabelQUICKey.Enabled := FieldsEnabled;
  if FieldsEnabled then
    ComboBoxQUICSecurityChange(nil);
end;

procedure TFormEditProfile.TiggerHostPath(FieldsEnabled: boolean);
begin
  LabelHostname.Enabled := FieldsEnabled;
  LabelPath.Enabled := FieldsEnabled;
  EditHostname.Enabled := FieldsEnabled;
  EditPath.Enabled := FieldsEnabled;
end;

procedure TFormEditProfile.ComboBoxNetworkChange(Sender: TObject);
var
  E: TRemoteTransport;
begin
  E := TRemoteTransport(ComboBoxNetwork.ItemIndex);
  case E of
    rtKCP:
    begin
      TiggerQUIC(False);
      TiggerHostPath(False);
      TiggerUDP(True);
    end;
    rtQUIC:
    begin
      TiggerHostPath(False);
      TiggerQUIC(True);
      TiggerUDP(True);
    end;
    rtTCP:
    begin
      TiggerHostPath(False);
      TiggerQUIC(False);
      TiggerUDP(False);
    end;
    rtWS, rtHTTP:
    begin
      TiggerHostPath(True);
      TiggerQUIC(False);
      TiggerUDP(False);
    end;
  end;
end;

procedure TFormEditProfile.ComboBoxQUICSecurityChange(Sender: TObject);
begin
  if ComboBoxQUICSecurity.ItemIndex = 0 then begin
    LabelQUICKey.Enabled := False;
    EditQUICKey.Enabled := False;
  end
  else
  begin
    LabelQUICKey.Enabled := True;
    EditQUICKey.Enabled := True;
  end;
end;

end.

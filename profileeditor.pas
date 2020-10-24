unit ProfileEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, StdCtrls,
  Spin, Menus, ComCtrls, V2rayJsonConfig, Profile, Types, Controls;

type

  { TFormEditProfile }

  TFormEditProfile = class(TForm)
    ButtonSave: TButton;
    CheckBoxXTLS: TCheckBox;
    CheckBoxEnableTLS: TCheckBox;
    ComboBoxFlow: TComboBox;
    ComboBoxMethod: TComboBox;
    ComboBoxProtocol: TComboBox;
    ComboBoxQUICSecurity: TComboBox;
    ComboBoxNetwork: TComboBox;
    ComboBoxUDPHeaderType: TComboBox;
    EditTrojanPassword: TEdit;
    EditVLESSUUID: TEdit;
    EditVLESSEncryption: TEdit;
    EditPassword: TEdit;
    EditQUICKey: TEdit;
    EditAddress: TEdit;
    EditPath: TEdit;
    EditHostname: TEdit;
    EditUUID: TEdit;
    EditProfileName: TEdit;
    GroupBoxGeneral: TGroupBox;
    GroupBoxStream: TGroupBox;
    GroupBoxUser: TGroupBox;
    LabelFlow: TLabel;
    LabelTrojanPassword: TLabel;
    LabelVLESSUUID: TLabel;
    LabelVLESSEncryption: TLabel;
    LabelMethod: TLabel;
    LabelPassword: TLabel;
    LabelProtocol: TLabel;
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
    PageControlProtocolSwitch: TPageControl;
    SpinEditPort: TSpinEdit;
    SpinEditAlterID: TSpinEdit;
    TabSheetTrojanConfig: TTabSheet;
    TabSheetVLESSConfig: TTabSheet;
    TabSheetShadowsocksConfig: TTabSheet;
    TabSheetVMessConfig: TTabSheet;
    procedure ButtonSaveClick(Sender: TObject);
    procedure ApplyProfile(Profile: TProfile);
    procedure CheckBoxEnableTLSChange(Sender: TObject);
    procedure CheckBoxXTLSChange(Sender: TObject);
    procedure ComboBoxNetworkChange(Sender: TObject);
    procedure ComboBoxProtocolChange(Sender: TObject);
    procedure ComboBoxQUICSecurityChange(Sender: TObject);
  public
    SaveAfterExit: boolean;
  private
    ProfileObj: TProfile;
    procedure TiggerXTLS(XTLSEnabled: boolean);
    procedure AllowXTLS(Allowed: boolean);
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
  with ProfileObj do begin
    Name := EditProfileName.Text;
    Address := EditAddress.Text;
    Port := SpinEditPort.Value;
    Protocol := TRemoteProtocol(ComboBoxProtocol.ItemIndex);
    Flow := ComboBoxFlow.Text;
    UUID := EditUUID.Text;
    AlterID := SpinEditAlterID.Value;
    SSPassword := EditPassword.Text;
    SSMethod := ComboBoxMethod.Text;
    VLESSID := EditVLESSUUID.Text;
    VLESSEncryption := EditVLESSEncryption.Text;
    TrojanPassword := EditTrojanPassword.Text;
    Network := TRemoteTransport(ComboBoxNetwork.ItemIndex);
    if CheckBoxXTLS.Checked then
      StreamSecurity := soXTLS
    else if CheckBoxEnableTLS.Checked then
      StreamSecurity := soTLS
    else
      StreamSecurity := soNONE;
    Hostname := EditHostname.Text;
    Path := EditPath.Text;
    UDPHeaderType := ComboBoxUDPHeaderType.Text;
    QUICSecurity := ComboBoxQUICSecurity.Text;
    QUICKey := EditQUICKey.Text;
  end;
  SaveAfterExit := True;
  FormEditProfile.Close;
end;

procedure TFormEditProfile.ApplyProfile(Profile: TProfile);
begin
  with Profile do begin
    EditProfileName.Text := Profile.Name;
    EditAddress.Text := Address;
    SpinEditPort.Value := Port;
    ComboBoxProtocol.ItemIndex := integer(Protocol);
    EditUUID.Text := UUID;
    SpinEditAlterID.Value := AlterID;
    EditPassword.Text := SSPassword;
    EditVLESSUUID.Text := VLESSID;
    EditTrojanPassword.Text := TrojanPassword;
    ComboBoxFlow.Text := Flow;
    if StreamSecurity = soTLS then CheckBoxEnableTLS.Checked := True;
    if Protocol <> rpVLESS then EditVLESSEncryption.Text := 'none';
    AllowXTLS(True);
    TiggerXTLS(StreamSecurity = soXTLS);
    case Protocol of
      rpVLESS: begin
        EditVLESSEncryption.Text := VLESSEncryption;
        if StreamSecurity = soXTLS then TiggerXTLS(True);
      end;
      rpTROJAN: ;
      else begin
        AllowXTLS(False);
      end;
    end;
    ComboBoxMethod.Text := SSMethod;
    ComboBoxNetwork.ItemIndex := integer(Network);
    EditHostname.Text := Hostname;
    EditPath.Text := Path;
    ComboBoxUDPHeaderType.Text := UDPHeaderType;
    ComboBoxQUICSecurity.Text := QUICSecurity;
    EditQUICKey.Text := QUICKey;
  end;
  ProfileObj := Profile;
  ComboBoxProtocolChange(nil);
  ComboBoxNetworkChange(nil);
  SaveAfterExit := False;
end;

procedure TFormEditProfile.CheckBoxEnableTLSChange(Sender: TObject);
begin
  TiggerHostPath(LabelPath.Enabled);
end;

procedure TFormEditProfile.CheckBoxXTLSChange(Sender: TObject);
begin
  TiggerXTLS(CheckBoxXTLS.Checked);
end;

procedure TFormEditProfile.AllowXTLS(Allowed: boolean);
begin
  if Allowed then begin
    CheckBoxXTLS.Enabled := True;
    TiggerXTLS(CheckBoxXTLS.Checked);
  end
  else begin
    CheckBoxXTLS.Enabled := False;
    CheckBoxXTLS.Checked := False;
    TiggerXTLS(False);
  end;
end;

procedure TFormEditProfile.TiggerXTLS(XTLSEnabled: boolean);
begin
  CheckBoxXTLS.Checked := XTLSEnabled;
  LabelFlow.Enabled := XTLSEnabled;
  ComboBoxFlow.Enabled := XTLSEnabled;
  CheckBoxEnableTLS.Enabled := not XTLSEnabled;
  ComboBoxNetwork.Enabled := not XTLSEnabled;
  if XTLSEnabled then begin
    ComboBoxNetwork.ItemIndex := 0;
    CheckBoxEnableTLS.Checked := True;
    TiggerHostPath(False);
  end;
end;

procedure TFormEditProfile.TiggerUDP(FieldsEnabled: boolean);
begin
  LabelUDPHeaderType.Enabled := FieldsEnabled;
  ComboBoxUDPHeaderType.Enabled := FieldsEnabled;
  if FieldsEnabled then
  begin
    CheckBoxEnableTLS.Enabled := False;
    CheckBoxEnableTLS.Checked := False;
  end
  else CheckBoxEnableTLS.Enabled := True;
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
  LabelHostname.Enabled := FieldsEnabled or CheckBoxEnableTLS.Checked;
  EditHostname.Enabled := FieldsEnabled or CheckBoxEnableTLS.Checked;
  LabelPath.Enabled := FieldsEnabled;
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
    rtWS:
    begin
      TiggerHostPath(True);
      TiggerQUIC(False);
      TiggerUDP(False);
    end;
    rtHTTP:
    begin
      TiggerHostPath(True);
      TiggerQUIC(False);
      TiggerUDP(False);
      CheckBoxEnableTLS.Checked := True;
      CheckBoxEnableTLS.Enabled := False;
    end;
  end;
end;

procedure TFormEditProfile.ComboBoxProtocolChange(Sender: TObject);
begin
  PageControlProtocolSwitch.PageIndex := ComboBoxProtocol.ItemIndex;
  case ComboBoxProtocol.ItemIndex of
    0, 1: AllowXTLS(False);
    else AllowXTLS(True);
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

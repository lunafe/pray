unit GlobalSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  Spin, Menus, V2rayJsonConfig, FPJson, ProgramSettings;

type
  { TFormGlobalSettings }

  TFormGlobalSettings = class(TForm)
    ButtonRouteSave: TButton;
    ButtonRouteRestore: TButton;
    ButtonRouteClear: TButton;
    ButtonFindV2rayAssets: TButton;
    ButtonCancel: TButton;
    ButtonSaveConfigs: TButton;
    ButtonFindV2rayBinary: TButton;
    CheckBoxCongestion: TCheckBox;
    CheckBoxEnableHTTPProxy: TCheckBox;
    CheckBoxEnableSocksProxy: TCheckBox;
    ComboBoxLogLevel: TComboBox;
    ComboBoxKCPHeaderType: TComboBox;
    ComboBoxDomainStrategy: TComboBox;
    EditV2rayAssetsPath: TEdit;
    EditDnsServers: TEdit;
    EditV2rayPath: TEdit;
    GroupBoxDNSServers: TGroupBox;
    GroupBoxKCPSettings: TGroupBox;
    GroupBoxV2rayConfig: TGroupBox;
    GroupBoxInbound: TGroupBox;
    LabelLogLevel: TLabel;
    LabelKCPHeader: TLabel;
    LabelV2rayAssetsPath: TLabel;
    LabelDomainStrategy: TLabel;
    LabelWriteBufferSize: TLabel;
    LabelReadBufferSize: TLabel;
    LabelDownlinkCapacity: TLabel;
    LabelUplinkCapacity: TLabel;
    LabelKCPTTI: TLabel;
    LabelKCPMTU: TLabel;
    LabelV2RayBinaryPath: TLabel;
    LabelHTTPPort: TLabel;
    LabelSocksPort: TLabel;
    MemoRuleList: TMemo;
    OpenDialogFindV2rayBinary: TOpenDialog;
    PageControlGlobalSettings: TPageControl;
    SelectDirectoryDialogFindV2rayAssets: TSelectDirectoryDialog;
    SpinEditWriteBufferSize: TSpinEdit;
    SpinEditReadBufferSize: TSpinEdit;
    SpinEditDownlinkCapacity: TSpinEdit;
    SpinEditUplinkCapacity: TSpinEdit;
    SpinEditKCPTTI: TSpinEdit;
    SpinEditKCPMTU: TSpinEdit;
    SpinEditHTTPPort: TSpinEdit;
    SpinEditSocksPort: TSpinEdit;
    TabControlRouteSetting: TTabControl;
    TabSheetMisc: TTabSheet;
    TabSheetGeneralSettings: TTabSheet;
    TabSheetRouteSettings: TTabSheet;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonFindV2rayAssetsClick(Sender: TObject);
    procedure ButtonFindV2rayBinaryClick(Sender: TObject);
    procedure ButtonRouteClearClick(Sender: TObject);
    procedure ButtonRouteRestoreClick(Sender: TObject);
    procedure ButtonRouteSaveClick(Sender: TObject);
    procedure ButtonSaveConfigsClick(Sender: TObject);
    procedure CheckBoxEnableHTTPProxyEditingDone(Sender: TObject);
    procedure CheckBoxEnableSocksProxyEditingDone(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure LoadSettings(Settings: TProgramSettings);
    procedure TabControlRouteSettingChange(Sender: TObject);
    function SaveSettings(Settings: TProgramSettings): boolean;
  private
    RouteListDirect: string;
    RouteListProxy: string;
    RouteListDeny: string;
  end;

var
  FormGlobalSettings: TFormGlobalSettings;
  Settings: TProgramSettings;
  SettingsJsonPath: string;

implementation

{$R *.lfm}

procedure TFormGlobalSettings.ButtonFindV2rayBinaryClick(Sender: TObject);
begin
  OpenDialogFindV2rayBinary.InitialDir := ExtractFileDir(EditV2rayPath.Text);
  if OpenDialogFindV2rayBinary.Execute then
  begin
    EditV2rayPath.Text := OpenDialogFindV2rayBinary.FileName;
  end;
end;

procedure TFormGlobalSettings.ButtonRouteClearClick(Sender: TObject);
begin
  case TabControlRouteSetting.TabIndex of
    0: MemoRuleList.Text := '';
    1: MemoRuleList.Text := '';
    2: MemoRuleList.Text := '';
  end;
end;

procedure TFormGlobalSettings.ButtonRouteRestoreClick(Sender: TObject);
begin
  case TabControlRouteSetting.TabIndex of
    0: MemoRuleList.Text := RouteListDirect;
    1: MemoRuleList.Text := RouteListProxy;
    2: MemoRuleList.Text := RouteListDeny;
  end;
end;

procedure TFormGlobalSettings.ButtonRouteSaveClick(Sender: TObject);
begin
  case TabControlRouteSetting.TabIndex of
    0: RouteListDirect := MemoRuleList.Text;
    1: RouteListProxy := MemoRuleList.Text;
    2: RouteListDeny := MemoRuleList.Text;
  end;
end;

procedure TFormGlobalSettings.ButtonSaveConfigsClick(Sender: TObject);
begin
  if SaveSettings(Settings) then
  begin
    Settings.SaveFile(SettingsJsonPath);
    FormGlobalSettings.Close;
  end;
end;

procedure TFormGlobalSettings.CheckBoxEnableHTTPProxyEditingDone(Sender: TObject);
begin
  SpinEditHTTPPort.Enabled := CheckBoxEnableHTTPProxy.Checked;
end;

procedure TFormGlobalSettings.CheckBoxEnableSocksProxyEditingDone(Sender: TObject);
begin
  SpinEditSocksPort.Enabled := CheckBoxEnableSocksProxy.Checked;
end;

procedure TFormGlobalSettings.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  LoadSettings(Settings);
end;

procedure TFormGlobalSettings.FormCreate(Sender: TObject);
begin
  Settings := TProgramSettings.Create;
  if FileExists(SettingsJsonPath) then
    Settings.LoadFile(SettingsJsonPath);
  LoadSettings(Settings);
end;

procedure TFormGlobalSettings.LoadSettings(Settings: TProgramSettings);
begin
  CheckBoxEnableHTTPProxy.Checked := Settings.EnableHTTPProxy;
  CheckBoxEnableSocksProxy.Checked := Settings.EnableSocksProxy;
  SpinEditSocksPort.Value := Settings.SocksProxyPort;
  SpinEditHTTPPort.Value := Settings.HTTPProxyPort;
  EditV2rayPath.Text := Settings.V2rayBinaryPath;
  EditV2rayAssetsPath.Text := Settings.V2rayAssetsPath;
  ComboBoxLogLevel.ItemIndex := integer(Settings.V2rayLogLevel);
  RouteListDirect := Settings.Routes[1];
  RouteListProxy := Settings.Routes[2];
  RouteListDeny := Settings.Routes[3];
  SpinEditKCPMTU.Value := Settings.KCPMTU;
  SpinEditKCPTTI.Value := Settings.KCPTTI;
  SpinEditUplinkCapacity.Value := Settings.KCPUplinkCapacity;
  SpinEditDownlinkCapacity.Value := Settings.KCPDownlinkCapacity;
  SpinEditReadBufferSize.Value := Settings.KCPReadBufferSize;
  SpinEditWriteBufferSize.Value := Settings.KCPWriteBufferSize;
  CheckBoxCongestion.Checked := Settings.KCPCongestionAlgorithm;
  ComboBoxKCPHeaderType.ItemIndex := integer(Settings.KCPHeaderType);
  EditDnsServers.Text := Settings.DNSServers;
  ComboBoxDomainStrategy.ItemIndex := integer(Settings.DomainStrategy);
  CheckBoxEnableHTTPProxyEditingDone(nil);
  CheckBoxEnableSocksProxyEditingDone(nil);
  PageControlGlobalSettings.ActivePageIndex := 0;
  MemoRuleList.Text := RouteListDirect;
end;

function TFormGlobalSettings.SaveSettings(Settings: TProgramSettings): boolean;
begin
  if EditV2rayPath.Text = '' then
    Result := False
  else if (SpinEditKCPMTU.Value < 576) or (SpinEditKCPMTU.Value > 1460) then
    Result := False
  else if (SpinEditKCPTTI.Value < 10) or (SpinEditKCPTTI.Value > 100) then
    Result := False
  else if (ComboBoxKCPHeaderType.ItemIndex < 0) or
    (ComboBoxKCPHeaderType.ItemIndex > 5) then
    Result := False
  else if (ComboBoxDomainStrategy.ItemIndex < 0) or
    (ComboBoxDomainStrategy.ItemIndex > 2) then
    Result := False
  else if (SpinEditSocksPort.Value < 1) or (SpinEditSocksPort.Value > 65534) or
    (SpinEditHTTPPort.Value < 1) or (SpinEditHTTPPort.Value > 65534) then
    Result := False
  else
  begin
    Settings.EnableHTTPProxy := CheckBoxEnableHTTPProxy.Checked;
    Settings.EnableSocksProxy := CheckBoxEnableSocksProxy.Checked;
    Settings.SocksProxyPort := SpinEditSocksPort.Value;
    Settings.HTTPProxyPort := SpinEditHTTPPort.Value;
    Settings.V2rayBinaryPath := EditV2rayPath.Text;
    Settings.V2rayAssetsPath := EditV2rayAssetsPath.Text;
    Settings.V2rayLogLevel := TV2rayLogLevel(ComboBoxLogLevel.ItemIndex);
    Settings.Routes[1] := RouteListDirect;
    Settings.Routes[2] := RouteListProxy;
    Settings.Routes[3] := RouteListDeny;
    Settings.KCPMTU := SpinEditKCPMTU.Value;
    Settings.KCPTTI := SpinEditKCPTTI.Value;
    Settings.KCPUplinkCapacity := SpinEditUplinkCapacity.Value;
    Settings.KCPDownlinkCapacity := SpinEditDownlinkCapacity.Value;
    Settings.KCPReadBufferSize := SpinEditReadBufferSize.Value;
    Settings.KCPWriteBufferSize := SpinEditWriteBufferSize.Value;
    Settings.KCPCongestionAlgorithm := CheckBoxCongestion.Checked;
    Settings.KCPHeaderType := TKCPHeaderType(ComboBoxKCPHeaderType.ItemIndex);
    Settings.DNSServers := EditDnsServers.Text;
    Settings.DomainStrategy := TRouteDomainStrategy(ComboBoxDomainStrategy.ItemIndex);
    Result := True;
  end;
end;

procedure TFormGlobalSettings.ButtonCancelClick(Sender: TObject);
begin
  FormGlobalSettings.Close;
end;

procedure TFormGlobalSettings.ButtonFindV2rayAssetsClick(Sender: TObject);
begin
  SelectDirectoryDialogFindV2rayAssets.InitialDir := EditV2rayAssetsPath.Text;
  if SelectDirectoryDialogFindV2rayAssets.Execute then
  begin
    EditV2rayAssetsPath.Text := SelectDirectoryDialogFindV2rayAssets.FileName;
  end;
end;

procedure TFormGlobalSettings.TabControlRouteSettingChange(Sender: TObject);
begin
  case TabControlRouteSetting.TabIndex of
    0: MemoRuleList.Text := RouteListDirect;
    1: MemoRuleList.Text := RouteListProxy;
    2: MemoRuleList.Text := RouteListDeny;
  end;
end;

end.

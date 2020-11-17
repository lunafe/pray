unit GlobalSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, Forms, Controls, Dialogs, ComCtrls, StdCtrls,
  Spin, Menus, V2rayJsonConfig, ProgramSettings;

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
    CheckBoxTLSAllowInsecure: TCheckBox;
    CheckBoxEnableMux: TCheckBox;
    CheckBoxCongestion: TCheckBox;
    CheckBoxEnableHTTPProxy: TCheckBox;
    CheckBoxEnableSocksProxy: TCheckBox;
    ComboBoxLogLevel: TComboBox;
    ComboBoxDomainStrategy: TComboBox;
    EditV2rayAssetsPath: TEdit;
    EditDnsServers: TEdit;
    EditV2rayPath: TEdit;
    GroupBoxOthers: TGroupBox;
    GroupBoxDNSServers: TGroupBox;
    GroupBoxKCPSettings: TGroupBox;
    GroupBoxV2rayConfig: TGroupBox;
    GroupBoxInbound: TGroupBox;
    LabelMuxConcurrency: TLabel;
    LabelLogLevel: TLabel;
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
    SpinEditMuxConcurrency: TSpinEdit;
    SpinEditWriteBufferSize: TSpinEdit;
    SpinEditReadBufferSize: TSpinEdit;
    SpinEditDownlinkCapacity: TSpinEdit;
    SpinEditUplinkCapacity: TSpinEdit;
    SpinEditKCPTTI: TSpinEdit;
    SpinEditKCPMTU: TSpinEdit;
    SpinEditHTTPPort: TSpinEdit;
    SpinEditSocksPort: TSpinEdit;
    SQLQueryUpdateSettings: TSQLQuery;
    SQLQueryReadSettings: TSQLQuery;
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
    procedure CheckBoxEnableMuxEditingDone(Sender: TObject);
    procedure CheckBoxEnableSocksProxyEditingDone(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure LoadSettings(Settings: TProgramSettings);
    procedure TabControlRouteSettingChange(Sender: TObject);
    procedure UpdateSQL(SettingName: string; SettingValue: string);
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
uses
  PrayMainUnit;

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
  SaveSettings(Settings);
  FormGlobalSettings.Close;
end;

procedure TFormGlobalSettings.CheckBoxEnableHTTPProxyEditingDone(Sender: TObject);
begin
  SpinEditHTTPPort.Enabled := CheckBoxEnableHTTPProxy.Checked;
end;

procedure TFormGlobalSettings.CheckBoxEnableMuxEditingDone(Sender: TObject);
begin
  SpinEditMuxConcurrency.Enabled := CheckBoxEnableMux.Checked;
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
var
  ValueString: string;
begin
  Settings := TProgramSettings.Create;
  with SQLQueryReadSettings, Settings do
  begin
    Open;
    while not EOF do
    begin
      ValueString := FieldByName('value').AsString;
      case FieldByName('name').AsString of
        'http_enabled': EnableHTTPProxy := ValueString.ToBoolean;
        'socks_enabled': EnableSocksProxy := ValueString.ToBoolean;
        'http_port': HTTPProxyPort := ValueString.ToInteger;
        'socks_port': SocksProxyPort := ValueString.ToInteger;
        'v2ray_binary': V2rayBinaryPath := ValueString;
        'v2ray_assets': V2rayAssetsPath := ValueString;
        'loglevel': V2rayLogLevel := TV2rayLogLevel(ValueString.ToInteger);
        'route_direct': Routes[1] := ValueString;
        'route_proxy': Routes[2] := ValueString;
        'route_reject': Routes[3] := ValueString;
        'kcp_mtu': KCPMTU := ValueString.ToInteger;
        'kcp_tti': KCPTTI := ValueString.ToInteger;
        'kcp_upcap': KCPUplinkCapacity := ValueString.ToInteger;
        'kcp_downcap': KCPDownlinkCapacity := ValueString.ToInteger;
        'kcp_rbufsize': KCPReadBufferSize := ValueString.ToInteger;
        'kcp_wbufsize': KCPWriteBufferSize := ValueString.ToInteger;
        'kcp_congestion': KCPCongestionAlgorithm := ValueString.ToBoolean;
        'route_strategy': DomainStrategy := TRouteDomainStrategy(ValueString.ToInteger);
        'dns': DNSServers := ValueString;
        'mux_enabled': MuxEnabled := ValueString.ToBoolean;
        'mux_concurrency': MuxConcurrency := ValueString.ToInteger;
        'tls_allowinsecure': TLSAllowInsecure := ValueString.ToBoolean;
      end;
      Next;
    end;
  end;
  LoadSettings(Settings);
end;

procedure TFormGlobalSettings.UpdateSQL(SettingName: string; SettingValue: string);
begin
  with SQLQueryUpdateSettings do
  begin
    ParamByName('name').AsString := SettingName;
    ParamByName('value').AsString := SettingValue;
    ExecSQL;
  end;
end;

procedure TFormGlobalSettings.LoadSettings(Settings: TProgramSettings);
begin
  with Settings do
  begin
    CheckBoxEnableHTTPProxy.Checked := EnableHTTPProxy;
    CheckBoxEnableSocksProxy.Checked := EnableSocksProxy;
    SpinEditHTTPPort.Value := HTTPProxyPort;
    SpinEditSocksPort.Value := SocksProxyPort;
    EditV2rayPath.Text := V2rayBinaryPath;
    EditV2rayAssetsPath.Text := V2rayAssetsPath;
    ComboBoxLogLevel.ItemIndex := integer(V2rayLogLevel);
    RouteListDirect := Routes[1];
    RouteListProxy := Routes[2];
    RouteListDeny := Routes[3];
    SpinEditKCPMTU.Value := KCPMTU;
    SpinEditKCPTTI.Value := KCPTTI;
    SpinEditUplinkCapacity.Value := KCPUplinkCapacity;
    SpinEditDownlinkCapacity.Value := KCPDownlinkCapacity;
    SpinEditReadBufferSize.Value := KCPReadBufferSize;
    SpinEditWriteBufferSize.Value := KCPWriteBufferSize;
    CheckBoxCongestion.Checked := KCPCongestionAlgorithm;
    EditDnsServers.Text := DNSServers;
    ComboBoxDomainStrategy.ItemIndex := integer(DomainStrategy);
    CheckBoxEnableMux.Checked := MuxEnabled;
    SpinEditMuxConcurrency.Value := MuxConcurrency;
    CheckBoxTLSAllowInsecure.Checked := TLSAllowInsecure;
    CheckBoxEnableHTTPProxyEditingDone(nil);
    CheckBoxEnableSocksProxyEditingDone(nil);
    CheckBoxEnableMuxEditingDone(nil);
    PageControlGlobalSettings.ActivePageIndex := 0;
    TabControlRouteSetting.TabIndex := 0;
    MemoRuleList.Text := RouteListDirect;
  end;
end;

function TFormGlobalSettings.SaveSettings(Settings: TProgramSettings): boolean;
begin
  if EditV2rayPath.Text = '' then
    Result := False
  else if (SpinEditKCPMTU.Value < 576) or (SpinEditKCPMTU.Value > 1460) then
    Result := False
  else if (SpinEditKCPTTI.Value < 10) or (SpinEditKCPTTI.Value > 100) then
    Result := False
  else if (ComboBoxDomainStrategy.ItemIndex < 0) or
    (ComboBoxDomainStrategy.ItemIndex > 2) then
    Result := False
  else if (SpinEditSocksPort.Value < 1) or (SpinEditSocksPort.Value > 65534) or
    (SpinEditHTTPPort.Value < 1) or (SpinEditHTTPPort.Value > 65534) then
    Result := False
  else
  begin
    with Settings do
    begin
      if EnableHTTPProxy <> CheckBoxEnableHTTPProxy.Checked then
      begin
        EnableHTTPProxy := CheckBoxEnableHTTPProxy.Checked;
        UpdateSQL('http_enabled', EnableHTTPProxy.ToString);
      end;
      if EnableSocksProxy <> CheckBoxEnableSocksProxy.Checked then
      begin
        EnableSocksProxy := CheckBoxEnableSocksProxy.Checked;
        UpdateSQL('socks_enabled', EnableSocksProxy.ToString);
      end;
      if HTTPProxyPort <> SpinEditHTTPPort.Value then
      begin
        HTTPProxyPort := SpinEditHTTPPort.Value;
        UpdateSQL('http_port', HTTPProxyPort.ToString);
      end;
      if SocksProxyPort <> SpinEditSocksPort.Value then
      begin
        SocksProxyPort := SpinEditSocksPort.Value;
        UpdateSQL('socks_port', SocksProxyPort.ToString);
      end;
      if V2rayBinaryPath <> EditV2rayPath.Text then
      begin
        V2rayBinaryPath := EditV2rayPath.Text;
        UpdateSQL('v2ray_binary', V2rayBinaryPath);
      end;
      if V2rayAssetsPath <> EditV2rayAssetsPath.Text then
      begin
        V2rayAssetsPath := EditV2rayAssetsPath.Text;
        UpdateSQL('v2ray_assets', V2rayAssetsPath);
      end;
      if V2rayLogLevel <> TV2rayLogLevel(ComboBoxLogLevel.ItemIndex) then
      begin
        V2rayLogLevel := TV2rayLogLevel(ComboBoxLogLevel.ItemIndex);
        UpdateSQL('loglevel', ComboBoxLogLevel.ItemIndex.ToString);
      end;
      if Routes[1] <> RouteListDirect then
      begin
        Routes[1] := RouteListDirect;
        UpdateSQL('route_direct', RouteListDirect);
      end;
      if Routes[2] <> RouteListProxy then
      begin
        Routes[2] := RouteListProxy;
        UpdateSQL('route_proxy', RouteListProxy);
      end;
      if Routes[3] <> RouteListDeny then
      begin
        Routes[3] := RouteListDeny;
        UpdateSQL('route_reject', RouteListDeny);
      end;
      if KCPMTU <> SpinEditKCPMTU.Value then
      begin
        KCPMTU := SpinEditKCPMTU.Value;
        UpdateSQL('kcp_mtu', KCPMTU.ToString);
      end;
      if KCPTTI <> SpinEditKCPTTI.Value then
      begin
        KCPTTI := SpinEditKCPTTI.Value;
        UpdateSQL('kcp_tti', KCPTTI.ToString);
      end;
      if KCPUplinkCapacity <> SpinEditUplinkCapacity.Value then
      begin
        KCPUplinkCapacity := SpinEditUplinkCapacity.Value;
        UpdateSQL('kcp_upcap', KCPUplinkCapacity.ToString);
      end;
      if KCPDownlinkCapacity <> SpinEditDownlinkCapacity.Value then
      begin
        KCPDownlinkCapacity := SpinEditDownlinkCapacity.Value;
        UpdateSQL('kcp_downcap', KCPDownlinkCapacity.ToString);
      end;
      if KCPReadBufferSize <> SpinEditReadBufferSize.Value then
      begin
        KCPReadBufferSize := SpinEditReadBufferSize.Value;
        UpdateSQL('kcp_rbufsize', KCPReadBufferSize.ToString);
      end;
      if KCPWriteBufferSize <> SpinEditWriteBufferSize.Value then
      begin
        KCPWriteBufferSize := SpinEditWriteBufferSize.Value;
        UpdateSQL('kcp_wbufsize', KCPWriteBufferSize.ToString);
      end;
      if TLSAllowInsecure <> CheckBoxTLSAllowInsecure.Checked then
      begin
        TLSAllowInsecure := CheckBoxTLSAllowInsecure.Checked;
        UpdateSQL('tls_allowinsecure', KCPWriteBufferSize.ToString);
      end;
      if DNSServers <> EditDnsServers.Text then
      begin
        DNSServers := EditDnsServers.Text;
        UpdateSQL('dns', DNSServers);
      end;
      if DomainStrategy <> TRouteDomainStrategy(ComboBoxDomainStrategy.ItemIndex) then
      begin
        DomainStrategy := TRouteDomainStrategy(ComboBoxDomainStrategy.ItemIndex);
        UpdateSQL('route_strategy', ComboBoxDomainStrategy.ItemIndex.ToString);
      end;
      if MuxEnabled <> CheckBoxEnableMux.Checked then
      begin
        MuxEnabled := CheckBoxEnableMux.Checked;
        UpdateSQL('mux_enabled', MuxEnabled.ToString);
      end;
      if MuxConcurrency <> SpinEditMuxConcurrency.Value then
      begin
        MuxConcurrency := SpinEditMuxConcurrency.Value;
        UpdateSQL('mux_concurrency', MuxConcurrency.ToString);
      end;
    end;
    PrayMainWindow.SQLTransactionPrayDB.Commit;
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

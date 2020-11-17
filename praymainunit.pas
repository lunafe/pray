unit PrayMainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, FGL,
  Buttons, ComCtrls, FPJson, Process, SQLDB, SQLite3Conn, Profile,
  V2rayJsonConfig;

type
  TIntegerList = specialize TFPGList<Integer>;
  TV2rayWatchThread = class(TThread)
    constructor Create(CreateSuspended: boolean; V2rayProcess: TProcess; AProfileName: string = '');
  protected
    procedure Execute; override;
  private
    Content: string;
    V2Process: TProcess;
    LineNum: word;
    ProfileName: string;
    procedure UpdateUI;
    procedure ProcessStoped;
  end;

  { TPrayMainWindow }

  TPrayMainWindow = class(TForm)
    BitBtnDisconnect: TBitBtn;
    BitBtnConnect: TBitBtn;
    ButtonImport: TButton;
    ButtonShareLink: TButton;
    ButtonGlobalSettings: TButton;
    ButtonAddProfile: TButton;
    ButtonRemoveProfile: TButton;
    ButtonEditProfile: TButton;
    ListBoxProfiles: TListBox;
    MemoV2rayOutput: TMemo;
    MemoServerInfo: TMemo;
    PanelMainPanel: TPanel;
    SQLConnectorPrayDB: TSQLConnector;
    SQLQueryDeleteProfile: TSQLQuery;
    SQLQueryUpdateProfile: TSQLQuery;
    SQLQueryCreateProfile: TSQLQuery;
    SQLQueryGetProfileDetails: TSQLQuery;
    SQLQueryFetchProfiles: TSQLQuery;
    SQLQueryDBVersion: TSQLQuery;
    SQLScriptInitDatabase: TSQLScript;
    SQLTransactionPrayDB: TSQLTransaction;
    StatusBarConnectionStatus: TStatusBar;
    TrayIconRunningIcon: TTrayIcon;
    procedure BitBtnConnectClick(Sender: TObject);
    procedure BitBtnDisconnectClick(Sender: TObject);
    procedure ButtonAddProfileClick(Sender: TObject);
    procedure ButtonEditProfileClick(Sender: TObject);
    procedure ButtonGlobalSettingsClick(Sender: TObject);
    procedure ButtonImportClick(Sender: TObject);
    procedure ButtonRemoveProfileClick(Sender: TObject);
    procedure ButtonShareLinkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure ListBoxProfilesSelectionChange(Sender: TObject; User: boolean);
    procedure LoadProfiles;
    procedure SaveProfile(Profile: TProfile; ProfileID: integer = 0);
    procedure TrayIconRunningIconClick(Sender: TObject);
  private
    V2RayProcess: TProcess;
    V2Thread: TV2rayWatchThread;
    CurrentProfile: TProfile;
    procedure IfProfileSelect(ASelected: boolean);
  end;

var
  ProfileIDList: TIntegerList;
  PrayMainWindow: TPrayMainWindow;
  ApplicationRootDirectory: string;
  TemporaryDirectory: string;
  ProfileJsonPath: string;
  GeneratedJsonPath: string;
  DatabaseFilePath: string;

implementation

uses GlobalSettings, ProfileEditor, ShareLinkForm, ImportLinksForm;

constructor TV2rayWatchThread.Create(CreateSuspended: boolean; V2rayProcess: TProcess; AProfileName: string = '');
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
  V2Process := V2rayProcess;
  LineNum := 0;
  ProfileName := AProfileName;
end;

procedure TV2rayWatchThread.Execute;
var
  B: char;
begin
  Content := '';
  B := #10;
  while (not Terminated) and V2Process.Running do
  begin
    try
      V2Process.Output.ReadBuffer(B, 1);
      Content := Content + B;
      if B = #10 then
      begin
        Synchronize(@UpdateUI);
        Content := '';
      end;
    except
      on EReadError do
        Synchronize(@ProcessStoped);
    end;
  end;
end;

procedure TV2rayWatchThread.UpdateUI;
begin
  with PrayMainWindow do
  begin
    if LineNum < 1000 then
      Inc(LineNum)
    else
      MemoV2rayOutput.Lines.Delete(0);
    MemoV2rayOutput.SelStart := Length(MemoV2rayOutput.Text);
    MemoV2rayOutput.Lines.Add(Content.Trim);
    StatusBarConnectionStatus.SimpleText := 'Connected ' + ProfileName;
  end;
end;

procedure TV2rayWatchThread.ProcessStoped;
begin
  if not V2Process.Running then
  begin
    PrayMainWindow.StatusBarConnectionStatus.SimpleText := 'Disconnected';
    Terminate;
  end;
end;

{$R *.lfm}

procedure TPrayMainWindow.ButtonAddProfileClick(Sender: TObject);
var
  P: TProfile;
begin
  P := TProfile.Create;
  FormEditProfile.ApplyProfile(P);
  FormEditProfile.ShowModal;
  if FormEditProfile.SaveAfterExit then
  begin
    SaveProfile(P);
    SQLTransactionPrayDB.Commit;
    LoadProfiles;
  end;
end;

procedure TPrayMainWindow.BitBtnConnectClick(Sender: TObject);
var
  F: TFileStream;
  S: string;
  I: integer;
begin
  if ListBoxProfiles.ItemIndex <> -1 then
  begin
    F := TFileStream.Create(GeneratedJsonPath, fmCreate);
    S := CurrentProfile.CreateJSON(Settings).FormatJSON;
    F.WriteBuffer(Pointer(S)^, Length(S));
    F.Free;
    BitBtnDisconnectClick(nil);
    MemoV2rayOutput.Lines.Clear;
    MemoV2rayOutput.Lines.Add('! starting v2ray.');
    V2RayProcess := TProcess.Create(nil);
    with V2RayProcess do
    begin
      Executable := Settings.V2rayBinaryPath;
      Parameters.Clear;
      Parameters.Add('-config');
      Parameters.Add(GeneratedJsonPath);
      for I := 1 to GetEnvironmentVariableCount do
        Environment.Add(GetEnvironmentString(I));
      Environment.Add(Format('V2RAY_LOCATION_ASSET=%s', [Settings.V2rayAssetsPath]));
      Options := [poNoConsole, poStderrToOutPut, poUsePipes];
      Execute;
    end;
    V2Thread := TV2rayWatchThread.Create(True, V2RayProcess, CurrentProfile.Name);
    V2Thread.Start;
  end;
end;

procedure TPrayMainWindow.BitBtnDisconnectClick(Sender: TObject);
begin
  if Assigned(V2RayProcess) then
  begin
    MemoV2rayOutput.Lines.Add('! stoping v2ray.');
    try
      V2RayProcess.Terminate(0);
    finally
    end;
  end;
  if Assigned(V2Thread) then
    try
      V2Thread.Terminate;
    finally
    end;
end;

procedure TPrayMainWindow.ButtonEditProfileClick(Sender: TObject);
begin
  if ListBoxProfiles.ItemIndex <> -1 then
  begin
    FormEditProfile.ApplyProfile(CurrentProfile);
    FormEditProfile.ShowModal;
    if FormEditProfile.SaveAfterExit then
    begin
      SaveProfile(CurrentProfile, ProfileIDList[ListBoxProfiles.ItemIndex]);
      SQLTransactionPrayDB.Commit;
      ListBoxProfiles.Items[ListBoxProfiles.ItemIndex] := CurrentProfile.Name;
    end;
    ListBoxProfilesSelectionChange(nil, False);
  end;
end;

procedure TPrayMainWindow.ButtonGlobalSettingsClick(Sender: TObject);
begin
  FormGlobalSettings.Show;
end;

procedure TPrayMainWindow.ButtonImportClick(Sender: TObject);
var
  P: Pointer;
begin
  with FormImportLinks do
  begin
    Refresh;
    ShowModal;
    if SaveAfterExit then
    begin
      for P in ReadyProfileList do SaveProfile(TProfile(P));
      SQLTransactionPrayDB.Commit;
      LoadProfiles;
    end;
  end;
end;

procedure TPrayMainWindow.ButtonRemoveProfileClick(Sender: TObject);
var
  I: integer;
begin
  I := ListBoxProfiles.ItemIndex;
  if I <> -1 then
  if MessageDlg('Confirm remove', 'Delete selected profiles?',
      mtWarning, mbYesNo, 0, mbNo) = mrYes then
  begin
    //ListBoxProfiles.DeleteSelected;
    for I := ListBoxProfiles.Items.Count - 1 downto 0 do
    begin
      if ListBoxProfiles.Selected[I] then
      with SQLQueryDeleteProfile do begin
        ParamByName('id').AsInteger := ProfileIDList[I];
        ExecSQL;
        //ProfileIDList.Delete(I);
      end;
    end;
    SQLTransactionPrayDB.Commit;
    LoadProfiles;
    ListBoxProfilesSelectionChange(nil, False);
  end;
end;

procedure TPrayMainWindow.ButtonShareLinkClick(Sender: TObject);
begin
  if ListBoxProfiles.ItemIndex <> -1 then
  begin
    FormShareLink.ApplyProfile(CurrentProfile);
    FormShareLink.Show;
  end;
end;

procedure TPrayMainWindow.FormCreate(Sender: TObject);
var
  DBVersion: integer;
  DBNeedUpgrade: boolean;
begin
  DBVersion := 0;
  DBNeedUpgrade := True;
  ProfileIDList := TIntegerList.Create;
  SQLConnectorPrayDB.DatabaseName := DatabaseFilePath;
  try
    SQLQueryDBVersion.ParamByName('name').AsString := 'pray_dbversion';
    SQLQueryDBVersion.Open;
    DBVersion := SQLQueryDBVersion.FieldByName('value').AsString.ToInteger;
  except
    on E: Exception do
      SQLQueryDBVersion.Close;
  end;
  if DBVersion = 3 then DBNeedUpgrade := False;
  if DBVersion = 0 then DBVersion := 3
  else SQLScriptInitDatabase.Script.Clear;
  if DBVersion = 1 then with SQLScriptInitDatabase.Script do begin
    Add('ALTER TABLE `profiles` RENAME `tls_enabled` TO `stream_security`;');
    Add('ALTER TABLE `profiles` ADD `flow` TEXT;');
    DBVersion := 2;
  end;
  if DBVersion = 2 then with SQLScriptInitDatabase.Script do begin
    Add('INSERT INTO `settings` VALUES(''tls_allowinsecure'', ''0'');');
    DBVersion := 3;
  end;
  if DBNeedUpgrade then with SQLScriptInitDatabase do begin
    Script.Add('UPDATE `settings` SET `value`=''3'' WHERE `name`=''pray_dbversion'';');
    Execute;
    SQLTransactionPrayDB.Commit;
  end;
  LoadProfiles;
  ListBoxProfilesSelectionChange(nil, False);
end;

procedure TPrayMainWindow.FormDestroy(Sender: TObject);
begin
  BitBtnDisconnectClick(nil);
end;

procedure TPrayMainWindow.FormWindowStateChange(Sender: TObject);
begin
  if WindowState = wsMinimized then
  begin
    TrayIconRunningIcon.Show;
    Hide;
  end;
end;

procedure TPrayMainWindow.IfProfileSelect(ASelected: boolean);
var
  I: integer;
  V1, V2: string;
begin
  ButtonEditProfile.Enabled := ASelected;
  ButtonRemoveProfile.Enabled := ASelected;
  ButtonShareLink.Enabled := ASelected;
  BitBtnConnect.Enabled := ASelected;
  MemoServerInfo.Lines.Clear;
  if ASelected then with SQLQueryGetProfileDetails do
  begin
    I := ProfileIDList[ListBoxProfiles.ItemIndex];
    CurrentProfile := TProfile.Create;
    ParamByName('id').AsInteger := I;
    Open;
    with CurrentProfile do
    begin
      Name := FieldByName('name').AsString;
      Address := FieldByName('address').AsString;
      Port := FieldByName('port').AsInteger;
      Protocol := TRemoteProtocol(FieldByName('protocol').AsInteger);
      Flow := FieldByName('flow').AsString;
      Network := TRemoteTransport(FieldByName('network').AsInteger);
      StreamSecurity := TSecurityOptions(FieldByName('stream_security').AsInteger);
      Hostname := FieldByName('hostname').AsString;
      Path := FieldByName('path').AsString;
      UDPHeaderType := FieldByName('udp_header').AsString;
      QUICSecurity := FieldByName('quic_security').AsString;
      QUICKey := FieldByName('quic_key').AsString;
      V1 := FieldByName('protocol_value1').AsString;
      V2 := FieldByName('protocol_value2').AsString;
      case Protocol of
        rpVMESS:
          begin
            UUID := V1;
            AlterID := V2.ToInteger;
          end;
        rpSHADOWSOCKS:
          begin
            SSPassword := V1;
            SSMethod := V2;
          end;
        rpVLESS:
          begin
            VLESSID := V1;
            VLESSEncryption := V2;
          end;
        rpTROJAN:
          TrojanPassword := V1;
      end;
      with MemoServerInfo.Lines do
      begin
        Add(Format('Remote: %s:%d', [Address, Port]));
        Add(Format('Protocol: %s', [RemoteProtocolToString(Protocol)]));
        Add(Format('Network: %s', [TransportToString(Network)]));
      end;
    end;
    Close;
  end;
end;

procedure TPrayMainWindow.ListBoxProfilesSelectionChange(Sender: TObject;
  User: boolean);
begin
  MemoServerInfo.Lines.Clear;
  if ListBoxProfiles.ItemIndex = -1 then IfProfileSelect(False)
  else IfProfileSelect(True);
end;

procedure TPrayMainWindow.LoadProfiles;
begin
  ListBoxProfiles.Items.Clear;
  ProfileIDList.Clear;
  with SQLQueryFetchProfiles do
  begin
    Open;
    while not EOF do
    begin
      ListBoxProfiles.Items.Add(FieldByName('name').AsString);
      ProfileIDList.Add(FieldByName('id').AsInteger);
      Next;
    end;
    Close;
  end;
end;

procedure TPrayMainWindow.SaveProfile(Profile: TProfile; ProfileID: integer = 0);
var
  V1, V2: string;
begin
  with Profile do
  begin
    case Protocol of
      rpVMESS:
        begin
          V1 := UUID;
          V2 := AlterID.ToString;
        end;
      rpSHADOWSOCKS:
        begin
          V1 := SSPassword;
          V2 := SSMethod;
        end;
      rpVLESS:
        begin
          V1 := VLESSID;
          V2 := VLESSEncryption;
        end;
      rpTROJAN:
        begin
          V1 := TrojanPassword;
          V2 := ''
        end;
    end;
    if ProfileID < 1 then
    begin
      with SQLQueryCreateProfile do
      begin
        ParamByName('name').AsString := Profile.Name;
        ParamByName('address').AsString := Address;
        ParamByName('port').AsInteger := Port;
        ParamByName('protocol').AsInteger := integer(Protocol);
        ParamByName('network').AsInteger := integer(Network);
        ParamByName('pv1').AsString := V1;
        ParamByName('pv2').AsString := V2;
        ParamByName('flow').AsString := Flow;
        ParamByName('ssec').AsInteger := integer(StreamSecurity);
        ParamByName('hostname').AsString := Hostname;
        ParamByName('path').AsString := Path;
        ParamByName('udph').AsString := UDPHeaderType;
        ParamByName('quicsec').AsString := QUICSecurity;
        ParamByName('quickey').AsString := QUICKey;
        ExecSQL;
      end;
    end
    else
    begin
      with SQLQueryUpdateProfile do
      begin
        ParamByName('id').AsInteger := ProfileID;
        ParamByName('name').AsString := Profile.Name;
        ParamByName('address').AsString := Address;
        ParamByName('port').AsInteger := Port;
        ParamByName('protocol').AsInteger := integer(Protocol);
        ParamByName('network').AsInteger := integer(Network);
        ParamByName('pv1').AsString := V1;
        ParamByName('pv2').AsString := V2;
        ParamByName('flow').AsString := Flow;
        ParamByName('ssec').AsInteger := integer(StreamSecurity);
        ParamByName('hostname').AsString := Hostname;
        ParamByName('path').AsString := Path;
        ParamByName('udph').AsString := UDPHeaderType;
        ParamByName('quicsec').AsString := QUICSecurity;
        ParamByName('quickey').AsString := QUICKey;
        ExecSQL;
      end;
    end;
    //SQLTransactionPrayDB.Commit;
  end;
end;

procedure TPrayMainWindow.TrayIconRunningIconClick(Sender: TObject);
begin
  WindowState := wsNormal;
  TrayIconRunningIcon.Hide;
  Show;
end;

end.

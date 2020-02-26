unit PrayMainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, ComCtrls, FPJson, Process, Profile, V2rayJsonConfig;

type

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
    procedure SaveProfiles;
    procedure TrayIconRunningIconClick(Sender: TObject);
  private
    V2RayProcess: TProcess;
    V2Thread: TV2rayWatchThread;
    procedure IfProfileSelect(ASelected: boolean);
  end;

var
  ProfileList: TList;
  PrayMainWindow: TPrayMainWindow;
  ApplicationRootDirectory: string;
  TemporaryDirectory: string;
  ProfileJsonPath: string;
  GeneratedJsonPath: string;

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
    ProfileList.Add(P);
    ListBoxProfiles.Items.Add(P.Name);
    ListBoxProfilesSelectionChange(nil, False);
    SaveProfiles;
  end;
end;

procedure TPrayMainWindow.BitBtnConnectClick(Sender: TObject);
var
  F: TFileStream;
  P: TProfile;
  S: string;
  I: integer;
begin
  if ListBoxProfiles.ItemIndex <> -1 then
  begin
    F := TFileStream.Create(GeneratedJsonPath, fmCreate);
    P := TProfile(ProfileList[ListBoxProfiles.ItemIndex]);
    S := P.CreateJSON(Settings).FormatJSON;
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
    V2Thread := TV2rayWatchThread.Create(True, V2RayProcess, P.Name);
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
var
  P: TProfile;
begin
  if ListBoxProfiles.ItemIndex <> -1 then
  begin
    P := TProfile(ProfileList[ListBoxProfiles.ItemIndex]);
    FormEditProfile.ApplyProfile(P);
    FormEditProfile.ShowModal;
    if FormEditProfile.SaveAfterExit then
    begin
      ListBoxProfiles.Items[ListBoxProfiles.ItemIndex] := P.Name;
      ListBoxProfilesSelectionChange(nil, False);
      SaveProfiles;
    end;
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
      for P in ReadyProfileList do
      begin
        ProfileList.Add(P);
        ListBoxProfiles.Items.Add(TProfile(P).Name);
      end;
      ListBoxProfilesSelectionChange(nil, False);
      SaveProfiles;
    end;
  end;
end;

procedure TPrayMainWindow.ButtonRemoveProfileClick(Sender: TObject);
var
  I: integer;
begin
  I := ListBoxProfiles.ItemIndex;
  if I <> -1 then
  begin
    ListBoxProfiles.DeleteSelected;
    TProfile(ProfileList[I]).Free;
    ProfileList.Delete(I);
    ListBoxProfilesSelectionChange(nil, False);
    SaveProfiles;
  end;
end;

procedure TPrayMainWindow.ButtonShareLinkClick(Sender: TObject);
begin
  if ListBoxProfiles.ItemIndex <> -1 then
  begin
    FormShareLink.ApplyProfile(TProfile(ProfileList[ListBoxProfiles.ItemIndex]));
    FormShareLink.Show;
  end;
end;

procedure TPrayMainWindow.FormCreate(Sender: TObject);
begin
  ProfileList := TList.Create;
  if FileExists(ProfileJsonPath) then
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
  P: TProfile;
begin
  ButtonEditProfile.Enabled := ASelected;
  ButtonRemoveProfile.Enabled := ASelected;
  ButtonShareLink.Enabled := ASelected;
  BitBtnConnect.Enabled := ASelected;
  MemoServerInfo.Lines.Clear;
  if ASelected then
  begin
    I := ListBoxProfiles.ItemIndex;
    P := TProfile(ProfileList[I]);
    with MemoServerInfo.Lines do
    begin
      Add(Format('Remote: %s:%d', [P.Address, P.Port]));
      Add(Format('Protocol: %s', [RemoteProtocolToString(P.Protocol)]));
      Add(Format('Network: %s', [TransportToString(P.Network)]));
    end;
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
var
  F: TFileStream;
  S: string;
  J: TJSONArray;
  I: TJSONEnum;
  K: TJSONObject;
  P: TProfile;
begin
  F := TFileStream.Create(ProfileJsonPath, fmOpenRead);
  SetLength(S, F.Size);
  F.ReadBuffer(Pointer(S)^, Length(S));
  F.Free;
  J := TJSONArray(GetJSON(S));
  ListBoxProfiles.Items.Clear;
  ProfileList.Clear;
  for I in J do
  begin
    P := TProfile.Create;
    K := TJSONObject(I.Value);
    P.Name := K.Get('name', 'Unamed');
    P.Address := K.Get('addr', '0.0.0.0');
    P.Port := K.Get('port', 0);
    P.Protocol := TRemoteProtocol(K.Get('pc', 0));
    P.UUID := K.Get('id', '');
    P.AlterID := K.Get('aid', 0);
    P.SSPassword := K.Get('sp', '');
    P.SSMethod := TShadowsocksEncryption(K.Get('sm', 2));
    P.Network := TRemoteTransport(K.Get('net', 0));
    P.EnableTLS := K.Get('tls', False);
    P.Hostname := K.Get('host', '');
    P.Path := K.Get('path', '');
    P.UDPHeaderType := TUDPHeaderType(K.Get('udph', 0));
    P.QUICSecurity := TQUICSecurity(K.Get('qs', 0));
    P.QUICKey := K.Get('qk', '');
    ListBoxProfiles.Items.Add(P.Name);
    ProfileList.Add(P);
  end;
end;

procedure TPrayMainWindow.SaveProfiles;
var
  F: TFileStream;
  J: TJSONArray;
  I: Pointer;
  P: TProfile;
  S: string;
begin
  F := TFileStream.Create(ProfileJsonPath, fmCreate);
  J := TJSONArray.Create();
  for I in ProfileList do
  begin
    P := TProfile(I);
    J.Add(TJSONObject.Create([
      'name', P.Name,
      'addr', P.Address,
      'port',  P.Port,
      'pc', integer(P.Protocol),
      'id', P.UUID,
      'aid', P.AlterID,
      'sp', P.SSPassword,
      'sm', integer(P.SSMethod),
      'net', integer(P.Network),
      'tls', P.EnableTLS,
      'host', P.Hostname,
      'path', P.Path,
      'udph', integer(P.UDPHeaderType),
      'qs', integer(P.QUICSecurity),
      'qk', P.QUICKey]));
  end;
  S := J.AsJSON;
  F.WriteBuffer(Pointer(S)^, Length(S));
  F.Free;
  J.Free;
end;

procedure TPrayMainWindow.TrayIconRunningIconClick(Sender: TObject);
begin
  WindowState := wsNormal;
  TrayIconRunningIcon.Hide;
  Show;
end;

end.

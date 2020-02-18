unit PrayMainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, ComCtrls, FPJson, Process, Profile,
  GlobalSettings, ProfileEditor, V2rayJsonConfig;

type

  TV2rayWatchThread = class(TThread)
    constructor Create(CreateSuspended: boolean; V2rayProcess: TProcess);
  protected
    procedure Execute; override;
  private
    Content: string;
    V2Process: TProcess;
    LineNum: word;
    procedure UpdateUI;
    procedure ProcessStoped;
  end;

  { TPrayMainWindow }

  TPrayMainWindow = class(TForm)
    BitBtnDisconnect: TBitBtn;
    BitBtnConnect: TBitBtn;
    ButtonGlobalSettings: TButton;
    ButtonAddProfile: TButton;
    ButtonRemoveProfile: TButton;
    ButtonEditProfile: TButton;
    ListBoxProfiles: TListBox;
    MemoV2rayOutput: TMemo;
    MemoServerInfo: TMemo;
    PanelMainPanel: TPanel;
    StatusBarConnectionStatus: TStatusBar;
    procedure BitBtnConnectClick(Sender: TObject);
    procedure BitBtnDisconnectClick(Sender: TObject);
    procedure ButtonAddProfileClick(Sender: TObject);
    procedure ButtonEditProfileClick(Sender: TObject);
    procedure ButtonGlobalSettingsClick(Sender: TObject);
    procedure ButtonRemoveProfileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBoxProfilesSelectionChange(Sender: TObject; User: boolean);
    procedure LoadProfiles;
    procedure SaveProfiles;
  private
    V2RayProcess: TProcess;
    V2Thread: TV2rayWatchThread;
  end;

var
  ProfileList: TList;
  PrayMainWindow: TPrayMainWindow;
  ApplicationRootDirectory: string;
  TemporaryDirectory: string;
  ProfileJsonPath: string;
  GeneratedJsonPath: string;

implementation

constructor TV2rayWatchThread.Create(CreateSuspended: boolean; V2rayProcess: TProcess);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
  V2Process := V2rayProcess;
  LineNum := 0;
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
  if LineNum < 1000 then
    Inc(LineNum)
  else
    PrayMainWindow.MemoV2rayOutput.Lines.Delete(0);
  PrayMainWindow.MemoV2rayOutput.SelStart := Length(PrayMainWindow.MemoV2rayOutput.Text);
  PrayMainWindow.MemoV2rayOutput.Lines.Add(Content.Trim);
end;

procedure TV2rayWatchThread.ProcessStoped;
begin
  PrayMainWindow.MemoV2rayOutput.Lines.Add('! v2ray process stoped.');
  PrayMainWindow.StatusBarConnectionStatus.SimpleText := 'Disconnected';
  Terminate;
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
    V2Thread := TV2rayWatchThread.Create(True, V2RayProcess);
    V2Thread.Start;
    StatusBarConnectionStatus.SimpleText := 'Connected';
  end;
end;

procedure TPrayMainWindow.BitBtnDisconnectClick(Sender: TObject);
begin
  if Assigned(V2RayProcess) then
    try
      V2RayProcess.Terminate(0);
    finally
    end;
  if Assigned(V2Thread) then
    try
      V2Thread.Terminate;
    finally
    end;
  StatusBarConnectionStatus.SimpleText := 'Disconnected';
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

procedure TPrayMainWindow.FormCreate(Sender: TObject);
begin
  ProfileList := TList.Create;
  if FileExists(ProfileJsonPath) then
    LoadProfiles;
end;

procedure TPrayMainWindow.FormDestroy(Sender: TObject);
begin
  BitBtnDisconnectClick(nil);
end;

procedure TPrayMainWindow.ListBoxProfilesSelectionChange(Sender: TObject;
  User: boolean);
var
  P: TProfile;
  I: integer;
begin
  I := ListBoxProfiles.ItemIndex;
  if I <> -1 then
  begin
    P := TProfile(ProfileList[I]);
    MemoServerInfo.Lines := TStringList.Create;
    MemoServerInfo.Text := Format('Remote: %s:%d', [P.Address, P.Port]);
  end;
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
    P.UUID := K.Get('id', '');
    P.AlterID := K.Get('aid', 0);
    P.Network := TRemoteTransport(K.Get('net', 0));
    P.EnableTLS := K.Get('tls', False);
    P.Hostname := K.Get('host', '');
    P.Path := K.Get('path', '');
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
    J.Add(TJSONObject.Create(['name', P.Name, 'addr', P.Address, 'port',
      P.Port, 'id', P.UUID, 'aid', P.AlterID, 'net', integer(P.Network),
      'tls', P.EnableTLS, 'host', P.Hostname, 'path', P.Path]));
  end;
  S := J.AsJSON;
  F.WriteBuffer(Pointer(S)^, Length(S));
  F.Free;
  J.Free;
end;

end.

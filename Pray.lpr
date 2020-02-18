program Pray;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, SysUtils, PrayMainUnit, Config, GlobalSettings, V2rayJsonConfig, ProfileEditor,
  ProgramSettings, Profile;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  ApplicationRootDirectory := ExtractFileDir(Application.ExeName);
  TemporaryDirectory := GetTempDir;
  if TemporaryDirectory = '' then
    TemporaryDirectory := ApplicationRootDirectory;
  ProfileJsonPath := ConcatPaths([ApplicationRootDirectory, 'prayprofiles.json']);
  SettingsJsonPath := ConcatPaths([ApplicationRootDirectory, 'praysettings.json']);
  GeneratedJsonPath := ConcatPaths([TemporaryDirectory, 'praygenerate.json']);
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TPrayMainWindow, PrayMainWindow);
  Application.CreateForm(TFormGlobalSettings, FormGlobalSettings);
  Application.CreateForm(TFormEditProfile, FormEditProfile);
  Application.Run;
end.


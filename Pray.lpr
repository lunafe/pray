program Pray;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, SysUtils, PrayMainUnit, GlobalSettings, V2rayJsonConfig, ProfileEditor,
  ProgramSettings, Profile, ShareLinkForm, ImportLinksForm;

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
  DatabaseFilePath := ConcatPaths([ApplicationRootDirectory, 'pray_data.db']);
  Application.Scaled:=True;
  Application.Title:='Pray';
  Application.Initialize;
  Application.CreateForm(TPrayMainWindow, PrayMainWindow);
  Application.CreateForm(TFormGlobalSettings, FormGlobalSettings);
  Application.CreateForm(TFormEditProfile, FormEditProfile);
  Application.CreateForm(TFormShareLink, FormShareLink);
  Application.CreateForm(TFormImportLinks, FormImportLinks);
  Application.Run;
end.


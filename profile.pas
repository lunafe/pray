unit Profile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPJson, V2rayJsonConfig, ProgramSettings;

type
  TProfile = class
    Name: string;
    Address: string;
    Port: word;
    UUID: string;
    AlterID: word;
    Network: TRemoteTransport;
    Hostname: string;
    Path: string;
    constructor Create;
    function CreateJSON(Settings: TProgramSettings): TJSONObject;
  end;

implementation

constructor TProfile.Create;
var
  Y, M, D: word;
begin
  DecodeDate(Date, Y, M, D);
  Name:=Format('%d%0.2d%0.2d',[Y,M,D]);
  Address:='0.0.0.0';
  Port:=2020;
  UUID:='00000000-1111-2222-3333-444444444444';
  AlterID:=64;
  Network:=rtTCP;
  Hostname:='';
  Path:='';
end;

function TProfile.CreateJSON(Settings: TProgramSettings): TJSONObject;
var
  C: TV2rayJsonConfig;
begin
  C:=TV2rayJsonConfig.Create(Address, Port);
  C.SetUser(UUID, AlterID);
  if Settings.EnableSocksProxy then C.SetSocksProxy(Settings.SocksProxyPort);
  if Settings.EnableHTTPProxy then C.SetHTTPProxy(Settings.HTTPProxyPort);
  C.SetRoute(rlDIRECT, CommaStringList(Settings.Routes[1]));
  C.SetRoute(rlPROXY, CommaStringList(Settings.Routes[2]));
  C.SetRoute(rlDENY, CommaStringList(Settings.Routes[3]));
  C.SetTransport(Network, Hostname, Path);
  C.SetLogLevel(Settings.V2rayLogLevel);
  if Network = rtKCP then
  begin
    C.KCPMTU:=Settings.KCPMTU;
    C.KCPTTI:=Settings.KCPTTI;
    C.KCPDownlinkCapacity:=Settings.KCPDownlinkCapacity;
    C.KCPUplinkCapacity:=Settings.KCPUplinkCapacity;
    C.KCPReadBufferSize:=Settings.KCPReadBufferSize;
    C.KCPWriteBufferSize:=Settings.KCPWriteBufferSize;
    C.KCPCongestionAlgorithm:=Settings.KCPCongestionAlgorithm;
  end;
  Result:=C.ToJSON;
end;

end.


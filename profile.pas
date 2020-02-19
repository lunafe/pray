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
    Protocol: TRemoteProtocol;
    UUID: string;
    AlterID: word;
    SSPassword: string;
    SSMethod: TShadowsocksEncryption;
    Network: TRemoteTransport;
    EnableTLS: boolean;
    Hostname: string;
    Path: string;
    UDPHeaderType: TUDPHeaderType;
    QUICSecurity: TQUICSecurity;
    QUICKey: string;
    constructor Create;
    function CreateJSON(Settings: TProgramSettings): TJSONObject;
  end;

implementation

constructor TProfile.Create;
var
  Y, M, D: word;
begin
  DecodeDate(Date, Y, M, D);
  Name := Format('%d%0.2d%0.2d', [Y, M, D]);
  Address := '0.0.0.0';
  Port := 2020;
  Protocol := rpVMESS;
  UUID := '';
  AlterID := 64;
  SSPassword := '';
  SSMethod := seAES128GCM;
  Network := rtTCP;
  EnableTLS := False;
  Hostname := '';
  Path := '';
  UDPHeaderType := uhNONE;
  QUICSecurity := qsNONE;
  QUICKey := '';
end;

function TProfile.CreateJSON(Settings: TProgramSettings): TJSONObject;
var
  C: TV2rayJsonConfig;
begin
  C := TV2rayJsonConfig.Create(Address, Port);
  case Protocol of
    rpVMESS: C.SetVMessUser(UUID, AlterID);
    rpSHADOWSOCKS: C.SetShadowsocks(SSPassword, SSMethod);
  end;
  if Settings.EnableSocksProxy then
    C.SetSocksProxy(Settings.SocksProxyPort);
  if Settings.EnableHTTPProxy then
    C.SetHTTPProxy(Settings.HTTPProxyPort);
  C.SetDNSServers(Settings.DNSServers);
  C.SetRouteDomainStrategy(Settings.DomainStrategy);
  C.SetRoute(rlDIRECT, CommaStringList(Settings.Routes[1]));
  C.SetRoute(rlPROXY, CommaStringList(Settings.Routes[2]));
  C.SetRoute(rlDENY, CommaStringList(Settings.Routes[3]));
  if Settings.MuxEnabled then C.SetMux(Settings.MuxConcurrency);
  C.SetTransport(Network, EnableTLS);
  C.SetHostPath(Hostname, Path);
  C.SetQUIC(QUICSecurity, QUICKey);
  C.SetLogLevel(Settings.V2rayLogLevel);
  C.SetUDPHeaderType(UDPHeaderType);
  if Network = rtKCP then
  begin
    C.KCPMTU := Settings.KCPMTU;
    C.KCPTTI := Settings.KCPTTI;
    C.KCPDownlinkCapacity := Settings.KCPDownlinkCapacity;
    C.KCPUplinkCapacity := Settings.KCPUplinkCapacity;
    C.KCPReadBufferSize := Settings.KCPReadBufferSize;
    C.KCPWriteBufferSize := Settings.KCPWriteBufferSize;
    C.KCPCongestionAlgorithm := Settings.KCPCongestionAlgorithm;
  end;
  Result := C.ToJSON;
end;

end.

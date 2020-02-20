unit Profile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPJson, Base64, V2rayJsonConfig, ProgramSettings;

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
    function GenerateLink: string;
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
  C.UDPHeaderType := UDPHeaderType;
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

function TProfile.GenerateLink: string;
var
  TLSStr: string;
  HostStr: string;
  PathStr: string;
  TypeStr: string;
  R: TJSONObject;
begin
  case Network of
    rtTCP: if EnableTLS then TLSStr := 'tls';
    rtKCP:
      if UDPHeaderType <> uhNONE then
        TypeStr := UDPHeaderTypeToString(UDPHeaderType);
    rtQUIC: begin
      HostStr := QUICSecurityToString(QUICSecurity);
      if QUICSecurity <> qsNONE then
        PathStr := QUICKey;
      if UDPHeaderType <> uhNONE then
        TypeStr := UDPHeaderTypeToString(UDPHeaderType);
    end;
    else begin
      if EnableTLS then TLSStr := 'tls';
      HostStr := Hostname;
      PathStr := Path;
    end;
  end;
  case Protocol of
    rpVMESS:
    begin
      R := TJSONObject.Create([
        'add', Address,
        'port', Port,
        'id', UUID,
        'aid', AlterID,
        'net', TransportToString(Network),
        'ps', Name]);
      if TLSStr <> '' then R.Add('tls', TLSStr);
      if HostStr <> '' then R.Add('host', HostStr);
      if PathStr <> '' then R.Add('path', PathStr);
      if TypeStr <> '' then R.Add('type', TypeStr);
      Result := 'vmess://' + EncodeStringBase64(StringReplace(R.AsJSON, ' ', '', [rfReplaceAll]));
    end;
    rpSHADOWSOCKS:
      Result := 'ss://' + EncodeStringBase64(Format('%s:%s@%s:%d', [
        ShadowsocksEncMethodToString(SSMethod),
        StringReplace(SSPassword, '@', '%40', [rfReplaceAll]),
        Address, Port]));
    else Result := '';
  end;
end;

end.

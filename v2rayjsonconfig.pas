unit V2rayJsonConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPJson, RegExpr;

type
  TRemoteTransport = (rtTCP, rtKCP, rtWS, rtHTTP, rtQUIC);
  TRemoteProtocol = (rpVMESS, rpSHADOWSOCKS, rpVLESS);
  TShadowsocksEncryption = (seAES128CFB, seAES256CFB, seAES128GCM, seAES256GCM, seCHACHA20, seCHACHA20IETF, seCHACHA20POLY1305, seCHACHA20IETFPOLY1305, seUNSUPPORTED);
  TUDPHeaderType = (uhNONE, uhSRTP, uhUTP, uhDTLS, uhWECHATVIDEO, uhWIREGUARD);
  TV2rayLogLevel = (llDEBUG, llINFO, llWARNING, llERROR, llNONE);
  TRouteListType = (rlDIRECT, rlPROXY, rlDENY);
  TRouteDomainStrategy = (dsASIS, dsNONMATCH, dsDEMAND);
  TQUICSecurity = (qsNONE, qsAES, qsCHACHA);

  TV2rayJsonConfig = class
    RemoteAddr: string;
    RemotePort: word;
    EnableLocalHTTPProxy: boolean;
    EnableLocalSocksProxy: boolean;
    LocalHTTPProxyPort: word;
    LocalSocksProxyPort: word;
    EnableLocalSocksUDP: boolean;
    EnableTLS: boolean;
    TLSServerName: string;
    RemoteHostname: string;
    RemotePath: string;
    KCPMTU: word;
    KCPTTI: byte;
    KCPUplinkCapacity: word;
    KCPDownlinkCapacity: word;
    KCPReadBufferSize: byte;
    KCPWriteBufferSize: byte;
    KCPCongestionAlgorithm: boolean;
    UDPHeaderType: TUDPHeaderType;
    constructor Create(Address: string; Port: word);
    procedure SetLogLevel(Level: TV2rayLogLevel);
    procedure SetVMessUser(ID: string; Alter: word);
    procedure SetShadowsocks(Password: string; EncryptMethod: TShadowsocksEncryption);
    procedure SetVLESS(ID: string; Encryption: string);
    procedure SetTransport(Transport: TRemoteTransport; TLS: boolean);
    procedure SetMux(Concurrency: word);
    procedure SetRoute(RouteType: TRouteListType; RouteList: TStrings);
    procedure SetRouteDomainStrategy(Strategy: TRouteDomainStrategy);
    procedure SetSocksProxy(Port: word);
    procedure SetHTTPProxy(Port: word);
    procedure SetDNSServers(ServerListString: string);
    procedure SetHostPath(Hostname: string = ''; Path: string = '');
    procedure SetQUIC(Security: TQUICSecurity = qsNONE; Key: string = '');
    function ToJSON: TJSONObject;
  protected
    LogLevel: string;
    DomainStrategy: string;
    NetworkTransport: TRemoteTransport;
    QUICSecurity: TQUICSecurity;
    QUICKey: string;
    VMessUserID: string;
    VMessUserAlterID: word;
    SSPassword: string;
    SSEncryption: TShadowsocksEncryption;
    VLESSUserID: string;
    VLESSEncryption: string;
    MuxEnabled: boolean;
    MuxConcurrency: word;
  private
    Protocol: TRemoteProtocol;
    RouteDirectDomainList: TStrings;
    RouteDirectIPList: TStrings;
    RouteProxyDomainList: TStrings;
    RouteProxyIPList: TStrings;
    RouteDenyIPList: TStrings;
    RouteDenyDomainList: TStrings;
    DNSServers: TStrings;
    function GenerateRouteJSON: TJSONArray;
    function GenerateDNSServerJSON: TJSONArray;
    function GenerateProxyJSON: TJSONObject;
    function GenerateStreamSettingsJSON: TJSONObject;
  end;

function CommaStringList(CommaStr: string): TStrings;
function TransportToString(Transport: TRemoteTransport): string;
function UDPHeaderTypeToString(HeaderType: TUDPHeaderType): string;
function QUICSecurityToString(Security: TQUICSecurity): string;
function ShadowsocksEncMethodToString(Method: TShadowsocksEncryption): string;
function RemoteProtocolToString(Protocol: TRemoteProtocol): string;
function GetTransportFromString(TransportString: string): TRemoteTransport;
function GetUDPHeaderTypeFromString(HeaderType: string): TUDPHeaderType;
function GetQUICSecurityFromString(Security: string): TQUICSecurity;
function GetSSEncMethodFromString(Method: string): TShadowsocksEncryption;

implementation

function CommaStringList(CommaStr: string): TStrings;
var
  A: TStringArray;
  X: string;
  Y: string;
begin
  A := CommaStr.Split(',');
  Result := TStringList.Create;
  for X in A do
  begin
    Y := X.Trim;
    if Length(Y) > 0 then
      Result.Add(Y);
  end;
end;

function TransportToString(Transport: TRemoteTransport): string;
begin
  case Transport of
    rtTCP: Result := 'tcp';
    rtKCP: Result := 'kcp';
    rtWS: Result := 'ws';
    rtHTTP: Result := 'http';
    rtQUIC: Result := 'quic';
    else
      Result := 'tcp';
  end;
end;

function UDPHeaderTypeToString(HeaderType: TUDPHeaderType): string;
begin
  case HeaderType of
    uhNONE: Result := 'none';
    uhSRTP: Result := 'srtp';
    uhUTP: Result := 'utp';
    uhDTLS: Result := 'dtls';
    uhWECHATVIDEO: Result := 'wechat-video';
    uhWIREGUARD: Result := 'wireguard';
    else
      Result := 'none';
  end;
end;

function QUICSecurityToString(Security: TQUICSecurity): string;
begin
  case Security of
    qsNONE: Result := 'none';
    qsAES: Result := 'aes-128-gcm';
    qsCHACHA: Result := 'chacha20-poly1305';
    else Result := 'none';
  end;
end;

function ShadowsocksEncMethodToString(Method: TShadowsocksEncryption): string;
begin
  case Method of
    seAES128CFB: Result := 'aes-128-cfb';
    seAES256CFB: Result := 'aes-256-cfb';
    seAES128GCM: Result := 'aes-128-gcm';
    seAES256GCM: Result := 'aes-256-gcm';
    seCHACHA20: Result := 'chacha20';
    seCHACHA20IETF: Result := 'chacha20-ietf';
    seCHACHA20POLY1305: Result := 'chacha20-poly1305';
    seCHACHA20IETFPOLY1305: Result := 'chacha20-ietf-poly1305';
  end;
end;

function RemoteProtocolToString(Protocol: TRemoteProtocol): string;
begin
  case Protocol of
    rpVMESS: Result := 'VMess';
    rpSHADOWSOCKS: Result := 'Shadowsocks';
    rpVLESS: Result := 'VLESS';
    else Result := 'Unknown';
  end;
end;

function GetTransportFromString(TransportString: string): TRemoteTransport;
var
  S: string;
begin
  S := TransportString.ToLower;
  case S of
    'tcp': Result := rtTCP;
    'kcp': Result := rtKCP;
    'ws': Result := rtWS;
    'http': Result := rtHTTP;
    'quic': Result := rtQUIC;
    else Result := rtTCP;
  end;
end;

function GetUDPHeaderTypeFromString(HeaderType: string): TUDPHeaderType;
var
  S: string;
begin
  S := HeaderType.ToLower;
  case S of
    'none': Result := uhNONE;
    'srtp': Result := uhSRTP;
    'utp': Result := uhUTP;
    'dtls': Result := uhDTLS;
    'wechat-video': Result := uhWECHATVIDEO;
    'wireguard': Result := uhWIREGUARD;
    else Result := uhNONE;
  end;
end;

function GetQUICSecurityFromString(Security: string): TQUICSecurity;
var
  S: string;
begin
  S := Security.ToLower;
  case S of
    'none': Result := qsNONE;
    'aes-128-gcm': Result := qsAES;
    'chacha20-poly1305': Result := qsCHACHA;
    else Result := qsNONE;
  end;
end;

function GetSSEncMethodFromString(Method: string): TShadowsocksEncryption;
var
  S: string;
begin
  S := Method.ToLower;
  case S of
    'aes-128-cfb': Result := seAES128CFB;
    'aes-256-cfb': Result := seAES256CFB;
    'aes-128-gcm': Result := seAES128GCM;
    'aes-256-gcm': Result := seAES256GCM;
    'aead_aes_128_gcm': Result := seAES128GCM;
    'aead_aes_256_gcm': Result := seAES256GCM;
    'chacha20': Result := seCHACHA20;
    'chacha20-ietf': Result := seCHACHA20IETF;
    'chacha20-poly1305': Result := seCHACHA20POLY1305;
    'chacha20-ietf-poly1305': Result := seCHACHA20IETFPOLY1305;
    'aead_chacha20_poly1305': Result := seCHACHA20IETFPOLY1305;
    else Result := seUNSUPPORTED;
  end;
end;

constructor TV2rayJsonConfig.Create(Address: string; Port: word);
begin
  RemoteAddr := Address;
  RemotePort := Port;
  EnableLocalHTTPProxy := False;
  EnableLocalSocksProxy := False;
  EnableLocalSocksUDP := True;
  RemoteHostname := Address;
  RemotePath := '';
  LogLevel := 'debug';
  NetworkTransport := rtTCP;
  TLSServerName := '';
  UDPHeaderType := uhNONE;
  KCPMTU := 1350;
  KCPTTI := 20;
  KCPUplinkCapacity := 5;
  KCPDownlinkCapacity := 50;
  KCPReadBufferSize := 1;
  KCPWriteBufferSize := 1;
  DomainStrategy := 'IPIfNonMatch';
  KCPCongestionAlgorithm := False;
  RouteDirectDomainList := TStringList.Create;
  RouteDirectIPList := TStringList.Create;
  RouteProxyDomainList := TStringList.Create;
  RouteProxyIPList := TStringList.Create;
  RouteDenyDomainList := TStringList.Create;
  RouteDenyIPList := TStringList.Create;
  DNSServers := TStringList.Create;
  SetQUIC;
end;

procedure TV2rayJsonConfig.SetLogLevel(Level: TV2rayLogLevel);
begin
  case Level of
    llDEBUG: LogLevel := 'debug';
    llINFO: LogLevel := 'info';
    llWARNING: LogLevel := 'warning';
    llERROR: LogLevel := 'error';
    llNONE: LogLevel := 'none';
    else
      LogLevel := 'debug';
  end;
end;

procedure TV2rayJsonConfig.SetVMessUser(ID: string; Alter: word);
begin
  VMessUserID := ID;
  VMessUserAlterID := Alter;
  Protocol := rpVMESS;
end;

procedure TV2rayJsonConfig.SetShadowsocks(Password: string; EncryptMethod: TShadowsocksEncryption);
begin
  SSPassword := Password;
  SSEncryption := EncryptMethod;
  Protocol := rpSHADOWSOCKS;
end;

procedure TV2rayJsonConfig.SetVLESS(ID: string; Encryption: string);
begin
  VLESSUserID := ID;
  VLESSEncryption := Encryption;
  Protocol := rpVLESS;
end;

procedure TV2rayJsonConfig.SetTransport(Transport: TRemoteTransport; TLS: boolean);
begin
  case Transport of
    rtKCP, rtQUIC: EnableTLS := False;
    else  EnableTLS := TLS;
  end;
  NetworkTransport := Transport;
end;

procedure TV2rayJsonConfig.SetMux(Concurrency: word);
begin
  if Concurrency < 1 then MuxEnabled := False
  else
  begin
    MuxEnabled := True;
    MuxConcurrency := Concurrency;
  end;
end;

procedure TV2rayJsonConfig.SetHostPath(Hostname: string = ''; Path: string = '');
var
  I: integer;
begin
  if Hostname <> '' then
  begin
    RemoteHostname := Hostname;
    I := RemoteHostname.IndexOf(':');
    if I <> -1 then TLSServerName := RemoteHostname.Substring(0, I)
    else TLSServerName := Hostname;
  end;
  RemotePath := Path;
end;

procedure TV2rayJsonConfig.SetQUIC(Security: TQUICSecurity = qsNONE; Key: string = '');
begin
  if Security = qsNONE then QUICKey := ''
  else QUICKey := Key;
  QUICSecurity := Security;
end;

procedure TV2rayJsonConfig.SetRoute(RouteType: TRouteListType; RouteList: TStrings);
var
  IPList: TStrings;
  DomainList: TStrings;
  R: string;
  I: integer;
  IPRegex: TRegExpr;
  IP6Regex: TRegExpr;
begin
  case RouteType of
    rlDIRECT:
    begin
      IPList := RouteDirectIPList;
      DomainList := RouteDirectDomainList;
    end;
    rlPROXY:
    begin
      IPList := RouteProxyIPList;
      DomainList := RouteProxyDomainList;
    end;
    rlDENY:
    begin
      IPList := RouteDenyIPList;
      DomainList := RouteDenyDomainList;
    end;
  end;
  IPRegex := TRegExpr.Create;
  IP6Regex := TRegExpr.Create;
  IPRegex.Expression := '\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}(/\d{1,2})?';
  IP6Regex.Expression :=
    '(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))(/\d{1,3})?';
  for R in RouteList do
  begin
    I := R.IndexOf(':');
    if I <> -1 then
    begin
      case R.Substring(0, I) of
        'regexp': DomainList.Add(R);
        'domain': DomainList.Add(R);
        'full': DomainList.Add(R);
        'geosite': DomainList.Add(R);
        'geoip': IPList.Add(R);
        else
          if IP6Regex.Exec(R) then
            IPList.Add(R);
      end;
    end
    else if IPRegex.Exec(R) then
      IPList.Add(R);
  end;
end;

procedure TV2rayJsonConfig.SetRouteDomainStrategy(Strategy: TRouteDomainStrategy);
begin
  case Strategy of
    dsASIS: DomainStrategy := 'AsIs';
    dsNONMATCH: DomainStrategy := 'IPIfNonMatch';
    dsDEMAND: DomainStrategy := 'IPOnDemand';
  end;
end;

procedure TV2rayJsonConfig.SetSocksProxy(Port: word);
begin
  if (Port > 0) and (Port < 65535) then
  begin
    LocalSocksProxyPort := Port;
    EnableLocalSocksProxy := True;
  end
  else
    EnableLocalSocksProxy := False;
end;

procedure TV2rayJsonConfig.SetHTTPProxy(Port: word);
begin
  if (Port > 0) and (Port < 65535) then
  begin
    LocalHTTPProxyPort := Port;
    EnableLocalHTTPProxy := True;
  end
  else
    EnableLocalHTTPProxy := False;
end;

procedure TV2rayJsonConfig.SetDNSServers(ServerListString: string);
begin
  DNSServers := CommaStringList(ServerListString);
end;

function TV2rayJsonConfig.GenerateDNSServerJSON: TJSONArray;
var
  S: string;
begin
  Result := TJSONArray.Create;
  for S in DNSServers do
    Result.Add(S);
end;

function TV2rayJsonConfig.GenerateRouteJSON: TJSONArray;
var
  L: TJSONArray;
  X: string;
begin
  Result := TJSONArray.Create;
  if RouteDirectDomainList.Count <> 0 then
  begin
    L := TJSONArray.Create;
    for X in RouteDirectDomainList do
      L.Add(X);
    Result.Add(TJSONObject.Create(['type', 'field', 'outboundTag', 'direct', 'domain', L]));
  end;
  if RouteProxyDomainList.Count <> 0 then
  begin
    L := TJSONArray.Create;
    for X in RouteProxyDomainList do
      L.Add(X);
    Result.Add(TJSONObject.Create(['type', 'field', 'outboundTag', 'proxy', 'domain', L]));
  end;
  if RouteDenyDomainList.Count <> 0 then
  begin
    L := TJSONArray.Create;
    for X in RouteDenyDomainList do
      L.Add(X);
    Result.Add(TJSONObject.Create(['type', 'field', 'outboundTag', 'deny', 'domain', L]));
  end;
  if RouteDirectIPList.Count <> 0 then
  begin
    L := TJSONArray.Create;
    for X in RouteDirectIPList do
      L.Add(X);
    Result.Add(TJSONObject.Create(['type', 'field', 'outboundTag', 'direct', 'ip', L]));
  end;
  if RouteProxyIPList.Count <> 0 then
  begin
    L := TJSONArray.Create;
    for X in RouteProxyIPList do
      L.Add(X);
    Result.Add(TJSONObject.Create(['type', 'field', 'outboundTag', 'proxy', 'ip', L]));
  end;
  if RouteDenyIPList.Count <> 0 then
  begin
    L := TJSONArray.Create;
    for X in RouteDenyIPList do
      L.Add(X);
    Result.Add(TJSONObject.Create(['type', 'field', 'outboundTag', 'deny', 'ip', L]));
  end;
end;

function TV2rayJsonConfig.GenerateProxyJSON: TJSONObject;
var
  P: string;
  S: TJSONObject;
begin
  case Protocol of
    rpVMESS:
    begin
      P := 'vmess';
      S := TJSONObject.Create([
        'vnext', TJSONArray.Create([
           TJSONObject.Create([
             'address', RemoteAddr,
             'port', RemotePort,
             'users', TJSONArray.Create([
               TJSONObject.Create([
                 'id', VMessUserID,
                 'alterId', VMessUserAlterID,
                 'level', 0])])])])]);
    end;
    rpSHADOWSOCKS:
    begin
      P := 'shadowsocks';
      S := TJSONObject.Create([
        'servers', TJSONArray.Create([
          TJSONObject.Create([
            'address', RemoteAddr,
            'port', RemotePort,
            'method', ShadowsocksEncMethodToString(SSEncryption),
            'password', SSPassword,
            'level', 0])])]);
    end;
    rpVLESS:
    begin
      P := 'vless';
      S := TJSONObject.Create([
        'vnext', TJSONArray.Create([
          TJSONObject.Create([
          'address', RemoteAddr,
          'port', RemotePort,
          'users', TJSONArray.Create([
            TJSONObject.Create([
              'id', VLESSUserID,
              'encryption', VLESSEncryption])])])])]);
    end;
  end;
  Result := TJSONObject.Create([
    'tag', 'proxy',
    'protocol', P,
    'settings', S,
    'streamSettings', GenerateStreamSettingsJSON]);
  if MuxEnabled then Result.Add('mux', TJSONObject.Create([
    'enabled', True,
    'concurrency', MuxConcurrency]));
end;

function TV2rayJsonConfig.GenerateStreamSettingsJSON: TJSONObject;
var TLSStr: string;
begin
  if EnableTLS then
    TLSStr := 'tls'
  else
    TLSStr := 'none';
  Result := TJSONObject.Create([
    'network', TransportToString(NetworkTransport),
    'security', TLSStr]);
  if TLSStr <> 'none' then
    Result.Add('tlsSettings', TJSONObject.Create([
      'serverName', TLSServerName,
      'allowInsecure', True]));
  case NetworkTransport of
    rtWS: Result.Add('wsSettings', TJSONObject.Create([
      'path', RemotePath,
      'headers', TJSONObject.Create(['Host', RemoteHostname])]));
    rtHTTP: Result.Add('httpSettings', TJSONObject.Create([
      'host', TJSONArray.Create([RemoteHostname]),
      'path', RemotePath]));
    rtKCP: Result.Add('kcpSettings', TJSONObject.Create([
      'header', TJSONObject.Create([
        'type', UDPHeaderTypeToString(UDPHeaderType)]),
      'congestion', KCPCongestionAlgorithm,
      'mtu', KCPMTU,
      'tti', KCPTTI,
      'readBufferSize', KCPReadBufferSize,
      'writeBufferSize', KCPWriteBufferSize,
      'uplinkCapacity', KCPUplinkCapacity,
      'downlinkCapacity', KCPDownlinkCapacity]));
    rtQUIC: Result.Add('quicSettings', TJSONObject.Create([
      'security', QUICSecurity,
      'key', QUICKey,
      'header', TJSONObject.Create([
        'type', UDPHeaderTypeToString(UDPHeaderType)])]));
  end;
end;

function TV2rayJsonConfig.ToJSON: TJSONObject;
var
  InboundList: TJSONArray;
begin
  InboundList := TJSONArray.Create;
  if EnableLocalHTTPProxy then
    InboundList.Add(TJSONObject.Create([
      'port', LocalHTTPProxyPort,
      'listen', '127.0.0.1',
      'protocol', 'http',
      'settings', TJSONObject.Create]));
  if EnableLocalSocksProxy then
    InboundList.Add(TJSONObject.Create([
      'port', LocalSocksProxyPort,
      'listen', '127.0.0.1',
      'protocol', 'socks',
      'settings', TJSONObject.Create([
        'udp', EnableLocalSocksUDP])]));
  Result := TJSONObject.Create([
    'log', TJSONObject.Create(['loglevel', LogLevel]),
    'dns', TJSONObject.Create(['servers', GenerateDNSServerJSON]),
    'routing', TJSONObject.Create([
      'domainStrategy', DomainStrategy,
      'rules', GenerateRouteJSON]),
    'inbounds', InboundList,
    'outbounds', TJSONArray.Create([
      GenerateProxyJSON,
      TJSONObject.Create([
        'tag', 'direct',
        'protocol', 'freedom',
        'settings', TJSONObject.Create]),
      TJSONObject.Create([
        'tag', 'deny',
        'protocol', 'blackhole',
        'settings', TJSONObject.Create])])]);
end;

end.

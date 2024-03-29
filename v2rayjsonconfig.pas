unit V2rayJsonConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPJson, RegExpr;

type
  TRemoteTransport = (rtTCP, rtKCP, rtWS, rtHTTP, rtQUIC);
  TRemoteProtocol = (rpVMESS, rpSHADOWSOCKS, rpVLESS, rpTROJAN);
  TV2rayLogLevel = (llDEBUG, llINFO, llWARNING, llERROR, llNONE);
  TRouteListType = (rlDIRECT, rlPROXY, rlDENY);
  TRouteDomainStrategy = (dsASIS, dsNONMATCH, dsDEMAND);
  TSecurityOptions = (soNONE, soTLS, soXTLS);

  TV2rayJsonConfig = class
    RemoteAddr: string;
    RemotePort: word;
    EnableLocalHTTPProxy: boolean;
    EnableLocalSocksProxy: boolean;
    LocalHTTPProxyPort: word;
    LocalSocksProxyPort: word;
    EnableLocalSocksUDP: boolean;
    StreamSecurity: TSecurityOptions;
    TLSServerName: string;
    TLSAllowInsecure: boolean;
    RemoteHostname: string;
    RemotePath: string;
    WSMaxEarlyData: word;
    WSEarlyDataHeaderName: string;
    KCPMTU: word;
    KCPTTI: byte;
    KCPUplinkCapacity: word;
    KCPDownlinkCapacity: word;
    KCPReadBufferSize: byte;
    KCPWriteBufferSize: byte;
    KCPCongestionAlgorithm: boolean;
    UDPHeaderType: string;
    constructor Create(Address: string; Port: word);
    procedure SetLogLevel(Level: TV2rayLogLevel);
    procedure SetVMessUser(ID: string; Alter: word);
    procedure SetShadowsocks(Password: string; EncryptMethod: string);
    procedure SetVLESS(ID: string; Encryption: string = 'none'; VLESSFlow: string = '');
    procedure SetTrojan(Password: string; TrojanFlow: string = '');
    procedure SetTransport(Transport: TRemoteTransport; Security: TSecurityOptions);
    procedure SetMux(Concurrency: word);
    procedure SetRoute(RouteType: TRouteListType; RouteList: TStrings);
    procedure SetRouteDomainStrategy(Strategy: TRouteDomainStrategy);
    procedure SetSocksProxy(Port: word);
    procedure SetHTTPProxy(Port: word);
    procedure SetDNSServers(ServerListString: string);
    procedure SetHostPath(Hostname: string = ''; Path: string = '');
    procedure SetWSEarlyData(Length: word = 0; HeaderName: string = '');
    procedure SetQUIC(Security: string = 'none'; Key: string = '');
    function ToJSON: TJSONObject;
  protected
    LogLevel: string;
    DomainStrategy: string;
    NetworkTransport: TRemoteTransport;
    QUICSecurity: string;
    QUICKey: string;
    Flow: string;
    VMessUserID: string;
    VMessUserAlterID: word;
    SSPassword: string;
    SSEncryption: string;
    VLESSUserID: string;
    VLESSEncryption: string;
    TrojanPassword: string;
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
function SecurityOptionToString(SecurityOption: TSecurityOptions): string;
function TransportToString(Transport: TRemoteTransport): string;
function RemoteProtocolToString(Protocol: TRemoteProtocol): string;
function GetTransportFromString(TransportString: string): TRemoteTransport;

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

function SecurityOptionToString(SecurityOption: TSecurityOptions): string;
begin
  case SecurityOption of
    soNONE: Result := 'none';
    soTLS: Result := 'tls';
    soXTLS: Result := 'xtls';
    else Result := 'none';
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

function RemoteProtocolToString(Protocol: TRemoteProtocol): string;
begin
  case Protocol of
    rpVMESS: Result := 'VMess';
    rpSHADOWSOCKS: Result := 'Shadowsocks';
    rpVLESS: Result := 'VLESS';
    rpTROJAN: Result := 'Trojan';
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
  TLSAllowInsecure := False;
  WSMaxEarlyData := 0;
  WSEarlyDataHeaderName := '';
  UDPHeaderType := 'none';
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

procedure TV2rayJsonConfig.SetShadowsocks(Password: string; EncryptMethod: string);
begin
  SSPassword := Password;
  SSEncryption := EncryptMethod;
  Protocol := rpSHADOWSOCKS;
end;

procedure TV2rayJsonConfig.SetVLESS(ID: string; Encryption: string = 'none'; VLESSFlow: string = '');
begin
  VLESSUserID := ID;
  VLESSEncryption := Encryption;
  Flow := VLESSFlow;
  Protocol := rpVLESS;
end;

procedure TV2rayJsonConfig.SetTrojan(Password: string; TrojanFlow: string = '');
begin
  TrojanPassword := Password;
  Flow := TrojanFlow;
  Protocol := rpTROJAN;
end;

procedure TV2rayJsonConfig.SetTransport(Transport: TRemoteTransport; Security: TSecurityOptions);
begin
  case Transport of
    rtKCP, rtQUIC: StreamSecurity := soNONE;
    else StreamSecurity := Security;
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

procedure TV2rayJsonConfig.SetQUIC(Security: string = 'none'; Key: string = '');
begin
  if Security = 'none' then QUICKey := ''
  else QUICKey := Key;
  QUICSecurity := Security;
end;

procedure TV2rayJsonConfig.SetWSEarlyData(Length: word = 0; HeaderName: string = '');
begin
  if Length = 0 then WSEarlyDataHeaderName := ''
  else WSEarlyDataHeaderName := HeaderName;
  WSMaxEarlyData := Length;
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
            'method', SSEncryption,
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
              'flow', Flow,
              'encryption', VLESSEncryption])])])])]);
    end;
    rpTROJAN:
    begin
      P := 'trojan';
      S := TJSONObject.Create([
        'servers', TJSONArray.Create([
          TJSONObject.Create([
            'address', RemoteAddr,
            'port', RemotePort,
            'password', TrojanPassword,
            'flow', Flow,
            'level', 0])])]);
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
var
  SecurityOption: string;
begin
  Result := TJSONObject.Create([
    'network', TransportToString(NetworkTransport),
    'security', SecurityOptionToString(StreamSecurity)]);
  case StreamSecurity of
    soTLS: Result.Add('tlsSettings', TJSONObject.Create([
      'serverName', TLSServerName,
      'allowInsecure', TLSAllowInsecure]));
    soXTLS: Result.Add('xtlsSettings', TJSONObject.Create([
      'serverName', TLSServerName,
      'allowInsecure', False]));
  end;
  case NetworkTransport of
    rtWS: Result.Add('wsSettings', TJSONObject.Create([
      'path', RemotePath,
      'headers', TJSONObject.Create(['Host', RemoteHostname]),
      'maxEarlyData', WSMaxEarlyData, 'earlyDataHeaderName', WSEarlyDataHeaderName]));
    rtHTTP: Result.Add('httpSettings', TJSONObject.Create([
      'host', TJSONArray.Create([RemoteHostname]),
      'path', RemotePath]));
    rtKCP: Result.Add('kcpSettings', TJSONObject.Create([
      'header', TJSONObject.Create(['type', UDPHeaderType]),
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
      'header', TJSONObject.Create(['type', UDPHeaderType])]));
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

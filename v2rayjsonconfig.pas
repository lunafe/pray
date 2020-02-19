unit V2rayJsonConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPJson, RegExpr;

type
  TRemoteTransport = (rtTCP, rtKCP, rtWS, rtHTTP, rtQUIC);
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
    VMessUserID: string;
    VMessUserAlterID: word;
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
    MuxEnabled: boolean;
    MuxConcurrency: word;
    constructor Create(Address: string; Port: word);
    procedure SetLogLevel(Level: TV2rayLogLevel);
    procedure SetUser(ID: string; Alter: word);
    procedure SetTransport(Transport: TRemoteTransport; TLS: boolean);
    procedure SetUDPHeaderType(HeaderType: TUDPHeaderType);
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
    NetworkTransport: string;
    UDPHeaderType: string;
    DomainStrategy: string;
    QUICSecurity: string;
    QUICKey: string;
  private
    RouteDirectDomainList: TStrings;
    RouteDirectIPList: TStrings;
    RouteProxyDomainList: TStrings;
    RouteProxyIPList: TStrings;
    RouteDenyIPList: TStrings;
    RouteDenyDomainList: TStrings;
    DNSServers: TStrings;
    function GenerateRouteJSON: TJSONArray;
    function GenerateDNSServerJSON: TJSONArray;
    function GenerateStreamSettingsJSON: TJSONObject;
  end;

function CommaStringList(CommaStr: string): TStrings;

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
  NetworkTransport := 'tcp';
  TLSServerName := '';
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

procedure TV2rayJsonConfig.SetUser(ID: string; Alter: word);
begin
  VMessUserID := ID;
  VMessUserAlterID := Alter;
end;

procedure TV2rayJsonConfig.SetTransport(Transport: TRemoteTransport; TLS: boolean);
begin
  case Transport of
    rtTCP: NetworkTransport := 'tcp';
    rtKCP: NetworkTransport := 'kcp';
    rtWS: NetworkTransport := 'ws';
    rtHTTP: NetworkTransport := 'http';
    rtQUIC: NetworkTransport := 'quic';
    else
      NetworkTransport := 'tcp';
  end;
  case Transport of
    rtKCP, rtQUIC: EnableTLS := False;
    else  EnableTLS := TLS;
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
  case Security of
    qsNONE: QUICSecurity := 'none';
    qsAES: QUICSecurity := 'aes-128-gcm';
    qsCHACHA: QUICSecurity := 'chacha20-poly1305';
    else QUICSecurity := 'none';
  end;
  if Security = qsNONE then QUICKey := ''
  else QUICKey := Key;
end;

procedure TV2rayJsonConfig.SetUDPHeaderType(HeaderType: TUDPHeaderType);
begin
  case HeaderType of
    uhNONE: UDPHeaderType := 'none';
    uhSRTP: UDPHeaderType := 'srtp';
    uhUTP: UDPHeaderType := 'utp';
    uhDTLS: UDPHeaderType := 'dtls';
    uhWECHATVIDEO: UDPHeaderType := 'wechat-video';
    uhWIREGUARD: UDPHeaderType := 'wireguard';
    else
      UDPHeaderType := 'none';
  end;
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
  IPRegex.Expression := '\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}(/\d{0,2})?';
  IP6Regex.Expression :=
    '(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))';
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
    Result.Add(TJSONObject.Create(['type', 'field', 'outboundTag',
      'direct', 'domain', L]));
  end;
  if RouteProxyDomainList.Count <> 0 then
  begin
    L := TJSONArray.Create;
    for X in RouteProxyDomainList do
      L.Add(X);
    Result.Add(TJSONObject.Create(['type', 'field', 'outboundTag',
      'proxy', 'domain', L]));
  end;
  if RouteDenyDomainList.Count <> 0 then
  begin
    L := TJSONArray.Create;
    for X in RouteDenyDomainList do
      L.Add(X);
    Result.Add(TJSONObject.Create(['type', 'field', 'outboundTag',
      'deny', 'domain', L]));
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

function TV2rayJsonConfig.GenerateStreamSettingsJSON: TJSONObject;
var TLSStr: string;
begin
  if EnableTLS then
    TLSStr := 'tls'
  else
    TLSStr := 'none';
  Result := TJSONObject.Create(['network', NetworkTransport, 'security', TLSStr]);
  if TLSStr <> 'none' then
    Result.Add('tlsSettings', TJSONObject.Create([
      'serverName', TLSServerName,
      'allowInsecure', True]));
  case NetworkTransport of
    'ws': Result.Add('wsSettings', TJSONObject.Create([
      'path', RemotePath,
      'headers', TJSONObject.Create(['Host', RemoteHostname])]));
    'http': Result.Add('httpSettings', TJSONObject.Create([
      'host', TJSONArray.Create([RemoteHostname]),
      'path', RemotePath]));
    'kcp': Result.Add('kcpSettings', TJSONObject.Create([
      'header', TJSONObject.Create(['type', UDPHeaderType]),
      'congestion', KCPCongestionAlgorithm,
      'mtu', KCPMTU,
      'tti', KCPTTI,
      'readBufferSize', KCPReadBufferSize,
      'writeBufferSize', KCPWriteBufferSize,
      'uplinkCapacity', KCPUplinkCapacity,
      'downlinkCapacity', KCPDownlinkCapacity]));
    'quic': Result.Add('quicSettings', TJSONObject.Create([
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
      TJSONObject.Create([
        'tag', 'proxy',
        'protocol', 'vmess',
        'settings', TJSONObject.Create([
           'vnext', TJSONArray.Create([
           TJSONObject.Create([
             'address', RemoteAddr,
             'port', RemotePort,
             'users', TJSONArray.Create([
               TJSONObject.Create([
                 'id', VMessUserID,
                 'level', 0,
                 'alterId', VMessUserAlterID])])])])]),
        'mux', TJSONObject.Create([
          'enabled', MuxEnabled,
          'concurrency', MuxConcurrency]),
        'streamSettings', GenerateStreamSettingsJSON]),
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

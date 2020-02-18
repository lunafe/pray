unit ProgramSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, FPJson, V2rayJsonConfig;

type
  TProgramSettings = class
    EnableSocksProxy: boolean;
    EnableHTTPProxy: boolean;
    SocksProxyPort: word;
    HTTPProxyPort: word;
    V2rayBinaryPath: string;
    V2rayAssetsPath: string;
    V2rayLogLevel: TV2rayLogLevel;
    DNSServers: string;
    KCPMTU: word;
    KCPTTI: byte;
    KCPUplinkCapacity: word;
    KCPDownlinkCapacity: word;
    KCPReadBufferSize: word;
    KCPWriteBufferSize: word;
    KCPCongestionAlgorithm: boolean;
    UDPHeaderType: TUDPHeaderType;
    DomainStrategy: TRouteDomainStrategy;
    MuxEnabled: boolean;
    MuxConcurrency: word;
    Routes: array [1..3] of string;
    constructor Create;
    procedure LoadFile(FileName: string);
    procedure SaveFile(FileName: string);
  private
    ApplicationRoot: string;
  end;

implementation

constructor TProgramSettings.Create;
begin
  ApplicationRoot := ExtractFileDir(Application.ExeName);
  EnableSocksProxy := True;
  EnableHTTPProxy := False;
  SocksProxyPort := 31080;
  HTTPProxyPort := 38080;
  V2rayBinaryPath := ConcatPaths([ApplicationRoot, 'v2ray.exe']);
  V2rayAssetsPath := ApplicationRoot;
  DNSServers := '208.67.222.222,208.67.220.220';
  KCPMTU := 1350;
  KCPTTI := 20;
  KCPUplinkCapacity := 5;
  KCPDownlinkCapacity := 100;
  KCPReadBufferSize := 2;
  KCPWriteBufferSize := 2;
  KCPCongestionAlgorithm := False;
  UDPHeaderType := uhNONE;
  DomainStrategy := dsNONMATCH;
  MuxEnabled := False;
  MuxConcurrency := 16;
  Routes[1] := 'geosite:cn,geoip:cn,geoip:private';
  Routes[2] := '';
  Routes[3] := '';
end;

procedure TProgramSettings.LoadFile(FileName: string);
var
  F: TFileStream;
  S: string;
  J: TJSONObject;
  R: TJSONArray;
begin
  F := TFileStream.Create(FileName, fmOpenRead);
  SetLength(S, F.Size);
  F.ReadBuffer(Pointer(S)^, Length(S));
  F.Free;
  J := TJSONObject(GetJSON(S));
  EnableSocksProxy := J.Get('socks', EnableSocksProxy);
  EnableHTTPProxy := J.Get('http', EnableHTTPProxy);
  SocksProxyPort := J.Get('socks_port', SocksProxyPort);
  HTTPProxyPort := J.Get('http_port', HTTPProxyPort);
  V2rayBinaryPath := J.Get('v2ray', V2rayBinaryPath);
  V2rayAssetsPath := J.Get('assets', V2rayAssetsPath);
  V2rayLogLevel := TV2rayLogLevel(J.Get('log', byte(V2rayLogLevel)));
  DNSServers := J.Get('dns', DNSServers);
  KCPMTU := J.Get('mtu', KCPMTU);
  KCPTTI := J.Get('tti', KCPTTI);
  KCPUplinkCapacity := J.Get('upcap', KCPUplinkCapacity);
  KCPDownlinkCapacity := J.Get('downcap', KCPDownlinkCapacity);
  KCPCongestionAlgorithm := J.Get('congestion', KCPCongestionAlgorithm);
  UDPHeaderType := TUDPHeaderType(J.Get('khead', byte(UDPHeaderType)));
  DomainStrategy := TRouteDomainStrategy(J.Get('domstg', byte(DomainStrategy)));
  MuxEnabled := J.Get('mux', MuxEnabled);
  MuxConcurrency := J.Get('muxcon', MuxConcurrency);
  R := J.Get('route', TJSONArray.Create(['', '', '']));
  Routes[1] := R[0].AsString;
  Routes[2] := R[1].AsString;
  Routes[3] := R[2].AsString;
end;

procedure TProgramSettings.SaveFile(FileName: string);
var
  F: TFileStream;
  J: string;
begin
  F := TFileStream.Create(FileName, fmCreate);
  J := TJSONObject.Create(['socks', EnableSocksProxy, 'http',
    EnableHTTPProxy, 'socks_port', SocksProxyPort, 'http_port',
    HTTPProxyPort, 'v2ray', V2rayBinaryPath, 'assets', V2rayAssetsPath,
    'log', byte(V2rayLogLevel), 'dns', DNSServers, 'mtu', KCPMTU,
    'tti', KCPTTI, 'upcap', KCPUplinkCapacity, 'downcap', KCPDownlinkCapacity,
    'rbsize', KCPReadBufferSize, 'wbsize', KCPWriteBufferSize,
    'congestion', KCPCongestionAlgorithm, 'khead', byte(UDPHeaderType),
    'domstg', byte(DomainStrategy), 'mux', MuxEnabled, 'muxcon',
    MuxConcurrency, 'route', TJSONArray.Create([Routes[1], Routes[2], Routes[3]])]).FormatJSON;
  F.WriteBuffer(Pointer(J)^, Length(J));
  F.Free;
end;

end.

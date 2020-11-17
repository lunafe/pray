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
    TLSAllowInsecure: boolean;
    KCPMTU: word;
    KCPTTI: byte;
    KCPUplinkCapacity: word;
    KCPDownlinkCapacity: word;
    KCPReadBufferSize: word;
    KCPWriteBufferSize: word;
    KCPCongestionAlgorithm: boolean;
    DomainStrategy: TRouteDomainStrategy;
    MuxEnabled: boolean;
    MuxConcurrency: word;
    Routes: array [1..3] of string;
    constructor Create;
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
  TLSAllowInsecure := False;
  KCPMTU := 1350;
  KCPTTI := 20;
  KCPUplinkCapacity := 5;
  KCPDownlinkCapacity := 100;
  KCPReadBufferSize := 2;
  KCPWriteBufferSize := 2;
  KCPCongestionAlgorithm := False;
  DomainStrategy := dsNONMATCH;
  MuxEnabled := False;
  MuxConcurrency := 16;
  Routes[1] := 'geosite:cn,geoip:cn,geoip:private';
  Routes[2] := '';
  Routes[3] := '';
end;

end.

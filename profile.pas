unit Profile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPJson, Base64, RegExpr, StrUtils, V2rayJsonConfig, ProgramSettings;

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
    VLESSID: string;
    VLESSEncryption: string;
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

function ParseLinkToProfile(Link: string; var LinkProfile: TProfile): boolean;

implementation

function URLDecode(S: string): string;
var
  I, LengthSource: integer;
  Source: PAnsiChar;
begin
  Result := '';
  Source := PAnsiChar(S);
  LengthSource := Length(Source);
  I := 1;
  while (I <= LengthSource) do
    begin
      if Source[I-1] <> '%' then Result := Result + Source[I - 1]
      else if (Source[I - 1] = '%') and (I + 1 <= LengthSource) then
        try
          begin
            Result := Result + Chr(Hex2Dec('$'+Source[I]+Source[I + 1]));
            I := I + 2;
          end;
        except
        end
      else
        Result := Result + Source[I - 1];
      Inc(I);
    end;
end;

function ParseLinkToProfile(Link: string; var LinkProfile: TProfile): boolean;
var
  DecodedContent: string;
  VMessJSONObj: TJSONObject;
  R: TRegExpr;
  C: integer;
  Q: integer;
  SI: string;
  SSName: string;
  SSBase64Length: integer;
begin
  if Link.StartsWith('vmess://', True) then
  with LinkProfile do
  begin
    try
      DecodedContent := DecodeStringBase64(Link.Substring(8), False);
      VMessJSONObj := TJSONObject(GetJSON(DecodedContent));
      Protocol := rpVMESS;
      Address := VMessJSONObj.Get('add', '');
      Port := VMessJSONObj.Get('port', 0);
      if Port = 0 then
      begin
        SI := VMessJSONObj.Get('port', '');
        if SI <> '' then Val(SI, Port, C);
      end;
      Name := VMessJSONObj.Get('ps', Format(
        'VMess<%s:%d>',
        [Address, Port]));
      UUID := VMessJSONObj.Get('id', '');
      AlterID := VMessJSONObj.Get('aid', 0);
      if AlterID = 0 then
      begin
        SI := VMessJSONObj.Get('aid', '');
        if SI <> '' then Val(SI, AlterID, C);
      end;
      Network := GetTransportFromString(VMessJSONObj.Get('net', ''));
      case Network of
        rtKCP: UDPHeaderType := GetUDPHeaderTypeFromString(VMessJSONObj.Get('type', ''));
        rtWS, rtHTTP:
        begin
          Hostname := VMessJSONObj.Get('host', '');
          Path := VMessJSONObj.Get('path', '');
          EnableTLS := VMessJSONObj.Get('tls', False);
          if not EnableTLS then
          begin
            SI := VMessJSONObj.Get('tls', '');
            SI := SI.ToLower;
            if (SI <> 'none') and (SI <> 'false') and (SI <> '0') and (SI <> '') then
              EnableTLS := True;
            if not EnableTLS then
             if VMessJSONObj.Get('tls', 0) <> 0 then EnableTLS := True;
          end;
        end;
        rtQUIC:
        begin
          QUICSecurity := GetQUICSecurityFromString(VMessJSONObj.Get('host', ''));
          QUICKey := VMessJSONObj.Get('path', '');
          UDPHeaderType := GetUDPHeaderTypeFromString(VMessJSONObj.Get('type', ''));
        end;
        rtTCP:
        begin
          EnableTLS := VMessJSONObj.Get('tls', False);
          if not EnableTLS then
          begin
            SI := VMessJSONObj.Get('tls', '');
            SI := SI.ToLower;
            if (SI <> 'none') and (SI <> 'false') and (SI <> '0') and (SI <> '') then
              EnableTLS := True;
            if not EnableTLS then
             if VMessJSONObj.Get('tls', 0) <> 0 then EnableTLS := True;
          end;
        end;
      end;
      Result := True;
    except
      Result := False;
    end;
  end
  else if Link.StartsWith('ss://', True) then
  begin
    Q := Link.IndexOf('?');
    C := Link.IndexOf('#');
    if C = -1 then SSName := ''
    else SSName := URLDecode(Link.Substring(C + 1));
    if (C = -1) and (Q = -1) then SSBase64Length := Link.Length - 5
    else if (C = -1) then SSBase64Length := Q - 5
    else if (Q = -1) then SSBase64Length := C - 5
    else if (C < Q) then SSBase64Length := C - 5
    else SSBase64Length := Q - 5;
    DecodedContent := DecodeStringBase64(Link.Substring(5, SSBase64Length), False);
    R := TRegExpr.Create('^([\w\-]+):(.+)@([0-9a-zA-Z\-_\.]+):(\d+)$');
    if R.Exec(DecodedContent) then
    begin
      LinkProfile.Protocol := rpSHADOWSOCKS;
      LinkProfile.Address := R.Match[3];
      Val(R.Match[4], LinkProfile.Port, C);
      if SSName = '' then
        LinkProfile.Name := Format('SS<%s:%d>', [LinkProfile.Address, LinkProfile.Port])
      else LinkProfile.Name := SSName;
      LinkProfile.SSMethod := GetSSEncMethodFromString(R.Match[1]);
      LinkProfile.SSPassword := R.Match[2];
      if LinkProfile.SSMethod = seUNSUPPORTED then Result := False
      else Result := True;
    end
    else Result := False;
  end
  else Result := False;
end;

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
  VLESSID := '';
  VLESSEncryption := '';
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
    rpVLESS: C.SetVLESS(VLESSID, VLESSEncryption);
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
        SSPassword,
        Address, Port]));
    else Result := '';
  end;
end;

end.

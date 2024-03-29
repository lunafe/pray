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
    SSMethod: string;
    VLESSID: string;
    VLESSEncryption: string;
    Flow: string;
    TrojanPassword: string;
    Network: TRemoteTransport;
    StreamSecurity: TSecurityOptions;
    Hostname: string;
    Path: string;
    WSEDLength: word;
    WSEDHeader: string;
    UDPHeaderType: string;
    QUICSecurity: string;
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

function URLEncode(S: string): AnsiString;
var
  C: AnsiChar;
begin
  Result := '';
  for C in S do begin
    if ((Ord(C) < 65) or (Ord(C) > 90)) and ((Ord(C) < 97) or (Ord(C) > 122)) then begin
      Result := Result + '%' + IntToHex(Ord(C), 2);
    end else
      Result := Result + C;
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
        rtKCP: UDPHeaderType := VMessJSONObj.Get('type', '');
        rtWS, rtHTTP, rtTCP:
        begin
          Hostname := VMessJSONObj.Get('host', '');
          Path := VMessJSONObj.Get('path', '');
          if VMessJSONObj.Get('tls', False) then
            StreamSecurity := soTLS
          else
          begin
            SI := VMessJSONObj.Get('tls', '');
            SI := SI.ToLower;
            if SI = 'xtls' then begin
              StreamSecurity := soXTLS;
            end
            else if (SI <> 'none') and (SI <> 'false') and (SI <> '0') and (SI <> '') then
              StreamSecurity := soTLS;
            if (StreamSecurity = soNONE) and (VMessJSONObj.Get('tls', 0) <> 0) then
              StreamSecurity := soTLS;
          end;
        end;
        rtQUIC:
        begin
          QUICSecurity := VMessJSONObj.Get('host', '');
          QUICKey := VMessJSONObj.Get('path', '');
          UDPHeaderType := VMessJSONObj.Get('type', '');
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
      LinkProfile.SSMethod := R.Match[1];
      LinkProfile.SSPassword := R.Match[2];
      //if LinkProfile.SSMethod = seUNSUPPORTED then Result := False
      //else Result := True;
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
  SSMethod := 'aes-128-gcm';
  VLESSID := '';
  VLESSEncryption := '';
  Flow := '';
  TrojanPassword := '';
  Network := rtTCP;
  Hostname := '';
  Path := '';
  UDPHeaderType := 'none';
  QUICSecurity := 'none';
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
    rpVLESS: C.SetVLESS(VLESSID, VLESSEncryption, Flow);
    rpTROJAN: C.SetTrojan(TrojanPassword, Flow);
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
  C.SetTransport(Network, StreamSecurity);
  C.SetHostPath(Hostname, Path);
  C.SetWSEarlyData(WSEDLength, WSEDHeader);
  C.SetQUIC(QUICSecurity, QUICKey);
  C.SetLogLevel(Settings.V2rayLogLevel);
  C.UDPHeaderType := UDPHeaderType;
  C.TLSAllowInsecure := Settings.TLSAllowInsecure;
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
  I: integer;
  UrlElements: TStringList;
begin
  case Network of
    rtKCP:
      if UDPHeaderType <> 'none' then
        TypeStr := UDPHeaderType;
    rtQUIC: begin
      HostStr := QUICSecurity;
      if QUICSecurity <> 'none' then
        PathStr := QUICKey;
      if UDPHeaderType <> 'none' then
        TypeStr := UDPHeaderType;
    end;
    else begin
      TLSStr := SecurityOptionToString(StreamSecurity);
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
      if TLSStr <> 'none' then R.Add('tls', TLSStr);
      if HostStr <> '' then R.Add('host', HostStr);
      if PathStr <> '' then R.Add('path', PathStr);
      if TypeStr <> '' then R.Add('type', TypeStr);
      Result := 'vmess://' + EncodeStringBase64(StringReplace(R.AsJSON, ' ', '', [rfReplaceAll]));
    end;
    rpSHADOWSOCKS:
      Result := 'ss://' + EncodeStringBase64(Format('%s:%s@%s:%d', [
        SSMethod, SSPassword, Address, Port]));
    rpVLESS:
    begin
      UrlElements := TStringList.Create;
      Result := 'vless://' + Format('%s@%s:%d', [VLESSID, Address, Port]);
      if VLESSEncryption <> 'none' then UrlElements.Add('encryption=' + VLESSEncryption);
      if Network <> rtTCP then UrlElements.Add('type=' + TransportToString(Network));
      case Network of
        rtQUIC:
        begin
          if QUICSecurity <> 'none' then UrlElements.Add('quicSecurity=' + QUICSecurity);
          if QUICKey <> '' then UrlElements.Add('key=' + URLEncode(QUICKey));
          if UDPHeaderType <> 'none' then UrlElements.Add('headerType=' + UDPHeaderType);
        end;
        rtWS, rtHTTP:
        begin
          if Hostname <> '' then UrlElements.Add('host=' + URLEncode(Hostname));
          if Path <> '' then UrlElements.Add('path=' + URLEncode(Path));
        end;
        rtKCP: if UDPHeaderType <> 'none' then UrlElements.Add('headerType=' + UDPHeaderType);
      end;
      if StreamSecurity <> soNONE then
        UrlElements.Add('security=' + SecurityOptionToString(StreamSecurity));
      if UrlElements.Count > 0 then
      begin
        Result := Result + '/?';
        for I := 0 to UrlElements.Count - 1 do
        begin
          Result := Result + UrlElements[I];
          if I = UrlElements.Count - 1 then Break;
          Result := Result + '&';
        end;
      end;
      UrlElements.Free;
    end;
    else Result := '';
  end;
end;

end.

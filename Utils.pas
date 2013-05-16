unit Utils;

interface

uses
  System.Classes, System.SysUtils,
  snmpsend, synautil, asn1util;

type
  TMAC = String[6];
  TSMAC = String[19];

  // SNMP Utilites
  function SNMPGetBulk(const BaseOID, Community, SNMPHost: AnsiString; const Value: TStrings): Boolean; overload;
  function SNMPGetBulk(const BaseOID, Community, SNMPHost: AnsiString; const Value: TStrings; Vlan: AnsiString): Boolean; overload;
  // Other Utilites
  function MAC_bin2str(BMAC: TMAC): TSMAC;
  function MAC_oid2bin(OMAC: String): TMAC;

const
  CrLf = #13#10;
  sysDescr = '1.3.6.1.2.1.1.1.0';
  sysContact = '1.3.6.1.2.1.1.4.0';
  sysName = '1.3.6.1.2.1.1.5.0';
  sysLocation = '1.3.6.1.2.1.1.6.0';
  dot1dBaseBridgeAddress = '1.3.6.1.2.1.17.1.1.0';
  dot1dTpFdbPort = '1.3.6.1.2.1.17.4.3.1.2';
  dot1dTpFdbAddress = '1.3.6.1.2.1.17.4.3.1.1';
  dot1dBasePortIfIndex = '1.3.6.1.2.1.17.1.4.1.2';

  ifIndex = '1.3.6.1.2.1.2.2.1.1';
  ifDescr = '1.3.6.1.2.1.2.2.1.2';
  ifType = '1.3.6.1.2.1.2.2.1.3';
  ifType_ethernet = 6;
  ifPhysAddress = '1.3.6.1.2.1.2.2.1.6';
  ipNetToMediaPhysAddress = '1.3.6.1.2.1.4.22.1.2';

implementation

//  SNMP Utilites
function SNMPGetBulk(const BaseOID, Community, SNMPHost: AnsiString; const Value: TStrings; Vlan: AnsiString): Boolean;
begin
  Result := SNMPGetBulk(BaseOID, Community + '@' + Vlan, SNMPHost, Value);
end;
function SNMPGetBulk(const BaseOID, Community, SNMPHost: AnsiString; const Value: TStrings): Boolean;
var
  OID, idx: AnsiString;
  SNMPSend: TSNMPSend;
  RowList: TStringList;
  i: Integer;
begin
  Result := false;
  Value.Clear;
  SNMPSend := TSNMPSend.Create;
  RowList := TStringList.Create;
  try
    OID := BaseOID;
    with SNMPSend do begin
      TargetHost := SNMPHost;
      Query.Clear;
      Query.Version := SNMP_V2C;
      Query.ID := Query.ID + 1;
      Query.MaxRepetitions := 28;
      Query.Community := Community;
      Query.PDUType := PDUGetBulkRequest;
      Query.MIBAdd(OID, '', ASN1_NULL);
      while SendRequest do begin
        Result := True;
        Query.ID := Query.ID + 1;
        if Reply.SNMPMibList.Count > 0 then begin
          for i := 0 to Reply.SNMPMibList.Count-1 do begin
            OID := TSNMPMib(Reply.SNMPMibList[i]).OID;
            if Pos(BaseOID, OID) <> 1 then Exit;
            idx := separateright(OID, baseOID + '.');
            Value.Values[idx] := TSNMPMib(Reply.SNMPMibList[i]).Value;
          end;
          TSNMPMib(Query.SNMPMibList[0]).OID := OID;
        end else
          Exit;
      end;
    end;
  finally
    SNMPSend.Free;
    RowList.Free;
  end;
end;

// Other Utilites
function MAC_bin2str(BMAC: TMAC): TSMAC;
var
  SMAC: TSMAC;
begin
  SMAC := '00:00:00:00:00:00';
  asm
    lea ecx, BMAC
    lea edx, SMAC
    add ecx, 1
    add edx, 1
    mov bl, ':'
    mov bh, 6

  @Do:
    mov ah, [ecx]
    and ah, $0F
    mov al, [ecx]
    and al, $F0
    shr al, 4

    add ah, '0'
    cmp ah, '9'
    jbe @Next1
    add ah, 7

  @Next1:
    add al, '0'
    cmp al, '9'
    jbe @Next2
    add al, 7

  @Next2:
    mov [edx], ax
    mov [edx + 2], bl

    add ecx, 1
    add edx, 3

    sub bh, 1
    jnz @Do
  end;
  Result := SMAC;
end;
function MAC_oid2bin(OMAC: String): TMAC;
var
  List: TStringList;
  i: Integer;
begin
  Result := '000000';
  List := TStringList.Create;
  try
    List.Delimiter := '.';
    List.DelimitedText := OMAC;
    for i := 0 to 5 do
      Result[i+1] := AnsiChar(StrToInt(List[i]));
  finally
    List.Free;
  end;
end;
end.

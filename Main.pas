unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.IniFiles,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus,
  IdBaseComponent, IdComponent, IdUDPBase, IdUDPClient, IdSNMP,
  VirtualTrees,
  Utils;

type
  objType = (otRootDevice, otDevice, otPort);
  TTreeData = record
    idx: Integer;
    oType: objType;
    Parent: PVirtualNode;
  end;
  TsnmpReply = array of String;
  TIP = String[15];
  TPort = record
    snmpID: Integer;
    Descr: String;
    MAC: TMAC;
    MACList: array of TMAC;
  end;
  TDevice = record
    IP: TIP;
    MAC: TMAC;
    sysDescr, sysContact, sysName, sysLocation: String;
    Ports: array of TPort;
  end;
  TfrmMain = class(TForm)
    idSNMP: TIdSNMP;
    Memo: TMemo;
    vstDevices: TVirtualStringTree;
    Menu: TMainMenu;
    mScan: TMenuItem;
    mTools: TMenuItem;
    mFile: TMenuItem;
    mExit: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure mScanClick(Sender: TObject);
    procedure vstDevicesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstDevicesGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: string);
    procedure mToolsClick(Sender: TObject);
  private
    cfg: TINIFile;
    FVer: Integer;
    FRO: String;
    function snmpWalk(DevIP, OID: String; var AReply: TStringList): Boolean;
    function snmpGet(DevIP, OID: String; var AReply: String): Boolean; overload;
    function snmpGet(DevIP: String; OIDs: array of String; var AReply: TsnmpReply): Boolean; overload;
    function getDeviceDescr (var Device: TDevice): Boolean;
    function getDevicePorts (var Device: TDevice): Boolean;
  public
    ipRoot: TDevice;
    ipRange: TStringList;
    procedure Log(msg: String; ShowTime: Boolean = false);
  end;

var
  frmMain: TfrmMain;

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

implementation

{$R *.dfm}

uses IdASN1Util;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(ipRange) then ipRange.Free;
  if Assigned(cfg) then cfg.Free;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  ipRange := TStringList.Create;
  cfg := TINIFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  FVer := cfg.ReadInteger('General', 'snmpVer', 1);
  FRO := cfg.ReadString('General', 'snmpRO', 'public');
  vstDevices.NodeDataSize := SizeOf(TTreeData);
  Log('Application Started.', true);
end;

// **** Utilites ****
function TfrmMain.snmpGet(DevIP: string; OID: string; var AReply: String): Boolean;
var
  cnt, i: Integer;
begin
  Result := False;
  with idSNMP do begin
    //Active := True;
    Query.Clear;
    Query.Version := FVer;
    Query.PDUType := PDUGetRequest;
    Query.Host := DevIP;
    Query.Community :=  FRO;
    Query.MIBAdd(OID, '', ASN1_NULL);
    Result :=  idSNMP.SendQuery;
    if not Result then begin
      Log ('SNMP GET: ' + OID + ' Error !!!', true);
      Exit;
    end;
    AReply := Reply.Value[0];
  end;
end;
function TfrmMain.snmpGet(DevIP: String; OIDs: array of String; var AReply: TsnmpReply): Boolean;
var
  cnt, i: Integer;
begin
  Result := False;
  with idSNMP do begin
    //Active := True;
    Query.Clear;
    Query.Version := FVer;
    Query.PDUType := PDUGetRequest;
    Query.Host := DevIP;
    Query.Community :=  FRO;
    for i := Low(OIDs) to High(OIDs) do
      Query.MIBAdd(OIDs[i], '', ASN1_NULL);
    Result :=  idSNMP.SendQuery;
    if not Result then begin
      Log ('SNMP GET Error !!!', true);
      Exit;
    end;
    cnt := Reply.ValueCount;
    SetLength(AReply, cnt);
    for i := 0 to cnt-1 do
      AReply[i] := Reply.Value[i];
  end;
end;

function TfrmMain.snmpWalk(DevIP, OID: String; var AReply: TStringList): Boolean;
var
  i, loid: Integer;
begin
  Result := False;
  loid := Length(OID);
  with idSNMP do begin
    //Active := True;
    Query.Clear;
    Query.Version := FVer;
    Query.PDUType := PDUGetNextRequest;
    Query.Host := DevIP;
    Query.Community :=  FRO;
    Query.MIBAdd(OID, '', ASN1_NULL);
    AReply.Clear;
    while idSNMP.SendQuery do begin
      Result := True;
      if Copy(Reply.MIBOID[0], 1, loid) <> OID then
        Break;
      Query.MIBAdd(Reply.MIBOID[0], '', ASN1_NULL);
      Query.MIBDelete(0);
      AReply.Values[Copy(Reply.ValueOID[0], loid + 2, MAXINT)] := Reply.Value[0];
    end;
  end;
end;
// *** Utilites
function TfrmMain.getDeviceDescr (var Device: TDevice): Boolean;
var
  Reply: TsnmpReply;
  dDescr: array [0..4] of String;
begin
  dDescr[0] := sysDescr;
  dDescr[1] := sysContact;
  dDescr[2] := sysName;
  dDescr[3] := sysLocation;
  dDescr[4] := dot1dBaseBridgeAddress;
  if snmpGet(Device.IP, dDescr, Reply) then begin
    Device.sysDescr := Reply[0];
    Device.sysContact := Reply[1];
    Device.sysName := Reply[2];
    Device.sysLocation := Reply[3];
    Device.MAC := Reply[4];
  end;
end;
function TfrmMain.getDevicePorts (var Device: TDevice): Boolean;
var
  Ports: TStringList;
  Reply: TsnmpReply;
  OIDs: array of String;
  i, n: Integer;
  str: String;
begin
  Result := False;
  Ports := TStringList.Create;
  try
    snmpWalk(Device.IP, ifType, Ports);
    if Ports.Count < 1 then Exit;
    i := 0;
    while i < Ports.Count do begin
      n := StrToInt(Ports.ValueFromIndex[i]);
      if not ((n = 6) or (n = 135)) then
        Ports.Delete(i)
      else
        Inc(i);
    end;
    SetLength(Device.Ports, Ports.Count);
    for i := 0 to Ports.Count-1 do
      Device.Ports[i].snmpID := StrToIntDef(Ports.Names[i], -1);
    SetLength(OIDs, Ports.Count);
    // Get Descriptions
    for i := 0 to Ports.Count-1 do
      snmpGet(Device.IP, ifDescr + '.' + Ports.Names[i], Device.Ports[i].Descr);
    // Get MAC
    for i := 0 to Ports.Count-1 do begin
      snmpGet(Device.IP, ifPhysAddress + '.' + Ports.Names[i], str);
      Device.Ports[i].MAC := str;
    end;
  finally
    Ports.Free;
  end;
end;
procedure TfrmMain.Log(msg: String; ShowTime: Boolean = false);
var
  strTime: String;
begin
  if ShowTime then
    strTime := TimeToStr(now()) + ': '
  else
    strTime := '';
  Memo.Lines.Add(strTime + msg);
end;
//  *** Actions
procedure TfrmMain.mScanClick(Sender: TObject);
var
  nData: ^TTreeData;
  Reply: TStringList;
  cnt, i: Integer;
  r: Boolean;
  nRoot: PVirtualNode;
begin
  Log('Scanning Start', true);
  ipRoot.IP := cfg.ReadString('General','Root','0.0.0.0');
  Log('  Root device: ' + ipRoot.IP);
  getDeviceDescr(ipRoot);
  //Log('    sysDescr: ' + ipRoot.sysDescr);
  Log('    sysName: ' + ipRoot.sysName + '['+Bin2MAC(ipRoot.MAC)+']');
  with vstDevices do begin
    nRoot := AddChild(nil);
    nData := GetNodeData(nRoot);
    nData.idx := -1;
    nData.oType := otRootDevice;
    nData.Parent := nil;
  end;
  getDevicePorts(ipRoot);
  for i := 0 to High(ipRoot.Ports) do
    with vstDevices do begin
      nData := GetNodeData(AddChild(nRoot));
      nData.idx := i;
      nData.oType := otPort;
      //nData.Parent := nRoot;
    end;
end;
procedure TfrmMain.mToolsClick(Sender: TObject);
begin

end;

// *** Tree Events

procedure TfrmMain.vstDevicesGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
var
  nData: ^TTreeData;
begin
  nData := (Sender as TVirtualStringTree).GetNodeData(Node);
  case nData.oType of
    otRootDevice:
      HintText := ipRoot.sysName + CrLf +
                  ipRoot.sysDescr + CrLf +
                  ipRoot.sysContact + CrLf +
                  ipRoot.sysLocation;
  end;
end;

procedure TfrmMain.vstDevicesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  nData, rData: ^TTreeData;
begin
  nData := (Sender as TVirtualStringTree).GetNodeData(Node);
  case nData.oType of
    otRootDevice:
      CellText := ipRoot.sysName;
    otPort: begin
      rData := (Sender as TVirtualStringTree).GetNodeData(Node.Parent);
      case rData.oType of
        otRootDevice:
          CellText := ipRoot.Ports[nData.idx].Descr;
      end;
    end;
  end;
end;
end.

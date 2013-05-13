unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.IniFiles,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus,
  VirtualTrees,
  Utils;

type
  objType = (otRootDevice, otDevice, otPort, otUnsorted);
  TTreeData = record
    idx: Integer;
    oType: objType;
    Parent: PVirtualNode;
  end;
  TsnmpReply = array of String;
  TIP = String[15];
  TPort = record
    snmpID: String;
    Descr: String;
    MAC: TMAC;
    MACList: array of TMAC;
  end;
  TDevice = record
    IP: TIP;
    MAC: TMAC;
    sysDescr, sysContact, sysName, sysLocation: String;
    Ports: array of TPort;
    MACList: array of TMAC;
  end;
  TfrmMain = class(TForm)
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
    snmpVersion: Integer;
    snmpRO: String;
    mngtVlan: String;
    function getDeviceDescr (var Device: TDevice): Boolean;
    function getDevicePorts (var Device: TDevice; Vlan: String = ''): Boolean;
  public
    ipRoot: TDevice;
    ipRange: array of TDevice;
    procedure Log(msg: String; ShowTime: Boolean = false);
    procedure LogEOL(msg: String);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  IdASN1Util, snmpsend;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(cfg) then cfg.Free;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  cfg := TINIFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  snmpVersion := cfg.ReadInteger('General', 'snmpVer', 1);
  snmpRO := cfg.ReadString('General', 'snmpRO', 'public');
  mngtVlan := cfg.ReadString('General', 'Vlan', '1');

  vstDevices.NodeDataSize := SizeOf(TTreeData);
  Log('Application Started.', true);
end;

// **** Utilites ****
function TfrmMain.getDeviceDescr (var Device: TDevice): Boolean;
var
  Reply: TsnmpReply;
  dDescr: array [0..4] of String;
  Value: AnsiString;
begin
  SNMPGet(sysDescr, snmpRO, Device.IP, Value);
  Device.sysDescr := Value;
  SNMPGet(sysContact, snmpRO, Device.IP, Value);
  Device.sysContact := Value;
  SNMPGet(sysName, snmpRO, Device.IP, Value);
  Device.sysName := Value;
  SNMPGet(sysLocation, snmpRO, Device.IP, Value);
  Device.sysLocation := Value;
  SNMPGet(dot1dBaseBridgeAddress, snmpRO, Device.IP, Value);
  Device.MAC := Value;
  Application.ProcessMessages;
end;
function TfrmMain.getDevicePorts (var Device: TDevice; Vlan: String = ''): Boolean;
var
  Ports: TStringList;
  Reply: TsnmpReply;
  OIDs: array of String;
  i, n, h, l: Integer;
  OID, Community, SNMPHost, Value: AnsiString;
  mac: TMAC;
begin
  Result := False;
  Ports := TStringList.Create;
  SNMPHost := Device.IP;
  Community := snmpRO;
  try
    Log('Scan Ethernet ports for ' + Device.IP + ' [' + MAC_bin2str(Device.MAC) + '] ');
    // Get ports List
    OID := ifType;
    if not SNMPGetBulk(OID, Community, SNMPHost, Ports) then Exit;
    if Ports.Count < 1 then Exit;
    i := 0;
    while i < Ports.Count do begin
      Application.ProcessMessages;
      n := StrToInt(Ports.ValueFromIndex[i]);
      //if not ((n = 6) or (n = 135)) then
      if not (n = 6) then
        Ports.Delete(i)
      else
        Inc(i);
    end;
    SetLength(Device.Ports, Ports.Count);
    for i := 0 to Ports.Count-1 do
      Device.Ports[i].snmpID := Ports.Names[i];
    //SetLength(OIDs, Ports.Count);
    LogEOL('.');
    // Get Descriptions
    for i := 0 to High(Device.Ports) do begin
      Application.ProcessMessages;
      OID := ifDescr + '.' + Device.Ports[i].snmpID;
      if not SNMPGet(OID, Community, SNMPHost, Value) then Exit;
      Device.Ports[i].Descr := Value;
    end;
    LogEOL('.');
    // Get MAC
    for i := 0 to High(Device.Ports) do begin
      Application.ProcessMessages;
      OID := ifPhysAddress + '.' + Device.Ports[i].snmpID;
      if not SNMPGet(OID, Community, SNMPHost, Value) then Exit;
      Device.Ports[i].MAC := Value;
      SetLength(Device.Ports[i].MACList, 0);
    end;
    LogEOL ('. ' + IntToStr(Ports.Count) + ' found.');
    Application.ProcessMessages;

    Log('Scan MAC table for ' + Device.IP + ' [' + MAC_bin2str(Device.MAC) + '] .. ');
    OID := dot1dTpFdbPort;
    if Length(Vlan) > 0 then begin
      if not SNMPGetBulk(OID, Community, SNMPHost, Ports, Vlan) then Exit;
    end else begin
      if not SNMPGetBulk(OID, Community, SNMPHost, Ports) then Exit;
    end;
    LogEOL ('.');
    for i := 0 to Ports.Count-1 do begin
      Application.ProcessMessages;
      n := StrToInt(Ports.ValueFromIndex[i]) - 1;
      mac := MAC_oid2bin(Ports.Names[i]);
      l := Length(Device.Ports[n].MACList);
      SetLength(Device.Ports[n].MACList, l + 1);
      Device.Ports[n].MACList[l] := mac;
    end;
    LogEOL(' ' + IntToStr(Ports.Count) + ' found.');
    Application.ProcessMessages;
    Result := true;
  finally
    Ports.Free;
    if not Result then LogEOL(' Error!');
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
procedure TfrmMain.LogEOL(msg: String);
var
  n: Integer;
begin
  n := Memo.Lines.Count - 1;
  Memo.Lines[n] := Memo.Lines[n] + msg;
end;
//  *** Actions
procedure TfrmMain.mScanClick(Sender: TObject);
var
  rData, nData, cData, uData: ^TTreeData;
  i, n, d, p, m: Integer;
  nRoot, uNode, Node, cNode, tNode: PVirtualNode;
  List: TStringList;
begin
  Log('Scanning Start', true);
  ipRoot.IP := cfg.ReadString('General','IP','0.0.0.0');
  Log('  Root device: ' + ipRoot.IP);
  // Scan Root Device;
  getDeviceDescr(ipRoot);
  Log('    sysName: ' + ipRoot.sysName + '['+MAC_bin2str(ipRoot.MAC)+']');
  with vstDevices do begin
    BeginUpdate;
    nRoot := AddChild(nil);
    nData := GetNodeData(nRoot);
    nData.Parent := nil;
    nData.idx := -1;
    nData.oType := otRootDevice;
    FocusedNode := nRoot;
    FullyVisible[nRoot] := True;
    EndUpdate;
  end;

  getDevicePorts(ipRoot, mngtVlan);
  for i := 0 to High(ipRoot.Ports) do
    with vstDevices do begin
      BeginUpdate;
      Node := AddChild(nRoot);
      nData := GetNodeData(Node);
      nData.Parent := nRoot;
      nData.idx := i;
      nData.oType := otPort;
      FocusedNode := Node;
      FullyVisible[Node] := True;
      EndUpdate;
      Application.ProcessMessages;
    end;

  // Scan Child Devices
  Log ('Scan child devices.', True);
  uNode := vstDevices.AddChild(nil);
  uData := vstDevices.GetNodeData(uNode);
  uData.oType := otUnsorted;
  List := TStringList.Create;
  try
    List.Delimiter := ',';
    List.DelimitedText := cfg.ReadString('ipList', 'R01', ipRoot.IP);
    if List.Count < 2 then Exit;
    SetLength(ipRange, List.Count);
    for i := 0 to List.Count-1 do begin
      Application.ProcessMessages;
      ipRange[i].IP := List[i];
      getDeviceDescr(ipRange[i]);
      Node := vstDevices.GetFirstChild(nRoot);
      with vstDevices do begin
        BeginUpdate;
        Node := AddChild(uNode);
        nData := GetNodeData(Node);
        nData.Parent := uNode;
        nData.idx := i;
        nData.oType := otDevice;
        FocusedNode := Node;
        FullyVisible[Node] := True;
        EndUpdate;
      end;
      Application.ProcessMessages;

      getDevicePorts(ipRange[i]);
      for p := 0 to High(ipRange[i].Ports) do
        with vstDevices do begin
          BeginUpdate;
          cNode := AddChild(Node);
          cData := GetNodeData(cNode);
          cData.Parent := Node;
          cData.idx := p;
          cData.oType := otPort;
          FocusedNode := cNode;
          FullyVisible[cNode] := True;
          EndUpdate;
          Application.ProcessMessages;
      end;
    end;
  finally
    List.Free;
  end;

  Log ('Split child devices.', True);
  Node := vstDevices.GetFirstChild(nRoot);
  while not (Node = nil) do begin
    nData := vstDevices.GetNodeData(Node);
    // Scan unsorted;
    cNode := vstDevices.GetFirstChild(uNode);
    while not (cNode = nil) do begin
      cData := vstDevices.GetNodeData(cNode);
      tNode := vstDevices.GetNextSibling(cNode);
      if Length(ipRoot.Ports[nData.idx].MACList) > 0 then
        for m := 0 to High(ipRoot.Ports[nData.idx].MACList) do
          if ipRange[cData.idx].MAC = ipRoot.Ports[nData.idx].MACList[m] then begin
            vstDevices.MoveTo(cNode, Node, amAddChildLast, false);
            vstDevices.FocusedNode := cNode;
            vstDevices.FullyVisible[cNode] := True;
            Break;
          end;
      cNode := tNode;
      Application.ProcessMessages;
    end;
    Node := vstDevices.GetNextSibling(Node);
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
      CellText := ipRoot.sysName + ' [' + MAC_bin2str(ipRoot.MAC) + ']';
    otDevice:
      CellText := ipRange[nData.idx].sysName + ' [' + MAC_bin2str(ipRange[nData.idx].MAC) + ']';
    otPort: begin
      rData := (Sender as TVirtualStringTree).GetNodeData(nData.Parent);
      case rData.oType of
        otRootDevice:
          CellText := ipRoot.Ports[nData.idx].Descr + ' [' + MAC_bin2str(ipRoot.Ports[nData.idx].MAC) + ']';
        otDevice:
          CellText := ipRange[rData.idx].Ports[nData.idx].Descr + ' [' + MAC_bin2str(ipRange[rData.idx].Ports[nData.idx].MAC) + ']';
      end;
    end;
    otUnsorted:
      CellText := 'Unsorted';
  end;
end;
end.

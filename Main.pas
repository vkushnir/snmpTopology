unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.IniFiles,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus,
  VirtualTrees,
  Utils, Vcl.ImgList;

type
  objType = (otRootDevice, otDevice, otPort, otUnsorted);
  TTreeData = record
    idx: Integer;
    oType: objType;
    Parent: PVirtualNode;
  end;
  PTreeData = ^TTreeData;
  TsnmpReply = array of String;
  TIP = String[15];
  TPort = record
    snmpID: String;
    Descr: String;
    MAC: TMAC;
    Node: PVirtualNode;
    Weight: Integer;
  end;
  TPortMAC = record
    Port: Integer;
    MAC: TMAC;
  end;
  TDevice = record
    IP: TIP;
    MAC: TMAC;
    sysDescr, sysContact, sysName, sysLocation: String;
    Ports: array of TPort;
    MACList: array of TPortMAC;
    Node: PVirtualNode;
    MaxPort, MinPort: Integer;
  end;
  PDevice = ^TDevice;
  TfrmMain = class(TForm)
    Memo: TMemo;
    vstDevices: TVirtualStringTree;
    Menu: TMainMenu;
    mScan: TMenuItem;
    mTools: TMenuItem;
    mFile: TMenuItem;
    mExit: TMenuItem;
    Icons: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure mScanClick(Sender: TObject);
    procedure vstDevicesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure mToolsClick(Sender: TObject);
    procedure vstDevicesNodeDblClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
    procedure vstDevicesGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
  private
    cfg: TINIFile;
    snmpVersion: Integer;
    snmpRO: String;
    mngtVlan: String;
    function getDeviceDescr (var Device: TDevice; Vlan: String = '1'): Boolean;
    function getDevicePorts (var Device: TDevice; Vlan: String = ''): Boolean;
    function isChildDevice (RootDevice, TestDevice: TDevice): Boolean; overload;
    function isChildDevice (RootDevice, TestDevice: TDevice; var Port: Integer): Boolean; overload;
    function isChildDevice (RootDevice: TDevice; TestMAC: TMAC): Boolean; overload;
    function isChildDevice (RootDevice: TDevice; TestMAC: TMAC; var Port: Integer): Boolean; overload;
    function MaxPort (Device: PDevice): Integer;
    function MinPort (Device: PDevice): Integer;
    procedure ScanChilds (nodeRoot: PVirtualNode; macRoot: TMAC; nodesDevices: array of PVirtualNode);
    procedure SortChilds (var Neighbors: array of PDevice);
    procedure FindPortWeights (rootMAC: TMAC; Neighbors: array of PDevice);
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
function TfrmMain.MaxPort (Device: PDevice): Integer;
var
  i, MaxWeight: Integer;
begin
  Result := -1;
  MaxWeight := -MAXINT;
  for i := 0 to High(Device.Ports) do
    if Device.Ports[i].Weight > MaxWeight then begin
      MaxWeight := Device.Ports[i].Weight;
      Result := i;
    end;
end;
function TfrmMain.MinPort (Device: PDevice): Integer;
var
  i, MinWeight: Integer;
begin
  Result := -1;
  MinWeight := MAXINT;
  for i := 0 to High(Device.Ports) do
    if Device.Ports[i].Weight < MinWeight then begin
      MinWeight := Device.Ports[i].Weight;
      Result := i;
    end;
end;

procedure TfrmMain.SortChilds (var Neighbors: array of PDevice);
var
  i, n, p, iMax, nMax: Integer;
  swap: PDevice;
begin
  for i := 0 to High(Neighbors) do
    for n := i + 1 to High(Neighbors) do begin
      iMax := Neighbors[i].Ports[Neighbors[i].MaxPort].Weight;
      nMax := Neighbors[n].Ports[Neighbors[n].MaxPort].Weight;
      if iMax > nMax then begin
        swap := Neighbors[i];
        Neighbors[i] := Neighbors[n];
        Neighbors[n] := swap;
      end;
    end;
end;

procedure TfrmMain.FindPortWeights (rootMAC: TMAC; Neighbors: array of PDevice);
var
  p, i, m, d: Integer;
  mac: TMAC;
begin
  for d := 0 to High(Neighbors) do begin
    for m := 0 to High(Neighbors[d].MACList) do begin
      for i := 0 to High(Neighbors) do begin
        if (i <> d) and (Neighbors[d].MACList[m].MAC = Neighbors[i].MAC) then
          Dec(Neighbors[d].Ports[Neighbors[d].MACList[m].Port].Weight);
      end;
    end;
  end;
  for d := 0 to High(Neighbors) do begin
    for m := 0 to High(Neighbors[d].MACList) do begin
      if Neighbors[d].MACList[m].MAC = rootMAC then begin
        Neighbors[d].Ports[Neighbors[d].MACList[m].Port].Weight := Neighbors[d].Ports[Neighbors[d].MACList[m].Port].Weight * -1;
        Inc(Neighbors[d].Ports[Neighbors[d].MACList[m].Port].Weight);
      end;
    end;
  end;
  for i := 0 to High(Neighbors) do begin
    Neighbors[i].MaxPort := MaxPort(Neighbors[i]);
    Neighbors[i].MinPort := MinPort(Neighbors[i]);
  end;
end;
procedure TfrmMain.ScanChilds (nodeRoot: PVirtualNode; macRoot: TMAC; nodesDevices: array of PVirtualNode);
var
  i: Integer;
  dataRoot, dataChild, dataPort, dataDevice, data, dataUnsorted: ^TTreeData;
  n, d, p, m, rootPort, devicePort: Integer;
  nodeChild, nodePort, nodeDevice, node, nodeUnsorted, tNode: PVirtualNode;
begin
   for i :=0  to High(nodesDevices) do begin
    dataDevice := vstDevices.GetNodeData(nodesDevices[i]);
    if (not (ipRange[dataDevice.idx].MAC = macRoot)) and
       isChildDevice(ipRange[dataDevice.idx], macRoot, devicePort) then begin
      with vstDevices do begin
        nodeChild := AddChild(nodeRoot);
        dataChild := GetNodeData(nodeChild);
        dataChild.Parent := nodesDevices[i];
        dataChild.idx := devicePort;
        dataChild.oType := otPort;
        ipRange[dataDevice.idx].Ports[devicePort].Node := nodeChild;
        MoveTo(nodesDevices[i], nodeChild, amAddChildLast, false);
      end;
      LogEOL('.');
    end;
  end;
end;

function TfrmMain.isChildDevice (RootDevice: TDevice; TestMAC: TMAC; var Port: Integer): Boolean;
var
  i, n: Integer;
begin
  Port := -1;
  n := Length(RootDevice.MACList);
  Result := false;
  for i := 0 to n-1 do
    if RootDevice.MACList[i].MAC = TestMAC then begin
      Result := true;
      Port := RootDevice.MACList[i].Port;
    end;
end;
function TfrmMain.isChildDevice (RootDevice: TDevice; TestMAC: TMAC): Boolean;
var
  Port: Integer;
begin
  Result := isChildDevice(RootDevice, TestMAC, Port);
end;

function TfrmMain.isChildDevice (RootDevice, TestDevice: TDevice; var Port: Integer): Boolean;
var
  i, n: Integer;
begin
  Port := -1;
  n := Length(RootDevice.MACList);
  Result := false;
  for i := 0 to n-1 do
    if RootDevice.MACList[i].MAC = TestDevice.MAC then begin
      Result := true;
      Port := RootDevice.MACList[i].Port;
    end;
end;
function TfrmMain.isChildDevice (RootDevice, TestDevice: TDevice): Boolean;
var
  Port: Integer;
begin
  Result := isChildDevice (RootDevice, TestDevice, Port);
end;

function TfrmMain.getDeviceDescr (var Device: TDevice; Vlan: String = '1'): Boolean;
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
  if (not SNMPGet(Concat(ipNetToMediaPhysAddress, '.', Vlan, '.', Device.IP) , snmpRO, Device.IP, Value)) or
     (Length(Value) = 0) then
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
      Device.Ports[i].Weight := 0;
    //SetLength(OIDs, Ports.Count);
    LogEOL('.');
    Application.ProcessMessages;
    // Get Descriptions
    for i := 0 to High(Device.Ports) do begin      
      OID := ifDescr + '.' + Device.Ports[i].snmpID;
      if not SNMPGet(OID, Community, SNMPHost, Value) then Exit;
      Device.Ports[i].Descr := Value;
    end;
    LogEOL('.');
    Application.ProcessMessages;
    // Get MAC
    for i := 0 to High(Device.Ports) do begin
      OID := ifPhysAddress + '.' + Device.Ports[i].snmpID;
      if not SNMPGet(OID, Community, SNMPHost, Value) then Exit;
      Device.Ports[i].MAC := Value;
      //SetLength(Device.Ports[i].MACList, 0);
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
    SetLength(Device.MACList, Ports.Count);
    if Ports.Count > 0 then begin                
      for i := 0 to Ports.Count-1 do begin        
        n := StrToInt(Ports.ValueFromIndex[i]) - 1;
        mac := MAC_oid2bin(Ports.Names[i]);
        Device.MACList[i].Port := n;
        Device.MACList[i].MAC := mac;
      end;    
    end else
      SetLength(Device.MACList, 0);
    LogEOL(' ' + IntToStr(Ports.Count) + ' found.');    
    Result := true;
  finally
    Ports.Free;
    if not Result then LogEOL(' Error!');
    Application.ProcessMessages;
  end;
end;

procedure TfrmMain.Log(msg: String; ShowTime: Boolean = false);
var
  strTime: String;
begin
  if ShowTime then
    strTime := TimeToStr(now()) + ': '
  else
    strTime := '    ';
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
  dataRoot, dataChild, dataPort, dataDevice, data, dataUnsorted, tData: ^TTreeData;
  i, n, d, p, m, rootPort, devicePort: Integer;
  nodeRoot, nodeChild, nodePort, nodeDevice, node, nodeUnsorted, tNode: PVirtualNode;
  List: TStringList;
  nodesDevices: array of PVirtualNode;
  macPort: TMAC;
  Neighbors: array of PDevice;
begin
  Log('Scanning Start', true);
  vstDevices.Clear;
  ipRoot.IP := cfg.ReadString('General','IP','0.0.0.0');
  Log('  Root device: ' + ipRoot.IP);
  // Scan Root Device;
  getDeviceDescr(ipRoot, mngtVlan);
  Log('    sysName: ' + ipRoot.sysName + '['+MAC_bin2str(ipRoot.MAC)+']');
  with vstDevices do begin
    BeginUpdate;
    nodeRoot := AddChild(nil);
    dataRoot := GetNodeData(nodeRoot);
    ipRoot.Node := nodeRoot;
    dataRoot.Parent := nil;
    dataRoot.idx := -1;
    dataRoot.oType := otRootDevice;    
    FocusedNode := nodeRoot;
    FullyVisible[nodeRoot] := True;
    EndUpdate;
  end;

  getDevicePorts(ipRoot, mngtVlan);  
    with vstDevices do begin
      BeginUpdate;
      for i := 0 to High(ipRoot.Ports) do begin
        nodePort := AddChild(nodeRoot);
        dataPort := GetNodeData(nodePort);
        ipRoot.Ports[i].Node := nodePort;
        dataPort.Parent := nodeRoot;
        dataPort.idx := i;
        dataPort.oType := otPort;
        FocusedNode := nodePort;
        FullyVisible[nodePort] := True;
      end;
      EndUpdate;
    end;
  Application.ProcessMessages;

  // Scan Child Devices
  Log ('Scan child devices.', True);
  nodeUnsorted := vstDevices.AddChild(nil);
  dataUnsorted := vstDevices.GetNodeData(nodeUnsorted);
  dataUnsorted.oType := otUnsorted;
  dataUnsorted.idx := -1;
  dataUnsorted.Parent := nil;

  List := TStringList.Create;
  vstDevices.BeginUpdate;
  try
    List.Delimiter := ',';
    List.DelimitedText := cfg.ReadString('ipList', 'R01', ipRoot.IP);
    if List.Count < 1 then Exit;
    SetLength(ipRange, List.Count);

    for i := 0 to List.Count-1 do begin
      ipRange[i].IP := List[i];
      getDeviceDescr(ipRange[i]);
      vstDevices.BeginUpdate;

      with vstDevices do begin
        if isChildDevice(ipRoot, ipRange[i], rootPort) then
          tNode := ipRoot.Ports[rootPort].Node
        else
          tNode := nodeUnsorted;
        nodeDevice := AddChild(tNode);
        ipRange[i].Node := nodeDevice;
        dataDevice := GetNodeData(nodeDevice);
        dataDevice.Parent := nodeUnsorted;
        dataDevice.idx := i;
        dataDevice.oType := otDevice;
      end;

      getDevicePorts(ipRange[i]);
      vstDevices.EndUpdate;
      Application.ProcessMessages;
    end;

    // Sort Device Ports
    vstDevices.BeginUpdate;
    for p := 0 to High(ipRoot.Ports) do begin
      nodePort := ipRoot.Ports[p].Node;
      dataPort := vstDevices.GetNodeData(nodePort);
      Log(Concat('Scan port: ', ipRoot.Ports[dataPort.idx].Descr, ' '));
      macPort := ipRoot.Ports[dataPort.idx].MAC;

      n := vstDevices.ChildCount[nodePort];
      SetLength(nodesDevices, n);
      SetLength(Neighbors, n);
      nodeDevice := vstDevices.GetFirstChild(nodePort);
      // Scan children
      for i :=0  to n-1 do begin
        nodesDevices[i] := nodeDevice;
        dataDevice := vstDevices.GetNodeData(nodeDevice);
        Neighbors[i] := @ipRange[dataDevice.idx];
        nodeDevice := vstDevices.GetNextSibling(nodeDevice);
      end;

      // Sort Childs
      FindPortWeights(ipRoot.MAC, Neighbors);
      SortChilds(Neighbors);
      tNode := nodePort;
      vstDevices.BeginUpdate;
      for i := 0 to High(Neighbors) do
        with vstDevices do begin
          nodeChild := AddChild(tNode);
          dataChild := GetNodeData(nodeChild);
          dataChild.idx := Neighbors[i].MaxPort;
          dataChild.oType := otPort;
          dataChild.Parent := Neighbors[i].Node;
          MoveTo(Neighbors[i].Node, nodeChild, amAddChildLast, false);

          nodeChild := AddChild(Neighbors[i].Node);
          dataChild := GetNodeData(nodeChild);
          dataChild.idx := Neighbors[i].MinPort;
          dataChild.oType := otPort;
          dataChild.Parent := Neighbors[i].Node;
          tNode := nodeChild;
        end;
      vstDevices.EndUpdate;
      LogEOL(' Ok.');
    end;
    vstDevices.EndUpdate;
    Application.ProcessMessages;
  finally
    List.Free;
    vstDevices.EndUpdate;
  end;
end;
procedure TfrmMain.mToolsClick(Sender: TObject);
begin

end;

// *** Tree Events

procedure TfrmMain.vstDevicesGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  NodeData: PTreeData;
begin
  with Sender as TVirtualStringTree do begin
    NodeData := GetNodeData(Node);
    case NodeData.oType of
      otRootDevice: ImageIndex := 0;
      otDevice: ImageIndex := 1;
      otPort: ImageIndex := 2;
    end;
  end;
end;

procedure TfrmMain.vstDevicesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  nData, rData: PTreeData;
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
          //CellText := ipRoot.Ports[nData.idx].Descr + ' [' + MAC_bin2str(ipRoot.Ports[nData.idx].MAC) + ']';
          CellText := ipRoot.Ports[nData.idx].Descr;
        otDevice:
          //CellText := ipRange[rData.idx].Ports[nData.idx].Descr + ' [' + MAC_bin2str(ipRange[rData.idx].Ports[nData.idx].MAC) + ']';
          CellText := ipRange[rData.idx].Ports[nData.idx].Descr;
      end;
    end;
    otUnsorted:
      CellText := 'Unsorted';
  end;
end;

procedure TfrmMain.vstDevicesNodeDblClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
begin
  with Sender as TVirtualStringTree do begin
    if Expanded[HitInfo.HitNode] then
      FullCollapse(HitInfo.HitNode)
    else
      FullExpand(HitInfo.HitNode);
  end;
end;

end.

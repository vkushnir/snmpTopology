unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.IniFiles,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus,
  VirtualTrees,
  Utils, Vcl.ImgList, Vcl.ExtCtrls;

type
  objType = (otNone, otRootDevice, otDevice, otPort, otUnsorted);
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
  TARP = record
    IP: TIP;
    MAC: TMAC;
  end;
  TDevice = record
    IP: TIP;
    MAC: TMAC;
    sysDescr, sysContact, sysName, sysLocation: String;
    Ports: array of TPort;
    MACList: array of TPortMAC;
    ARPList: array of TARP;
    Node: PVirtualNode;
    MaxPort, MinPort: Integer;
  end;
  PDevice = ^TDevice;
  TfrmMain = class(TForm)
    Memo: TMemo;
    vstDevices: TVirtualStringTree;
    Menu: TMainMenu;
    mTools: TMenuItem;
    mFile: TMenuItem;
    mExit: TMenuItem;
    Icons: TImageList;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure mScanClick(Sender: TObject);
    procedure vstDevicesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstDevicesNodeClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
    procedure vstDevicesGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vstDevicesInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
  private
    cfg: TINIFile;
    snmpVersion: Integer;
    snmpRO: String;
    mngtVlan: String;
    function GetDeviceDescr (var Device: TDevice; Vlan: String = '1'): Boolean;
    function GetDevicePorts (var Device: TDevice; Vlan: String = ''): Boolean;
    function GetDeviceMACTable (var Device: TDevice; Vlan: String = ''): Boolean;
    function GetDeviceARPTable (var Device: TDevice; Vlan: String = ''): Boolean;
    function isChildDevice (RootDevice, TestDevice: TDevice): Boolean; overload;
    function isChildDevice (RootDevice, TestDevice: TDevice; var Port: Integer): Boolean; overload;
    //function isChildDevice (RootDevice: TDevice; TestMAC: TMAC; var Port: Integer): Boolean; overload;
    function MaxPort (Device: TDevice): Integer;
    function MinPort (Device: TDevice): Integer;
    procedure ScanChilds (Device: TDevice; Port: Integer; var Childs: TStringList);
    procedure SortChilds (var Neighbors: array of Integer);
    procedure FindPortWeights (rootMAC: TMAC; Neighbors: array of Integer);
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
var
  ANList: TStringList;
  mItem: TMenuItem;
  i: Integer;
begin
  cfg := TINIFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  ANList := TStringList.Create;
  try
    cfg.ReadSections(ANList);
    for i := 0 to ANList.Count - 1 do begin
      mItem := TMenuItem.Create(Menu);
      mItem.Caption := Concat('Scan ', ANList[i]);
      mItem.Hint := ANList[i];
      mItem.OnClick := mScanClick;
      mItem.ImageIndex := 0;
      mTools.Add(mItem);
    end;
  finally
    ANList.Free;
  end;
  vstDevices.NodeDataSize := SizeOf(TTreeData);
  Log('Application Started.', true);
end;

// **** Utilites ****
function TfrmMain.MaxPort (Device: TDevice): Integer;
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
function TfrmMain.MinPort (Device: TDevice): Integer;
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

procedure TfrmMain.SortChilds (var Neighbors: array of Integer);
var
  i, n, iMax, nMax, swap: Integer;
begin
  for i := 0 to High(Neighbors) do
    for n := i + 1 to High(Neighbors) do begin
      iMax := ipRange[Neighbors[i]].Ports[ipRange[Neighbors[i]].MaxPort].Weight;
      nMax := ipRange[Neighbors[n]].Ports[ipRange[Neighbors[n]].MaxPort].Weight;
      if iMax > nMax then begin
        swap := Neighbors[i];
        Neighbors[i] := Neighbors[n];
        Neighbors[n] := swap;
      end;
    end;
end;

procedure TfrmMain.FindPortWeights (rootMAC: TMAC; Neighbors: array of Integer);
var
  i, m, d: Integer;
begin
  for d := 0 to High(Neighbors) do begin
    for i := 0 to High(Neighbors) do begin
      if i <> d then
        for m := 0 to High(ipRange[Neighbors[d]].MACList) do begin
          if ipRange[Neighbors[d]].MACList[m].MAC = ipRange[Neighbors[i]].MAC then
            Dec(ipRange[Neighbors[d]].Ports[ipRange[Neighbors[d]].MACList[m].Port].Weight);
        end;
    end;
  end;
  for d := 0 to High(Neighbors) do begin
    for m := 0 to High(ipRange[Neighbors[d]].MACList) do begin
      if ipRange[Neighbors[d]].MACList[m].MAC = rootMAC then begin
        ipRange[Neighbors[d]].Ports[ipRange[Neighbors[d]].MACList[m].Port].Weight := - ipRange[Neighbors[d]].Ports[ipRange[Neighbors[d]].MACList[m].Port].Weight;
        Inc(ipRange[Neighbors[d]].Ports[ipRange[Neighbors[d]].MACList[m].Port].Weight);
      end;
    end;
  end;
{  for d := 0 to High(Neighbors) do begin
    for m := 0 to High(ipRange[Neighbors[d]].MACList) do begin
      for i := 0 to High(Neighbors) do begin
        if (i <> d) and (ipRange[Neighbors[d]].MACList[m].MAC = ipRange[Neighbors[i]].MAC) then
          Dec(ipRange[Neighbors[d]].Ports[ipRange[Neighbors[d]].MACList[m].Port].Weight);
      end;
    end;
  end;
  for d := 0 to High(Neighbors) do begin
    for m := 0 to High(ipRange[Neighbors[d]].MACList) do begin
      if ipRange[Neighbors[d]].MACList[m].MAC = rootMAC then begin
        ipRange[Neighbors[d]].Ports[ipRange[Neighbors[d]].MACList[m].Port].Weight := ipRange[Neighbors[d]].Ports[ipRange[Neighbors[d]].MACList[m].Port].Weight * -1;
        Inc(ipRange[Neighbors[d]].Ports[ipRange[Neighbors[d]].MACList[m].Port].Weight);
      end;
    end;
  end;}
  for i := 0 to High(Neighbors) do begin
    ipRange[Neighbors[i]].MaxPort := MaxPort(ipRange[Neighbors[i]]);
    ipRange[Neighbors[i]].MinPort := MinPort(ipRange[Neighbors[i]]);
  end;
end;

procedure TfrmMain.ScanChilds (Device: TDevice; Port: Integer; var Childs: TStringList);
var
  a, m: Integer;
begin
    for m := 0 to High(Device.MACList) do
      if Device.MACList[m].Port = Port then
        for a := 0 to High(Device.ARPList) do
          if Device.ARPList[a].MAC = Device.MACList[m].MAC then
            Childs.Add(Device.ARPList[a].IP);
end;

{function TfrmMain.isChildDevice (RootDevice: TDevice; TestMAC: TMAC; var Port: Integer): Boolean;
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
end;}
{function TfrmMain.isChildDevice (RootDevice: TDevice; TestMAC: TMAC): Boolean;
var
  Port: Integer;
begin
  Result := isChildDevice(RootDevice, TestMAC, Port);
end;}

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
  Value: AnsiString;
begin
  Result := False;
  Log(Concat('Get device description: ', Device.IP), true);
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
  LogEOL(Concat(' [', MAC_bin2str(Device.MAC), ']'));
  Result := true;
end;

function TfrmMain.getDevicePorts (var Device: TDevice; Vlan: String = ''): Boolean;
var
  Ports: TStringList;
  i, n: Integer;
  OID, Value: AnsiString;
begin
  Result := False;
  Ports := TStringList.Create;
  try
    Log('Scan Ethernet ports for ' + Device.IP + ' [' + MAC_bin2str(Device.MAC) + '] ', true);
    // Get ports List
    if not SNMPGetBulk(ifType, snmpRO, Device.IP, Ports) then Exit;
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
    for i := 0 to Ports.Count-1 do begin
      Device.Ports[i].snmpID := Ports.Names[i];
      Device.Ports[i].Weight := 0;
    end;
    LogEOL('.');
    Application.ProcessMessages;
    // Get Descriptions
    for i := 0 to High(Device.Ports) do begin      
      OID := ifDescr + '.' + Device.Ports[i].snmpID;
      if not SNMPGet(OID, snmpRO, Device.IP, Value) then Exit;
      Device.Ports[i].Descr := Value;
    end;
    LogEOL('.');
    Application.ProcessMessages;
    // Get MAC
    for i := 0 to High(Device.Ports) do begin
      OID := ifPhysAddress + '.' + Device.Ports[i].snmpID;
      if not SNMPGet(OID, snmpRO, Device.IP, Value) then Exit;
      Device.Ports[i].MAC := Value;
      //SetLength(Device.Ports[i].MACList, 0);
    end;
    LogEOL ('. ' + IntToStr(Ports.Count) + ' found.');
    Application.ProcessMessages;

{    Log('Scan MAC table for ' + Device.IP + ' [' + MAC_bin2str(Device.MAC) + '] .. ');
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
    LogEOL(' ' + IntToStr(Ports.Count) + ' found.');  }
    Result := true;
  finally
    Ports.Free;
    if not Result then LogEOL(' Error!');
    Application.ProcessMessages;
  end;
end;

function TfrmMain.GetDeviceMACTable (var Device: TDevice; Vlan: String = ''): Boolean;
var
  MACList: TStringList;
  i, n: Integer;
  mac: TMAC;
begin
  Result := false;
  Log('Scan MAC table for ' + Device.IP + ' [' + MAC_bin2str(Device.MAC) + '] .', true);
  MACList := TStringList.Create;
  try
    LogEOL('.');
    if Length(Vlan) > 0 then begin
      if not SNMPGetBulk(dot1dTpFdbPort, snmpRO, Device.IP, MACList, Vlan) then Exit;
    end else begin
      if not SNMPGetBulk(dot1dTpFdbPort, snmpRO, Device.IP, MACList) then Exit;
    end;
    LogEOL ('.');
    SetLength(Device.MACList, MACList.Count);
    if MACList.Count > 0 then begin
      for i := 0 to MACList.Count-1 do begin
        n := StrToInt(MACList.ValueFromIndex[i]) - 1;
        mac := MAC_oid2bin(MACList.Names[i]);
        Device.MACList[i].Port := n;
        Device.MACList[i].MAC := mac;
      end;
    end else
      SetLength(Device.MACList, 0);
    LogEOL(' ' + IntToStr(MACList.Count) + ' found.');
    Result := true;
  finally
    MACList.Free;
    if not Result then LogEOL(' Error!');
  end;
end;

function TfrmMain.GetDeviceARPTable (var Device: TDevice; Vlan: String = ''): Boolean;
var
  ARPList: TStringList;
  i, n, p: Integer;
  mac: TMAC;
  ip: TIP;
  Value: String;
begin
  Result := false;
  Log('Scan ARP table for ' + Device.IP + ' [' + MAC_bin2str(Device.MAC) + '] .', true);
  ARPList := TStringList.Create;
  try
    if Length(Vlan) > 0 then begin
      if not SNMPGetBulk(ipNetToMediaPhysAddress, snmpRO, Device.IP, ARPList, Vlan) then Exit;
      LogEOL('.');
      i := 0;
      n := Length(Vlan);
      while i < ARPList.Count do begin
        Value := ARPList.Names[i];
        p := Pos(Vlan, Value);
        if p = 1 then begin
          ARPList[i] := Copy(ARPList[i], n+2, MAXINT);
          if ARPList.Names[i] = Device.IP then
            ARPList.Delete(i)
          else
            Inc(i);
        end else begin
          ARPList.Delete(i);
        end;
      end;
    end else begin
      if not SNMPGetBulk(ipNetToMediaPhysAddress, snmpRO, Device.IP, ARPList) then Exit;
      LogEOL('.');
    end;
    LogEOL('.');
    SetLength(Device.ARPList, ARPList.Count);
    if ARPList.Count > 0 then begin
      for i := 0 to ARPList.Count-1 do begin
        ip := ARPList.Names[i];
        mac := ARPList.ValueFromIndex[i];
        Device.ARPList[i].IP := ip;
        Device.ARPList[i].MAC := mac;
      end;
    end else
      SetLength(Device.ARPList, 0);
    LogEOL(' ' + IntToStr(ARPList.Count) + ' found.');
    Result := true;
  finally
    ARPList.Free;
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
  dataRoot: PTreeData;
  nodeRoot: PVirtualNode;
  Section: String;
begin
  vstDevices.Clear;
  SetLength(ipRange, 0);
  Section := (Sender as TMenuItem).Hint;
  Log(Concat('Scanning Start "', Section, '"'), true);
  snmpVersion := cfg.ReadInteger(Section, 'snmpVer', 1);
  snmpRO := cfg.ReadString(Section, 'snmpRO', 'public');
  mngtVlan := cfg.ReadString(Section, 'Vlan', '1');
  ipRoot.IP := cfg.ReadString(Section,'IP','0.0.0.0');

  //Log('Root device: ' + ipRoot.IP);
  // Scan Root Device;

  GetDeviceDescr(ipRoot, mngtVlan);
  GetDeviceMACTable(ipRoot, mngtVlan);
  GetDeviceARPTable(ipRoot, mngtVlan);
  GetDevicePorts(ipRoot, mngtVlan);
  //Log('  sysName: ' + ipRoot.sysName + '['+MAC_bin2str(ipRoot.MAC)+']');
  with vstDevices do begin
    BeginUpdate;
    nodeRoot := AddChild(nil);
    dataRoot := GetNodeData(nodeRoot);
    ipRoot.Node := nodeRoot;
    dataRoot.Parent := nil;
    dataRoot.idx := -1;
    dataRoot.oType := otRootDevice;
    HasChildren[nodeRoot] := Length(ipRoot.Ports) > 0;
    EndUpdate;
  end;
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
          CellText := Concat(ipRange[rData.idx].Ports[nData.idx].Descr, ' (', Format('%.*d', [2, nData.idx + 1]), ')');
      end;
    end;
    otUnsorted:
      CellText := 'Unsorted';
  end;
end;

procedure TfrmMain.vstDevicesInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
var
  PortData, ParentData, ChildData, NodeData: PTreeData;
  PortNode, ChildNode, NodeNode: PVirtualNode;
  i, n: Integer;
  Childs: TStringList;
  DeviceIdx: array of Integer;
begin
  NodeData := Sender.GetNodeData(Node);
  case NodeData.oType of
    otRootDevice: begin
      for i := 0 to High(ipRoot.Ports) do begin
        PortNode := Sender.AddChild(Node);
        PortData := Sender.GetNodeData(PortNode);
        ipRoot.Ports[i].Node := PortNode;
        PortData.Parent := Node;
        PortData.idx := i;
        PortData.oType := otPort;
        Childs := TStringList.Create;
        try
          ScanChilds(ipRoot, i, Childs);
          Sender.HasChildren[PortNode] :=  Childs.Count > 0;
        finally
          Childs.Free;
        end;
      end;
      ChildCount := Length(ipRoot.Ports);
    end;
    otPort: begin
      ParentData := Sender.GetNodeData(NodeData.Parent);
      case ParentData.oType of

        otRootDevice: begin
          Childs := TStringList.Create;
          try
            ScanChilds(ipRoot, NodeData.idx, Childs);
            n := Length(ipRange);
            SetLength(ipRange, n + Childs.Count);
            SetLength(DeviceIdx, Childs.Count);
            for i := 0 to Childs.Count - 1 do begin
              ipRange[n+i].IP := Childs[i];
              GetDeviceDescr(ipRange[n+i]);
              GetDevicePorts(ipRange[n+i]);
              GetDeviceMACTable(ipRange[n+i]);
              DeviceIdx[i] := n+i;
            end;
            FindPortWeights(ipRoot.MAC, DeviceIdx);
            SortChilds(DeviceIdx);
            NodeNode := Node;
            for i := 0 to High(DeviceIdx) do begin
              PortNode := Sender.AddChild(NodeNode);
              PortData := Sender.GetNodeData(PortNode);
              PortData.idx := ipRange[DeviceIdx[i]].MaxPort;
              PortData.oType := otPort;
              ipRange[DeviceIdx[i]].Ports[ipRange[DeviceIdx[i]].MaxPort].Node := PortNode;

              ChildNode := Sender.AddChild(PortNode);
              ChildData := Sender.GetNodeData(ChildNode);
              ChildData.idx := DeviceIdx[i];
              ChildData.oType := otDevice;
              ChildData.Parent := Node;
              ipRange[DeviceIdx[i]].Node := ChildNode;
              PortData.Parent := ChildNode;

              PortNode := Sender.AddChild(ChildNode);
              PortData := Sender.GetNodeData(PortNode);
              PortData.idx := ipRange[DeviceIdx[i]].MinPort;
              PortData.oType := otPort;
              ipRange[DeviceIdx[i]].Ports[ipRange[DeviceIdx[i]].MinPort].Node := PortNode;
              PortData.Parent := ChildNode;

              NodeNode := PortNode;
            end;

              ChildCount := 1;
          finally
            Childs.Free;
          end;
        end;

      end;
    end;
  end;
end;

procedure TfrmMain.vstDevicesNodeClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
var
  NodeData: PTreeData;
begin
  with Sender as TVirtualStringTree do begin
    if Expanded[HitInfo.HitNode] then
      FullCollapse(HitInfo.HitNode)
    else begin
      NodeData := Sender.GetNodeData(HitInfo.HitNode);
      if NodeData.oType = otPort then
        FullExpand(HitInfo.HitNode)
      else
        Sender.Expanded[HitInfo.HitNode] := true;
    end;
  end;
end;

end.

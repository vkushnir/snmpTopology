object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'SNMP Topology Scanner'
  ClientHeight = 562
  ClientWidth = 704
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = Menu
  OldCreateOrder = False
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 0
    Top = 376
    Width = 704
    Height = 186
    Hint = 'Log Records'
    Align = alBottom
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object vstDevices: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 704
    Height = 376
    Hint = 'Device Tree'
    Align = alClient
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnGetText = vstDevicesGetText
    OnGetHint = vstDevicesGetHint
    Columns = <>
  end
  object Menu: TMainMenu
    Left = 8
    Top = 8
    object mFile: TMenuItem
      Caption = '&File'
      object mExit: TMenuItem
        Caption = '&Exit'
      end
    end
    object mTools: TMenuItem
      Caption = '&Tools'
      OnClick = mToolsClick
      object mScan: TMenuItem
        Caption = '&Scan'
        OnClick = mScanClick
      end
    end
  end
end

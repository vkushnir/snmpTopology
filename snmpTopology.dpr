program snmpTopology;

uses
  Vcl.Forms,
  Main in 'Main.pas' {frmMain},
  Utils in 'Utils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

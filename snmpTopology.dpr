program snmpTopology;

uses
  Vcl.Forms,
  Main in 'Main.pas' {frmMain},
  Utils in 'Utils.pas',
  asn1util in 'Synapse\asn1util.pas',
  blcksock in 'Synapse\blcksock.pas',
  synacode in 'Synapse\synacode.pas',
  synacrypt in 'Synapse\synacrypt.pas',
  synaip in 'Synapse\synaip.pas',
  synautil in 'Synapse\synautil.pas',
  synafpc in 'Synapse\synafpc.pas',
  synsock in 'Synapse\synsock.pas',
  snmpsend in 'Synapse\snmpsend.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

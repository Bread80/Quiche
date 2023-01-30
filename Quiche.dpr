program Quiche;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  ILData in 'ILData.pas',
  QuicheParser in 'QuicheParser.pas',
  Variables in 'Variables.pas',
  MSourceReader in 'Includes\MSourceReader.pas',
  MErrors in 'Includes\MErrors.pas',
  ILExec in 'ILExec.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

program Quiche;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  ILData in 'ILData.pas',
  MErrors in '..\Z80Assembler\MErrors.pas',
  MSourceReader in '..\Z80Assembler\MSourceReader.pas',
  QuicheParser in 'QuicheParser.pas',
  Variables in 'Variables.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

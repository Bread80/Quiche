program z80Emulator;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  mZ80 in 'mZ80.pas',
  mZ80State in 'mZ80State.pas',
  mMemory in 'mMemory.pas',
  mSymbols in 'mSymbols.pas',
  mz80Tester in 'mz80Tester.pas',
  vRegView in 'vRegView.pas' {RegView},
  MDisassembler in '..\..\Z80Disassembler\MDisassembler.pas',
  MZ80Data in '..\..\Z80Disassembler\MZ80Data.pas',
  devRC2014SIO2 in 'devRC2014SIO2.pas',
  mHardware in 'mHardware.pas',
  vTerminal in 'vTerminal.pas' {TerminalForm},
  vMemView in 'vMemView.pas' {MemView},
  CommandLine in 'CommandLine.pas',
  mConfigFile in 'mConfigFile.pas',
  vBreakpoints in 'vBreakpoints.pas' {Breakpoints},
  mMapFile in 'mMapFile.pas',
  vSourceFileView in 'vSourceFileView.pas' {SourceFileView},
  EditableText in '..\TextEditor\EditableText.pas',
  ViewableText in '..\TextEditor\ViewableText.pas',
  TextBrowser in '..\TextEditor\TextBrowser.pas';

{$R *.res}

begin
//  if true then //ParamCount > 0 then
//    DoCommandLine
//  else
  begin
    Application.Initialize;
    Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TSourceFileView, SourceFileView);
  Application.Run;
  end;
end.

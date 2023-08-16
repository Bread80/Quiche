program Quiche;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  ILData in 'ILData.pas',
  Parse in 'Parse.pas',
  Variables in 'Variables.pas',
  MSourceReader in 'Includes\MSourceReader.pas',
  MErrors in 'Includes\MErrors.pas',
  ILExec in 'ILExec.pas',
  CodeLibrary in 'CodeLibrary.pas',
  Operators in 'Operators.pas',
  Shell in 'Shell.pas',
  Compiler in 'Compiler.pas',
  Testing in 'Testing.pas',
  Functions in 'Functions.pas',
  ParseExpr in 'ParseExpr.pas',
  ParserBase in 'ParserBase.pas',
  Scopes in 'Scopes.pas',
  ParserFixups in 'ParserFixups.pas',
  PrimitivesEx in 'PrimitivesEx.pas',
  CodeGenZ80AsmEx in 'CodeGenZ80AsmEx.pas',
  Eval in 'Eval.pas',
  QTypes in 'QTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

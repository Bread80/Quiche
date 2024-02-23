program Quiche;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  ILData in 'ILData.pas',
  Parse in 'Parse.pas',
  Variables in 'Variables.pas',
  ILExec in 'ILExec.pas',
  Fragments in 'Fragments.pas',
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
  QTypes in 'QTypes.pas',
  Globals in 'Globals.pas',
  ParseErrors in 'ParseErrors.pas',
  SourceReader in 'SourceReader.pas',
  ParseIntrinsics in 'ParseIntrinsics.pas',
  ParseFuncDef in 'ParseFuncDef.pas',
  ParseFuncCall in 'ParseFuncCall.pas',
  MConfig in 'MConfig.pas',
  TextBrowser in '..\TextEditor\TextBrowser.pas',
  EditableText in '..\TextEditor\EditableText.pas',
  Emulator in 'Emulator.pas',
  Intrinsics in 'Intrinsics.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

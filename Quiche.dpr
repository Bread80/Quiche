program Quiche;

uses
  System.StartUpCopy,
  FMX.Forms,
  DUNitX.Loggers.GUIX in '..\vcl\DUnitX\Source\DUNitX.Loggers.GUIX.pas' {GUIXTestRunner},
  IDE.Main in 'IDE.Main.pas' {Form1},
  ILData in 'ILData.pas',
  Parse in 'Parse.pas',
  Variables in 'Variables.pas',
  ILExec in 'ILExec.pas',
  Fragments in 'Fragments.pas',
  Operators in 'Operators.pas',
  IDE.Shell in 'IDE.Shell.pas',
  IDE.Compiler in 'IDE.Compiler.pas',
  IDE.Testing in 'IDE.Testing.pas',
  Functions in 'Functions.pas',
  ParseExpr in 'ParseExpr.pas',
  ParserBase in 'ParserBase.pas',
  Scopes in 'Scopes.pas',
  ParserFixups in 'ParserFixups.pas',
  PrimitivesEx in 'PrimitivesEx.pas',
  CodeGen in 'CodeGen.pas',
  Eval in 'Eval.pas',
  QTypes in 'QTypes.pas',
  Globals in 'Globals.pas',
  ParseErrors in 'ParseErrors.pas',
  SourceReader in 'SourceReader.pas',
  ParseFuncDef in 'ParseFuncDef.pas',
  ParseFuncCall in 'ParseFuncCall.pas',
  IDE.Config in 'IDE.Config.pas',
  TextBrowser in '..\TextEditor\TextBrowser.pas',
  EditableText in '..\TextEditor\EditableText.pas',
  Emulator in 'Emulator.pas',
  Intrinsics in 'Intrinsics.pas',
  Z80.Optimise in 'Z80.Optimise.pas',
  Z80.CPUState in 'Z80.CPUState.pas',
  Z80.CPU in 'Z80.CPU.pas',
  IDE.OptionsForm in 'IDE.OptionsForm.pas' {Options},
  Z80.CodeGen in 'Z80.CodeGen.pas',
  Z80.LoadStoreMove in 'Z80.LoadStoreMove.pas',
  ViewableText in '..\TextEditor\ViewableText.pas',
  SysUtils,
  GUITestRunner,
  Test.Z80.LoadStoreMove in 'Test.Z80.LoadStoreMove.pas',
  Z80.Load in 'Z80.Load.pas',
  Z80.Store in 'Z80.Store.pas',
  Z80.Validation in 'Z80.Validation.pas',
  IDE.SelfTest in 'IDE.SelfTest.pas';

{$R *.res}

begin
  Application.Initialize;
//	if (ParamCount > 0) and (CompareText(ParamStr(1), 'test') = 0) then
		GUITestRunner.RunRegisteredTests;
//  Application.CreateForm(TGUIXTestRunner, GUIXTestRunner);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

program Quiche;

uses
  System.StartUpCopy,
  FMX.Forms,
  DUNitX.Loggers.GUIX in '..\vcl\DUnitX\Source\DUNitX.Loggers.GUIX.pas' {GUIXTestRunner},
  IDE.Main in 'IDE.Main.pas' {Form1},
  Def.IL in 'Def.IL.pas',
  Parse in 'Parse.pas',
  Def.Variables in 'Def.Variables.pas',
  IDE.ILExec in 'IDE.ILExec.pas',
  CG.Fragments in 'CG.Fragments.pas',
  Def.Operators in 'Def.Operators.pas',
  IDE.Shell in 'IDE.Shell.pas',
  IDE.Compiler in 'IDE.Compiler.pas',
  IDE.Testing in 'IDE.Testing.pas',
  Def.Functions in 'Def.Functions.pas',
  Parse.Expr in 'Parse.Expr.pas',
  Parse.Base in 'Parse.Base.pas',
  Def.Scopes in 'Def.Scopes.pas',
  Parse.Fixups in 'Parse.Fixups.pas',
  Def.Primitives in 'Def.Primitives.pas',
  CodeGen in 'CodeGen.pas',
  Parse.Eval in 'Parse.Eval.pas',
  Def.QTypes in 'Def.QTypes.pas',
  Def.Globals in 'Def.Globals.pas',
  Parse.Errors in 'Parse.Errors.pas',
  Parse.Source in 'Parse.Source.pas',
  Parse.FuncDef in 'Parse.FuncDef.pas',
  Parse.FuncCall in 'Parse.FuncCall.pas',
  IDE.Config in 'IDE.Config.pas',
  TextBrowser in '..\TextEditor\TextBrowser.pas',
  EditableText in '..\TextEditor\EditableText.pas',
  IDE.Emulator in 'IDE.Emulator.pas',
  Def.Intrinsics in 'Def.Intrinsics.pas',
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

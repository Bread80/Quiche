program QuicheIDE;

uses
  System.StartUpCopy,
  FMX.Forms,
  DUNitX.Loggers.GUIX in '..\..\vcl\DUnitX\Source\DUNitX.Loggers.GUIX.pas' {GUIXTestRunner},
  IDE.Main in 'IDE.Main.pas' {Form1},
  Def.IL in 'Def.IL.pas',
  Parse in 'Parse.pas',
  Def.Variables in 'Def.Variables.pas',
  IDE.ILExec in 'IDE.ILExec.pas',
  Lib.GenFragments in 'Lib.GenFragments.pas',
  Def.Operators in 'Def.Operators.pas',
  IDE.Shell in 'IDE.Shell.pas',
  IDE.Compiler in 'IDE.Compiler.pas',
  Def.Functions in 'Def.Functions.pas',
  Parse.Expr in 'Parse.Expr.pas',
  Parse.Base in 'Parse.Base.pas',
  Def.Scopes in 'Def.Scopes.pas',
  Parse.Fixups in 'Parse.Fixups.pas',
  Lib.Primitives in 'Lib.Primitives.pas',
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
  CG.VarMap in 'CG.VarMap.pas',
  CG.CPUState.Z80 in 'CG.CPUState.Z80.pas',
  IDE.OptionsForm in 'IDE.OptionsForm.pas' {Options},
  CG.Z80 in 'CG.Z80.pas',
  CG.LoadStoreMove.Z80 in 'CG.LoadStoreMove.Z80.pas',
  ViewableText in '..\TextEditor\ViewableText.pas',
  SysUtils,
  GUITestRunner,
  CG.LoadStoreMove.Z80.Test in 'CG.LoadStoreMove.Z80.Test.pas',
  CG.Load.Z80 in 'CG.Load.Z80.pas',
  CG.Store.Z80 in 'CG.Store.Z80.pas',
  CG.Validation.Z80 in 'CG.Validation.Z80.pas',
  IDE.SelfTest in 'IDE.SelfTest.pas',
  IDE.SelfTestForm in 'IDE.SelfTestForm.pas' {SelfTestF},
  CG.RegAlloc.Z80 in 'CG.RegAlloc.Z80.pas',
  Def.Consts in 'Def.Consts.pas',
  Parse.Directives in 'Parse.Directives.pas',
  CG.Data in 'CG.Data.pas',
  CleverPuppy in 'CleverPuppy.pas',
  Z80.AlgoData in 'Z80.AlgoData.pas',
  Z80.Hardware in 'Z80.Hardware.pas',
  Z80.Assembler in 'Z80.Assembler.pas',
  Z80.GenProcs in 'Z80.GenProcs.pas',
  Z80.Algos in 'Z80.Algos.pas',
  Lib.CPUState in 'Lib.CPUState.pas',
  Puppy.Source in 'Puppy.Source.pas',
  Lib.Data in 'Lib.Data.pas';

{$R *.res}

begin
  Application.Initialize;
//	if (ParamCount > 0) and (CompareText(ParamStr(1), 'test') = 0) then
		GUITestRunner.RunRegisteredTests;
//  Application.CreateForm(TGUIXTestRunner, GUIXTestRunner);
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TSelfTestF, SelfTestF);
  Application.Run;
end.

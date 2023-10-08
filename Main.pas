{
Directory structure:

/Data - Compile time data
  Operations DONE
  Primitives DONE
  Fragments DONE

/Assembler - Libraries written in assembler
  Core library main DONE
  Unpacked core libraries included in QuicheCore.asm

/Quiche - Libraries written in Quiche

/Tests
  /DeepTests (Including auto-generated)

/Config
  UI state
  Compiler defaults
  /Build - Build configs, Debug, Deploy etc.

/Platforms
  /<platform-name> - Eg Amstrad CPC
    /Config - Config for platform
    /Assembler - Libraries written in assembler
      <platform-name>.asm - Main file for platform. Eg AmstradCPC.asm
    /Quiche - Libraries written in Quiche
    /Deploy - Deployment options. E.g. emulator, to hardware via serial, etc
            - Or under config folder??
      Emulator.txt - eg.
      ViaSerial.txt - etc.

/Docs DONE
  Compilable examples/documentation

/Examples
}



unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, FMX.Edit,
  System.Actions, FMX.ActnList, FMX.ListBox, Compiler, FMX.TabControl;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Source: TLabel;
    mmSource: TMemo;
    Panel2: TPanel;
    mmIL: TMemo;
    Panel3: TPanel;
    btnParse: TButton;
    Label2: TLabel;
    edError: TEdit;
    mmAssembly: TMemo;
    Splitter1: TSplitter;
    Panel4: TPanel;
    btnInterpret: TButton;
    btnCodeGen: TButton;
    btnAssemble: TButton;
    btnEmulate: TButton;
    StyleBook1: TStyleBook;
    ActionList1: TActionList;
    Run: TAction;
    btnRun: TButton;
    btnTest: TButton;
    cbTests: TComboBox;
    Label4: TLabel;
    rbStatement: TRadioButton;
    rbBlock: TRadioButton;
    rbGlobal: TRadioButton;
    TabControl1: TTabControl;
    tbILCode: TTabItem;
    tbVariables: TTabItem;
    tbFunctions: TTabItem;
    tbAssembly: TTabItem;
    tbTests: TTabItem;
    pnlVariables: TPanel;
    mmVariables: TMemo;
    Panel5: TPanel;
    mmFunctions: TMemo;
    Panel6: TPanel;
    mmTests: TMemo;
    Panel7: TPanel;
    Panel8: TPanel;
    Label1: TLabel;
    cbScope: TComboBox;
    cbStopOnError: TCheckBox;
    cbOverflowChecks: TCheckBox;
    tbEmulate: TTabItem;
    mmEmulate: TMemo;
    procedure btnParseClick(Sender: TObject);
    procedure btnInterpretClick(Sender: TObject);
    procedure btnCodeGenClick(Sender: TObject);
    procedure btnAssembleClick(Sender: TObject);
    procedure btnEmulateClick(Sender: TObject);
    procedure RunExecute(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbScopeChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FEditorFileName: String;
    procedure LoadTestList;
    function GetCompileScope: TCompileScope;
    procedure SaveState;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses IOUtils, Testing;

{$R *.fmx}

procedure TForm1.RunExecute(Sender: TObject);
begin
  btnParseClick(nil);
  if Compiler.LastErrorNo <> 0 then
    EXIT;
  btnCodeGenClick(nil);
  if Compiler.LastErrorNo <> 0 then
    EXIT;
  btnAssembleClick(nil);
  if Compiler.AssembleError then
    EXIT;
  btnEmulateClick(nil);
  if Compiler.LastErrorNo <> 0 then
    EXIT;
  SetFocused(mmSource);
end;

procedure TForm1.SaveState;
begin
  mmSource.Lines.SaveToFile(FEditorFileName);
end;

procedure TForm1.btnAssembleClick(Sender: TObject);
begin
  if not Compiler.Assemble(Compiler.AssemblerFileName) then
    ShowMessage('Error: ' + Compiler.AssemblerLog);
  TabControl1.ActiveTab := tbAssembly;
end;

procedure TForm1.btnCodeGenClick(Sender: TObject);
begin
//  Compiler.LoadFragmentsLibrary(TPath.Combine(Compiler.QuicheFolder, FragmentsFilename));

  if not Compiler.DoCodeGen then
    edError.Text := LastErrorString;

  Compiler.GetObjectCode(mmAssembly.Lines);

  Compiler.SaveObjectCode(TPath.Combine(Compiler.OutputFolder, 'quicheoutput.asm'));
  TabControl1.ActiveTab := tbAssembly;
end;

procedure TForm1.btnEmulateClick(Sender: TObject);
begin
  Compiler.Emulate(Compiler.BinaryFileName);

  mmIL.Lines.Add('');
  Compiler.GetVarsText(mmEmulate.Lines, False);
  mmEmulate.Lines.Add(#13'Write Buffer:');
  mmEmulate.Lines.Add(Compiler.WriteBuffer);
  TabControl1.ActiveTab := tbEmulate;
end;

procedure TForm1.btnInterpretClick(Sender: TObject);
begin
  Compiler.RunInterpreter;

  Compiler.GetInterpreterOutput(mmAssembly.Lines);

  Compiler.GetVarsText(mmVariables.Lines, False);
  TabControl1.ActiveTab := tbVariables;
end;

procedure TForm1.btnParseClick(Sender: TObject);
var Good: Boolean;
begin
  SaveState;
  Compiler.Initialise(True);

  Compiler.SetOverflowChecks(cbOverflowChecks.IsChecked);

  Compiler.LoadSourceString(mmSource.Text);

  Good := Compiler.Parse(GetCompileScope);
  btnRun.Enabled := Good;
  btnCodeGen.Enabled := Good;

  Compiler.GetScopeList(cbScope.Items);
  if cbScope.Items.Count > 0 then
    cbScope.ItemIndex := 0;

  Compiler.GetILText(mmIL.Lines);
  Compiler.GetVarsText(mmVariables.Lines, True);
  Compiler.GetFunctionsText(mmFunctions.Lines);

  if Good then
  begin
    edError.Text := '';
//    mmIL.Lines.Add('');
//    Compiler.GetVarsText(mmVariables.Lines, True);
  end
  else
  begin
    edError.Text := Compiler.LastErrorString;
    mmIL.Lines.Add(mmSource.Lines[LastErrorLine-1]);
    mmIL.Lines.Add(StringOfChar(' ',LastErrorPos)+'^');
    mmIL.Lines.Add(edError.Text);
  end;

  Focused := mmSource;
  TabControl1.ActiveTab := tbILCode;
end;

procedure TForm1.btnTestClick(Sender: TObject);
begin
  Testing.Initialise;
  if cbTests.ItemIndex = 0 then
    Testing.RunAllTests(Compiler.GetTestsFolder, cbStopOnError.IsChecked)
  else
    Testing.RunTestFile(TPath.Combine(Compiler.GetTestsFolder, cbTests.Items[cbTests.ItemIndex] + '.tst'),
      cbStopOnError.IsChecked);

  mmTests.Lines.Clear;
  Testing.TestLogToStrings(mmTests.Lines);
  TabControl1.ActiveTab := tbTests;
end;

procedure TForm1.cbScopeChange(Sender: TObject);
begin
  if cbScope.ItemIndex >= 0 then
  begin
    if not Compiler.SelectScope(cbScope.Items[cbScope.ItemIndex]) then
      raise Exception.Create('Scope not found :(');

    Compiler.GetILText(mmIL.Lines);

    Compiler.GetObjectCode(mmAssembly.Lines);

    Compiler.GetVarsText(mmVariables.Lines, False);
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveState;
end;

procedure TForm1.FormCreate(Sender: TObject);
var Folder: String;
begin
  Folder := TPath.GetFullPath(TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), '..\..\'));
  Compiler.SetQuicheFolder(Folder);
  Compiler.SetPlatform('TestCase');
  Compiler.OutputFolder := 'C:\RetroTools\Quiche';

  LoadTestList;
  FEditorFileName := TPath.Combine(Folder, 'Config\uifile.qch');
  if TFile.Exists(FEditorFileName) then
    mmSource.Lines.LoadFromFile(FEditorFileName);
end;

function TForm1.GetCompileScope: TCompileScope;
begin
  if rbBlock.isChecked then
    Result := csBlock
  else if rbGlobal.IsChecked then
    Result := csGlobal
  else
    raise Exception.Create('Invalid compile scope selection');
end;

procedure TForm1.LoadTestList;
var FullFolder: String;
  Files: TArray<String>;
  Filename: STring;
begin
  FullFolder := Compiler.GetTestsFolder;

  Files := TDirectory.GetFiles(FullFolder, '*.tst');

  cbTests.Items.Clear;
  cbTests.Items.Add('<All Tests>');
  for FileName in Files do
    cbTests.Items.Add(TPath.GetFilenameWithoutExtension(Filename));

  cbTests.ItemIndex := 0;
end;

end.

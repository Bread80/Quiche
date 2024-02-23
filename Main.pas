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
  System.Actions, FMX.ActnList, FMX.ListBox, Compiler, FMX.TabControl,
  TextBrowser, Globals;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    mmIL: TMemo;
    Panel3: TPanel;
    Label2: TLabel;
    edError: TEdit;
    mmAssembly: TMemo;
    Splitter1: TSplitter;
    Panel4: TPanel;
    StyleBook1: TStyleBook;
    ActionList1: TActionList;
    Run: TAction;
    btnRun: TButton;
    btnTest: TButton;
    cbTests: TComboBox;
    rbStack: TRadioButton;
    rbStatic: TRadioButton;
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
    Panel9: TPanel;
    Label3: TLabel;
    cbPlatforms: TComboBox;
    Label5: TLabel;
    cbDeploy: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    rbDeclarations: TRadioButton;
    rbCode: TRadioButton;
    procedure btnInterpretClick(Sender: TObject);
    procedure btnEmulateClick(Sender: TObject);
    procedure RunExecute(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbScopeChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbPlatformsChange(Sender: TObject);
    procedure cbOverflowChecksChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbDeployChange(Sender: TObject);
  private
    tdSource: TTextBrowser;
    Created: Boolean;
    CompilerConfigFilename: String;
    FEditorFileName: String;
    procedure LoadDeployList;
    procedure LoadTestList;
    function GetBlockType: TBlockType;
    function GetParseType: TParseType;
    procedure SaveState;
    procedure DelayedSetFocus(Control: TControl);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses IOUtils, Testing;

{$R *.fmx}

const DeployExt = '.deploy';

procedure TForm1.RunExecute(Sender: TObject);
var Good: Boolean;
begin
  SaveState;
  Good := Compiler.CompileString(tdSource.Text.AsString, GetBlockType, GetParseType,
    True, False);

  Compiler.GetScopeList(cbScope.Items);
  if cbScope.Items.Count > 0 then
    cbScope.ItemIndex := 0;

  Compiler.GetILText(mmIL.Lines);
  Compiler.GetVarsText(mmVariables.Lines, True);
  Compiler.GetFunctionsText(mmFunctions.Lines);

  if not Good then
  begin
    if Compiler.LastErrorNo <> 0 then
    begin //Compile error
      tdSource.Text.acSetCursor(Compiler.LastErrorPos, Compiler.LastErrorLine-1);
      edError.Text := Compiler.LastErrorString;
      mmIL.Lines.Add(tdSource.Text.Lines[LastErrorLine-1]);
      mmIL.Lines.Add(StringOfChar(' ',LastErrorPos)+'^');
      mmIL.Lines.Add(edError.Text);
      mmIL.Lines.Add(ErrorHelp);
      TabControl1.ActiveTab := tbILCode;
    end
    else if Compiler.AssembleError then
    begin
      ShowMessage('Assembly error: ' + Compiler.AssemblerLog);
      TabControl1.ActiveTab := tbAssembly;
    end;
  end
  else
  begin
    edError.Text := '';

    btnEmulateClick(nil);
    TabControl1.ActiveTab := tbEmulate;
//    if Compiler.LastErrorNo <> 0 then
//      EXIT;
//    mmIL.Lines.Add('');
//    Compiler.GetVarsText(mmVariables.Lines, True);
  end;

  DelayedSetFocus(tdSource);
end;

procedure TForm1.SaveState;
begin
  tdSource.Text.SaveToFile;
end;

procedure TForm1.btnEmulateClick(Sender: TObject);
begin
  if Compiler.Emulate(Compiler.BinaryFileName) then
  begin
    mmIL.Lines.Add('');
    Compiler.GetVarsText(mmEmulate.Lines, False);
    mmEmulate.Lines.Add(#13'Write Buffer:');
    mmEmulate.Lines.Add(Compiler.WriteBuffer);
  end
  else
    mmEmulate.Lines.Add('Emulation failed or not available');

  TabControl1.ActiveTab := tbEmulate;
end;

procedure TForm1.btnInterpretClick(Sender: TObject);
begin
  Compiler.RunInterpreter;

  Compiler.GetInterpreterOutput(mmAssembly.Lines);

  Compiler.GetVarsText(mmVariables.Lines, False);
  TabControl1.ActiveTab := tbVariables;
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

procedure TForm1.cbDeployChange(Sender: TObject);
begin
  if cbDeploy.ItemIndex = -1 then
    Compiler.Deploy.Clear
  else
    Compiler.Deploy.LoadFromFile(TPath.Combine(
      TPath.Combine(Compiler.GetPlatformFolder, DeployFolderName),
      cbDeploy.Items[cbDeploy.ItemIndex] + DeployExt));
end;

procedure TForm1.cbOverflowChecksChange(Sender: TObject);
begin
  Compiler.Config.OverflowChecks := cbOverflowChecks.IsChecked;
  if Created then
    Compiler.Config.SaveToFile(CompilerConfigFileName);
end;

procedure TForm1.cbPlatformsChange(Sender: TObject);
begin
  if cbPlatforms.ItemIndex >= 0 then
  begin
    Compiler.Config.PlatformName := cbPlatforms.Items[cbPlatforms.ItemIndex];
    if Created then
      Compiler.Config.SaveToFile(CompilerConfigFileName);
    LoadDeployList;
    //Set default deployment? From config file?
  end;
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

procedure TForm1.DelayedSetFocus(Control: TControl);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Synchronize( nil,
         procedure
         begin
           Control.SetFocus;
         end
      );
    end
  ).Start;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveState;
end;

procedure TForm1.FormCreate(Sender: TObject);
var Folder: String;
  TestCase: Integer;
  PlatformIndex: Integer;
begin
  Folder := TPath.GetFullPath(TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), '..\..\'));
  Compiler.SetQuicheFolder(Folder);
  Compiler.OutputFolder := 'C:\RetroTools\Quiche';
  CompilerConfigFilename := TPath.Combine(Compiler.GetQuicheFolder, 'Config\Compiler.cfg');

  Compiler.GetPlatformList(cbPlatforms.Items);
  TestCase := cbPlatforms.Items.IndexOf('TestCase');
  if TestCase >= 0 then
    cbPlatforms.ItemIndex := TestCase
  else
    cbPlatforms.ItemIndex := 0;

  Compiler.Config.LoadFromFile(CompilerConfigFilename);
  PlatformIndex := cbPlatforms.Items.IndexOf(Config.PlatformName);
  if PlatformIndex < 0 then
    ShowMessage('Unknown platform: ' + Config.PlatformName)
  else
    cbPlatforms.ItemIndex := PlatformIndex;
  cbOverflowChecks.IsChecked := Config.OverflowChecks;

  LoadTestList;

  tdSource := TTextBrowser.Create(Self);
  tdSource.Parent := Panel1;
  tdSource.Theme := DarkTheme;
  tdSource.Align := TAlignLayout.Client;
  FEditorFileName := TPath.Combine(Folder, 'Config\uifile.qch');
  if TFile.Exists(FEditorFileName) then
    tdSource.Text.LoadFromFile(FEditorFilename);

  DelayedSetFocus(tdSource);
  Created := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  inherited;
  tdSource.Free;
end;

function TForm1.GetBlockType: TBlockType;
begin
  if rbStack.isChecked then
    Result := btStack
  else if rbStatic.IsChecked then
    Result := btStatic
  else
    raise Exception.Create('Invalid Block Type selection');
end;

function TForm1.GetParseType: TParseType;
begin
  if rbDeclarations.IsChecked then
    Result := ptDeclarations
  else if rbCode.IsChecked then
    Result := ptCode
  else
    raise Exception.Create('Invalid Parse Type selection');
end;

procedure TForm1.LoadDeployList;
var DeployPath: String;
  Files: TArray<String>;
  Filename: String;
begin
  cbDeploy.Items.Clear;
  if Config.PlatformName = '' then
    EXIT;

  DeployPath := TPath.Combine(Compiler.GetPlatformFolder, DeployFolderName);
  if TDirectory.Exists(DeployPath) then
  begin
    Files := TDirectory.GetFiles(DeployPath, '*' + DeployExt);
    for Filename in Files do
      cbDeploy.Items.Add(TPath.GetFilenameWithoutExtension(Filename));

    if cbDeploy.Items.Count > 0 then
      cbDeploy.ItemIndex := 0;
  end;
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

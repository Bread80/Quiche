(*

[Corrupts F]function KM_READ_CHAR(out Character: A as Char): CF; call $bb09;[PreservesAll]procedure TXT_OUTPUT(Character: A as Char); call $bb5a;begin  var Ch: Char  while true  begin    if KM_READ_CHAR(Ch)      TXT_OUTPUT(Ch)    else      write('.')  endend.
*)

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



unit IDE.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, FMX.Edit,
  System.Actions, FMX.ActnList, FMX.ListBox, IDE.Compiler, FMX.TabControl,
  TextBrowser, FMX.Menus,
  Def.Globals;

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
    btnRun: TButton;
    rbStack: TRadioButton;
    rbStatic: TRadioButton;
    TabControl1: TTabControl;
    tbILCode: TTabItem;
    tbVariables: TTabItem;
    tbFunctions: TTabItem;
    tbAssembly: TTabItem;
    pnlVariables: TPanel;
    mmVariables: TMemo;
    Panel5: TPanel;
    mmFunctions: TMemo;
    Panel7: TPanel;
    Panel8: TPanel;
    Label1: TLabel;
    cbScope: TComboBox;
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
    MenuBar1: TMenuBar;
    MenuItem2: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem3: TMenuItem;
    cbRangeChecks: TCheckBox;
    mnuOpenProject: TMenuItem;
    mnuOpenScratchFile: TMenuItem;
    mnuSave: TMenuItem;
    FileOpenDialog: TOpenDialog;
    mnuNew: TMenuItem;
    mnuSaveAs: TMenuItem;
    FileSaveDialog: TSaveDialog;
    MenuItem9: TMenuItem;
    ActionList1: TActionList;
    acOpenProject: TAction;
    acOpenScratchFile: TAction;
    acSave: TAction;
    acNew: TAction;
    acSaveAs: TAction;
    acRun: TAction;
    MenuItem1: TMenuItem;
    MenuItem5: TMenuItem;
    procedure btnInterpretClick(Sender: TObject);
    procedure btnEmulateClick(Sender: TObject);
    procedure acRunExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbScopeChange(Sender: TObject);
    procedure cbPlatformsChange(Sender: TObject);
    procedure cbOverflowChecksChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbDeployChange(Sender: TObject);
    procedure acOptionsExecute(Sender: TObject);
    procedure cbRangeChecksChange(Sender: TObject);
    procedure acOpenScratchExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acOpenProjectExecute(Sender: TObject);
    procedure acNewExecute(Sender: TObject);
    procedure acSaveAsExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure acSelfTestFormExecute(Sender: TObject);
  private
    tdSource: TTextBrowser;
    //True if this window is still being created. Prevents options being autosaved etc.
    Created: Boolean;
    procedure LoadDeployList;
    function GetBlockType: TBlockType;
    function GetParseType: TParseType;
    procedure UpdateCaption;
    procedure OpenProject(const Filename: String);
    procedure SaveConfigFile;
    function SaveProjectAs: Boolean;
    //Returns False if the project couldn't be saved for any reason
    function SaveProject: Boolean;
    procedure DelayedSetFocus(Control: TControl);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
  FMX.DialogService, IOUtils,
  IDE.OptionsForm, IDE.SelfTestForm;

{$R *.fmx}

const
  scCaption = 'Quiche Z80 Cross Compiler by @Bread80';

  scScratchFile = 'Config\uifile.quiche';

procedure TForm1.acNewExecute(Sender: TObject);
begin
  OpenProject('');
end;

procedure TForm1.acOpenProjectExecute(Sender: TObject);
begin
  if FileOpenDialog.Execute then
  begin
    OpenProject(FileOpenDialog.FileName);
  end;
end;

procedure TForm1.acOpenScratchExecute(Sender: TObject);
begin
  OpenProject(TPath.Combine(GetQuicheFolder, scScratchFile));
end;

procedure TForm1.acOptionsExecute(Sender: TObject);
var Opt: TOptions;
begin
  Opt := TOptions.Create(Self);
  Opt.ShowModal;
  Opt.Free;

  SaveConfigFile;
end;

procedure TForm1.acRunExecute(Sender: TObject);
var Good: Boolean;
begin
  if not SaveProject then
    EXIT;
  Good := IDE.Compiler.CompileString(tdSource.Text.AsString, GetBlockType, GetParseType,
    True, False);

  IDE.Compiler.GetScopeList(cbScope.Items);
  if cbScope.Items.Count > 0 then
    cbScope.ItemIndex := 0;

  IDE.Compiler.GetILText(mmIL.Lines);
  IDE.Compiler.GetVarsText(mmVariables.Lines, True);
  IDE.Compiler.GetFunctionsText(mmFunctions.Lines);

  if not Good then
  begin
    if IDE.Compiler.ParseErrorNo <> 0 then
    begin //Compile error
      tdSource.Text.acSetCursor(IDE.Compiler.ParseErrorPos, IDE.Compiler.ParseErrorLine-1);
      edError.Text := IDE.Compiler.ParseErrorString;
      mmIL.Lines.Add(tdSource.Text.Lines[ParseErrorLine-1]);
      mmIL.Lines.Add(StringOfChar(' ',ParseErrorPos)+'^');
      mmIL.Lines.Add(edError.Text);
      mmIL.Lines.Add(ParseErrorHelp);
      TabControl1.ActiveTab := tbILCode;
    end
    else if IDE.Compiler.AssembleError then
    begin
      ShowMessage('Assembly error: ' + IDE.Compiler.AssemblerLog);
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

procedure TForm1.acSaveAsExecute(Sender: TObject);
begin
  SaveProjectAs
end;

procedure TForm1.acSaveExecute(Sender: TObject);
begin
  SaveProject;
end;

procedure TForm1.acSelfTestFormExecute(Sender: TObject);
var ST: TSelfTestF;
begin
  ST := TSelfTestF.Create(Application);
  ST.StyleBook := StyleBook1;
  ST.ShowModal;
  ST.Free;
end;

procedure TForm1.SaveConfigFile;
begin
  IDE.Compiler.GetConfig.SaveToFile(GetConfigFileName);
end;

function TForm1.SaveProject: Boolean;
begin
  Result := False;
  try
    if IDE.Compiler.GetConfig.IDESettings.ProjectFile = '' then
    begin
      if not SaveProjectAs then
        EXIT(False)
    end
    else
      tdSource.Text.SaveToFile;
    SaveConfigFile;
    Result := True;
  except
    on e:Exception do
      ShowMessage('Error saving project: ' + e.Message);
  end;
end;

function TForm1.SaveProjectAs: Boolean;
begin
  Result := FileSaveDialog.Execute;
  if Result then
  begin
    IDE.Compiler.GetConfig.IDESettings.ProjectFile := FileSaveDialog.Filename;
    tdSource.Text.Filename := FileSaveDialog.Filename;
    Result := SaveProject;
    UpdateCaption;
  end;
end;

procedure TForm1.UpdateCaption;
begin
  if IDE.Compiler.GetConfig.IDESettings.ProjectFile <> '' then
    Caption := TPath.GetFilename(IDE.Compiler.GetConfig.IDESettings.ProjectFile) + ' - ' + scCaption
  else
    Caption := '<Untitled> - ' + scCaption;
end;

procedure TForm1.btnEmulateClick(Sender: TObject);
begin
  if IDE.Compiler.Emulate(IDE.Compiler.GetBinaryFileName) then
  begin
    mmIL.Lines.Add('');
    IDE.Compiler.GetVarsText(mmEmulate.Lines, False);
    mmEmulate.Lines.Add(#13'Write Buffer:');
    mmEmulate.Lines.Add(IDE.Compiler.WriteBuffer);
  end
  else
    mmEmulate.Lines.Add('Emulation failed or not available');

  TabControl1.ActiveTab := tbEmulate;
end;

procedure TForm1.btnInterpretClick(Sender: TObject);
begin
  IDE.Compiler.RunInterpreter;

  IDE.Compiler.GetInterpreterOutput(mmAssembly.Lines);

  IDE.Compiler.GetVarsText(mmVariables.Lines, False);
  TabControl1.ActiveTab := tbVariables;
end;

procedure TForm1.cbDeployChange(Sender: TObject);
begin
  if cbDeploy.ItemIndex = -1 then
    IDE.Compiler.ClearDeploy
  else
    IDE.Compiler.SetDeploy(cbDeploy.Items[cbDeploy.ItemIndex]);
end;

procedure TForm1.cbOverflowChecksChange(Sender: TObject);
begin
  IDE.Compiler.GetConfig.OverflowChecks := cbOverflowChecks.IsChecked;
  if Created then
    SaveConfigFile;
end;

procedure TForm1.cbPlatformsChange(Sender: TObject);
begin
  if cbPlatforms.ItemIndex >= 0 then
  begin
    IDE.Compiler.GetConfig.PlatformName := cbPlatforms.Items[cbPlatforms.ItemIndex];
    if Created then
      SaveConfigFile;
    LoadDeployList;
    //Set default deployment? From config file?
  end;
end;

procedure TForm1.cbRangeChecksChange(Sender: TObject);
begin
  IDE.Compiler.GetConfig.RangeChecks := cbRangeChecks.IsChecked;
  if Created then
    SaveConfigFile;
end;

procedure TForm1.cbScopeChange(Sender: TObject);
begin
  if cbScope.ItemIndex >= 0 then
  begin
    if not IDE.Compiler.SelectScope(cbScope.Items[cbScope.ItemIndex]) then
      raise Exception.Create('Scope not found :(');

    IDE.Compiler.GetILText(mmIL.Lines);

    IDE.Compiler.GetObjectCode(mmAssembly.Lines);

    IDE.Compiler.GetVarsText(mmVariables.Lines, False);
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

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var LCanClose: Boolean;
begin
  if not SaveProject then
  begin
    with TDialogService do
    begin
      PreferredMode := TPreferredMode.Platform;
      MessageDialog('Exit without saving?', TMsgDlgType.mtConfirmation,
      [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbNo, 0,
        procedure(const AResult: TModalResult)
        begin
          LCanClose := AResult = mrYes
        end);
    end;

    CanClose := LCanClose;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var PlatformIndex: Integer;
begin
//                             FileName = 'Amakrits.Style'
  StyleBook1.Filename := 'Amakrits.Style';

  IDE.Compiler.DefaultInitFolders;

  IDE.Compiler.GetPlatformList(cbPlatforms.Items);

  PlatformIndex := cbPlatforms.Items.IndexOf(GetConfig.PlatformName);
  if PlatformIndex < 0 then
    ShowMessage('Unknown platform: ' + GetConfig.PlatformName)
  else
    cbPlatforms.ItemIndex := PlatformIndex;
  cbOverflowChecks.IsChecked := GetConfig.OverflowChecks;
  cbRangeChecks.IsChecked := GetConfig.RangeChecks;

  tdSource := TTextBrowser.Create(Self);
  tdSource.Parent := Panel1;
  tdSource.Theme := DarkTheme;
  tdSource.Align := TAlignLayout.Client;
  if FileExists(IDE.Compiler.GetConfig.IDESettings.ProjectFile) then
    OpenProject(IDE.Compiler.GetConfig.IDESettings.ProjectFile);
  UpdateCaption;

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
  Files: TStringList;
  Filename: String;
begin
  cbDeploy.Items.Clear;
  if GetConfig.PlatformName = '' then
    EXIT;

  DeployPath := TPath.Combine(IDE.Compiler.GetPlatformFolder, GetDeployFolder);
  if TDirectory.Exists(DeployPath) then
  begin
    Files := TStringList.Create;
    try
    GetDeployList(Files);
    for Filename in Files do
      cbDeploy.Items.Add(TPath.GetFilenameWithoutExtension(Filename));
    finally
      Files.Free;
    end;

    if cbDeploy.Items.Count > 0 then
      cbDeploy.ItemIndex := 0;
  end;
end;

procedure TForm1.OpenProject(const Filename: String);
begin
  if Created then
  begin
    SaveProject;
    SaveConfigFile;
  end;

  if Filename = '' then
  begin
    tdSource.Text.acSelectAll;
    tdSource.Text.acDeleteSelected;
    tdSource.Text.Filename := Filename;
  end
  else if TFile.Exists(FileName) then
    tdSource.Text.LoadFromFile(Filename)
  else
  begin
    ShowMessage('Failed to open project: ' + Filename);
    EXIT;
  end;

  if Created then
  begin
    IDE.Compiler.GetConfig.IDESettings.ProjectFile := Filename;
    SaveConfigFile;
    SaveProject;
  end;

  UpdateCaption;
end;

end.

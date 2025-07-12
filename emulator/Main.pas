unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  MZ80Tester, mSymbols, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.StdCtrls, FMX.TabControl, FMX.Edit,
  vRegView, vMemView, vTerminal, vBreakpoints, vSourceFileView,
  mConfigFile, System.Actions,
  FMX.ActnList;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Step: TButton;
    MemoIO: TMemo;
    Run: TButton;
    Pause: TButton;
    Reset: TButton;
    TabRight: TTabControl;
    TabIO: TTabItem;
    TabMemRW: TTabItem;
    MemoMemory: TMemo;
    Splitter1: TSplitter;
    btnTest: TButton;
    TabMemory: TTabItem;
    Initialise: TButton;
    Panel3: TPanel;
    pnlCPUView: TPanel;
    cbQuiet: TCheckBox;
    tabMain: TTabControl;
    Splitter2: TSplitter;
    tabTerminal: TTabItem;
    pnlBreakpoints: TPanel;
    Panel2: TPanel;
    MilliTest: TButton;
    Label1: TLabel;
    edConfigFile: TEdit;
    btnLoadConfig: TButton;
    ActionList1: TActionList;
    acRun: TAction;
    acStepInto: TAction;
    pnlSourceFileView: TPanel;
    acStepOver: TAction;
    procedure FormCreate(Sender: TObject);
    procedure StepClick(Sender: TObject);
    procedure RunClick(Sender: TObject);
    procedure PauseClick(Sender: TObject);
    procedure ResetClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure InitialiseClick(Sender: TObject);
    procedure cbQuietChange(Sender: TObject);
    procedure MilliTestClick(Sender: TObject);
    procedure acStepOverExecute(Sender: TObject);
  private
    FConfig: TConfigFile;
    Terminal: TTerminalForm;
    FSourceFileView: TSourceFileView;
    FRegView: TRegView;
    FMemView: TMemView;
    FBreakpoints: TBreakpoints;

    Z80TestManager: TZ80TestManager;

    Log: TStringList;
  protected
    procedure EmbedForm(AParent: TControl; AForm: TCustomForm);

    procedure SetupTestZexAll;
  public
  end;

var
  Form1: TForm1;

implementation
uses MZ80State, mHardware, CommandLine;

{$R *.fmx}

procedure TForm1.StepClick(Sender: TObject);
begin
  Hardware.ExecOpcode;
  Hardware.Pause;
end;

procedure TForm1.acStepOverExecute(Sender: TObject);
begin
  //TODO
  StepClick(Sender);
end;

procedure TForm1.btnTestClick(Sender: TObject);
begin
  Z80TestManager := TZ80TestManager.Create;
  try
    Z80TestManager.TestAll;
    Z80TestManager.Log.SaveToFile('Z80TestLog.txt');
  finally
    Z80TestManager.Free;
  end;
end;

procedure TForm1.cbQuietChange(Sender: TObject);
begin
  if cbQuiet.IsChecked then
    Hardware.UpdateView := uvQuiet
  else
    Hardware.UpdateView := uvVerbose;
end;

procedure TForm1.EmbedForm(AParent: TControl; AForm: TCustomForm);
begin
  while AForm.ChildrenCount>0 do
    AForm.Children[0].Parent:=AParent;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  edConfigFile.Text := 'C:\Dropbox\Delphi\Quiche\redist\Platforms\quiche\Deploy\quiche.emulator';

  if not Assigned(FSourceFileView) then
  begin
    FSourceFileView := TSourceFileView.Create(Self);
    EmbedForm(pnlSourceFileView, FSourceFileView);
  end;
  if not Assigned(Terminal) then
  begin
    Terminal := TTerminalForm.Create(Self);
    EmbedForm(tabTerminal, Terminal);
    Hardware.Term1OutProc := Terminal.TermOut;
    Hardware.Term1InProc := Terminal.TermIn;
  end;

  if not Assigned(FRegView) then
  begin
    FRegView := TRegView.Create(Self);
    EmbedForm(pnlCPUView, FRegView);
  end;
  if not Assigned(FBreakpoints) then
  begin
    FBreakpoints := TBreakpoints.Create(Self);
    EmbedForm(pnlBreakpoints, FBreakpoints);
  end;

  if not Assigned(FMemView) then
  begin
    FMemView := TMemView.Create(Self);
    EmbedForm(tabMemory, FMemView);
    FRegView.MemView := FMemView;
  end;


  InitialiseClick(nil);


(*
  Memory.HookWrite := procedure(Addr: Word; DataOld, DataNew: Byte)
    var
      LSymbol: String;
    begin
      LSymbol := Symbols.ValueToSymbol(Addr);
      if Symbols.IsInRange(Addr,'MSBT',0,13) then
        EXIT;
      if Symbols.IsInRange(Addr,'SPBT',0,1) then
        EXIT;
      if LSymbol = 'CNTBIT+0000' then
        EXIT;
      if LSymbol = 'SHFBIT+0000' then
        EXIT;
      if Symbols.IsInRange(Addr,'CNTBYT',0,1) then
        EXIT;
      if Symbols.IsInRange(Addr,'SHFBYT',0,1) then
        EXIT;
      if LSymbol = 'FLGMSK+0001' then
        EXIT;
      if Symbols.IsInRange(Addr,'COUNTER',0,39) then
        EXIT;
      if Symbols.IsInRange(Addr,'SHIFTER',0,39) then
        EXIT;
      //****Instruction Under Test****
      if Symbols.IsInRange(Addr,'IUT',0,3) then
        EXIT;
      if Symbols.IsInRange(Addr,'CRCVAL',0,3) then
        EXIT;
      if Symbols.IsInRange(Addr,'SPSAV',0,1) then
        EXIT;
      if Symbols.IsInRange(Addr,'SPAT',0,1) then
        EXIT;
      if Symbols.IsInRange(Addr,'MSAT',0,13) then
        EXIT;
{      if Symbols.IsInRange(Addr,'IUT',0,3) then
        EXIT;if Symbols.IsInRange(Addr,'IUT',0,3) then
        EXIT;if Symbols.IsInRange(Addr,'IUT',0,3) then
        EXIT;if Symbols.IsInRange(Addr,'IUT',0,3) then
        EXIT;if Symbols.IsInRange(Addr,'IUT',0,3) then
        EXIT;if Symbols.IsInRange(Addr,'IUT',0,3) then
        EXIT;
}
//      if Addr = $40 then
//        MemoMemory.Lines.Add(IntToHex(Addr,4) + ': ' + IntToHex(DataNew,2) + ' ' + IntToHex(Z80Exec.Z80.F));
      if Addr < $ff00 then
        MemoMemory.Lines.Add('W ' + IntToHex(Addr, 4) + ': ' + IntToHex(DataOld,2) + '->' + IntToHex(DataNew,2) + ' ' +
          LSymbol +
          ' PC:' + IntToHex(Z80Exec.Z80.PC) + ' ' + Symbols.ValueToSymbol(Z80Exec.Z80.PC));

      MemoMemory.GoToTextEnd;
    end;
*)
{  Memory.HookRead := function(Addr: Word; DataOld: Byte): Byte
    begin
      Result := DataOld;
//      if Addr < $7000 then
        MemoMemory.Lines.Add('R ' + IntToHex(Addr, 4) + ': ' + IntToHex(DataOld,2) + ' ' +
          Symbols.ValueToSymbol(Addr) +
          ' PC:' + IntToHex(Z80Exec.Z80.PC) + ' ' + Symbols.ValueToSymbol(Z80Exec.Z80.PC));

      MemoMemory.GoToTextEnd;
    end;
}end;

procedure TForm1.InitialiseClick(Sender: TObject);
begin
  if Assigned(Log) then
    Log.Free;

  Log := TStringList.Create;

  if ParamCount > 0 then
  begin
    Hardware.Reset;
    DoCommandLine(Hardware);
    Hardware.UpdateViewers;
  end
  else
  begin
    FConfig := TConfigFile.Create(Hardware);
    FConfig.LoadConfigFile(edConfigFile.Text);
    Hardware.UpdateViewers;
  end;


{  Z80TestManager := TZ80TestManager.Create(Z80Exec);
  Z80TestManager.Log :=
    procedure(S: String)
    begin
//      Memo1.Lines.Add(S);
//      Memo1.GotoTextEnd;
    end;
}
//  Memory.LoadBin('C:\DropBox\Assembler\Bread80\LCD2\LCD.bin', 0);

//  SetupTestZexAll;

(*  IO.HookOut := procedure(Port: Word;Data: Byte)
    var Title: String;
    begin
      case Port and $ff of
      $78: Title := 'Memory Bank 0';
      $79: Title := 'Memory Bank 1';
      $7a: Title := 'Memory Bank 2';
      $7b: Title := 'Memory Bank 0';

      $81: Title := 'Serial out: ' + Chr(Data);
      else
        Title := '<Unknown>';
      end;
      MemoIO.Lines.Add('OUT ' + IntToHex(Port, 4) + ' ' + Data.ToString + ' ' + Title);
{      if MemoIO.Lines.Count = 0 then
        MemoIO.Lines.Add('');
      if Data = 13 then
        MemoIO.Lines.Add('')
      else
        MemoIO.Lines[MemoIO.Lines.Count-1] := MemoIO.Lines[MemoIO.Lines.Count-1] + Chr(Data);

}      MemoIO.GoToTextEnd;
    end;
*)
(*  IO.HookInp := function(Port: Word): Byte
    var Title: String;
    begin
      case Port and $ff of
      $80:
      begin
        Title := 'Serial status';
        Result := 4
      end
      else
        Title := '<Unknown>';
        Result := 0;
      end;
      MemoIO.Lines.Add('IN ' + IntToHex(Port, 4) + ' ' + Result.ToString + ' ' + Title);
{      if MemoIO.Lines.Count = 0 then
        MemoIO.Lines.Add('');
      if Data = 13 then
        MemoIO.Lines.Add('')
      else
        MemoIO.Lines[MemoIO.Lines.Count-1] := MemoIO.Lines[MemoIO.Lines.Count-1] + Chr(Data);
}
      MemoIO.GoToTextEnd;
    end;
*)end;

procedure TForm1.MilliTestClick(Sender: TObject);
var I: Integer;
  Start: TDateTime;
  Done: TDateTime;
  //Convert fractions of a day to seconds.
  //60 seconds * 60 minutes * 24 hours
const Multiplier = 60 * 60 * 24;
begin
  Hardware.ExecStatus := esRunning;
  Start := Now;
  for I := 1 to 1000000 do
    Hardware.ExecOpcode;

  Done := Now;
  ShowMessage('Time taken: ' + ((Done-Start)* Multiplier).ToString);
end;

procedure TForm1.PauseClick(Sender: TObject);
begin
  Hardware.Pause;
end;

procedure TForm1.ResetClick(Sender: TObject);
begin
  Hardware.Reset;
  Terminal.Reset;
end;
procedure TForm1.RunClick(Sender: TObject);
var I: Integer;
begin
  Hardware.Run;

  while Hardware.ExecStatus = esRunning do
  begin
    for I := 0 to 10000 do
      Hardware.ExecOpcode;

    Application.ProcessMessages;
  end;
  Hardware.Pause;
end;

procedure TForm1.SetupTestZexAll;
begin
  Hardware.LoadBinary('C:\DropBox\InfoLaptop\Bread80\Assembler\Tests\ZexAllCPM\rasmoutput.bin', 0);
  Hardware.LoadSymbols('C:\DropBox\InfoLaptop\Bread80\Assembler\Tests\ZexAllCPM\rasmoutput.sym');

  Hardware.Z80.Z80.FromString(
    'A F  B C  D E  H L  IX   IY   PC   SP   IR    A''F'' B''C'' D''E'' H''L'''#13#10+
//    '0102 0304 0506 0708 090a 0b0c 0000 0d0e 0f10 1112 1314 1516 1718'#13#10+
      'FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF'#13#10+
    'SZYHXPNC');
end;

end.

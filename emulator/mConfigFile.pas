unit mConfigFile;

interface
uses mHardware;

type TConfigFile = class
  private
    Hardware: THardware;
  protected
    function DoCreateMemoryDevice(Data: String): Boolean;
    function DoCreateIODevice(Data: STring): Boolean;

    function DoHookTerminal1Out: Boolean;
    function DoHookTerminal1In: Boolean;

    function DoDie(Data: String): Boolean;
    function DoDumpCPUState(Data: String): Boolean;
    function DoDumpMemory(Data: String): Boolean;
    function DoLoadBinary(Data: String): Boolean;
    function DoLoadMapFile(const Data: String): Boolean;
    function DoLoadSymbols(Data: String): Boolean;
    function DoSetSourceFilename(Data: String): Boolean;
    function DoRunToHALT: Boolean;
    function DoSetPC(Data: String): Boolean;
    function DoCommand(Command, Data: String): Boolean;
  public
    constructor Create(AHardware: THardware);
    procedure LoadConfigFile(Filename: String);
  end;

implementation
uses SysUtils, {$ifdef fpc}LazFileUtils, {$else}IOUtils,{$endif} Classes;

{ TConfigFile }

constructor TConfigFile.Create(AHardware: THardware);
begin
  Hardware := AHardware;
end;

function TConfigFile.DoCommand(Command, Data: String): Boolean;
begin
  Command := Command.ToLower;
  //Creation
  if Command = 'creatememorydevice' then
    Result := DoCreateMemoryDevice(Data)
  else if Command = 'createiodevice' then
    Result := DoCreateIODevice(Data)

  //Files
  else if Command = 'loadbinary' then
    Result := DoLoadBinary(Data)
  else if Command = 'loadmapfile' then
    Result := DoLoadMapFile(Data)
  else if Command = 'loadsymbols' then
    Result := DoLoadSymbols(Data)
  else if Command = 'setsourcefilename' then
    Result := DoSetSourceFilename(Data)

  //Wiring
  else if Command = 'hookterminal1out' then
    Result := DoHookTerminal1Out
  else if Command = 'hookterminal1in' then
    Result := DoHookTerminal1In

  //Data
  else if Command = 'setpc' then
    Result := DoSetPC(Data)

  //Execution
  else if Command = 'die' then
    Result := DoDie(Data)
  else if Command = 'runtohalt' then
    Result := DoRunToHALT

  //After execution
  else if Command = 'dumpcpustate' then
    Result := DoDumpCPUState(Data)
  else if Command = 'dumpmemory' then
    Result := DoDumpMemory(Data)

  else
    Result := False;
end;

function TConfigFile.DoCreateIODevice(Data: STring): Boolean;
begin
  Hardware.CreateIODevice(Data);
  Result := True;
end;

function TConfigFile.DoCreateMemoryDevice(Data: String): Boolean;
//begin
//var Split: TArray<String>;
begin
//  Split := Data.Split([',']);
//  if Length(Split) <> 2 then
//    EXIT(False);
//  if not TryStrToInt(Split[1], Addr) then
//    EXIT(False);
  Hardware.CreateMemoryDevice(Data);
  Result := True;
end;

function TConfigFile.DoDie(Data: String): Boolean;
begin
  HALT;
  Result := True;
end;

function TConfigFile.DoDumpCPUState(Data: String): Boolean;
var SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := Hardware.Z80.Z80.ToString;
    SL.SaveToFile(Data);
  finally
    SL.Free;
  end;
//  TFile.WriteAllText(Data, Hardware.Z80.Z80.ToString);
  Result := True;
end;

function TConfigFile.DoDumpMemory(Data: String): Boolean;
var Split: TArray<String>;
begin
  Split := Data.Split([',']);
  if Length(Split) <> 2 then
    EXIT(False);
  Hardware.DumpMemory(Split[0], Split[1]);
  Result := True;
end;

function TConfigFile.DoHookTerminal1In: Boolean;
begin
  //Current Hardware does this automatically
//  Hardware.HookTerminal1In(Hardware.Terminal.Term);
  Result := True;
end;

function TConfigFile.DoHookTerminal1Out: Boolean;
begin
  //Current Hardware does this automatically
//  Hardware.HookTerminal1Out(Hardware.Terminal.Term);
  Result := True;
end;

function TConfigFile.DoSetPC(Data: String): Boolean;
var Addr: Integer;
begin
  if not TryStrToInt(Data, Addr) then
    EXIT(False);
  Hardware.SetPC(Addr);
  Result := True;
end;

function TConfigFile.DoSetSourceFilename(Data: String): Boolean;
var Split: TArray<String>;
begin
  Split := Data.Split([',']);
  if Length(Split) <> 1 then
    EXIT(False);
  Hardware.SetSourceFilename(Split[0]);
  Result := True;
end;

function TConfigFile.DoLoadBinary(Data: String): Boolean;
var Split: TArray<String>;
  Addr: Integer;
begin
  Split := Data.Split([',']);
  if Length(Split) <> 2 then
    EXIT(False);
  if not TryStrToInt(Split[1], Addr) then
    EXIT(False);
  Hardware.LoadBinary(Split[0], Addr);
  Result := True;
end;

function TConfigFile.DoLoadMapFile(const Data: String): Boolean;
begin
  Hardware.LoadMapFile(Data);
  Result := True;
end;

function TConfigFile.DoLoadSymbols(Data: String): Boolean;
var Split: TArray<String>;
begin
  Split := Data.Split([',']);
  if Length(Split) <> 1 then
    EXIT(False);
  Hardware.LoadSymbols(Split[0]);
  Result := True;
end;

function TConfigFile.DoRunToHALT: Boolean;
begin
  Hardware.RunToHalt;
  Result := True;
end;

procedure TConfigFile.LoadConfigFile(Filename: String);
var SL: TStringList;
  Line: String;
  Pos: Integer;
  Okay: Boolean;
begin
  if Filename.Chars[0] = '@' then
    Filename := Filename.Substring(1);
  SL := TStringList.Create;
  SL.LoadFromFile(Filename);

  for Line in SL do
    if (Line.Length > 0) and (Line.Chars[0] <> ';') then
    begin
      Pos := Line.IndexOf('=');
      if Pos < 0 then
        Okay := DoCommand(Line, '')
      else
        Okay := DoCommand(Line.Substring(0,Pos), Line.Substring(Pos+1));
      if not Okay then
        raise Exception.Create('Invalid command line: ' + Line);
    end;
end;

end.

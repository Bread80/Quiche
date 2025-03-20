unit mz80Tester;

interface
uses mZ80State, mZ80, Generics.Collections, SysUtils;

type
  TAccessDevice = (adNone, adRFSH, adIO, adMemory);
  TAccessType = (atNone, atRead, atWrite);
  TControlOut = (coHALT, coBUSAK, coINTAK); //TK
  TControlOuts = set of TControlOut;

type TDataLogItem = class
  public
    AccessDevice: TAccessDevice;
    AccessType: TAccessType;
    Addr: Word;
    Data: Byte;
    ControlOuts: TControlOuts;
    constructor Create(AAccessDevice: TAccessDevice;AAccessType: TAccessType;AAddr: Word;
      AData: Byte;AControlOuts: TControlOuts = []);
  end;

  TDataLog = TList<TDataLogItem>;


//;Code to load Z80 state
//Zero bytes are where data needs to be added
const LoadCode: TArray<Byte> = [
  $3e,0,      //LD A,n  ;Special registers <g>
  $ed,$47,    //LD I,A
  $3e,0,      //LD A,n
  $ed,$4f,    //LD R,A
  $f1,0,0,        //POP AF  ;Need to use stack to load Flags
  $08,        //EX AF,AF' ;Move to alt AF
  $f1,0,0,        //POP AF  ;Basic AF
  $01,0,0,    //LD BC,nn  ;Alt BC,DE,HL
  $11,0,0,    //LD DE,nn
  $21,0,0,    //LD HL,nn
  $d9,        //EXX
  $01,0,0,    //LD BC,nn  ;Basic BC,DE,HL
  $11,0,0,    //LD DE,nn
  $21,0,0,    //LD HL,nn
  $dd,$21,0,0,//LD IX,nn  ;Index registers
  $fd,$21,0,0,//LD IY,nn
  $31,0,0,    //LD SP,nn  ;Stack
  0,          //NOP or DI or EI  ;Interrupt state
  $c3,0,0];   //JP nn     ;And a sneaky LD PC,nn

//>>Do the opcode here

//;Code to save Z80 state
const SaveCode: TArray<Byte> = [
  $ed,$73,0,0,//LD (nn),SP  ;Capture PC and SP - nn is junk random throwaway
  $fd,$e5,    //PUSH IY  ;Index registers
  $dd,$e5,    //PUSH IX
  $e5,        //PUSH HL ;Basic registers
  $d5,        //PUSH DE
  $c5,        //PUSH BC
  $d9,        //EXX     ;Alt registers
  $e5,        //PUSH HL
  $d5,        //PUSH DE
  $c5,        //PUSH BC
  $f5,        //PUSH AF ;AF
  $08,        //EX AF,AF' ;Alt AF
  $f5,        //PUSH AF
  $ed,$5f,    //LD A,R  ;Refresh and it's flags
  $f5,        //PUSH AF
  $ed,$57,    //LD A,I  ;Interrupt vector and it's flags
  $f5];       //PUSH AF

type TZ80TestRunner = class
  private
    FInputState: TZ80State;
    FInputBytes: TArray<Byte>;
    FOutData: TDataLog;
    FOutputState: TZ80State;

    LoadBytes: TArray<Byte>;
    FZ80Exec: TZ80Executor;
    FByteCount: Integer;
    FLDARFlags: Byte;
    FLDAIFlags: Byte;
  protected
    //Combines the Z80 state with LoadCode and returns an array of bytes to send to the Z80
    //to load the initial state
    function LoadInputState(Z80: TZ80State): TArray<Byte>;
    //Load the bytes for the opcode to be executed - including memory and input reads for both
    //the opcode and the execution of the opcode.
    function LoadInputBytes(IndexModifier: TIndexModifier;InstructionSet: TInstructionSet;
      Opcode: Byte;Data: TArray<Byte>;var OpcodeLength: Integer): TArray<Byte>;
    procedure Execute(Z80Exec: TZ80Executor;Code: TArray<Byte>;EndEarly: Boolean;CodeLength: Integer;
      Log: TDataLog;var ByteCount: Integer);
    procedure FetchOutputState(Log: TDataLog;var Z80: TZ80State);

    //Returns:
    //AFBCDEHLAF'BC'DE'HL'IXIYSPPCIfRf
    //Wher If and Rf = I and R registers and flags returned by LD A,I and LD A,R
    //and b = Number or bytes read + written
    function StateToStr(Z80: TZ80State): String;
    //Returns the log as a string of the format:
    //aaaa[r|w|i|o]dd aaaa[r|w|i|o]dd etc...
    //Where aaaa = the address, dd = the data and
    //r = memory write
    //w = memory read
    //i = in(put)
    //o = out(put)
    function LogToStr(Log: TDataLog): String;
    function BytesToStr(Data: TArray<Byte>): String;
  public
    constructor Create(AZ80Exec: TZ80Executor);
    destructor Destroy;override;

    procedure RandomizeZ80State(Z80: TZ80State);
    function RandomizeData(Length: Integer): TArray<Byte>;

    procedure RunTest(IndexModifier: TIndexModifier;InstructionSet: TInstructionSet;
      Opcode: Byte;Data: TArray<Byte>);
    property Z80Exec: TZ80Executor read FZ80Exec write FZ80Exec;

    property InputState: TZ80State read FInputState write FInputState;
    property InputBytes: TArray<Byte> read FInputBytes write FInputBytes;
    //Number of bytes read from InputBytes
    property ByteCount: Integer read FByteCount;
    property OutputState: TZ80State read FOutputState write FOutputState;
    property OutData: TDataLog read FOutData write FOutData;
    property LDAIFlags: Byte read FLDAIFlags;
    property LDARFlags: Byte read FLDARFlags;
end;

type TZ80TestManager = class
  private
    FZ80TestRunner: TZ80TestRunner;
    FLog: TProc<String>;
    FRepeatCount: Integer;

    function TestToStr: String;
  public
    constructor Create(AZ80Exec: TZ80Executor);
    destructor Destroy;override;
    procedure TestOpcode(IndexModifier: TIndexModifier;InstructionSet: TInstructionSet;
      Opcode: Byte);
    procedure TestInstructionSet(IndexModifier: TIndexModifier;InstructionSet: TInstructionSet);
    procedure TestIndexModifier(IndexModifier: TIndexModifier);
    procedure TestAll;

    property Z80TestRunner: TZ80TestRunner read FZ80TestRunner write FZ80TestRunner;

    property RepeatCount: Integer read FRepeatCount write FRepeatCount;
    property Log: TProc<String> read FLog write FLog;
  end;

implementation

procedure Insert(var Code: TArray<Byte>; var Index: Integer; Data: Byte);
begin
  while (Index < Length(Code)) and (Code[Index] <> 0) do
    inc(Index);
  if Index >= Length(Code) then
    raise Exception.Create('Insert: Too many bytes to insert');

  Code[Index] := Data;
  inc(Index);
end;

procedure Insert16(var Code: TArray<Byte>; var Index: Integer; Data: Word);
begin
  Insert(Code, Index, Data and $FF);
  Insert(Code, Index, Data shr 8);
end;

procedure AppendByte(var Bytes: TArray<Byte>; var Index: Integer; Data: Byte);
begin
  if Index >= Length(Bytes) then
    raise Exception.Create('InputByte array length exceeded in AppendByte');

  Bytes[Index] := Data;
  inc(Index);
end;

function FetchByte(Data: TArray<Byte>; var Index: Integer): Byte;
begin
  if Index >= Length(Data) then
    raise Exception.Create('Opcode data array too short (from FetchByte)');
  Result := Data[Index];
  inc(Index);
end;

function LogFetch(Log: TDataLog;var Index: Integer): Byte;
begin
  while (Index < Log.Count) and (Log[Index].AccessType <> atWrite) do
    inc(Index);
  if Index >= Log.Count then
    raise Exception.Create('Error in LogFetch: not enough entries in Log');

  Result := Log[Index].Data;
  inc(Index);
end;

function LogFetch16(Log: TDataLog;var Index: Integer): Word;
begin
  //NOTE: PUSH puches the /high/ byte first and then the /low/ byte
  //so we process bytes in the reverse order to normal!!!
  Result := (LogFetch(Log, Index) shl 8) or LogFetch(Log, Index);
end;
{ TZ80TestRunner }

function TZ80TestRunner.BytesToStr(Data: TArray<Byte>): String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Length(Data) - 1 do
    Result := Result + IntToHex(Data[I], 2) + ' ';
end;

constructor TZ80TestRunner.Create(AZ80Exec: TZ80Executor);
begin
  inherited Create;
  FZ80Exec := AZ80Exec;
  FInputState := TZ80State.Create;
  FOutputState := TZ80State.Create;
  FOutData := TDataLog.Create;
end;

destructor TZ80TestRunner.Destroy;
begin
  FInputState.Free;
  FOutputState.Free;
  FOutData.Free;

  inherited;
end;

procedure TZ80TestRunner.Execute(Z80Exec: TZ80Executor; Code: TArray<Byte>;EndEarly: Boolean;CodeLength: Integer;
  Log: TDataLog; var ByteCount: Integer);
var
  CodePtr: Integer;
  IsM1: Boolean;
  Finished: Boolean;
begin
  CodePtr := 0;
  Log.Clear;

{  Z80Exec.HookM1 :=
    procedure(Addr: Word)
    begin
      IsM1 := True;
      Log.Add(TDataLogItem.Create(adRfsh, atNone, Addr, 0));
    end;
  Hardware.HookMemoryRead :=
    function(Addr: Word;Data:Byte):Byte
    begin
      Result := FetchByte(Code, CodePtr);
      Log.Add(TDataLogItem.Create(adMemory, atRead, Addr, Result));
    end;
  Z80Exec.Memory.HookWrite :=
    procedure(Addr: Word;Old,New:Byte)
    begin
      Log.Add(TDataLogItem.Create(adMemory, atWrite, Addr, New));
    end;
  Z80Exec.IO.HookInp :=
    function(Port: Word):Byte
    begin
      Result := FetchByte(Code, CodePtr);
      Log.Add(TDataLogItem.Create(adIO, atRead, Port, Result));
    end;
  Z80Exec.IO.HookOut :=
    procedure(Port: Word;Data:Byte)
    begin
      Log.Add(TDataLogItem.Create(adIO, atWrite, Port, Data));
    end;
}
  repeat
    IsM1 := False;
    Z80Exec.ExecOpcode;
    if EndEarly then
      Finished := IsM1 and (CodePtr >= CodeLength)
    else
      Finished := CodePtr >= CodeLength;
  until Finished;

  ByteCount := CodePtr;
end;

procedure TZ80TestRunner.FetchOutputState(Log: TDataLog; var Z80: TZ80State);
var
  Index: Integer;
begin
  Index := 0;
  while (Index < Log.Count) and not (Log[Index].AccessType = atRead) do
    inc(Index);
  if Index >= Log.Count then
    raise Exception.Create('Error in FetchOutputState: not enough entries in Log');

  Z80.PC := Log[Index].Addr;
  //SP is written to memory, so written little endian...
  Z80.SP := LogFetch(Log, Index) or (LogFetch(Log, Index) shl 8);
  //...other values are PUSHed on the stack so written big endian
  Z80.IY := LogFetch16(Log, Index);
  Z80.IX := LogFetch16(Log, Index);
  Z80.HL := LogFetch16(Log, Index);
  Z80.DE := LogFetch16(Log, Index);
  Z80.BC := LogFetch16(Log, Index);
  Z80.HLa := LogFetch16(Log, Index);
  Z80.DEa := LogFetch16(Log, Index);
  Z80.BCa := LogFetch16(Log, Index);
  Z80.AF := LogFetch16(Log, Index);
  Z80.AFa := LogFetch16(Log, Index);
  Z80.R := LogFetch(Log, Index);
  FLDARFlags := LogFetch(Log, Index);
  Z80.I := LogFetch(Log, Index);
  FLDAIFlags := LogFetch(Log, Index);
end;

function TZ80TestRunner.LoadInputBytes(IndexModifier: TIndexModifier;
  InstructionSet: TInstructionSet; Opcode: Byte;
  Data: TArray<Byte>;var OpcodeLength: Integer): TArray<Byte>;
const BytesLength = 100;
var
  ResultIndex: Integer;
  DataIndex: Integer;
begin
  SetLength(Result, BytesLength);

  DataIndex := 0;
  ResultIndex := 0;
  //Index modifier prefix
  case IndexModifier of
    imMain:;  //Do nothing
    imIX: AppendByte(Result, ResultIndex, $DD);
    imIY: AppendByte(Result, ResultIndex, $FD);
  end;

  //Add instruction set modifier
  case InstructionSet of
    isMain: ;  //Do nothing
    isED: AppendByte(Result, ResultIndex, $ED);
    isCB: AppendByte(Result, ResultIndex, $CB);
  end;

  OpcodeLength := ResultIndex + 1;

  //If we have a CB opcode with IX/IY then the +d offset appears /before/
  //the opcode byte
  if (InstructionSet = isCB) and (IndexModifier <> imMain) then
    AppendByte(Result, ResultIndex, FetchByte(Data, DataIndex));

  AppendByte(Result, ResultIndex, Opcode);

  while DataIndex < Length(Data) do
    AppendByte(Result, ResultIndex, FetchByte(Data, DataIndex));

  SetLength(Result, ResultIndex);
end;

function TZ80TestRunner.LoadInputState(Z80: TZ80State): TArray<Byte>;
var
  CodePtr: Integer;
begin
  SetLength(Result, Length(LoadCode));

  for CodePtr := 0 to Length(LoadCode)-1 do
    Result[CodePtr] := LoadCode[CodePtr];

  CodePtr := 0;
  Insert(Result, CodePtr, Z80.I);
  Insert(Result, CodePtr, Z80.R);
  Insert16(Result, CodePtr, Z80.AFa);
  Insert16(Result, CodePtr, Z80.AF);
  Insert16(Result, CodePtr, Z80.BCa);
  Insert16(Result, CodePtr, Z80.DEa);
  Insert16(Result, CodePtr, Z80.HLa);
  Insert16(Result, CodePtr, Z80.BC);
  Insert16(Result, CodePtr, Z80.DE);
  Insert16(Result, CodePtr, Z80.HL);
  Insert16(Result, CodePtr, Z80.IX);
  Insert16(Result, CodePtr, Z80.IY);
  Insert16(Result, CodePtr, Z80.SP);
  Insert(Result, CodePtr, 0);  //TK: DI/EI
  Insert16(Result, CodePtr, Z80.PC);
end;

function TZ80TestRunner.LogToStr(Log: TDataLog): String;
var
  Item: TDataLogItem;
begin
  Result := '';
  for Item in Log do
  begin
    Result := Result + IntToHex(Item.Addr, 4);
    case Item.AccessDevice of
      adMemory:
      case Item.AccessType of
        atNone: ;
        atRead: Result := Result + 'r';
        atWrite: Result := Result + 'w';
      end;
      adIO:
     case Item.AccessType of
        atNone: ;
        atRead: Result := Result + 'i';
        atWrite: Result := Result + 'o';
      end;
    end;
    Result := Result + IntToHex(Item.Data, 2) + ' ';
  end;
end;

function TZ80TestRunner.RandomizeData(Length: Integer): TArray<Byte>;
var
  I: Integer;
begin
  SetLength(Result, Length);
  for I := 0 to Length-1 do
    Result[I] := Random(256);
end;

procedure TZ80TestRunner.RandomizeZ80State(Z80: TZ80State);
begin
  Z80.AF := Random(65536);
  Z80.AFa := Random(65536);
  Z80.BC := Random(65536);
  Z80.BCa := Random(65536);
  Z80.DE := Random(65536);
  Z80.DEa := Random(65536);
  Z80.HL := Random(65536);
  Z80.HLa := Random(65536);
  Z80.IX := Random(65536);
  Z80.IY := Random(65536);
  Z80.SP := Random(65536);
  Z80.PC := Random(65536);
  Z80.I := Random(256);
  Z80.R := Random(256);
end;

procedure TZ80TestRunner.RunTest(IndexModifier: TIndexModifier;
  InstructionSet: TInstructionSet; Opcode: Byte;Data: TArray<Byte>);
var
  TempLog: TDataLog;
  TempByteCount: Integer;
  CodeLength: Integer;
begin
  TempLog := TDataLog.Create;
  try
    Z80Exec.Z80.ColdReset;

    //Load processor state
    LoadBytes := LoadInputState(InputState);
    TempByteCount := 0;
    Execute(Z80Exec, LoadBytes, False, Length(LoadBytes), TempLog, TempByteCount);

    //Execute opcode
    FInputBytes := LoadInputBytes(IndexModifier, InstructionSet, Opcode, Data, CodeLength);
    if not Assigned(OutData) then
      OutData := TDataLog.Create
    else
      OutData.Clear;
    FByteCount := 0;
    Execute(Z80Exec, FInputBytes, True, CodeLength, OutData, FByteCount);

    //Fetch processor state
    TempLog.Clear;
    Execute(Z80Exec, SaveCode, False, Length(SaveCode), TempLog, TempByteCount);
    FetchOutputState(TempLog, FOutputState);
  finally
    TempLog.Free;
  end;
end;

function TZ80TestRunner.StateToStr(Z80: TZ80State): String;
begin
  Result := IntToHex(Z80.AF, 4);
  Result := Result + IntToHex(Z80.BC, 4);
  Result := Result + IntToHex(Z80.DE, 4);
  Result := Result + IntToHex(Z80.HL, 4);
  Result := Result + IntToHex(Z80.AFa, 4);
  Result := Result + IntToHex(Z80.BCa, 4);
  Result := Result + IntToHex(Z80.DEa, 4);
  Result := Result + IntToHex(Z80.HLa, 4);
  Result := Result + IntToHex(Z80.IX, 4);
  Result := Result + IntToHex(Z80.IY, 4);
  Result := Result + IntToHex(Z80.SP, 4);
  Result := Result + IntToHex(Z80.PC, 4);
  Result := Result + IntToHex(Z80.I, 2);
  Result := Result + IntToHex(LDAIFlags, 2);
  Result := Result + IntToHex(Z80.R, 2);
  Result := Result + IntToHex(LDARFlags, 2);
end;

{ TDataLogItem }

constructor TDataLogItem.Create(AAccessDevice: TAccessDevice;
  AAccessType: TAccessType; AAddr: Word; AData: Byte;
  AControlOuts: TControlOuts);
begin
  inherited Create;
  AccessDevice := AAccessDevice;
  AccessType := AAccessType;
  Addr := AAddr;
  Data := AData;
  ControlOuts := AControlOuts;
end;

{ TTestManager }

constructor TZ80TestManager.Create(AZ80Exec: TZ80Executor);
begin
  inherited Create;

  FZ80TestRunner := TZ80TestRunner.Create(AZ80Exec);
  FRepeatCount := 1;
end;

destructor TZ80TestManager.Destroy;
begin

  inherited;
end;

procedure TZ80TestManager.TestAll;
begin
  TestIndexModifier(imMain);
  TestIndexModifier(imIX);
  TestIndexModifier(imIY);
end;

procedure TZ80TestManager.TestIndexModifier(IndexModifier: TIndexModifier);
begin
  TestInstructionSet(IndexModifier, isMain);
  TestInstructionSet(IndexModifier, isED);
  TestInstructionSet(IndexModifier, isCB);
end;

procedure TZ80TestManager.TestInstructionSet(IndexModifier: TIndexModifier;
  InstructionSet: TInstructionSet);
var
  Opcode: Byte;
begin
  for Opcode := 0 to 255 do
    TestOpcode(IndexModifier, InstructionSet, Opcode);
end;

procedure TZ80TestManager.TestOpcode(IndexModifier: TIndexModifier;
  InstructionSet: TInstructionSet; Opcode: Byte);
var
  Data: TArray<Byte>;
  LogStr: String;

  I: Integer;
begin
  for I := 1 to RepeatCount do
  begin
    Z80TestRunner.RandomizeZ80State(Z80TestRunner.InputState);
    Data := Z80TestRunner.RandomizeData(10);
    Z80TestRunner.RunTest(IndexModifier, InstructionSet, Opcode, Data);

    LogStr := TestToStr;
    if Assigned(Log) then
      Log(LogStr);
  end;
end;

function TZ80TestManager.TestToStr: String;
var
  Data: String;
begin
  Data := 'test:' + Z80TestRunner.BytesToStr(Z80TestRunner.InputBytes) + #10#13;
  Data := Data + 'start:' + Z80TestRunner.StateToStr(Z80TestRunner.InputState) + #10#13;
  Data := Data + 'final:' + Z80TestRunner.StateToStr(Z80TestRunner.OutputState) + #10#13;
  Data := Data + 'log:' + Z80TestRunner.LogToStr(Z80TestRunner.OutData) + #10#13;
  Result := Data;
end;

end.

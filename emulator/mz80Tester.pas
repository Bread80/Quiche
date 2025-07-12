(*
This code seeks to test a Z80 (or Z80 emulator) and thus allow validation of
said emulator. Tests can be run on a physucal Z80 by putting it into a physical
test harness which can control every input pin and read the state of every
output pin.

This enables the features to be tested which are difficult or impossible to test
just using 'on-chip' code. This includes features such as I/O and interrupts.
The use of a test harness removes the limitation of ROM or RAM based code. Thus
code can be run irrespective of memory addresses, values which are read by the
CPU can be manipulated at will, and all values written can easily be logged.

Eaxmples of this are the HALT and the block move and I/O instructions (ie LDIR,
CPIR, OTIR etc). The Z80 executes these by repeatedly reading the opcode and
executing the opcode, making on-chip tests complicated. By feeding instructions
via a harness the execution of a single iteration of these instructions can be
examined.

The Test Method
---------------

Each test begins with a short section of Z80 assembly which sets values to every
settable Z80 register. The opcode under test is then run, including any
subsequent cycles to read or write data (etc). Finally another short piece of
code is run which reads out the state of every readable register.

In practice this is handled as three separate 'runs' of the tester. The first
runs the setup code, the second the opcode, and the third runs the code to read
register states. On each run the tester logs the CPUs reads and writes and other
activities. On the first run (setup) run the logs are thrown away. On the second
run (opcode) the tester logs the CPUs read and writes. On the third run (read)
the tester extracts the values of writes from the log to establish the register
values after the test.

In Detail
---------

When the tester is run an array of 'read' values is passed in. This array is the
values which will be passed to the CPU each time it performs a read, whether from
memory or I/O. The array contains both the code to be executed *and* any values
which will be read from memory or I/O in the order they will be read.

The tester does not distintuish between instruction, immediate data in
instructions and other data (from memory or I/O). The array is simply a list of
bytes to be sent in turn for each read request.

When running the setup code to load registers this must be taken into account so
that, for example, where a POP is used to set a register pair the data to be
popped will be included inline after the POP instruction. Such data must also be
in the correct byte order for the read.

When running the opcode under test an array of bytes should be given which
includes both the opcode under test and any test values to be 'read' from memory
or I/O by the opcode. To avoid potential buffer overrun errors it is recommended
to create an array of values large enough for the 'worst case' scenario.

In practice (at present) the tester generates randomised values to be set into
every register during the setup phase, and random values to be read during the
opcode phase. It would, of course, be possible to use opcode specific values
for more detailed examination of certain features. This is not supported by the
current code.

Possible Extensions
-------------------

The code below records the CPU state for each memory and I/O cycle. It does not
record the number of clock cycles taken by an opcode. It does not record output
signals beyond those which specify the cycle type (other signals include HALT
and BUSRQ), and it does not record signals on a per cycle or half-cycle basis.
In theory it could be extended to record all of this data.

The code below also does not allow the Z80s input signals (eg. INT, WAIT, BUSAK)
to be driven. Again this functionality could be added with some extension to the
input data format.
*)
unit mz80Tester;

interface
uses mHardware, mZ80State, mZ80, Generics.Collections, SysUtils, Classes;

type
  TCycleType = (
    ctNone,
    ctRefresh,  //RFSH (+ MREQ))
    ctM1,       //M1 (+ MREQ + RD)
    ctMemRead,  //MREQ + RD
    ctMemWrite, //MREQ + WR
    ctIOIn,     //IORQ + RD
    ctIOOut,    //IORQ + WR
    ctINTAK);   //IORQ + M1

  //Output control signals - not currently recorded
  TControlOut = (coRFSH, coM1, coMREQ, coIORQ, coRD, coWR, coHALT, coBUSAK, coINTAK);
  TControlOuts = set of TControlOut;  //Control signals output by the Z80

//Log item for each CPU cycle
type TDataLogItem = class
  public
    CycleType: TCycleType;
    Addr: Word;     //Address or IO port to read/write
    Data: Byte;     //Data being read or written
    ControlOuts: TControlOuts;  //Actice control signals
    constructor Create(ACycleType: TCycleType;AAddr: Word;
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
  $f1,0,0,    //POP AF + bytes to pop ;Need to use stack to load Flags
  $08,        //EX AF,AF' ;Move to alt AF
  $f1,0,0,    //POP AF + bytes to pop ;Basic AF
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
    FHardware: THardware;
    FMemory: TMemory;
    FIO: TIODevice;
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
    //Patches the Z80 state into LoadCode and returns an array of bytes to send
    //to the Z80 to load the initial state
    function LoadInputState(Z80: TZ80State): TArray<Byte>;
    //Load the bytes for the opcode to be executed - including memory and input
    //reads for both the opcode and the execution of the opcode.
    function LoadInputBytes(IndexModifier: TIndexModifier;InstructionSet: TInstructionSet;
      Opcode: Byte;Data: TArray<Byte>;var OpcodeLength: Integer): TArray<Byte>;
    //Executes the given Code on the Emulator.
    //IfEndEarly is true, onlya single opcode will be executed (??)
    //CodeLength specifies the number of bytes within Code to execute (which allows
    //Code to include generalised data).
    //Log returns the logged data.
    //ByteCount returns the number of bytes read from Code
    procedure Execute(Z80Exec: TZ80Executor;Code: TArray<Byte>;EndEarly: Boolean;CodeLength: Integer;
      Log: TDataLog;var ByteCount: Integer);
    //Extracts the CPU state from the log. Log must be that generated by running
    //SaveCode.
    procedure FetchOutputState(Log: TDataLog;var Z80: TZ80State);

    //Returns the recorded CPU state in the format:
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
    //Returns Data as a string of hex bytes
    function BytesToStr(Data: TArray<Byte>): String;
  public
    constructor Create(AHardware: THardware;AMemory: TMemory;AIO: TIODevice);
    destructor Destroy;override;

    //Randomises every register (etc) in the Z80 state
    procedure RandomizeZ80State(Z80: TZ80State);
    //Generates a string of random bytes of length Length.
    function RandomizeData(Length: Integer): TArray<Byte>;

    //Runs the test for the given Opcode in the given instruction set with the
    //given index register modifier. Data is the list of values to be read by
    //the CPU
    procedure RunTest(IndexModifier: TIndexModifier;InstructionSet: TInstructionSet;
      Opcode: Byte;Data: TArray<Byte>);

    //The emulator
    property Z80Exec: TZ80Executor read FZ80Exec write FZ80Exec;
    //Input values
    property InputState: TZ80State read FInputState write FInputState;
    property InputBytes: TArray<Byte> read FInputBytes write FInputBytes;
    //Output results
    //Number of bytes read from InputBytes
    property ByteCount: Integer read FByteCount;
    property OutputState: TZ80State read FOutputState write FOutputState;
    property OutData: TDataLog read FOutData write FOutData;
    property LDAIFlags: Byte read FLDAIFlags;
    property LDARFlags: Byte read FLDARFlags;
end;

type TZ80TestManager = class
  private
    FHardware: THardware;
    FMemory: TMemory;
    FIO: TIODevice;
    FZ80TestRunner: TZ80TestRunner;
    FLog: TStringList;
    FRepeatCount: Integer;

    function TestToStr: String;
  public
    constructor Create;
    destructor Destroy;override;
    //Test a single opcode
    procedure TestOpcode(IndexModifier: TIndexModifier;InstructionSet: TInstructionSet;
      Opcode: Byte);
    //Test all opcodes with the given index modifier and instruction set
    procedure TestInstructionSet(IndexModifier: TIndexModifier;InstructionSet: TInstructionSet);
    //Test all opcodes (across all instruction sets) with the given index modifier
    procedure TestIndexModifier(IndexModifier: TIndexModifier);
    //Test all instructions in all instruction sets with all index modifiers
    //(including none)
    procedure TestAll;

    property Z80TestRunner: TZ80TestRunner read FZ80TestRunner write FZ80TestRunner;

    property RepeatCount: Integer read FRepeatCount write FRepeatCount;
    property Log: TStringList read FLog;
  end;

implementation
uses mMemory;

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
    raise Exception.Create('Opcode data array too short (from FetchByte).'#13+
      'Data length: ' + Length(Data).ToString + ', Index: ' + Index.ToString);
  Result := Data[Index];
  inc(Index);
end;

function LogFetch(Log: TDataLog;var Index: Integer): Byte;
begin
  while (Index < Log.Count) and not (Log[Index].CycleType in [ctMemWrite, ctIOOut]) do
    inc(Index);
  if Index >= Log.Count then
    raise Exception.Create('Error in LogFetch: not enough entries in Log');

  Result := Log[Index].Data;
  inc(Index);
end;

function LogFetch16(Log: TDataLog;var Index: Integer): Word;
begin
  //NOTE: PUSH pushes the /high/ byte first and then the /low/ byte
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

constructor TZ80TestRunner.Create(AHardware: THardware;AMemory: TMemory;AIO: TIODevice);
begin
  inherited Create;
  FHardware := AHardware;
  FMemory := AMemory;
  FIO := AIO;
  FZ80Exec := AHardware.Z80;
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

  Z80Exec.HookM1 :=
    procedure(Addr: Word)
    begin
      IsM1 := True;
      Log.Add(TDataLogItem.Create(ctRefresh, Addr, 0, [coRFSH, coMREQ]));
    end;
  FMemory.HookRead :=
    function(Device: TAbstractDevice;Addr: Word;Data:Byte):Byte
    begin
      Result := FetchByte(Code, CodePtr);
      if IsM1 then
        Log.Add(TDataLogItem.Create(ctM1, Addr, Result, [coM1, coMREQ, coRD]))
      else
        Log.Add(TDataLogItem.Create(ctMemRead, Addr, Result, [coMREQ, coRD]));
    end;
  FMemory.HookWrite :=
    function(Device: TAbstractDevice;Addr: Word;Old,New:Byte): Byte
    begin
      Result := New;
      Log.Add(TDataLogItem.Create(ctMemWrite, Addr, New, [coMREQ, coWR]));
    end;
  FIO.HookRead :=
    function(Device: TAbstractDevice;Port: Word;Data: Byte):Byte
    begin
      Result := FetchByte(Code, CodePtr);
      Log.Add(TDataLogItem.Create(ctIOIn, Port, Result, [coIORQ, coRD]));
    end;
  FIO.HookWrite :=
    function(Device: TAbstractDevice;Port: Word;Old, New:Byte): Byte
    begin
      Result := New;
      Log.Add(TDataLogItem.Create(ctIOOut, Port, New, [coIORQ, coWR]));
    end;

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
  //New PC is the address if the first M1 cycle
  Index := 0;
  while (Index < Log.Count) and (Log[Index].CycleType <> ctM1) do
    inc(Index);
  if Index > Log.Count then
    raise Exception.Create('Error in FetchOutputState: no M1 cycle found');
  Z80.PC := Log[Index].Addr;

  //We want to extract the bytes written to the log. Step through until we find
  //the index first write...
  Index := 0;
  while (Index < Log.Count) and not (Log[Index].CycleType in [{ctM1, ctMemRead,} ctMemWrite, {ctIOIn,} ctIOOut]) do
    inc(Index);
  if Index > Log.Count then
    raise Exception.Create('Error in FetchOutputState: not enough entries in Log');

  //...then step through the log to find subsequent writes:
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
  //AF are popped so read big endian
  Insert(Result, CodePtr, Z80.Aa);
  Insert(Result, CodePtr, Z80.Fa);
  Insert(Result, CodePtr, Z80.A);
  Insert(Result, CodePtr, Z80.F);
  //Other values are LDed so read little endian
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
const CycleTypeStrings: array[Low(TCycleType)..high(TCycleType)] of String =
  ('x', 'h', 'm', 'r', 'w', 'i', 'o', 'k');
begin
  Result := '';
  for Item in Log do
  begin
    Result := Result + CycleTypeStrings[Item.CycleType];
    Result := Result + IntToHex(Item.Addr, 4);
    Result := Result + IntToHex(Item.Data, 2) + ' ';
    //TODO: Control signals
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
  //Output values:
  //    A F B C D E H L A'F'B'C'D'E'H'L'IX  IY  SP  PC  I IfR Rf
  //(If and Rf are the flags after LD A,I and LD A,R respectively)
  Result := IntToHex(Z80.A, 2);
  Result := Result + IntToHex(Z80.F, 2);
  Result := Result + IntToHex(Z80.BC, 4);
  Result := Result + IntToHex(Z80.DE, 4);
  Result := Result + IntToHex(Z80.HL, 4);
  Result := Result + IntToHex(Z80.Aa, 2);
  Result := Result + IntToHex(Z80.Fa, 2);
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

constructor TDataLogItem.Create(ACycleType: TCycleType; AAddr: Word; AData: Byte;
  AControlOuts: TControlOuts);
begin
  inherited Create;
  CycleType := ACycleType;
  Addr := AAddr;
  Data := AData;
  ControlOuts := AControlOuts;
end;

{ TTestManager }

constructor TZ80TestManager.Create;
begin
  inherited Create;

  FLog := TStringList.Create;
  FHardware := THardware.Create;
  FHardware.RegisterDevice('RAM', TRAM);
  FHardware.CreateMemoryDevice('RAM', 'TestRAM', '');
  FMemory := FHardware.MemoryDevices[0] as TMemory;
  FHardware.RegisterDevice('IODevice', TIODevice);
  FHardware.CreateIODevice('IODevice', 'TestIO', '');
  FIO := FHardware.IODevices[0] as TIODevice;

  FZ80TestRunner := TZ80TestRunner.Create(FHardware, FMemory, FIO);
  FRepeatCount := 1;
end;

destructor TZ80TestManager.Destroy;
begin
  FLog.Free;
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
    if Assigned(FLog) then
      FLog.Add(LogStr);
  end;
end;

function TZ80TestManager.TestToStr: String;
var
  Data: String;
begin
  Data := 'test:' + Z80TestRunner.BytesToStr(Z80TestRunner.InputBytes) + #13;
  Data := Data + 'start:' + Z80TestRunner.StateToStr(Z80TestRunner.InputState) + #13;
  Data := Data + 'final:' + Z80TestRunner.StateToStr(Z80TestRunner.OutputState) + #13;
  Data := Data + 'log:' + Z80TestRunner.LogToStr(Z80TestRunner.OutData) + #13;
  Result := Data;
end;

end.

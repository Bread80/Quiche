unit IDE.Emulator;

interface

procedure Initialise(const ConfigFile: String;AInteractive: Boolean;
  const AConsoleInput: String);

procedure RunToHalt;

function TryReadByte(const Symbol: String;out Data: Byte): Boolean;
function TryReadWord(const Symbol: String;out Data: Word): Boolean;

//IXOffsetHack: Bytes to subtract from IX to get the stack pointer address.
//Required when running with a stack frame at root level.
procedure GetVarData(IXOffsetHack: Integer);

//Text (etc) written to the console during the last emulation
var ConsoleLog: String;

implementation
uses SysUtils,
  mHardware,
  //Include hardware so it is registered
  mMemory,
  {$ifdef fpc}CRT,{$endif}
  Def.VarTypes, Def.Consts, Def.Variables, Def.UserTypes;

var CurrConfigFile: String;
  Interactive: Boolean;

  ConsoleInput: String;
  ConsoleInputPtr: Integer;
  ConsoleEcho: Boolean;

procedure CPMWriteChar(Ch: Char);
begin
  if Interactive then
    write(Ch)
  else
    ConsoleLog := ConsoleLog + Ch;
end;

function CPMWaitChar: Char;
begin
  if Interactive then
{$ifdef fpc}
  begin
    while not KeyPressed do ;
    Result := ReadKey;
  end
{$else} //NOT WORKING
    Assert(False, 'NOT WORKING')
(*    repeat
      Read(Result);
    until Result <> #0
*){$endif}
  else
  begin
    if ConsoleInputPtr >= length(COnsoleInput) then
      raise Exception.Create('Read beyond end of ConsoleInput buffer');
    Result := ConsoleInput[ConsoleInputPtr];
    inc(ConsoleInputPtr);
  end;
  if ConsoleEcho then
    CPMWriteChar(Result);
end;

function CPMGetChar(Operation: Byte): Char;
begin
  if Interactive then
{$ifdef fpc}
  begin
    if KeyPressed then
      Result := ReadKey
    else
      Result := #0;
  end
{$else} //NOT WORKING
    Assert(False, 'NOT WORKING')
(*    repeat
      Read(Result);
    until Result <> #0
*){$endif}
  else
  begin
    if ConsoleInputPtr >= length(ConsoleInput) then
      Result := #0
    else
    begin
      Result := ConsoleInput[ConsoleInputPtr];
      inc(ConsoleInputPtr);
    end;
  end;

  if (Operation <> $ff) and (Result <> #0) then
    CPMWriteChar(Result);
end;

//Returns True is a console input character is available
function CPMTestChar: Boolean;
begin
  if Interactive then
{$ifdef fpc}
    Result := KeyPressed
{$else} //NOT WORKING
    Assert(False, 'NOT WORKING')
(*  begin
    if HaveChar <> -1 then
      Read(HaveChar);
    Result := HaveChar <> -1;
  end
*){$endif}
  else
    Result := ConsoleInputPtr >= Length(ConsoleInput);
end;

const OpcodeRET = $c9;

//For CPM emulation - based on V2.2
function HookOpcodeCPM(Addr: Word;Opcode: Byte): Byte;
begin
  case Addr of
    $0005:  //CPM BDOS address
    begin
      //BDOS function is in the C register
      case Hardware.Z80.Z80.C of
        1:  //Console input. Waits until a char is available.
            //Returns A=L=character. Echos to console.
        begin
          Hardware.Z80.Z80.A := ord(CPMWaitChar);
          Hardware.Z80.Z80.L := Hardware.Z80.Z80.A;
        end;
        2:  //Console output. Char in E
          CPMWriteChar(chr(Hardware.Z80.Z80.E));
        6:  //Direct Console I/O. Reads a character if one is available.
            //On entry: if E=$ff echos character if one is available
            //On exit: A contains the character read, zero if none is available
          Hardware.Z80.Z80.A := ord(CPMGetChar(Hardware.Z80.Z80.E));
        11: //Console status. Returns A=L=status.
            //Status is 0 if no character is avaiable, otherwise non-zero
        begin
          if CPMTestChar then
            Hardware.Z80.Z80.A := $ff
          else
            Hardware.Z80.Z80.A := 0;
          Hardware.Z80.Z80.L := Hardware.Z80.Z80.A;
        end;
      else
        //Invalid or non-emulated
      end;

      Result := OpcodeRET;  //RETurn
    end
  else  //All other addresses
    Result := Opcode;
  end;
end;

procedure Initialise(const ConfigFile: String;AInteractive: Boolean;
  const AConsoleInput: String);
begin
  Interactive := AInteractive;
  ConsoleInput := AConsoleInput;
  if ConfigFile = CurrConfigFile then
    Hardware.Reset
  else
  begin
    Hardware.Restart;
    Hardware.LoadConfigFile(ConfigFile);
    Hardware.Z80.HookOpcode := HookOpcodeCPM;
  end;
  ConsoleLog := '';
//  HaveChar := -1;
  ConsoleInputPtr := 0;
  ConsoleEcho := True;
end;

procedure RunToHalt;
begin
  Hardware.RunToHalt;
end;

function TryReadByte(const Symbol: String;out Data: Byte): Boolean;
var Addr: Word;
begin
  Result := Hardware.TrySymbolToAddr(Symbol, Addr);
  if Result then
    Data := Hardware.ReadMemoryByte(Addr);
end;

function TryReadWord(const Symbol: String;out Data: Word): Boolean;
var Addr: Word;
begin
  Result := Hardware.TrySymbolToAddr(Symbol, Addr);
  if Result then
    Data := Hardware.ReadMemoryWord(Addr);
end;

function TryReadOffsetByte(const Symbol: String;Offset: Integer;out Data: Byte): Boolean;
var Addr: Word;
begin
  Result := Hardware.TrySymbolToAddr(Symbol, Addr);
  if Result then
    Data := Hardware.ReadMemoryByte((Addr + Offset) and $ffff);
end;

function ReadByte(const Symbol: String): Integer;
var B: Byte;
begin
  if not TryReadByte(Symbol, B) then
    raise Exception.Create('Global symbol not found: ' + Symbol);
  Result := B;
end;

function ReadOffsetByte(const Symbol: String;Offset: Integer): Integer;
var B: Byte;
begin
  if not TryReadOffsetByte(Symbol, Offset, B) then
    raise Exception.Create('Global symbol not found: ' + Symbol);
  Result := B;
end;

function ReadWord(const Symbol: String): Integer;
var W: Word;
begin
  if not TryReadWord(Symbol, W) then
    raise Exception.Create('Global symbol not found: ' + Symbol);
  Result := W;
end;

function ReadInt8(const Symbol: String): Integer;
var B: Byte;
begin
  if not TryReadByte(Symbol, B) then
    raise Exception.Create('Global symbol not found: ' + Symbol);
  if B >= $80 then
    Result := (-1 xor $ff) or B
  else
    Result := B;
end;

function ReadInteger(const Symbol: String): Integer;
var W: Word;
begin
  if not TryReadWord(Symbol, W) then
    raise Exception.Create('Global symbol not found: ' + Symbol);
  if W >= $8000 then
    Result := (-1 xor $ffff) or W
  else
    Result := W;
end;

function ReadBlob(const Symbol: String;Size: Integer): TBlob;
var I: Integer;
begin
  SetLength(Result, Size);
  for I := 0 to Size-1 do
    Result[I] := ReadOffsetByte(Symbol, I);
end;

function ReadMemoryInt8(Addr: Word): Integer;
var B: Byte;
begin
  B := Hardware.ReadMemoryByte(Addr);
  if B >= $80 then
    Result := (-1 xor $ff) or B
  else
    Result := B;
end;

function ReadMemoryInteger(Addr: Word): Integer;
var W: Word;
begin
  W := Hardware.ReadMemoryWord(Addr);
  if W >= $8000 then
    Result := (-1 xor $ffff) or W
  else
    Result := W;
end;

//Addr is the address of the string data (not the variable)
function ReadMemoryString(Addr: Word): String;
var InString: Boolean;
  Len: Integer;
  I: Integer;
  Ch: Byte;
begin
  //Variable value -> string address
  Result := '$' + IntToHex(Addr,4).ToLower + ' (';
  if Addr = 0 then
    Result := Result + ''
  else
  begin
    InString := False;
    Len := Hardware.ReadMemoryByte(Addr);
    if Len > 0 then
    for I := 1 to Len do
    begin
      Ch := Hardware.ReadMemoryByte(Addr + I);
      if (Ch >= 32) and (Ch < 128) then
      begin
        if not InString then
        begin
          Result := Result + '''';
          InString := True;
        end;
        Result := Result + Chr(Ch);
      end
      else
      begin
        if InString then
        begin
          Result := Result + '''';
          InString := False;
        end;
        Result := Result + '#' + Ch.ToString;
      end;
    end;
    if InString then
      Result := Result + '''';
  end;
  Result := Result + ')';
end;

function ValueToString(const AsmName: String;AddrMode: TAddrMode;UserType: PUserType): TImmValue;
begin
  case AddrMode of
    amStatic:
      case UTToVT(UserType) of
        vtByte, vtChar, vtBoolean, vtEnumeration:
          Result := TImmValue.CreateTyped(UserType, ReadByte(AsmName));
        vtWord, vtPointer, vtTypedPointer: Result := TImmValue.CreateTyped(UserType, ReadWord(AsmName));
          vtInt8: Result := TImmValue.CreateTyped(UserType, ReadInt8(AsmName));
          vtInteger: Result := TImmValue.CreateTyped(UserType, ReadInteger(AsmName));
          vtString: Result := TImmValue.CreateString(ReadMemoryString(ReadWord(AsmName)));
          vtReal, vtFlag, vtTypeDef, vtUnknown: ;//TODO?
          vtSetMem: Result := TImmValue.CreateString('TODO: SetMem type');
//          vtArray, vtList: Result := TImmValue.CreateArray(ArrayToString(AsmName, AddrMode, UserType));
          vtArray, vtList: Result := TImmValue.CreateBlob(UserType, ReadBlob(AsmName, GetTypeSize(UserType)));
          vtRecord: Result := TImmValue.CreateString('TODO: Record types');
          vtFunction: Result := TImmValue.CreateString('TODO: Function types');
        else
          Assert(False);
        end;
(*      amStack:
      begin
        Addr := (IX + V.Offset) and $ffff;
        case V.VarType of
          vtByte, vtChar, vtBoolean, vtEnumeration:
            V.Value := TImmValue.CreateTyped(V.UserType, Hardware.ReadMemoryByte(Addr));
          vtWord, vtPointer, vtTypedPointer: V.Value := TImmValue.CreateTyped(V.UserType, Hardware.ReadMemoryWord(Addr));
          vtInt8: V.Value := TImmValue.CreateTyped(V.UserType, ReadMemoryInt8(Addr));
          vtInteger: V.Value := TImmValue.CreateTyped(V.UserType, ReadMemoryInteger(Addr));
          vtString: V.Value := TImmValue.CreateString(ReadMemoryString(Hardware.ReadMemoryWord(Addr)));
          vtReal, vtFlag, vtTypeDef, vtUnknown: ;//TODO?
          vtArray, vtList: V.Value := TImmValue.CreateString('TODO: Read List data');
          vtFunction: V.Value := TImmValue.CreateString('TODO: Function types');
        end;
      end;
*)
    amStaticRef: Result := TImmValue.CreateTyped(GetPointerToType(UserType), ReadWord(AsmName));
  else
      Assert(False);
  end;
end;

procedure GetVarData(IXOffsetHack: Integer);
var I: Integer;
  V: PVariable;
  IX: Word;
begin
  IX := (Hardware.Z80.Z80.IX - IXOffsetHack) and $ffff;

  for I := 0 to Vars.GetCount-1 do
  begin
    V := Vars.IndexToData(I);
    V.Value := ValueToString(V.GetAsmName, V.AddrMode, V.UserType);
  end;
end;

end.

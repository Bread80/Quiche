unit IDE.Emulator;

interface

procedure Initialise(const ConfigFile: String);

procedure RunToHalt;

function TryReadByte(const Symbol: String;out Data: Byte): Boolean;
function TryReadWord(const Symbol: String;out Data: Word): Boolean;

//IXOffsetHack: Bytes to subtract from IX to get the stack pointer address.
//Required when running with a stack frame at root level.
procedure GetVarData(IXOffsetHack: Integer);

implementation
uses SysUtils,
  mHardware,
  //Include hardware so it is registered
  mMemory,
  Def.QTypes, Def.Consts, Def.Variables, Def.UserTypes;

var CurrConfigFile: String;

procedure Initialise(const ConfigFile: String);
begin
  if ConfigFile = CurrConfigFile then
    Hardware.Reset
  else
  begin
    Hardware.Restart;
    Hardware.LoadConfigFile(ConfigFile);
  end;
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

function ReadByte(const Symbol: String): Integer;
var B: Byte;
begin
  Assert(TryReadByte(Symbol, B), 'Global symbol not found: ' + Symbol);
  Result := B;
end;

function ReadWord(const Symbol: String): Integer;
var W: Word;
begin
  Assert(TryReadWord(Symbol, W), 'Global symbol not found: ' + Symbol);
  Result := W;
end;

function ReadInt8(const Symbol: String): Integer;
var B: Byte;
begin
  Assert(TryReadByte(Symbol, B), 'Global symbol not found: ' + Symbol);
  if B >= $80 then
    Result := (-1 xor $ff) or B
  else
    Result := B;
end;

function ReadInteger(const Symbol: String): Integer;
var W: Word;
begin
  Assert(TryReadWord(Symbol, W), 'Global symbol not found: ' + Symbol);
  if W >= $8000 then
    Result := (-1 xor $ffff) or W
  else
    Result := W;
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

procedure GetVarData(IXOffsetHack: Integer);
var I: Integer;
  V: PVariable;
  Addr: Word;
  IX: Word;
begin
  IX := (Hardware.Z80.Z80.IX - IXOffsetHack) and $ffff;

  for I := 0 to VarGetCount-1 do
  begin
    V := VarIndexToData(I);
    case V.AddrMode of
      amStatic:
        case V.VarType of
          vtByte, vtChar, vtBoolean: V.Value := TImmValue.CreateTyped(V.UserType, ReadByte(V.GetAsmName));
          vtWord, vtPointer, vtTypedPointer: V.Value := TImmValue.CreateTyped(V.UserType, ReadWord(V.GetAsmName));
          vtInt8: V.Value := TImmValue.CreateTyped(V.UserType, ReadInt8(V.GetAsmName));
          vtInteger: V.Value := TImmValue.CreateTyped(V.UserType, ReadInteger(V.GetAsmName));
          vtString: V.Value := TImmValue.CreateString(ReadMemoryString(ReadWord(V.GetAsmName)));
          vtReal, vtFlag, vtTypeDef, vtUnknown: ;//TODO?
          vtEnumeration: V.Value := TImmValue.CreateEnumItem(V.UserType, ReadByte(V.GetAsmName));
          vtSubRange:
            case GetTypeSize(V.UserType) of
              1: V.Value := TImmValue.CreateTyped(V.UserType, ReadByte(V.GetAsmName));
              2: V.Value := TImmValue.CreateTyped(V.UserType, ReadWord(V.GetAsmName));
            else
              raise Exception.Create('Invalid SubRange size');
            end;
          vtSetMem: V.Value := TImmValue.CreateString('TODO: SetMem type');
          vtArray, vtList: V.Value := TImmValue.CreateString('TODO: Read List data');
          vtFunction: V.Value := TImmValue.CreateString('TODO: Function types');
        else
          Assert(False);
        end;
      amStack:
      begin
        Addr := (IX + V.Offset) and $ffff;
        case V.VarType of
          vtByte, vtChar, vtBoolean: V.Value := TImmValue.CreateTyped(V.UserType, Hardware.ReadMemoryByte(Addr));
          vtWord, vtPointer, vtTypedPointer: V.Value := TImmValue.CreateTyped(V.UserType, Hardware.ReadMemoryWord(Addr));
          vtInt8: V.Value := TImmValue.CreateTyped(V.UserType, ReadMemoryInt8(Addr));
          vtInteger: V.Value := TImmValue.CreateTyped(V.UserType, ReadMemoryInteger(Addr));
          vtString: V.Value := TImmValue.CreateString(ReadMemoryString(Hardware.ReadMemoryWord(Addr)));
          vtReal, vtFlag, vtTypeDef, vtUnknown: ;//TODO?
          vtEnumeration: V.Value := TImmValue.CreateEnumItem(V.UserType, Hardware.ReadMemoryByte(Addr));
          vtSubRange:
            case GetTypeSize(V.UserType) of
              1: V.Value := TImmValue.CreateTyped(V.UserType, Hardware.ReadMemoryByte(Addr));
              2: V.Value := TImmValue.CreateTyped(V.UserType, Hardware.ReadMemoryWord(Addr));
            else
              raise Exception.Create('Invalid SubRange size');
            end;
          vtArray, vtList: V.Value := TImmValue.CreateString('TODO: Read List data');
          vtFunction: V.Value := TImmValue.CreateString('TODO: Function types');
        end;
      end;
    else
      Assert(False);
    end;
  end;
end;

end.

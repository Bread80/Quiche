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
uses mHardware,
  //Inlcude hardware so it is registered
  mMemory,
  Def.QTypes, Def.Variables;

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
    case V.Storage of
      vsStatic:
        case V.VarType of
          vtByte, vtChar, vtBoolean: V.ValueInt := ReadByte(V.GetAsmName);
          vtWord, vtPointer: V.ValueInt := ReadWord(V.GetAsmName);
          vtInt8: V.ValueInt := ReadInt8(V.GetAsmName);
          vtInteger: V.ValueInt := ReadInteger(V.GetAsmName);
          vtReal, vtFlag, vtTypeDef, vtString, vtUnknown: ;//TODO?
        else
          Assert(False);
        end;
      vsStack:
      begin
        Addr := (IX + V.Offset) and $ffff;
        case V.VarType of
          vtByte, vtChar, vtBoolean: V.ValueInt := Hardware.ReadMemoryByte(Addr);
          vtWord, vtPointer: V.ValueInt := Hardware.ReadMemoryWord(Addr);
          vtInt8: V.ValueInt := ReadMemoryInt8(Addr);
          vtInteger: V.ValueInt := ReadMemoryInteger(Addr);
          vtReal, vtFlag, vtTypeDef, vtString, vtUnknown: ;//TODO?
        end;
      end;
    else
      Assert(False);
    end;
  end;
end;

end.

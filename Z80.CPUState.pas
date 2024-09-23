unit Z80.CPUState;

interface
uses SysUtils,
  Def.IL, Def.Variables,
  Z80.CPU;

type TRegStateKind =
  (rskUnknown,
  rskLiteral,     //A constant value
  rskVarValue,    //The value of a variable
  rskVarValueLow,  //Low byte of a 16-bit variable value
  rskVarValueHigh,  //High byte of a 16-bit variable value
  rskVarAddr);    //The address of a variable

//Sets all registers to rskUnknown and IsModified to False
procedure RegStateInitialise;
//Clears IsModified for all registers. Can be called after setting initial state (eg)
//at the start of a function call
procedure RegStateClearModified;

//Does NOT affect the state of any other register, including the given register as
//part of, or separate from, a pair.
//Also sets IsModified for Reg
//rFlags refers to flags other than CF, but (currently) includes rZF
procedure RegStateSetUnknown(Reg: TCPUReg);

//As above but takes multiple registers
procedure RegStateSetUnknowns(Regs: TCPURegSet);

//Also sets the state of the related register pair, or separate register
//Does NOT affect the other half of a register pair
procedure RegStateSetLiteral(Reg: TCPUReg;AValue: Integer);

//Also sets the state of the related register pair, or separate register
//Does NOT affect the other half of a register pair
//AKind must be VarValue or VarAddr
//Can also be called with AKind of rskUnknown, which makes some routines easier to code.
procedure RegStateSetVariable(Reg: TCPUReg;AVariable: PVariable;AVersion: Integer;AKind: TRegStateKind);

//Copy the value in FromReg to ToReg
procedure RegStateCopy(ToReg, FromReg: TCPUReg);

//Swap the values in HL and DE
procedure RegStateEXHLDE;

//If the variable (value) is currently stored in a register return the register
//AKind must be VarValue or VarAddr
function RegStateFindVariable(AVariable: PVariable;AVersion: Integer;AKind: TRegStateKind): TCPUReg;
function RegStateFindVariable16(AVariable: PVariable;AVersion: Integer;AKind: TRegStateKind): TCPUReg;
function RegStateFindVariable8(AVariable: PVariable;AVersion: Integer;AKind: TRegStateKind): TCPUReg;

//Returns True if the given variable already contains the given variable version
function RegStateEqualsVariable(Reg: TCPUReg;AVariable: PVariable;AVersion: Integer;AKind: TRegStateKind): Boolean;

//If the address (@ operator) of a variable is already loaded in registers returns
//the register, otherwise returns rNone
function RegStateFindAddrOf(AVariable: PVariable): TCPUReg;

//If any 8-bit register contains the required value, returns the name of that register,
//otherwise returns rNone
function RegStateFindLiteral8(AValue: Integer): TCPUReg;

function RegStateFindLiteral16(AValue: Integer): TCPUReg;

//Find and return a suitable 8-bit register for use.
//Regs specifies a set of registers which MUST NOT be returned.
function RegStateAllocReg8(Avoid: TCPURegSet): TCPUReg;

//Returns True if the the named register contains a literal value
function RegStateIsLiteral(Reg: TCPUReg): Boolean;

//If the register contains a literal returns it, otherwise raises an exception
function RegStateGetLiteral(Reg: TCPUReg): Integer;

//Does the given register contain the given literal value?
function RegStateEqualsLiteral(Reg: TCPUReg;Value: Integer): Boolean;



//For testing

//State string is a list of reg:value pairs of format:
//<r>:[<modified-test][<value>]
//Either <modified-test> or <value> can be omitted but not both

//R is any reg or flag
//Modified-test is:
//  ! - reg is modified
//  _ - reg is not modified
//An unmodified register doesn't have and 'unknown' state. This can be used to test
//that a routine doesn't corrupt a register which contains useful data.

//Value can be any of:
//  Literal values can be in decimal or $ prefixed hex
//  A variable-type sigil, name and version-number if the format:
//<sigil><variable-name>#<variable-version>
//Available sigils are:
//  '%'   Variable value
//  '\'   Lo byte of value
//  '/'   Hi byte of value
//  '@'   Address of the variable
//The variable name must be fully qualified

//NOTE: State strings passed as input are currently intollerant of surrounding whitespace.

//Initialises the CPU state to that given in the string
procedure CPUStringToState(State: String);

//Compares the given state to the current state. For any values specified in State
//which do not match current state, details will be returned. Only tests the state
//of items specified in State.
function CPUStateCompare(State: String): String;

//Returns the current state, ignoring registers which are unknown and unmodified
function CPUStateToString: String;

implementation

type
  TRegState = record
    IsModified: Boolean;
    procedure FromString(Reg: TCPUReg;State: String);
    function Compare(Reg: TCPUReg;State:String): Boolean;
    function ToString(Reg: TCPUReg): String;

    case Kind: TRegStateKind of
      rskUnknown: ();
      rskLiteral: (
        Literal: Integer;
        );
      rskVarValue, rskVarAddr: (
        Variable: PVariable;
        Version: Integer;
        );
  end;

//Note that not all entries here are used
type TCPUState = array[low(TCPUReg)..high(TCPUReg)] of TRegState;

var CPUState: TCPUState;

procedure RegStateInitialise;
var R: TCPUReg;
begin
  for R := low(TCPUReg) to high(TCPUReg) do
  begin
    CPUState[R].Kind := rskUnknown;
    CPUState[R].IsModified := False;
  end;
end;

procedure RegStateClearModified;
var R: TCPUReg;
begin
  for R := low(TCPUReg) to high(TCPUReg) do
    CPUState[R].IsModified := False;
end;

procedure RegStateSetUnknown(Reg: TCPUReg);
begin
  CPUState[Reg].Kind := rskUnknown;
  CPUState[Reg].IsModified := True;

  if Reg in CPUReg8Bit then
  begin
    CPUState[CPUReg8ToPair[Reg]].Kind := rskUnknown;
    CPUState[CPUReg8ToPair[Reg]].IsModified := True;
  end
  else if Reg in CPURegPairs then
  begin
    CPUState[CPURegPairToLow[Reg]].Kind := rskUnknown;
    CPUState[CPURegPairToLow[Reg]].IsModified := True;
    CPUState[CPURegPairToHigh[Reg]].Kind := rskUnknown;
    CPUState[CPURegPairToHigh[Reg]].IsModified := True;
  end
  else if Reg = rFlags then
  begin
    CPUState[rZF].Kind := rskUnknown;
    CPUState[rZF].IsModified := True;
    CPUSTate[rFlags].Kind := rskUnknown;
    CPUState[rFlags].IsModified := True;
  end;
end;

procedure RegStateSetUnknowns(Regs: TCPURegSet);
var R: TCPUReg;
begin
  for R in Regs do
    RegStateSetUnknown(R);
end;

procedure RegStateSetLiteral(Reg: TCPUReg;AValue: Integer);
begin
  case Reg of
    rHL, rDE, rBC:
    begin //Pairs
      CPUState[Reg].Kind := rskLiteral;
      CPUState[Reg].Literal := AValue;
      CPUState[Reg].IsModified := True;
      CPUState[CPURegPairToHigh[Reg]].Kind := rskLiteral;
      CPUState[CPURegPairToHigh[Reg]].Literal := (AValue shr 8) and $ff;
      CPUState[CPURegPairToHigh[Reg]].IsModified := True;
      CPUState[CPURegPairToLow[Reg]].Kind := rskLiteral;
      CPUState[CPURegPairToLow[Reg]].Literal := AValue and $ff;
      CPUState[CPURegPairToLow[Reg]].IsModified := True;
    end;
    rIX, rIY:
    begin //Index registers
      CPUState[Reg].Kind := rskLiteral;
      CPUState[Reg].Literal := AValue;
      CPUState[Reg].IsModified := True;
    end;
    rB, rD, rH:
    begin //High bytes
      if (CPUState[CPUReg8ToPair[Reg]].Kind = rskLiteral) then
        CPUState[CPUReg8ToPair[Reg]].Literal :=
          CPUState[CPUReg8ToPair[Reg]].Literal and $ff or ((AValue and $ff) shl 8)
      else if (CPUState[CPURegHighToLow[Reg]].Kind = rskLiteral) then
      begin
        CPUState[CPUReg8ToPair[Reg]].Kind := rskLiteral;
        CPUState[CPUReg8ToPair[Reg]].Literal :=
          CPUState[CPURegHighToLow[Reg]].Literal and $ff or ((AValue and $ff) shl 8)
      end
      else
        CPUState[CPUReg8ToPair[Reg]].Kind := rskUnknown;
      CPUState[CPUReg8ToPair[Reg]].IsModified := True;

      CPUState[Reg].Kind := rskLiteral;
      CPUState[Reg].Literal := AValue;
      CPUState[Reg].IsModified := True;
    end;
    rC, rE, rL:
    begin //Low bytes
      if CPUState[CPUReg8ToPair[Reg]].Kind = rskLiteral then
        CPUState[CPUReg8ToPair[Reg]].Literal :=
          CPUState[CPUReg8ToPair[Reg]].Literal and $ff00 or (AValue and $ff)
      else if CPUState[CPURegLowToHigh[Reg]].Kind = rskLiteral then
      begin
        CPUState[CPUReg8ToPair[Reg]].Kind := rskLiteral;
        CPUState[CPUReg8ToPair[Reg]].Literal :=
          CPUState[CPURegLowToHigh[Reg]].Literal and $ff00 or (AValue and $ff)
      end
      else
        CPUState[CPUReg8ToPair[Reg]].Kind := rskUnknown;
      CPUState[CPUReg8ToPair[Reg]].IsModified := True;

      CPUState[Reg].Kind := rskLiteral;
      CPUState[Reg].Literal := AValue;
      CPUState[Reg].IsModified := True;
    end;
    rA:
    begin //Not part of a pair
      CPUState[Reg].Kind := rskLiteral;
      CPUState[Reg].Literal := AValue;
      CPUState[Reg].IsModified := True;
    end;
    rCF, rZF:
    begin //Not part of a pair
      CPUState[Reg].Kind := rskLiteral;
      if AValue <> 0 then
        AValue := -1;
      CPUState[Reg].Literal := AValue;
      CPUState[Reg].IsModified := True;
    end;
    else
    Assert(False);
  end;
end;

procedure RegStateSetVariable(Reg: TCPUReg;AVariable: PVariable;AVersion: Integer;AKind: TRegStateKind);
var R: TCPUReg;
begin
  if AKind = rskUnknown then
  begin
    RegStateSetUnknown(Reg);
    EXIT;
  end;

  Assert(AKind in [rskVarValue, rskVarValueLow, rskVarValueHigh, rskVarAddr]);

  case Reg of
    rHL, rDE, rBC:
    begin //Pairs
      CPUState[Reg].Kind := AKind;
      CPUState[Reg].Variable := AVariable;
      CPUState[Reg].Version := AVersion;
      CPUState[Reg].IsModified := True;

      if AKind = rskVarValue then
      begin //Update state for 8-bit values (Beware Recursive loops!)
        RegStateSetVariable(CPURegPairToLow[Reg], AVariable, AVersion, rskVarValueLow);
        RegStateSetVariable(CPURegPairToHigh[Reg], AVariable, AVersion, rskVarValueHigh);
      end
      else
      begin
        CPUState[CPURegPairToHigh[Reg]].Kind := rskUnknown;
        CPUState[CPURegPairToHigh[Reg]].IsModified := True;
        CPUState[CPURegPairToLow[Reg]].Kind := rskUnknown;
        CPUState[CPURegPairToLow[Reg]].IsModified := True;
      end;
    end;
    rIX, rIY:
    begin //Index registers - we don't handle separate low/high bytes
      CPUState[Reg].Kind := AKind;
      CPUState[Reg].Variable := AVariable;
      CPUState[Reg].Version := AVersion;
      CPUState[Reg].IsModified := True;
    end;
    rA, rB, rC, rD, rE, rH, rL:
    begin //Singles and flags
      CPUState[Reg].Kind := AKind;
      CPUState[Reg].Variable := AVariable;
      CPUState[Reg].Version := AVersion;
      CPUState[Reg].IsModified := True;

      //If the reg is half of a pair, is the pair now a full var value?
      if (Reg in [rC, rE, rL]) and (Akind = rskVarValueLow) then
      begin //Low to high part
        R := CPURegLowToHigh[Reg];
        if (CPUState[R].Kind = rskVarValueHigh) and
          (CPUState[R].Variable = AVariable) and (CPUState[R].Version = AVersion) then
        begin
          R := CPUReg8ToPair[Reg];
          CPUState[R].Kind := rskVarValue;
          CPUState[R].Variable := AVariable;
          CPUState[R].Version := AVersion;
          CPUState[R].IsModified := True;
          EXIT;
        end;
      end
      else if (Reg in [rB, rD, rH]) and (Akind = rskVarValueHigh) then
      begin
        R := CPURegHighToLow[Reg];
        if (CPUState[R].Kind = rskVarValueLow) and
          (CPUState[R].Variable = AVariable) and (CPUState[R].Version = AVersion) then
        begin
          R := CPUReg8ToPair[Reg];
          CPUState[R].Kind := rskVarValue;
          CPUState[R].Variable := AVariable;
          CPUState[R].Version := AVersion;
          CPUState[R].IsModified := True;
          EXIT;
        end;
      end;

      //Otherwise pair is now junk
      CPUState[CPUReg8ToPair[Reg]].Kind := rskUnknown;
      CPUState[CPUReg8ToPair[Reg]].IsModified := True;
    end;
    rCF, rZF:  //Not sure if this is actually meaninful
    begin
      CPUState[Reg].Kind := AKind;
      CPUState[Reg].Variable := AVariable;
      CPUState[Reg].Version := AVersion;
      CPUState[Reg].IsModified := True;
    end;
    else
    Assert(False);
  end;
end;

procedure RegStateCopy(ToReg, FromReg: TCPUReg);
begin
  Assert((ToReg in CPUReg8Bit) = (FromReg in CPUReg8Bit), 'Can''t copy to reg of diffent size');
  Assert((ToReg in CPUReg16Bit) = (FromReg in CPUReg16Bit), 'Can''t copy to reg of diffent size');
  Assert((ToReg in CPURegPairs) = (FromReg in CPURegPairs), 'Can''t copy to reg of diffent size');

  case CPUState[FromReg].Kind of
    rskUnknown:
    begin
      CPUState[ToReg].Kind := rskUnknown;
      CPUState[ToReg].IsModified := True;
    end;
    rskLiteral: RegStateSetLiteral(ToReg, CPUState[FromReg].Literal);
    rskVarValue, rskVarValueHigh, rskVarValueLow, rskVarAddr:
      RegStateSetVariable(ToReg, CPUState[FromReg].Variable,
        CPUState[FromReg].Version, CPUState[FromReg].Kind);
  else
    Assert(False);
  end;
end;

procedure RegStateEXHLDE;
var Temp: TRegState;
begin
  Temp := CPUState[rHL];
  CPUState[rHL] := CPUState[rDE];
  CPUState[rDE] := Temp;

  Temp := CPUState[rH];
  CPUState[rH] := CPUState[rD];
  CPUState[rD] := Temp;

  Temp := CPUState[rL];
  CPUState[rL] := CPUState[rE];
  CPUState[rE] := Temp;

  CPUState[rHL].IsModified := True;
  CPUState[rDE].IsModified := True;
  CPUState[rH].IsModified := True;
  CPUState[rL].IsModified := True;
  CPUState[rD].IsModified := True;
  CPUState[rE].IsModified := True;
end;

function RegStateFindLiteral8(AValue: Integer): TCPUReg;
var R: TCPUReg;
begin
  for R := rA to rL do
    if CPUState[R].Kind = rskLiteral then
      if CPUState[R].Literal = AValue and $ff then
        EXIT(R);

  Result := rNone;
end;

function RegStateFindLiteral16(AValue: Integer): TCPUReg;
var R: TCPUReg;
begin
  for R := rHL to rBC do
    if CPUState[R].Kind = rskLiteral then
      if CPUState[R].Literal = AValue and $ffff then
        EXIT(R);

  Result := rNone;
end;

function RegStateFindVariable(AVariable: PVariable;AVersion: Integer;AKind: TRegStateKind): TCPUReg;
var R: TCPUReg;
  State: TRegState;
begin
  Assert(AKind in [rskVarValue, rskVarAddr]);

  for R := low(TCPUReg) to high(TCPUReg) do
  begin
    State := CPUState[R];
    if (State.Kind = AKind) and (State.Variable = AVariable) and (State.Version = AVersion) then
      EXIT(R);
  end;

  Result := rNone;
end;

function RegStateFindVariable16(AVariable: PVariable;AVersion: Integer;AKind: TRegStateKind): TCPUReg;
var R: TCPUReg;
  State: TRegState;
begin
  Assert(AKind in [rskVarValue, rskVarAddr]);

  for R := rHL to rIY do
  begin
    State := CPUState[R];
    if (State.Kind = AKind) and (State.Variable = AVariable) and (State.Version = AVersion) then
      EXIT(R);
  end;

  Result := rNone;
end;

function RegStateFindVariable8(AVariable: PVariable;AVersion: Integer;AKind: TRegStateKind): TCPUReg;
var R: TCPUReg;
  State: TRegState;
begin
  Assert(AKind in [rskVarValue, rskVarValueLow, rskVarValueHigh]);

  for R := rA to rL do
  begin
    State := CPUState[R];
    if (State.Kind = AKind) and (State.Variable = AVariable) and (State.Version = AVersion) then
      EXIT(R);
  end;

  Result := rNone;
end;

function RegStateEqualsVariable(Reg: TCPUReg;AVariable: PVariable;AVersion: Integer;
  AKind: TRegStateKind): Boolean;
begin
  Result := (CPUState[Reg].Kind = AKind) and (CPUState[Reg].Variable = AVariable) and
    (CPUState[Reg].Version = AVersion);
end;

function RegStateFindAddrOf(AVariable: PVariable): TCPUReg;
var R: TCPUReg;
  State: TRegState;
begin
  for R := rHL to rIY do
  begin
    State := CPUState[R];
    if (State.Kind = rskVarAddr) and (State.Variable = AVariable) then
      EXIT(R);
  end;

  Result := rNone;
end;

function RegStateAllocReg8(Avoid: TCPURegSet): TCPUReg;
var R: TCPUReg;
begin
  //Tries to return a unused register (rskUnknown), if not returns the
  //first register not in Regs
  Result := rNone;
  for R := rA to rL do
    if not (R in Avoid) then
      if CPUState[R].Kind = rskUnknown then
        EXIT(R)
      else if Result = rNone then
        Result := R;

  Assert(False, 'Unable to allocate a register');
end;

function RegStateIsLiteral(Reg: TCPUReg): Boolean;
begin
  Result := CPUState[Reg].Kind = rskLiteral;
end;

function RegStateGetLiteral(Reg: TCPUReg): Integer;
begin
  Assert(RegStateIsLiteral(Reg));
  Result := CPUState[Reg].Literal;
end;

function RegStateEqualsLiteral(Reg: TCPUReg;Value: Integer): Boolean;
begin
  if Reg in CPURegFlags then
    Result := (CPUState[Reg].Kind = rskLiteral) and ((CPUState[Reg].Literal = 0) = (Value = 0))
  else
    Result := (CPUState[Reg].Kind = rskLiteral) and (CPUState[Reg].Literal = Value);
end;

{ TRegState }

function TRegState.Compare(Reg: TCPUReg;State: String): Boolean;

  function VarCompare(AKind: TRegStateKind;State: String): Boolean;
  var VarData: TArray<String>;
    LVersion: Integer;
  begin
    if AKind <> Kind then
      EXIT(False);

    VarData := State.Substring(1).Split(['#']);
    Assert(Length(VarData) = 2, 'VarData state format: <qualified-varname>#<version>');
    if Variable = nil then
      EXIT(False);
    if Variable.GetAsmName <> VarData[0] then
      EXIT(False);
    Assert(TryStrToInt(VarData[1], LVersion), 'VarData state format: <qualified-varname>#<version>');
    Result := LVersion = Version;
  end;

var Ch: Char;
  Value: Integer;
begin
  Assert(State <> '');
  Result := False;

  Ch := State.Chars[0];
  if CharInSet(Ch, ['!','_']) then
  begin
    Result := IsModified = (Ch = '!');
    if not Result then
      EXIT;

    State := State.Substring(1);
    if State = '' then
      EXIT;
    Ch := State.Chars[0];
  end;

  case Ch of
    '?': EXIT(Kind = rskUnknown);
    '%': //Variable value
      EXIT(VarCompare(rskVarValue, State));
    '\': //Lo byte of var value
      EXIT(VarCompare(rskVarValueLow, State));
    '/': //Hi byte or var value
      EXIT(VarCompare(rskVarValueHigh, State));
    '@': //Var address
      EXIT(VarCompare(rskVarAddr, State));
  else  //Literal value
    if Kind <> rskLiteral then
      EXIT(False);
    Assert(TryStrToInt(State, Value));
    if Reg = rCF then
      EXIT((Literal <> 0) = (Value <> 0))
    else if Reg = rFlags then
      Assert(False, 'Flags can''t have a value')
    else
      EXIT(Literal = Value);
  end;
end;

procedure TRegState.FromString(Reg: TCPUReg;State: String);

  procedure FromVariable(AKind: TRegStateKind;State: String);
  var VarData: TArray<String>;
    LVersion: Integer;
    V: PVariable;
  begin
    VarData := State.Substring(1).Split(['#']);
    Assert(Length(VarData) = 2, 'VarData state format: <qualified-varname>#<version>');
    V := VarFindByNameAllScopes(VarData[0]);
    Assert(V <> nil);
    Assert(TryStrToInt(VarData[1], LVersion), 'VarData state format: <qualified-varname>#<version>');
    RegStateSetVariable(Reg, V, LVersion, AKind);
  end;

var Ch: Char;
  Value: Integer;
begin
  Assert(State <> '');

  Ch := State.Chars[0];
  if CharInSet(Ch, ['!','_']) then
  begin
    IsModified := Ch = '!';
    State := State.Substring(1);

    if State = '' then
      EXIT;
    Ch := State.Chars[0];
  end;

  case Ch of
    '?':
    begin
      Kind := rskUnknown;
      EXIT;
    end;
    '%': //Variable value
      FromVariable(rskVarValue, State);
    '\': //Lo byte of var value
      FromVariable(rskVarValueLow, State);
    '/': //Hi byte or var value
      FromVariable(rskVarValueHigh, State);
    '@': //Var address
      FromVariable(rskVarAddr, State);
  else  //Literal value
    Assert(TryStrToInt(State, Value));
    RegStateSetLiteral(Reg, Value);
  end;
end;

function TRegState.ToString(Reg: TCPUReg): String;
const KindStrings: array[low(TRegStateKind)..high(TRegStateKind)] of String =
  ('?', //Unknown
  '',   //Constant
  '%',  //Variable value
  '\',  //Lo byte of value
  '/',  //Hi byte of value
  '@'); //Variable address
begin
  Result := KindStrings[Kind];
  case Kind of
    rskUnknown: ;
    rskLiteral:
      if Reg in CPUReg8Bit then
        Result := Result + '$' + IntToHex(Literal, 2)
      else if Reg in CPUReg16Bit then
        Result := Result + '$' + IntToHex(Literal, 4)
      else if Reg in CPURegFlags then
        Result := Result + '$' + Literal.ToString
      else
        Result := Result + '<Unknown reg>';
    rskVarValue, rskVarAddr, rskVarValueHigh, rskVarValueLow:
      Result := Result + Variable.GetAsmName + '#' + Version.ToString;
  else
    Assert(False);
  end;
end;

function CPUStateToString: String;
var R: TCPUReg;
begin
  Result := 'CPU: ';
  for R := succ(rNone) to high(TCPUReg) do
    if CPUState[R].IsModified or (CPUState[R].Kind <> rskUnknown) then
      Result := Result + CPURegStrings[R].ToUpper + ':' + CPUState[R].ToString(R) + ' ';
end;

procedure CPUStringToState(State: String);
var Pairs: TArray<String>;
  Pair: String;
  Data: TArray<String>;
  Reg: TCPUReg;
begin
  Pairs := State.Split([' ']);
  for Pair in Pairs do
  begin
    Data := Pair.Split([':']);
    Assert(Length(Data) = 2);
    Reg := IdentToCPUReg(Data[0]);
    CPUState[Reg].FromString(Reg, Data[1]);
  end;
end;

function CPUStateCompare(State: String): String;
var Pairs: TArray<String>;
  Pair: String;
  Data: TArray<String>;
  Reg: TCPUReg;
begin
  Result := '';
  Pairs := State.Split([' ']);
  for Pair in Pairs do
  begin
    Data := Pair.Split([':']);
    Assert(Length(Data) = 2);
    if CompareText(Data[0], 'Flags') = 0 then
      Reg := rFlags
    else
      Reg := IdentToCPUReg(Data[0]);

    if not CPUState[Reg].Compare(Reg, Data[1]) then
      Result := Result + 'Fail on: ' + Pair + ' ';
  end;
end;

end.

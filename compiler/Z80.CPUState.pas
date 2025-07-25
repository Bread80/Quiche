unit Z80.CPUState;

interface
uses SysUtils,
  Def.Variables,
  Z80.Hardware;

type TRegStateKind =
  (rskUnknown,
  rskLiteral,     //A constant value
  rskVarValue,    //The value of a variable
  rskVarValueInverted,  //An inverted boolean value
  rskVarValueLow,  //Low byte of a 16-bit variable value
  rskVarValueHigh,  //High byte of a 16-bit variable value
  rskVarAddr,     //The address of a variable
  rskLabel);      //A label (for static data etc)

type
  TRegState = record
    IsModified: Boolean;
    StrData: String;  //For rskLabel
    function Compare(const State: TRegState): Boolean;

    procedure FromString(Reg: TCPUReg;State: String);
    function CompareString(Reg: TCPUReg;State:String): Boolean;
    function ToString(Reg: TCPUReg;Compact: Boolean): String;

    case Kind: TRegStateKind of
      rskUnknown: ();
      rskLiteral: (
        Literal: Integer;
        );
      rskVarValue, rskVarValueInverted,
        rskVarValueHigh, rskVarValueLow,
        rskVarAddr: (
        Variable: PVariable;
        Version: Integer;
        );
      rskLabel: ();  //Data stored in StrData
  end;

//Note that not all entries here are used
type
  PCPUState = ^TCPUState;
  TCPUState = array[low(TCPUReg)..high(TCPUReg)] of TRegState;



//Sets all registers to rskUnknown and IsModified to False
procedure RegStateInitialise;

//Sets the current state to that in State
procedure RegStateSet(const State: TCPUState);
//Copies the current state to State
procedure RegStateGet(var State: TCPUState);

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

//Reg must be 16-bit. Separate registers (if appropriate) will be set to unknown
procedure RegStateSetLabel(Reg: TCPUReg;const ALabel: String);

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

//Does the register contain the value of the given label?
//Reg must be 16-bit
function RegStateEqualsLabel(Reg: TCPUReg;const ALabel: String): Boolean;

//Compare all meaningful registers
function RegStateCompareAll(const State: TCPUState): Boolean;

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
//  '~'   An inverted boolean variable value
//  '\'   Lo byte of value
//  '/'   Hi byte of value
//  '@'   Address of the variable
//  '*'   A label
//The variable name must be fully qualified

//NOTE: State strings passed as input are currently intollerant of surrounding whitespace.

//Initialises the CPU state to that given in the string
procedure CPUStringToState(State: String);

//Compares the given state to the current state. For any values specified in State
//which do not match current state, details will be returned. Only tests the state
//of items specified in State.
function CPUStateCompare(State: String): String;

//Returns the current state, ignoring registers which are unknown and unmodified
function CPUStateToString(Compact: Boolean = False): String;

implementation

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

procedure RegStateSet(const State: TCPUState);
begin
  CPUState := State;
end;

procedure RegStateGet(var State: TCPUState);
begin
  State := CPUState;
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
  Flag: TCPUReg;
begin
  if AKind = rskUnknown then
  begin
    RegStateSetUnknown(Reg);
    EXIT;
  end;

  Assert(AKind in [rskVarValue, rskVarValueInverted, rskVarValueLow, rskVarValueHigh, rskVarAddr]);

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
    rCF, rZF, rZFA:  //Not sure if this is actually meaningful
    begin
      Assert(AKind = rskVarValue);
      if Reg = rZFA then
        Flag := rZF
      else
        Flag := Reg;
      CPUState[Flag].Kind := AKind;
      CPUState[Flag].Variable := AVariable;
      CPUState[Flag].Version := AVersion;
      CPUState[Flag].IsModified := True;
      if Reg = rZFA then
        RegStateSetVariable(rA, AVariable, AVersion, rskVarValue);
    end;
    rNCF, rNZF, rNZFA:
    begin
      Assert(AKind = rskVarValue);
      case Reg of
        rNCF: Flag := rCF;
        rNZF, rNZFA: Flag := rZF;
      else
        Flag := Reg;
      end;
      CPUState[Flag].Kind := rskVarValueInverted;
      CPUState[Flag].Variable := AVariable;
      CPUState[Flag].Version := AVersion;
      CPUState[Flag].IsModified := True;
      if Reg = rNZFA then
        RegStateSetVariable(rA, AVariable, AVersion, rskVarValueInverted);
    end;
    rCPLA:
    begin
      Assert(AKind = rskVarValue);
      RegStateSetVariable(rA, AVariable, AVersion, rskVarValueInverted);
    end;
    else
    Assert(False);
  end;
end;

procedure RegStateSetLabel(Reg: TCPUReg;const ALabel: String);
begin
  Assert(Reg in CPUReg16Bit);
  if Reg in CPURegPairs then
  begin
    CPUState[CPURegPairToHigh[Reg]].Kind := rskUnknown;
    CPUState[CPURegPairToHigh[Reg]].IsModified := True;
    CPUState[CPURegPairToLow[Reg]].Kind := rskUnknown;
    CPUState[CPURegPairToLow[Reg]].IsModified := True;
  end;

  CPUState[Reg].Kind := rskLabel;
  CPUState[Reg].StrData := ALabel;
  CPUState[Reg].IsModified := True;
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
    rskVarValue, rskVarValueInverted, rskVarValueHigh, rskVarValueLow, rskVarAddr:
      RegStateSetVariable(ToReg, CPUState[FromReg].Variable,
        CPUState[FromReg].Version, CPUState[FromReg].Kind);
    rskLabel: RegStateSetLabel(ToReg, CPUState[FromReg].StrData);
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
  Assert(AKind in [rskVarValue, rskVarValueInverted, rskVarAddr]);

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
  Assert(AKind in [rskVarValue, rskVarValueInverted, rskVarValueLow, rskVarValueHigh]);

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
  if Reg in [rNZF, rNCF] then
  begin
    case AKind of
      rskVarValue: AKind := rskVarValueInverted;
      rskVarValueInverted: AKind := rskVarValue;
    else
      Assert(False);
    end;

    case Reg of
      rNZF: Reg := rZF;
      rNCF: Reg := rCF;
    else
      Assert(False);
    end;
  end;

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

function RegStateEqualsLabel(Reg: TCPUReg;const ALabel: String): Boolean;
begin
  Assert(Reg in CPUReg16Bit);

  Result := (CPUState[Reg].Kind = rskLabel) and (CPUState[Reg].StrData = ALabel);
end;

function RegStateCompareAll(const State: TCPUState): Boolean;
var Reg: TCPUReg;
begin
  for Reg in CPURegsAll do
    if not CPUState[Reg].Compare(State[Reg]) then
      EXIT(False);
  Result := True;
end;

{ TRegState }

function TRegState.Compare(const State: TRegState): Boolean;
begin
  if Kind <> State.Kind then
    EXIT(False);
  if IsModified <> State.IsModified then
    EXIT(False);
  case Kind of
    rskUnknown: EXIT(True);
    rskLiteral: EXIT(Literal = State.Literal);
    rskVarValue, rskVarValueInverted,
    rskVarValueHigh, rskVarValueLow,
    rskVarAddr: EXIT((Variable = State.Variable) and (Version = State.Version));
    rskLabel:   EXIT(StrData = State.StrData);
  else
    Assert(False);
  end;
  Result := False;
end;

function TRegState.CompareString(Reg: TCPUReg;State: String): Boolean;

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
    '~': //Variable value
      EXIT(VarCompare(rskVarValueInverted, State));
    '\': //Lo byte of var value
      EXIT(VarCompare(rskVarValueLow, State));
    '/': //Hi byte or var value
      EXIT(VarCompare(rskVarValueHigh, State));
    '@': //Var address
      EXIT(VarCompare(rskVarAddr, State));
    '*':  //Label name
      EXIT((Kind = rskLabel) and (StrData = State));
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
    '~': //Inverted boolean value
      FromVariable(rskVarValueInverted, State);
    '\': //Lo byte of var value
      FromVariable(rskVarValueLow, State);
    '/': //Hi byte or var value
      FromVariable(rskVarValueHigh, State);
    '@': //Var address
      FromVariable(rskVarAddr, State);
    '*':  //Label
      RegStateSetLabel(Reg, State);
  else  //Literal value
    Assert(TryStrToInt(State, Value));
    RegStateSetLiteral(Reg, Value);
  end;
end;

function TRegState.ToString(Reg: TCPUReg;Compact: Boolean): String;
const KindStrings: array[low(TRegStateKind)..high(TRegStateKind)] of String =
  ('?', //Unknown
  '',   //Constant
  '%',  //Variable value
  '~',  //Inverted boolean value
  '\',  //Lo byte of value
  '/',  //Hi byte of value
  '@',  //Variable address
  '*'); //Label
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
    rskVarValue, rskVarValueInverted, rskVarAddr, rskVarValueHigh, rskVarValueLow:
    begin
      if Compact then
        Result := Result + Variable.Name + '_'
      else
        Result := Result + Variable.GetAsmName + '#';
      Result := Result + Version.ToString;
    end;
    rskLabel: Result := Result + '*' + StrData;
  else
    Assert(False);
  end;
end;

function CPUStateToString(Compact: Boolean = False): String;
var R: TCPUReg;
begin
  Result := 'CPU: ';
  for R := succ(rNone) to high(TCPUReg) do
    if CPUState[R].IsModified or (CPUState[R].Kind <> rskUnknown) then
      if not Compact or
        (Compact and (CPUReg8ToPair[R] = rNone) or
          //Filter out seperate halves of register pairs
         ((CPUReg8ToPair[R] <> rNone) and (CPUState[CPUReg8ToPair[R]].Kind = rskUnknown))) then
        Result := Result + CPURegStrings[R].ToUpper + ':' + CPUState[R].ToString(R, Compact) + ' ';
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

    if not CPUState[Reg].CompareString(Reg, Data[1]) then
      Result := Result + 'Fail on: ' + Pair + ' ';
  end;
end;

end.

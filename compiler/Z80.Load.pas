{
Load variables values into registers.
Values can be loaded from memory but if they are aloready in a register they
may be 'scavenged' from that register. If the value is alrady in a register no
code may be generated

All functions within this unit update the data in Z80.CPUState to reflect any code
generated.
}
unit Z80.Load;

interface
uses Def.IL, Def.VarTypes, Def.Consts, Def.UserTypes,
  Lib.Data,
  Z80.Hardware, Z80.GenProcs;

//Load any literals required for the fragment
procedure LoadEntryLiterals(const Meta: TCodeProcMeta);

//Generates the code to load a single parameter
//If ToType is vtUnknown no range checking will be performed.
//otherwise range checking will be performed depending on the Flags in Param,
//and whether such checking is required (ie. no checking is required in FromType
//(as given in Param) is the same as ToType or for Hi() or Lo() operations.
//Hi() or Lo() can be specified in PrimFlags.
//Value will be extended (signed or unsigned) from 8-bits to 16-bits or shrunk
//from 16-bits to 8-bits as required for the FromType (in Param) and ToType.
//The Options argument can specify certain registers or CPU flags which must be
//left untouched by GenLoadParam. This is mainly for 'sanity checking' that
//the caller has made the correct assumptions about this functions requirements.
//Requirements are basically:
//Signed extending will corrupt the A and flags registers
//Range checking will also (usually - depending on the exact check needed) corrupt
//the A and flags registers.
//Loading an 8-bit static value usually corrupts the A register. (A future version
//may be able to load static values via the HL register instead, but this is not
//currently an option).
procedure GenLoadParam(const Param: TILParam;ToType: PUserType;Options: TMoveOptionSet);

//Derefences a pointer - ie. Param is a pointer, loads the value at the address in Param1
//into the register given in Dest
procedure GenPtrLoad(const Item: TILItem;Options: TMoveOptionSet);

//Derefences a pointer - ie. Param is a pointer, stores the value of Param2 to the
//address in Param1
procedure GenPtrStore(const Item: TILItem;Options: TMoveOptionSet);

//RETIRED - Replaced by the generalised GenRegLoad in Z80.LoadStoreMove
//Loads parameters from memory* into registers as needed.
//Data is loaded from the locations specified by the ILItem parameters into the
//registers specified by ILItem.Param1Alloc and ILItem.Param2Alloc, if any registers
//are specified there.
//* - can also handle parameters which are already in registers
(*procedure LoadBeforePrim(const ILItem: TILItem; Prim: PPrimitive);
*)
implementation
uses Def.Variables,
  Z80.CG, Z80.Validation, Z80.CPUState, Z80.Assembler;

//Timings
//--8-bit loads
//ld A,(nn) ;3/4/13
//ld r,A    ;1/1/ 4
//    Total: 4/5/17

//ld HL,nn  ;3/3/10
//ld r,(HL) ;1/2/ 7
//    Total: 4/5/17

//--16-bit loads
//ld dd,(nn);4/6/20

//(if address already in hl)
//ld r,(HL) ;1/2/ 7
//inc hl    ;1/1/ 6
//ld r,(HL) ;1/2/ 7
//    Total: 3/5/20
//dec hl    ;1/1/ 6 (If preserve HL)
//    Total: 4/6/26


//Load any literals required for the fragment
procedure LoadEntryLiterals(const Meta: TCodeProcMeta);
var I: Integer;
  Options: TMoveOptionSet;
begin
  //Load any required literals
  //Establish what we can corrupt - based on what the primitive will corrupt
  Options := [moPreserveA];
  if not (rCF in Meta.Corrupts) then
    Options := Options + [moPreserveCF];
  if [rZF, rFlags] * Meta.Corrupts <> [rZF, rFlags] then
    Options := Options + [moPreserveOtherFlags];

  if Meta.LLoc = plRegister then
    if rCF in Meta.LRegs then
      Options := Options + [moPreserveCF]
    else if rZF in Meta.LRegs then
      Options := Options + [moPreserveOtherFlags];

  for I := 0 to high(Meta.OnEntry) do
    if Meta.OnEntry[I].Reg = rNone then
      EXIT
    else
      GenLoadRegLiteral(Meta.OnEntry[I].Reg, TImmValue.CreateInteger(Meta.OnEntry[I].Value),
        Options);
end;

//================================VARIABLES

function LoadTypeToKind(LoadType: TLoadParamType): TRegStateKind;
begin
  case LoadType of
    lptNormal: Result := rskVarValue;
    lptLow: Result := rskVarValueLow;
    lptHigh: Result := rskVarValueHigh;
  else
    Assert(False);
    Result := rskVarValue;
  end;
end;

//Basic load of an 8 bit variable into an 8-bit register
//Returns True if we loaded the value via the A register (but Reg <> rA)
function GenVarLoad8(Variable: PVariable;VarVersion: Integer;ToReg: TCPUReg;
  Kind: TRegStateKind;Options: TMoveOptionSet): Boolean;
begin
  Assert(ToReg in CPUReg8Bit);

  Result := False;
  case Variable.AddrMode of
    amStack:
      OpLD(ToReg, rIX, Variable);
    amStatic:
    begin
      OpLD(rA, Variable);
      if ToReg <> rA then
      begin
        Assert(not (moPreserveA in Options));
        OpLD(ToReg, rA);
        RegStateSetVariable(rA, Variable, VarVersion, Kind);
        Result := True;
      end;
    end;
  else
    Assert(False);
  end;
  RegStateSetVariable(ToReg, Variable, VarVersion, Kind);
end;

//Basic load of an 16 bit variable into an 16-bit register
//Returns True if we loaded the value via the A register (but Reg <> rA)
function GenVarLoad16(Variable: PVariable;VarVersion: Integer;ToReg: TCPUReg;
  Kind: TRegStateKind;Options: TMoveOptionSet): Boolean;
begin
  Assert(ToReg in CPURegPairs);

  Result := False;
  case Variable.AddrMode of
    amStack{, amStackPtr}:
      OpLD(ToReg, rIX, Variable);
    amStatic, amStaticRef:
      OpLD(ToReg, Variable);
  else
    Assert(False);
  end;
  RegStateSetVariable(ToReg, Variable, VarVersion, Kind);
end;

//Basic load of the high byte (and the next byte) of a 16 bit variable into an 16-bit register
//Used for loading RPHigh for static vars
//Returns True if we loaded the value via the A register (but Reg <> rA)
function GenVarLoadHighTo16(Variable: PVariable;VarVersion: Integer;ToReg: TCPUReg;
  Kind: TRegStateKind;Options: TMoveOptionSet): Boolean;
begin
  Assert(ToReg in CPURegPairs);

  Result := False;
  case Variable.AddrMode of
    amStack:
      OpLD(ToReg, rIX, Variable, 1);
    amStatic:
      OpLD(ToReg, Variable, 1);
  else
    Assert(False);
  end;
  RegStateSetVariable(ToReg, Variable, VarVersion, Kind);
end;

//Basic load of the high half of a 16 bit variable into an 8-bit register
//Returns True if we loaded the value via the A register (but Reg <> rA)
function GenVarLoad16High(Variable: PVariable;VarVersion: Integer;ToReg: TCPUReg;
  Kind: TRegStateKind;Options: TMoveOptionSet): Boolean;
begin
  Assert(ToReg in CPUReg8Bit);

  Result := False;
  case Variable.AddrMode of
    amStack:
      OpLD(ToReg, rIX, Variable, 1);
    amStatic:
    begin
      OpLD(rA, Variable, 1);
      if ToReg <> rA then
      begin
        Assert(not (moPreserveA in Options));
        OpLD(ToReg, rA);
        RegStateSetVariable(rA, Variable, VarVersion, Kind);
        Result := True;
      end;
    end;
  else
    Assert(False);
  end;
  RegStateSetVariable(ToReg, Variable, VarVersion, Kind);
end;

//Basic load of the low half of a 16 bit variable into an 8-bit register
//Returns True if we loaded the value via the A register (but Reg <> rA)
function GenVarLoad16Low(Variable: PVariable;VarVersion: Integer;ToReg: TCPUReg;
  Kind: TRegStateKind;Options: TMoveOptionSet): Boolean;
begin
  Assert(ToReg in CPUReg8Bit);

  Result := False;
  case Variable.AddrMode of
    amStack:
      OpLD(ToReg, rIX, Variable);
    amStatic:
    begin
      OpLD(rA, Variable);
      if ToReg <> rA then
      begin
        Assert(not (moPreserveA in Options));
        OpLD(ToReg, rA);
        RegStateSetVariable(rA, Variable, VarVersion, Kind);
        Result := True;
      end;
    end;
  else
    Assert(False);
  end;
  RegStateSetVariable(ToReg, Variable, VarVersion, Kind);
end;

//===============================

procedure GenLoadVar8BitToReg8Bit(Variable: PVariable;VarVersion: Integer;ToReg: TCPUReg;
  LoadType: TLoadParamType;ToType: PUserType;RangeCheck: Boolean;Options: TMoveOptionSet);
var ChangeSigned: Boolean;
  Kind: TRegStateKind;
  IsTypecast: Boolean;
  Scavenge: TCPUReg;
  ViaA: Boolean;
begin
  Assert(ToReg in CPUReg8Bit);
  Assert(GetTypeSize(Variable.UserType) = 1);

  Kind := LoadTypeToKind(LoadType);
  Assert(Kind <> rskVarValueHigh);         //Can't fetch high byte of 8 but value!

  IsTypecast := Kind <> rskVarValue;
  if Assigned(ToType) then
    ChangeSigned := IsSignedType(Variable.UserType) <> IsSignedType(ToType)
  else
    ChangeSigned := False;

  //Can we scavenge a value?
  Scavenge := RegStateFindVariable8(Variable, VarVersion, Kind);

  //NOTE: Changing types and scavenging
  //Try and help the scavenger here, but being cautious. We could argue that
  //the variable value is guaranteed to be in the register, since the value
  //here must be within range. BUT if code paths merge (eg if this code is
  //part of conditionally executed code, when the paths merge the allocator
  //and scavenger may not know what we've done.
  if ChangeSigned then
    Kind := rskUnknown;

  if Scavenge <> rNone then
  begin
    if Scavenge <> ToReg then
      GenRegMove(Scavenge, ToReg, False, Options);
    ViaA := Scavenge = rA;
  end
  else
    ViaA := GenVarLoad8(Variable, VarVersion, ToReg, Kind, Options);

  //Range check
  //Don't range check typecasts
  if RangeCheck and not IsTypecast then
  begin
    if ViaA then
      ToReg := rA;
    GenRangeCheck(ToReg, Variable.UserType, ToType, nil, Options);
  end;
end;

procedure GenLoadVar8BitToReg16Bit(Variable: PVariable;VarVersion: Integer;ToReg: TCPUReg;
  LoadType: TLoadParamType;ToType: PUserType;RangeCheck: Boolean;Options: TMoveOptionSet);
var SignedLoss: Boolean;
  Kind: TRegStateKind;
  IsTypecast: Boolean;
  Scavenge: TCPUReg;
  ViaA: Boolean;
  Via: TCPUReg;
begin
  Assert(ToReg in CPUReg16Bit);
  Assert(GetTypeSize(Variable.UserType) = 1);

  Kind := LoadTypeToKind(LoadType);
  Assert(Kind <> rskVarValueHigh);  //Can't fetch high byte of 8-bit value!

  IsTypecast := Kind <> rskVarValue;
  if Assigned(ToType) then
    SignedLoss := IsSignedType(Variable.UserType) and not IsSignedType(ToType)
  else
    SignedLoss := False;

(*  Scavenge := RegStateFindVariable16(Variable, VarVersion, Kind);
  if Scavenge = rNone then
*)    Scavenge := RegStateFindVariable8(Variable, VarVersion, Kind);

  //NOTE: Changing types and scavenging
  //Try and help the scavenger here, but being cautious. We could argue that
  //the variable value is guaranteed to be in the register, since the value
  //here must be within range. BUT if code paths merge (eg if this code is
  //part of conditionally executed code, when the paths merge the allocator
  //and scavenger may not know what we've done.
  if SignedLoss then
    Kind := rskUnknown;

  if Scavenge in CPURegPairs then
  begin
    //We can just copy - no need for range checking
    if Scavenge <> ToReg then
      GenRegMove(Scavenge, ToReg, False, Options)
  end
  else
  begin
    if Scavenge <> rNone then
    begin
      Assert(Scavenge in CPUReg8Bit);
      if Scavenge <> CPURegPairToLow[ToReg] then
        GenRegMove(Scavenge, CPURegPairToLow[ToReg], False, Options);
      ViaA := Scavenge = rA;
    end
    else
      case Variable.AddrMode of
        amStack:
          ViaA := GenVarLoad8(Variable, VarVersion, CPURegPairToLow[ToReg], Kind, Options);
        amStatic: //Load as pair then overwrite high byte
        begin
          ViaA := GenVarLoad16(Variable, VarVersion, ToReg, Kind, Options);
          RegStateSetVariable(CPURegPairToLow[ToReg], Variable, VarVersion, Kind);
        end;
      else
        Assert(False);
      end;

    if ViaA then
      Via := rA
    else
      Via := CPURegPairToLow[ToReg];

    //Range check
    //Don't range check typecasts
    if RangeCheck and not IsTypecast then
      GenRangeCheck(Via, Variable.UserType, ToType, nil, Options);

    //From To       Range Extend
    //Int8 Integer  Y     Signed
    //              N     Signed
    //Int8 Word     Y     Zero  (because illegal filtered out)
    //              N     Signed
    //Byte Integer  Y     Zero
    //              N     Zero
    //Byte Word     Y     Zero
    //              N     Zero
    if IsSignedType(Variable.UserType) and
      (IsSignedType(ToType) or (not IsSignedType(ToType) and not RangeCheck)) then
    begin
      //Sign extend
      GenSignExtend(Via, CPURegPairToHigh[ToReg], Options);
      RegStateSetUnknown(CPURegPairToHigh[ToReg]);
    end
    else  //Zero extend
      GenLoadRegLiteral(CPURegPairToHigh[ToReg], TImmValue.CreateInteger(0), Options);
  end;
end;

//Sub to GetLoadVar16BitToReg8Bit
procedure GenLoadVar16BitToReg8BitVarValue(Variable: PVariable;VarVersion: Integer;ToReg: TCPUReg;
  ToType: PUserType;RangeCheck: Boolean;Options: TMoveOptionSet);
var ChangeSigned: Boolean;
  Kind: TRegStateKind;
  Scavenge: TCPUReg;
  ViaA: Boolean;
begin
  //For SubRanges we can test the high byte (9-bits if signed to signed) in
  //the usual way and apply sub-range checks to the low byte only.

  //We just need low byte of the 16-bit value, but high byte might need
  //range checking
  ChangeSigned := Assigned(ToType) and
    (IsSignedType(Variable.UserType) <> IsSignedType(ToType));

  //Can we scavenge as 16-bit?
  Scavenge := RegStateFindVariable16(Variable, VarVersion, rskVarValue);
  if Scavenge = rNone then
    //If not can we scavenge low byte as 8-bit?
    Scavenge := RegStateFindVariable8(Variable, VarVersion, rskVarValueLow);

  if RangeCheck and IsSignedType(ToType) and not ChangeSigned then
  (*******************************************************************************
  TODO: When we load a 16-bit signed to an 8-bit signed we need to check all
  9 high bits are equal (if range checking). To do that we need to get bit 7 of
  the low byte into the carry flag, which means that we must load the low byte
  first. This is a PITA!
  *******************************************************************************)
  begin
    if Scavenge in CPURegPairs then
    begin //We already have the full value in registers, do regular range check
      GenRangeCheck(Scavenge, Variable.UserType, ToType, nil, Options);
      GenRegMove(CPURegPairToLow[Scavenge], ToReg, False, Options);
    end
    else
    begin
      //Load low byte into both target reg and A (if different)
      ViaA := GenVarLoad16Low(Variable, VarVersion, ToReg, rskVarValueLow, Options);
      if not ViaA and (ToReg <> rA) then
        GenRegMove(ToReg, rA, False, Options);
      //First part of the range check (RLA)
      GenRangeCheckIntegerToInt8Part1;
//-->          Low byte to Carry flag (RLA)
      //Load the high byte
      GenVarLoad16High(Variable, VarVersion, rA, rskVarValueHigh, Options + [moPreserveCF]);
      //Second part of the range check (ADC A,$00:JP NZ,RAISE_RANGE)
      GenRangeCheckIntegerToInt8Part2;
      //Reload the trashed low byte into A
      if ToReg = rA then
        GenVarLoad16Low(Variable, VarVersion, ToReg, rskVarValueLow, Options);
    end;
  end
  else
  begin
    //Load and range check the high byte
    if RangeCheck then
    begin //And we need to range check the shrink
      if Scavenge in CPURegPairs then
        GenRangeCheckHighByte(CPURegPairToHigh[Scavenge], Variable.UserType, ToType, Options)
      else
      begin
        Assert(not (moPreserveA in Options));
        GenVarLoad16High(Variable, VarVersion, rA, rskVarValueHigh, Options);
        //...and range check it
        GenRangeCheckHighByte(rA, Variable.UserType, ToType, Options);
      end;
    end;

    //NOTE: Changing types and scavenging
    //Try and help the scavenger here, but being cautious. We could argue that
    //the variable value is guaranteed to be in the register, since the value
    //here must be within range. BUT if code paths merge (eg if this code is
    //part of conditionally executed code, when the paths merge the allocator
    //and scavenger may not know what we've done.
    if ChangeSigned then
      Kind := rskUnknown
    else
      Kind := rskVarValueLow;

    //Load the low byte
    if Scavenge <> rNone then
    begin
      if Scavenge in CPURegPairs then
        Scavenge := CPURegPairToLow[Scavenge];
      if Scavenge <> ToReg then
        GenRegMove(Scavenge, ToReg, False, Options);
      ViaA := Scavenge = rA;
    end
    else
      ViaA := GenVarLoad16Low(Variable, VarVersion, ToReg, Kind, Options);

    //Range check
    //Don't range check typecasts
    if RangeCheck then
    begin
      if ViaA then
        ToReg := rA;
      //Low byte of a 16-bit value
      if ToType.IsSubRange then
        GenSubRangeCheckLowByte(ToReg, Variable.UserType, ToType, Options)
      else
        GenRangeCheckLowByte(ToReg, Variable.UserType, ToType, Options);
    end;
  end;
end;


//Load a 16-bit variable into an 8-bit register
procedure GenLoadVar16BitToReg8Bit(Variable: PVariable;VarVersion: Integer;ToReg: TCPUReg;
  LoadType: TLoadParamType;ToType: PUserType;RangeCheck: Boolean;Options: TMoveOptionSet);
var  Kind: TRegStateKind;
  Scavenge: TCPUReg;
begin
  Assert(ToReg in CPUReg8Bit);
  Assert(GetTypeSize(Variable.UserType) = 2);

  Kind := LoadTypeToKind(LoadType);

  case Kind of
    rskVarValueHigh:
    begin
      Scavenge := RegStateFindVariable8(Variable, VarVersion, Kind);
      if Scavenge <> rNone then
      begin
        if Scavenge <> ToReg then
          GenRegMove(Scavenge, ToReg, False, Options)
      end
      else
        GenVarLoad16High(Variable, VarVersion, ToReg, Kind, Options);
    end;
    rskVarValueLow:
    begin
      Scavenge := RegStateFindVariable8(Variable, VarVersion, Kind);
      if Scavenge <> rNone then
      begin
        if Scavenge <> ToReg then
          GenRegMove(Scavenge, ToReg, False, Options)
      end
      else
        GenVarLoad16Low(Variable, VarVersion, ToReg, Kind, Options);
    end;
    rskVarValue:
      GenLoadVar16BitToReg8BitVarValue(Variable, VarVersion, ToReg, ToType,
        RangeCheck, Options);
  else
    Assert(False);
  end;
end;

procedure GenLoadVar16BitToReg16Bit(Variable: PVariable;VarVersion: Integer;ToReg: TCPUReg;
  LoadType: TLoadParamType;ToType: PUserType;RangeCheck: Boolean;Options: TMoveOptionSet);
var ChangeSigned: Boolean;
  Kind: TRegStateKind;
  Scavenge: TCPUReg;
begin
  Assert(ToReg in CPUReg16Bit);
  Assert(GetTypeRegSize(Variable.UserType) = 2);

  Kind := LoadTypeToKind(LoadType);

  case Kind of
    rskVarValueHigh:
    begin
      //Load high byte of variable into low byte of reg pair
      //Can we scavenge?
      Scavenge := RegStateFindVariable8(Variable, VarVersion, Kind);
      if Scavenge <> rNone then
      begin
        if Scavenge <> ToReg then
          GenRegMove(Scavenge, ToReg, False, Options)
      end
      else
        case Variable.AddrMode of
          amStack:
            GenVarLoad16High(Variable, VarVersion, CPURegPairToLow[ToReg], rskVarValueHigh, Options);
          amStatic: //Easier to load the pair and overwrite the high byte
          begin
            GenVarLoadHighTo16(Variable, VarVersion, ToReg, rskVarValue, Options);
            RegStateSetVariable(CPURegPairToLow[ToReg], Variable, VarVersion, rskVarValueHigh);
          end;
        else
          Assert(False);
        end;

      //Zero extend
      GenLoadRegLiteral(CPURegPairToHigh[ToReg], TImmValue.CreateInteger(0), Options);
    end;
    rskVarValueLow:
    begin
      //Load low byte of variable into low byte of reg pair
      //Can we scavenge?
      Scavenge := RegStateFindVariable8(Variable, VarVersion, Kind);
      if Scavenge <> rNone then
      begin
        if Scavenge <> ToReg then
          GenRegMove(Scavenge, ToReg, False, Options)
      end
      else
        case Variable.AddrMode of
          amStack:
            GenVarLoad16Low(Variable, VarVersion, CPURegPairToLow[ToReg], rskVarValueLow, Options);
          amStatic: //Easier to load the pair and overwrite the high byte
            GenVarLoad16(Variable, VarVersion, ToReg, rskVarValue, Options);
        else
          Assert(False);
        end;

      //Zero extend
      GenLoadRegLiteral(CPURegPairToHigh[ToReg], TImmValue.CreateInteger(0), Options);
    end;
    rskVarValue:
    begin
      ChangeSigned := Assigned(ToType) and (IsSignedType(Variable.UserType) <> IsSignedType(ToType));

      Scavenge := RegStateFindVariable16(Variable, VarVersion, Kind);

      //NOTE: Changing types and scavenging
      //Try and help the scavenger here, but being cautious. We could argue that
      //the variable value is guaranteed to be in the register, since the value
      //here must be within range. BUT if code paths merge (eg if this code is
      //part of conditionally executed code, when the paths merge the allocator
      //and scavenger may not know what we've done.
      if ChangeSigned then
        Kind := rskUnknown;

      if Scavenge <> rNone then
      begin
        if Scavenge <> ToReg then
          GenRegMove(Scavenge, ToReg, False, Options)
      end
      else
        GenVarLoad16(Variable, VarVersion, ToReg, Kind, Options);

      if ChangeSigned then
        //We know what's in to low byte but not the high byte
        RegStateSetVariable(CPURegPairToLow[ToReg], Variable, VarVersion, rskVarValueLow);

      if RangeCheck then
        GenRangeCheck(ToReg, Variable.UserType, ToType, nil, Options);
    end;
  else
    Assert(False);
  end;
end;

//Load a 16 bit value to an index register
procedure GenLoadVar16BitToXY(Variable: PVariable;VarVersion: Integer;ToReg: TCPUReg;
  LoadType: TLoadParamType;ToType: PUserType;RangeCheck: Boolean;Options: TMoveOptionSet);
begin
  Assert(ToReg in [rIX, rIY]);
  Assert(Variable.AddrMode = amStatic,'Can''t load stack variables into index register');
  Assert(GetTypeSize(Variable.UserType) = 2, 'Can''t extend 8-bit load into index register');
  Assert(LoadType = lptNormal, 'Can''t load Hi() or Lo() to index register');

  OpLD(ToReg, Variable);
  RegStateSetVariable(ToReg, Variable, VarVersion, rskVarValue);

  if RangeCheck then
    //PS this will fail as can't currently range check index registers.
    //But maybe fixed later
    GenRangeCheck(ToReg, Variable.UserType, ToType, nil, Options);
end;

//Load and convert a boolean variable into a CPU flag (ready for a conditional branch)
procedure GenLoadBoolVarToFlag(Variable: PVariable;VarVersion: Integer;ToReg: TCPUReg;
  Options: TMoveOptionSet);
begin
  Assert(ToReg in [rNZF]);
  Assert(not (moPreserveA in Options), 'Can''t load boolean to CPU flag');

  //Load the variable into A register
  case GetTypeSize(Variable.UserType) of
    1: GenLoadVar8BitToReg8Bit(Variable, VarVersion, rA, lptNormal, GetSystemType(vtBoolean), False, Options);
    2: GenLoadVar16BitToReg8Bit(Variable, VarVersion, rA, lptLow, GetSystemType(vtBoolean), False, Options);
  else
    Assert(False);
  end;

  case ToReg of
    rNZF:
    begin
      OpANDA;
      RegStateSetUnknown(rFlags);
      RegStateSetLiteral(rCF, 0);
    end;
  else
    Assert(False);
  end;
  RegStateSetVariable(ToReg, Variable, VarVersion, rskVarValue);
end;

//Load the value of a variable into the given register
procedure GenLoadRegVarValue(Variable: PVariable;VarVersion: Integer;ToReg: TCPUReg;
  LoadType: TLoadParamType;ToType: PUserType;RangeCheck: Boolean;Options: TMoveOptionSet);
begin
  //Value already in register?
  if RegStateEqualsVariable(ToReg, Variable, VarVersion, rskVarValue) then
    EXIT;

  case ToReg of
    rA..rL:
      case GetTypeRegSize(Variable.UserType) of
        1: GenLoadVar8BitToReg8Bit(Variable, VarVersion, ToReg, LoadType, ToType, RangeCheck, Options);
        2: GenLoadVar16BitToReg8Bit(Variable, VarVersion, ToReg, LoadType, ToType, RangeCheck, Options);
      else
        Assert(False);
      end;
    rHL..rBC:
      case GetTypeRegSize(Variable.UserType) of
        1: GenLoadVar8BitToReg16Bit(Variable, VarVersion, ToReg, LoadType, ToType, RangeCheck, Options);
        2: GenLoadVar16BitToReg16Bit(Variable, VarVersion, ToReg, LoadType, ToType, RangeCheck, Options);
      else
        Assert(False);
      end;
    rIX, rIY:
      GenLoadVar16BitToXY(Variable, VarVersion, ToReg, LoadType, ToType, RangeCheck, Options);
    rNZF:
      GenLoadBoolVarToFlag(Variable, VarVersion, ToReg, Options);
  else
    System.Assert(False);
  end;
end;

//Load the address of Variable into ToReg
procedure GenLoadRegAddrOf(Variable: PVariable;ToReg: TCPUReg;ToType: TVarType;
  Options: TMoveOptionSet);
var Reg: TCPUReg;
  Optimise: Boolean;
  I: Integer;
begin
  Assert(ToReg in CPUReg16Bit);

  //Do we already have the value loaded anywhere?
  Reg := RegStateFindAddrOf(Variable);
  //Already in target register <g>
  if Reg = ToReg then
    EXIT;

  case Variable.AddrMode of
    amStatic:
      OpLD(ToReg, Variable.GetAsmName);
    amStack:
    begin
      if Reg <> rNone then
        //If we can copy this will be easier
        GenRegMove(Reg, ToReg, False, Options)
      else
      begin
        //Can we optimise?
        //Timings:             Combined
        //LD dd,nn - 3/3/10
        //ADD HL,ss - 1/3/11  4/6/21
        //ADD xy,ss - 2/4/15  5/7/25
        //INC/DEC rr - 1/1/6  4x = 4/4/24 <-- use for offsets <= 4
        //INC/DEC xy - 2/2/10 3x = 6/6/30 <-- use for offsets <= 3
        Optimise := ((ToReg in [rHL, rDE, rBC]) and (abs(Variable.Offset) <= 4))
          or ((ToReg in [rIX, rIY]) and (abs(Variable.Offset) <= 3));

        //Reg to use as accumulator in the addition
        if Optimise then
          Reg := ToReg
        else if ToReg in [rHL, rIX, rIY] then
          Reg := ToReg
        else
          Reg := rHL;

        //Copy stack base pointer to Accumulator Reg
        if ToReg <> rIX then
        begin
          OpPUSH(rIX);
          OpPOP(Reg);
        end;

        //Add (subtract) stack offset
        if Variable.Offset = 0 then
          //Nothing needed
        else if Optimise then
        begin
          for I := 1 to abs(Variable.Offset) do
            if Variable.Offset < 0 then
              OpDEC(Reg)
            else
              OpINC(Reg);
        end
        else
        begin
          //Load offset into DE
          GenLoadRegLiteral(rDE, TImmValue.CreateInteger(Variable.Offset), Options);
          OpADD(Reg, rDE);
          RegStateSetVariable(Reg, Variable, 0, rskVarAddr);
          RegStateSetUnknowns([rFlags, rCF]); //ZF not touched
        end;

        //Move to the final register
        if Reg <> ToReg then
          GenRegMove(Reg, ToReg, False, Options);
      end;
    end;
  else
    Assert(False);
  end;

  RegStateSetVariable(ToReg, Variable, 0, rskVarAddr);
end;

//Loads the address of the data for a Pointered Type
//The value will depend on the addressing mode of the variable
procedure GenLoadRegVarRef(Variable: PVariable;VarVersion: Integer;ToReg: TCPUReg;
{  LoadType: TLoadParamType;ToType: PUserType;RangeCheck: Boolean;}Options: TMoveOptionSet);
begin
  Assert(IsPointeredType(Variable.VarType));

  case Variable.AddrMode of
    amStatic:
      OpLD(ToReg, Variable.GetAsmName);
      //TODO: CPU State
  else
    Assert(False);
  end;

end;

procedure GenLoadLiteralPointer(ToReg: TCPUReg;const Imm: TImmValue; Options: TMoveOptionSet);
begin
  Assert(ToReg in CPUReg16Bit);
  OpLD(ToReg, Imm.ToLabel);
  RegStateSetLabel(ToReg, Imm.ToLabel);
end;

procedure GenLoadRegIndirect(ToReg, PtrReg: TCPUReg;Options: TMoveOptionSet);
begin
  Assert(PtrReg in CPUReg16Bit);
  if ToReg in CPUReg8Bit then
    OpLDFromIndirect(ToReg, PtrReg)
  else if ToReg in CPURegPairs then
  begin
    OpLDFromIndirect(CPURegPairToLow[ToReg], PtrReg);
    OpINC(PtrReg);
    OpLDFromIndirect(CPURegPairToHigh[ToReg], PtrReg);

    //TODO: Add better meta data for this
    RegStateSetUnknown(PtrReg);
  end
  else
    Assert(False);  //Unacceptable register combo

  //TODO: Add better meta data for this
  RegStateSetUnknown(ToReg);
end;

//Generate code to store the value of FromReg into memory addressed by PtrReg
procedure GenStoreRegIndirect(PtrReg, FromReg: TCPUReg;Options: TMoveOptionSet);
begin
  Assert(PtrReg in CPUReg16Bit);
  if FromReg in CPUReg8Bit then
    OpLDToIndirect(PtrReg, FromReg)
  else if FromReg in CPURegPairs then
  begin
    OpLDToIndirect(PtrReg, CPURegPairToLow[FromReg]);
    OpINC(PtrReg);
    OpLDToIndirect(PtrReg, CPURegPairToHigh[FromReg]);

    //TODO: Add better meta data for this
    RegStateSetUnknown(PtrReg);
  end
  else
    Assert(False);  //Unacceptable register combo
end;

//=========================================OLD CODE

//======================== LOAD PARAMETERS

//Param: The parameter to load, including the desired Reg and VarType if a variable
//ToType: The type of the destination. Used for range checking. If ToType is vtUnknown
//  no range checking will be performed
procedure GenLoadParam(const Param: TILParam; ToType: PUserType;Options: TMoveOptionSet);
begin
  case Param.Kind of
    pkNone: ; //No param to load
    pkImmediate:
      //If Reg is rNone the literal is loaded by the primitive itself
      if Param.Reg <> rNone then
        if IsPointeredType(Param.Imm.VarType) then
          GenLoadLiteralPointer(Param.Reg, Param.Imm, Options)
        else
          GenLoadRegLiteral(Param.Reg, Param.Imm, Options);
    pkVarSource:
      GenLoadRegVarValue(Param.Variable, Param.VarVersion, Param.Reg, Param.LoadType, ToType,
        cgRangeCheck in Param.Flags, Options);
    pkVarRef:
      GenLoadRegVarRef(Param.Variable, Param.VarVersion, Param.Reg, Options);
    pkVarAddr, pkVarPtr: ;  //Handled by the primitive
  else
    System.Assert(False, 'Invalid param kind for param load');
  end;
end;

procedure GenPtrLoad(const Item: TILItem;Options: TMoveOptionSet);
begin
  //Load address into Param1.Reg
  GenLoadParam(Item.Param1, GetSystemType(vtPointer), Options);  //Tweak Type(?)
  //Load value into Dest.Reg
  GenLoadRegIndirect(Item.Dest.Reg, Item.Param1.Reg, Options);
end;

procedure GenPtrStore(const Item: TILItem;Options: TMoveOptionSet);
begin
  //Load data - from Param2
  GenLoadParam(Item.Param2, Item.Param2.GetUserType, Options);

  //Load address into Param1.Reg - we'll use Param2's VarType
  GenLoadParam(Item.Param1, GetSystemType(vtPointer), Options);  //Tweak type(?)

  GenStoreRegIndirect(Item.Param1.Reg, Item.Param2.Reg, Options);
end;

(*
//Sub to LoadBeforePrim
//Loads the Param in the register specified in the Param whilst preserving the value
//in the register named in Reg. (Reg can either not be touched, or can be moved
//elsewhere (e.g. another register or the stack) and move back before the function
//returns.
procedure LoadPreserving(const Param: TILParam;Reg: TCPUReg;ToType: TVarType;PrimFlags: TPrimFlagSet);
var
  V: PVariable;
  PreserveIn: TCPUReg;
begin
  Assert(Param.Kind in [pkImmediate, pkVarSource]);

  PreserveIn := rNone;
  if Param.Kind = pkVarSource then
    //Do we need to move P1 to avoid it getting trashed during the load?
    //Trashing only happens if we load an 8-bit from static address (which
    //needs to go via A)
    case Reg of
      rNone: ;
      rA:
      begin
        V := Param.Variable;
        if (V.Storage = vsStatic) and (GetTypeSize(V.VarType) = 1) then
        begin //Find a register to preserve A into (which is neither A or the Param.Reg)
          PreserveIn := RegStateAllocReg8([rA, Param.Reg]);
          GenRegMove(Reg, PreserveIn, False, []);
        end;
      end
    else
      Assert(Reg <> Param.Reg, 'Can only preserve A reg at present');
    end;

  GenLoadParam(Param, ToType, PrimFlags, [moPreserveHLDE]);

  if PreserveIn <> rNone then
    GenRegMove(PreserveIn, Reg, False, []);
end;
*)(*
//Load a parameter whilst also moving FromReg into ToReg.
//Determines the best strategy based on whether the load will damage any registers
//and the registers required from FromReg and ToReg
procedure LoadWithMove(const Param: TILParam; ToType: TVarType;FromReg, ToReg: TCPUReg;
  PrimFlags: TPrimFlagSet);
var
  MoveBefore: Boolean;  //Do the move before or after the load?
begin
  //Move before if we're loading into FromReg, or if we're using FromReg during the load
  //(only A can be trashed by a load. It's easier to always move before if FromReg is A)
  MoveBefore := (FromReg = Param.Reg) or (FromReg = rA);
  if MoveBefore then
    GenRegMove(FromReg, ToReg, False, []);
  GenLoadParam(Param, ToType, PrimFlags, [moPreserveHLDE]);
  if not MoveBefore then
    GenRegMove(FromReg, ToReg, False, []);
end;
*)(*
//Load two Parameters. Determines which should be loaded first and which second based
//whether either will trash a register during loading.
procedure LoadBoth(ILItem: PILItem; P1Type, P2Type: TVarType; PrimFlags: TPrimFlagSet);
begin
  //Select a register loading order so the second load won't trash the first.

  //A will get trashed by an 8-bit load into a register other than A, so
  //swap load order just in case
  if ILItem.Param1.Reg = rA then
  begin
    GenLoadParam(ILItem.Param2, P2Type, PrimFlags, [moPreserveHLDE]);
    GenLoadParam(ILItem.Param1, P1Type, PrimFlags, [moPreserveHLDE]);
  end
  else
  begin
    GenLoadParam(ILItem.Param1, P1Type, PrimFlags, [moPreserveHLDE]);
    GenLoadParam(ILItem.Param2, P2Type, PrimFlags, [moPreserveHLDE]);
  end;
end;
*)(*
procedure SwapRegs(ILItem: PILItem;P1Type, P2Type: TVarType; PrimFlags: TPrimFlagSet);
begin
  if (ILItem.Param1.Reg in CPUReg16Bit) and (ILItem.Param2.Reg in CPUReg16Bit) then
  begin
    if (ILItem.Param1.Reg in [rHL, rDE]) and (ILItem.Param2.Reg in [rHL, rDE]) then
    begin
      OpEXHLDE;
      EXIT;
    end;
    OpPUSH(ILItem.Param1.Reg);
    GenRegMove(ILItem.Param2.Reg, ILItem.Param1.Reg, False, []);
    OpPOP(ILItem.Param2.Reg);
  end
  else
    //TODO: Temporary bodge by reloading both
    LoadBoth(ILItem, P1Type, P2Type, PrimFlags);
end;
*)(*
//Loads parameters from memory* into registers as needed.
//Data is loaded from the locations specified by the ILItem parameters into the
//registers specified by ILItem.Param1Alloc and ILItem.Param2Alloc, if any registers
//are specified there.
//* - can also handle parameters which are already in registers
procedure LoadBeforePrim(const ILItem: TILItem; Prim: PPrimitive);
var// Swap: Boolean;
  PrimFlags: TPrimFlagSet;
  P1Reg: TCPUReg; //If P1 is already in a register
  P2Reg: TCPUReg; //If P2 is already in a register
  P1Move: Boolean;  //P1 is already in a register but not the required one
  P2Move: Boolean;  //P2 is already in a register but not the required one
  P1Type: TVarType;
  P2Type: TVarType;
begin
  if Assigned(Prim) then
  begin
    PrimFlags := Prim.Flags;
    if cgRangeCheck in ILItem.Param1.Flags then
      P1Type := Prim.LType
    else
      P1Type := vtUnknown;
    if cgRangeCheck in ILItem.Param2.Flags then
      P2Type := Prim.RType
    else
      P2Type := vtUnknown;
  end
  else
  begin
    PrimFlags := [];
    P1Type := vtUnknown;
    P2Type := vtUnknown;
  end;

  //Finish early if we have zero or one param
  if ILItem.Param2.Kind = pkNone then
    if ILItem.Param1.Kind = pkNone then
      EXIT
    else
    begin
      GenLoadParam(ILItem.Param1, P1Type, PrimFlags, [moPreserveHLDE]);
      EXIT;
    end;

  if ILItem.Param1.Kind = pkVarSource then
    P1Reg := RegStateFindVariable(ILItem.Param1.Variable, ILItem.Param1.VarVersion, rskVarValue)
  else
    P1Reg := rNone;
  P1Move := (P1Reg <> rNone) and (P1Reg <> ILItem.Param1.Reg);

  if ILItem.Param2.Kind = pkVarSource then
    P2Reg := RegStateFindVariable(ILItem.Param2.Variable, ILItem.Param2.VarVersion, rskVarValue)
  else
    P2Reg := rNone;
  P2Move := (P2Reg <> rNone) and (P2Reg <> ILItem.Param2.Reg);

  if P1Reg = rNone then
  begin //Load P1 data
    if P2Reg = rNone then
      //Load P2 data
      LoadBoth(@ILItem, P1Type, P2Type, Prim.Flags)
    else if P2Move then
      //P2 is in a Reg but needs moving
      LoadWithMove(ILItem.Param1, P1Type, P2Reg, ILItem.Param2.Reg, PrimFlags)
    else //P2 is in the correct Reg
      LoadPreserving(ILItem.Param1, P2Reg, P1Type, PrimFlags)
  end
  else if P1Move then
  begin //P1 is in a Reg but needs moving
    if P2Reg = rNone then
      //Load P2 data
      LoadWithMove(ILItem.Param2, P2Type, P1Reg, ILItem.Param1.Reg, PrimFlags)
    else if P2Move then
    begin //P2 is in a Reg but needs moving, as is P1
      SwapRegs(@ILItem, P1Type, P2Type, Prim.Flags);
    end
    else
    begin //P2 is in the correct Reg
      System.Assert(False);
    end
  end
  else
  begin //P1 is in the correct Reg
    if P2Reg = rNone then
      //Load P2 data
      LoadPreserving(ILItem.Param2, P1Reg, P2Type, PrimFlags)
    else if P2Move then
    begin //P2 is in a Reg but needs moving
      System.Assert(False);
    end
    else
    begin //P2 is in the correct Reg
      //Do nothing
    end
  end
end;
*)
end.

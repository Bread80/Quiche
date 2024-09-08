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
uses Z80.CPU, QTypes,
  ILData, PrimitivesEx;

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
procedure GenLoadParam(const Param: TILParam;ToType: TVarType;PrimFlags: TPrimFlagSetNG;Options: TMoveOptionSet);

//Loads parameters from memory* into registers as needed.
//Data is loaded from the locations specified by the ILItem parameters into the
//registers specified by ILItem.Param1Alloc and ILItem.Param2Alloc, if any registers
//are specified there.
//* - can also handle parameters which are already in registers
procedure LoadBeforePrim(ILItem: PILItem; Prim: PPrimitiveNG);

implementation
uses CodeGen, Variables,
  Z80.CPUState, Z80.CodeGen, Z80.Validation;

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


//================================VARIABLES

function PrimFlagsToKind(PrimFlags: TPrimFlagSetNG): TRegStateKind;
begin
  if pfnLoadRPHigh in PrimFlags then
    Result := rskVarValueHigh
  else if pfnLoadRPLow in PrimFlags then
    Result := rskVarValueLow
  else
    Result := rskVarValue;
end;

//Basic load of an 8 bit variable into an 8-bit register
//Returns True if we loaded the value via the A register (but Reg <> rA)
function GenVarLoad8(Reg: TCPUReg;Variable: PVariable;VarVersion: Integer;
  Kind: TRegStateKind;Options: TMoveOptionSet): Boolean;
begin
  Assert(Reg in CPUReg8Bit);

  Result := False;
  case Variable.Storage of
    vsStack:
      OpLD(Reg, rIX, Variable);
    vsStatic:
    begin
      OpLD(rA, Variable);
      if Reg <> rA then
      begin
        Assert(not (moPreserveA in Options));
        OpLD(Reg, rA);
        RegStateSetVariable(rA, Variable, VarVersion, Kind);
        Result := True;
      end;
    end;
  else
    Assert(False);
  end;
  RegStateSetVariable(Reg, Variable, VarVersion, Kind);
end;

//Basic load of an 16 bit variable into an 16-bit register
//Returns True if we loaded the value via the A register (but Reg <> rA)
function GenVarLoad16(Reg: TCPUReg;Variable: PVariable;VarVersion: Integer;
  Kind: TRegStateKind;Options: TMoveOptionSet): Boolean;
begin
  Assert(Reg in CPURegPairs);

  Result := False;
  case Variable.Storage of
    vsStack:
      OpLD(Reg, rIX, Variable);
    vsStatic:
      OpLD(Reg, Variable);
  else
    Assert(False);
  end;
  RegStateSetVariable(Reg, Variable, VarVersion, Kind);
end;

//Basic load of the high byte (and the next byte) of a 16 bit variable into an 16-bit register
//Used for loading RPHigh for static vars
//Returns True if we loaded the value via the A register (but Reg <> rA)
function GenVarLoadHighTo16(Reg: TCPUReg;Variable: PVariable;VarVersion: Integer;
  Kind: TRegStateKind;Options: TMoveOptionSet): Boolean;
begin
  Assert(Reg in CPURegPairs);

  Result := False;
  case Variable.Storage of
    vsStack:
      OpLD(Reg, rIX, Variable, 1);
    vsStatic:
      OpLD(Reg, Variable, 1);
  else
    Assert(False);
  end;
  RegStateSetVariable(Reg, Variable, VarVersion, Kind);
end;

//Basic load of the high half of a 16 bit variable into an 8-bit register
//Returns True if we loaded the value via the A register (but Reg <> rA)
function GenVarLoad16High(Reg: TCPUReg;Variable: PVariable;VarVersion: Integer;
  Kind: TRegStateKind;Options: TMoveOptionSet): Boolean;
begin
  Assert(Reg in CPUReg8Bit);

  Result := False;
  case Variable.Storage of
    vsStack:
      OpLD(Reg, rIX, Variable, 1);
    vsStatic:
    begin
      OpLD(rA, Variable, 1);
      if Reg <> rA then
      begin
        Assert(not (moPreserveA in Options));
        OpLD(Reg, rA);
        RegStateSetVariable(rA, Variable, VarVersion, Kind);
        Result := True;
      end;
    end;
  else
    Assert(False);
  end;
  RegStateSetVariable(Reg, Variable, VarVersion, Kind);
end;

//Basic load of the low half of a 16 bit variable into an 8-bit register
//Returns True if we loaded the value via the A register (but Reg <> rA)
function GenVarLoad16Low(Reg: TCPUReg;Variable: PVariable;VarVersion: Integer;
  Kind: TRegStateKind;Options: TMoveOptionSet): Boolean;
begin
  Assert(Reg in CPUReg8Bit);

  Result := False;
  case Variable.Storage of
    vsStack:
      OpLD(Reg, rIX, Variable);
    vsStatic:
    begin
      OpLD(rA, Variable);
      if Reg <> rA then
      begin
        Assert(not (moPreserveA in Options));
        OpLD(Reg, rA);
        RegStateSetVariable(rA, Variable, VarVersion, Kind);
        Result := True;
      end;
    end;
  else
    Assert(False);
  end;
  RegStateSetVariable(Reg, Variable, VarVersion, Kind);
end;

//=======

function GenLoadVar8BitToReg8Bit(Reg: TCPUReg;Variable: PVariable;VarVersion: Integer;
  PrimFlags: TPrimFlagSetNG;ToType: TVarType;RangeCheck: Boolean;Options: TMoveOptionSet): Boolean;
var ChangeSigned: Boolean;
  Kind: TRegStateKind;
  IsTypecast: Boolean;
  Scavenge: TCPUReg;
  ViaA: Boolean;
begin
  Assert(Reg in CPUReg8Bit);
  Assert(GetTypeSize(Variable.VarType) = 1);

  Kind := PrimFlagsToKind(PrimFlags);
  Assert(Kind <> rskVarValueHigh);         //Can't fetch high byte of 8 but value!

  IsTypecast := Kind <> rskVarValue;
  ChangeSigned := (ToType <> vtUnknown) and (IsSignedType(Variable.VarType) <> IsSignedType(ToType));

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
    if Scavenge <> Reg then
      GenRegMove(Scavenge, Reg, False, Options);
    ViaA := Scavenge = rA;
  end
  else
    ViaA := GenVarLoad8(Reg, Variable, VarVersion, Kind, Options);

  //Range check
  //Don't range check typecasts
  if RangeCheck and not IsTypecast then
  begin
    if ViaA then
      Reg := rA;
    GenRangeCheck(Reg, Variable.VarType, ToType, nil, Options);
  end;
end;

function GenLoadVar8BitToReg16Bit(Reg: TCPUReg;Variable: PVariable;VarVersion: Integer;
  PrimFlags: TPrimFlagSetNG;ToType: TVarType;RangeCheck: Boolean;Options: TMoveOptionSet): Boolean;
var SignedLoss: Boolean;
  Kind: TRegStateKind;
  IsTypecast: Boolean;
  Scavenge: TCPUReg;
  ViaA: Boolean;
  Via: TCPUReg;
begin
  Assert(Reg in CPUReg16Bit);
  Assert(GetTypeSize(Variable.VarType) = 1);

  Kind := PrimFlagsToKind(PrimFlags);
  Assert(Kind <> rskVarValueHigh);  //Can't fetch high byte of 8-bit value!

  IsTypecast := Kind <> rskVarValue;
  SignedLoss := (ToType <> vtUnknown) and (IsSignedType(Variable.VarType) and not IsSignedType(ToType));

  Scavenge := RegStateFindVariable16(Variable, VarVersion, Kind);
  if Scavenge = rNone then
    Scavenge := RegStateFindVariable8(Variable, VarVersion, Kind);

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
    if Scavenge <> Reg then
      GenRegMove(Scavenge, Reg, False, Options)
  end
  else
  begin
    if Scavenge <> rNone then
    begin
      Assert(Scavenge in CPUReg8Bit);
      if Scavenge <> CPURegPairToLow[Reg] then
        GenRegMove(Scavenge, CPURegPairToLow[Reg], False, Options);
      ViaA := Scavenge = rA;
    end
    else
      if Variable.Storage = vsStack then
        ViaA := GenVarLoad8(CPURegPairToLow[Reg], Variable, VarVersion, Kind, Options)
      else  //Static - load as pair then overwrite high byte
      begin
        ViaA := GenVarLoad16(Reg, Variable, VarVersion, Kind, Options);
        RegStateSetVariable(CPURegPairToLow[Reg], Variable, VarVersion, Kind);
      end;

    if ViaA then
      Via := rA
    else
      Via := CPURegPairToLow[Reg];

    //Range check
    //Don't range check typecasts
    if RangeCheck and not IsTypecast then
      GenRangeCheck(Via, Variable.VarType, ToType, nil, Options);

    if IsSignedType(Variable.VarType) and (IsSignedType(ToType) or (ToType = vtUnknown)) and not IsTypecast then
    begin
      //Sign extend
      GenSignExtend(Via, CPURegPairToHigh[Reg], Options);
      RegStateSetUnknown(CPURegPairToHigh[Reg]);
    end
    else  //Zero extend
      GenLoadRegLiteral(CPURegPairToHigh[Reg], TImmValue.CreateInteger(0), Options);
  end;
end;

//Load a 16-bit variable into an 8-bit register
function GenLoadVar16BitToReg8Bit(Reg: TCPUReg;Variable: PVariable;VarVersion: Integer;
  PrimFlags: TPrimFlagSetNG;ToType: TVarType;RangeCheck: Boolean;Options: TMoveOptionSet): Boolean;
var ChangeSigned: Boolean;
  Kind: TRegStateKind;
  Scavenge: TCPUReg;
  ViaA: Boolean;
begin
  Assert(Reg in CPUReg8Bit);
  Assert(GetTypeSize(Variable.VarType) = 2);

  Kind := PrimFlagsToKind(PrimFlags);

  case Kind of
    rskVarValueHigh:
    begin
      Scavenge := RegStateFindVariable8(Variable, VarVersion, Kind);
      if Scavenge <> rNone then
      begin
        if Scavenge <> Reg then
          GenRegMove(Scavenge, Reg, False, Options)
      end
      else
        ViaA := GenVarLoad16High(Reg, Variable, VarVersion, Kind, Options);
    end;
    rskVarValueLow:
    begin
      Scavenge := RegStateFindVariable8(Variable, VarVersion, Kind);
      if Scavenge <> rNone then
      begin
        if Scavenge <> Reg then
          GenRegMove(Scavenge, Reg, False, Options)
      end
      else
        ViaA := GenVarLoad16Low(Reg, Variable, VarVersion, Kind, Options);
    end;
    rskVarValue:
    begin //We just need low byte of the 16-bit value, but high byte might need
          //range checking
      ChangeSigned := (ToType <> vtUnknown) and (IsSignedType(Variable.VarType) <> IsSignedType(ToType));

      //Can we scavenge as 16-bit?
      Scavenge := RegStateFindVariable16(Variable, VarVersion, Kind);
      if Scavenge = rNone then
        //If not can we scavenge low byte as 8-bit?
        Scavenge := RegStateFindVariable8(Variable, VarVersion, rskVarValueLow);


      //NOTE: Changing types and scavenging
      //Try and help the scavenger here, but being cautious. We could argue that
      //the variable value is guaranteed to be in the register, since the value
      //here must be within range. BUT if code paths merge (eg if this code is
      //part of conditionally executed code, when the paths merge the allocator
      //and scavenger may not know what we've done.
      if ChangeSigned then
        Kind := rskUnknown;

      //Load and range check the high byte
      if RangeCheck then
      begin //And we need to range check the shrink
        if Scavenge in CPURegPairs then
          GenRangeCheckHighByte(CPURegPairToHigh[Scavenge], Variable.VarType, ToType, Options)
        else
        begin
          Assert(not (moPreserveA in Options));
          ViaA := GenVarLoad16High(rA, Variable, VarVersion, rskVarValueHigh, Options);
          //...and range check it
          GenRangeCheckHighByte(rA, Variable.VarType, ToType, Options);
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
        if Scavenge <> Reg then
          GenRegMove(Scavenge, Reg, False, Options);
        ViaA := Scavenge = rA;
      end
      else
        ViaA := GenVarLoad16Low(Reg, Variable, VarVersion, Kind, Options);

      //Range check
      //Don't range check typecasts
      if RangeCheck then
      begin
        if ViaA then
          Reg := rA;
        //Low byte of a 16-bit value
        GenRangeCheckLowByte(Reg, Variable.VarType, ToType, Options);
      end;
    end;
  else
    Assert(False);
  end;
end;

function GenLoadVar16BitToReg16Bit(Reg: TCPUReg;Variable: PVariable;VarVersion: Integer;
  PrimFlags: TPrimFlagSetNG;ToType: TVarType;RangeCheck: Boolean;Options: TMoveOptionSet): Boolean;
var ChangeSigned: Boolean;
  Kind: TRegStateKind;
  Scavenge: TCPUReg;
  ViaA: Boolean;
begin
  Assert(Reg in CPUReg16Bit);
  Assert(GetTypeSize(Variable.VarType) = 2);

  Kind := PrimFlagsToKind(PrimFlags);

  case Kind of
    rskVarValueHigh:
    begin
      //Load high byte of variable into low byte of reg pair
      //Can we scavenge?
      Scavenge := RegStateFindVariable8(Variable, VarVersion, Kind);
      if Scavenge <> rNone then
      begin
        if Scavenge <> Reg then
          GenRegMove(Scavenge, Reg, False, Options)
      end
      else
        if Variable.Storage = vsStack then
          ViaA := GenVarLoad16High(CPURegPairToLow[Reg], Variable, VarVersion, rskVarValueHigh, Options)
        else //Static - easier to load the pair and overwrite the high byte
        begin
          ViaA := GenVarLoadHighTo16(Reg, Variable, VarVersion, rskVarValue, Options);
          RegStateSetVariable(CPURegPairToLow[Reg], Variable, VarVersion, rskVarValueHigh);
        end;

      //Zero extend
      GenLoadRegLiteral(CPURegPairToHigh[Reg], TImmValue.CreateInteger(0), Options);
    end;
    rskVarValueLow:
    begin
      //Load low byte of variable into low byte of reg pair
      //Can we scavenge?
      Scavenge := RegStateFindVariable8(Variable, VarVersion, Kind);
      if Scavenge <> rNone then
      begin
        if Scavenge <> Reg then
          GenRegMove(Scavenge, Reg, False, Options)
      end
      else
        if Variable.Storage = vsStack then
          ViaA := GenVarLoad16Low(CPURegPairToLow[Reg], Variable, VarVersion, rskVarValueLow, Options)
        else  //Static - easier to load the pair and overwrite the high byte
          ViaA := GenVarLoad16(Reg, Variable, VarVersion, rskVarValue, Options);

      //Zero extend
      GenLoadRegLiteral(CPURegPairToHigh[Reg], TImmValue.CreateInteger(0), Options);
    end;
    rskVarValue:
    begin
      ChangeSigned := (ToType <> vtUnknown) and (IsSignedType(Variable.VarType) <> IsSignedType(ToType));

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
        if Scavenge <> Reg then
          GenRegMove(Scavenge, Reg, False, Options)
      end
      else
        ViaA := GenVarLoad16(Reg, Variable, VarVersion, Kind, Options);

      if ChangeSigned then
        //We know what's in to low byte but not the high byte
        RegStateSetVariable(CPURegPairToLow[Reg], Variable, VarVersion, rskVarValueLow);

      if RangeCheck then
        GenRangeCheck(Reg, Variable.VarType, ToType, nil, Options);
    end;
  else
    Assert(False);
  end;
end;

//Load a 16 bit value to an index register
function GenLoadVar16BitToXY(Reg: TCPUReg;Variable: PVariable;VarVersion: Integer;
  PrimFlags: TPrimFlagSetNG;ToType: TVarType;RangeCheck: Boolean;Options: TMoveOptionSet): Boolean;
begin
  Assert(Reg in [rIX, rIY]);
  Assert(Variable.Storage = vsStatic,'Can''t load stack variables into index register');
  Assert(GetTypeSize(Variable.VarType) = 2, 'Can''t extend 8-bit load into index register');
  Assert(PrimFlags * [pfnLoadRPHigh, pfnLoadRPLow] = [], 'Can''t load RPHigh, RPLow to index register');

  OpLD(Reg, Variable);
  RegStateSetVariable(Reg, Variable, VarVersion, rskVarValue);

  if RangeCheck then
    //PS this will fail as can't currently range check index registers.
    //But maybe fixed later
    GenRangeCheck(Reg, Variable.VarType, ToType, nil, Options);
end;


//Load the value of a variable into the given register
function GenLoadRegVarValue(Reg: TCPUReg;Variable: PVariable;VarVersion: Integer;
  PrimFlags: TPrimFlagSetNG;ToType: TVarType;RangeCheck: Boolean;Options: TMoveOptionSet): Boolean;
begin
  //TODO: Move the scavenging code into the GenLoad routines used below (to make the
  //scavenging more granular)

  //Value already in register?
  if RegStateEqualsVariable(Reg, Variable, VarVersion, rskVarValue) then
    EXIT(True);
  //Can reg state be moved from a register(s)?
  //TODO: Temporarily disabled - we need to add support for type conversions, which means moving the
  //codebase into the GenLoad routines used below
(*  if TryScavengeVariable(Reg, Variable, VarVersion, PrimFlags, ToType, RangeCheck, Options) then
    EXIT(True);
*)
  case Reg of
    rA..rL:
      case GetTypeSize(Variable.VarType) of
        1: GenLoadVar8BitToReg8Bit(Reg, Variable, VarVersion, PrimFlags, ToType, RangeCheck, Options);
        2: GenLoadVar16BitToReg8Bit(Reg, Variable, VarVersion, PrimFlags, ToType, RangeCheck, Options);
      else
        Assert(False);
      end;
    rHL..rBC:
      case GetTypeSize(Variable.VarType) of
        1: GenLoadVar8BitToReg16Bit(Reg, Variable, VarVersion, PrimFlags, ToType, RangeCheck, Options);
        2: GenLoadVar16BitToReg16Bit(Reg, Variable, VarVersion, PrimFlags, ToType, RangeCheck, Options);
      else
        Assert(False);
      end;
    rIX, rIY:
      GenLoadVar16BitToXY(Reg, Variable, VarVersion, PrimFlags, ToType, RangeCheck, Options);
  else
    System.Assert(False);
  end;

  Result := True;
end;


//=========================================OLD CODE

//======================== LOAD PARAMETERS

//Param: The parameter to load, including the desired Reg and VarType if a variable
//ToType: The type of the destination. Used for range checking. If ToType is vtUnknown
//  no range checking will be performed
procedure GenLoadParam(const Param: TILParam; ToType: TVarType;PrimFlags: TPrimFlagSetNG;Options: TMoveOptionSet);
begin
  case Param.Kind of
    pkNone: ; //No param to load
    pkImmediate:
      //If Reg is rImm the data is loaded by the primitive itself
      if not (Param.Reg in [rNone, rImm]) then
        GenLoadRegLiteral(Param.Reg, Param.Imm, Options);
    pkVarSource:
      GenLoadRegVarValue(Param.Reg, Param.Variable, Param.VarVersion, PrimFlags, ToType,
        cgRangeCheck in Param.Flags, Options);

    //TODO: Load var address (@ operator)
  else
    System.Assert(False, 'Invalid param kind for param load');
  end;
end;

//Sub to LoadBeforePrim
//Loads the Param in the register specified in the Param whilst preserving the value
//in the register named in Reg. (Reg can either not be touched, or can be moved
//elsewhere (e.g. another register or the stack) and move back before the function
//returns.
procedure LoadPreserving(const Param: TILParam;Reg: TCPUReg;ToType: TVarType;PrimFlags: TPrimFlagSetNG);
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

//Load a parameter whilst also moving FromReg into ToReg.
//Determines the best strategy based on whether the load will damage any registers
//and the registers required from FromReg and ToReg
procedure LoadWithMove(const Param: TILParam; ToType: TVarType;FromReg, ToReg: TCPUReg;
  PrimFlags: TPrimFlagSetNG);
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

//Load two Parameters. Determines which should be loaded first and which second based
//whether either will trash a register during loading.
procedure LoadBoth(ILItem: PILItem; P1Type, P2Type: TVarType; PrimFlags: TPrimFlagSetNG);
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

//Loads parameters from memory* into registers as needed.
//Data is loaded from the locations specified by the ILItem parameters into the
//registers specified by ILItem.Param1Alloc and ILItem.Param2Alloc, if any registers
//are specified there.
//* - can also handle parameters which are already in registers
procedure LoadBeforePrim(ILItem: PILItem; Prim: PPrimitiveNG);
var// Swap: Boolean;
  PrimFlags: TPrimFlagSetNG;
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
    if not (cgRangeCheck in ILItem.Param1.Flags) then
      P1Type := Prim.LType
    else
      P1Type := vtUnknown;
    if not (cgRangeCheck in ILItem.Param2.Flags) then
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
      LoadBoth(ILItem, P1Type, P2Type, Prim.Flags)
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
    begin //P2 is in a Reg but needs moving
      System.Assert(False);
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

end.

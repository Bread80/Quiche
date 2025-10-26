(*
* Store values from into variables

All functions within this unit update the data in Z80.CPUState to reflect any code
generated.
*)
unit Z80.Store;

interface
uses Def.IL, Def.VarTypes, Def.UserTypes,
  Lib.Data,
  Z80.Validation, Z80.Hardware, Z80.GenProcs;

//Generates code to store an immediate/literal value into a variable in memory.
//ILItem contains a source parameter which is an immediate value and a dest
//param which is a variable.
procedure GenStoreImm(ILItem: PILItem;Options: TMoveOptionSet);

//Reverse of the GenLoadParam function in Z80.Load. See that function for details.
//Except:
//The FromType is given explicitly, the ToType is given in Param.
//Scavenging is not appropriate for writes and therefore not used.
//PrimFlags (Hi() and Lo() functions) are not relevent
//The RangeCheck parameter specifies whether range checking should be carried out
//or not (again subject to FromType <> vtUnknown, and the relevant types).
//RangeCheckProc, if not nil, specifies an optimised range checking procedure.
procedure GenDestParam(const Param: TILParam;FromType: PUserType;RangeCheck: Boolean;
  RangeCheckProc: TRangeCheckProc;Options: TMoveOptionSet);

//Takes the result from the register specified in ILItem.DestAlloc and stores it
//into the location specified in ILItem.Dest
//If a type conversion is to take place then:
// * if validation is enabled, will generate code to ensure the value will fit into
//the destination type
// * generates code to handle the type conversion (if necessary)
procedure StoreAfterPrim(ILItem: PILItem;Prim: PPrimitive);

//Generate code for an ILItem which is an unconditional jump
procedure GenUncondBranch(ILItem: PILItem);

//Generate a branch after the
procedure GenCondBranch(const Param: TILParam);


implementation
uses Classes, SysUtils,
  Def.Consts, Def.Variables,
  Lib.GenFragments,
  CodeGen, CG.Data,
  Z80.CG, Z80.CPUState, Z80.Assembler;

//Generates the code to convert a boolean value from various sources into a boolean
//value in A
procedure GenToBoolean(Reg: TCPUReg;const Param: TILParam;Options: TMoveOptionSet);
var FlagsCorrupt: Boolean;
  Unknowns: TCPURegSet;
  OptionTests: TMoveOptionSet;
begin
  FlagsCorrupt := True; //Assume
  OptionTests := [moPreserveCF, moPreserveOtherFlags, moPreserveA];
  Unknowns := [rCF, rZF, rFlags];
  case Reg of
(*
TODO: Upgrade RegState handling for all these items, as has been done for rCF
*)
    rA, rAF: FlagsCorrupt := False;
    rZF:   GenFragmentParamName('zftoboolean', Param, 'd');
    rZFA:  GenFragmentParamName('notatoboolean', Param, 'd');
    rNZF:  GenFragmentParamName('nzftoboolean', Param, 'd');
    rNZFA: GenFragmentParamName('atoboolean', Param, 'd');
    rCPLA: GenFragmentParamName('cpla', Param, 'd');
    rCF:
    begin
      GenFragmentParamName('cftoboolean', Param, 'd');
      RegStateSetUnknown(rFlags);
      RegStateSetVariable(rNZF, Param.Variable, Param.VarVersion, rskVarValue);
      Unknowns := [];
    end;
    rNCF:  GenFragmentParamName('ncftoboolean', Param, 'd');
  else
    Assert(False);
  end;
  if FlagsCorrupt then
  begin
    Assert(Options * OptionTests = [],
      'I can''t perform this action whilst preserving A or Flags');
    RegStateSetUnknowns(Unknowns);
  end;
  if Param.Kind = pkVarDest then
    RegStateSetVariable(rA, Param.Variable, Param.VarVersion, rskVarValue);
end;

//==================================================STORE REGISTERS TO VARIABLES

//Basic store of an 8 bit register to an 8-bit variable
//Returns True if we stored the value via the A register (but Reg <> rA)
function GenVarStore8(Reg: TCPUReg;Variable: PVariable;VarVersion: Integer;
  Options: TMoveOptionSet): Boolean;
begin
  Assert(Reg in CPUReg8Bit);

  RegStateSetVariable(Reg, Variable, VarVersion, rskVarValue);

  Result := False;
  case Variable.AddrMode of
    amStack:
      OpLD(rIX, Variable, Reg);
    amStatic:
    begin
      if Reg <> rA then
      begin
        Assert(not (moPreserveA in Options));
        OpLD(rA, Reg);
        RegStateSetVariable(rA, Variable, VarVersion, rskVarValue);
        Result := True;
      end;

      OpLD(Variable, rA);
    end;
  else
    Assert(False);
  end;
end;

//Basic store of a 16 bit register pair to a 16-bit variable
//Returns True if we stored the value via the A register (but Reg <> rA)
function GenVarStore16(Reg: TCPUReg;Variable: PVariable;VarVersion: Integer;
  Options: TMoveOptionSet): Boolean;
begin
  Assert(Reg in CPURegPairs);

  RegStateSetVariable(Reg, Variable, VarVersion, rskVarValue);

  Result := False;
  case Variable.AddrMode of
    amStack{, amStackPtr}:
      OpLD(rIX, Variable, Reg);
    amStatic, amStaticRef:
      OpLD(Variable, Reg);
  else
    Assert(False);
  end;
end;

//Basic store an 8 bit register to the high byte of a 16-bit variable
//Returns True if we stored the value via the A register (but Reg <> rA)
function GenVarStore16High(Reg: TCPUReg;Variable: PVariable;VarVersion: Integer;
  Options: TMoveOptionSet): Boolean;
begin
  Assert(Reg in CPUReg8Bit);

  RegStateSetVariable(Reg, Variable, VarVersion, rskVarValueHigh);

  Result := False;
  case Variable.AddrMode of
    amStack:
      OpLD(rIX, Variable, Reg, 1);
    amStatic:
    begin
      if Reg <> rA then
      begin
        Assert(not (moPreserveA in Options));
        OpLD(rA, Reg);
        RegStateSetVariable(rA, Variable, VarVersion, rskVarValueHigh);
        Result := True;
      end;
      OpLD(Variable, 1, Reg);
    end;
  else
    Assert(False);
  end;
end;

//===================================================STORE LITERALS TO VARIABLES

procedure GenStoreLiteralToVariable(Variable: PVariable;const Value: TImmValue;
  Options: TMoveOptionSet);
var Reg: TCPUReg;
begin
  case GetTypeSize(Variable.UserType) of
    1:
    case Variable.AddrMode of
      amStack:
      begin
        //Is the value already in a register?
        Reg := RegStateFindLiteral8(Value.ToInteger);
        if Reg = rNone then
        begin
          Assert(not (moPreserveA in Options));
          //TODO: If we can't touch A but we can touch HL then use that instead
          Reg := rA;
          GenLoadRegLiteral(rA, Value, []);
        end;

        OpLD(rIX, Variable, Reg);
      end;
      amStatic: //We need to go via A register :(
      begin
        Assert(not (moPreserveA in Options));
        GenLoadRegLiteral(rA, Value, []);
        OpLD(Variable, rA);
      end;
    else
      Assert(False);
    end;
    2:
    begin
      //Is the value already in a register?
      Reg := RegStateFindLiteral16(Value.ToInteger);
      if Reg = rNone then
      begin
        Assert(not (moPreserveHL in Options));

        Reg := rHL;
        GenLoadRegLiteral(rHL, Value, []);
      end;

      case Variable.AddrMode of
        amStack: OpLD(rIX, Variable, Reg);
        amStatic: OpLD(Variable, Reg);
      else
        Assert(False);
      end;
    end;
  else
    Assert(False);
  end;
end;

procedure GenStoreLiteralToVariableHigh(Variable: PVariable;const Value: TImmValue;
  Options: TMoveOptionSet);
var Reg: TCPUReg;
begin
  Assert(GetTypeSize(Variable.UserType) = 2);

  case Variable.AddrMode of
    amStack:
    begin
      //Is the value already in a register?
      Reg := RegStateFindLiteral8(Value.ToInteger);
      if Reg = rNone then
      begin
        Assert(not (moPreserveA in Options));
        //TODO: If we can't touch A but we can touch HL then use that instead
        Reg := rA;
        GenLoadRegLiteral(rA, Value, []);
      end;

      OpLD(rIX, Variable, Reg, 1);
    end;
    amStatic: //We need to go via A register :(
    begin
      Assert(not (moPreserveA in Options));
      GenLoadRegLiteral(rA, Value, []);
      OpLD(Variable, 1, rA);
    end;
  else
    Assert(False);
  end;
end;

//Store a pointer to a literal value in a register
procedure GenStoreLiteralPointerToVariable(Variable: PVariable;const Value: TImmValue;
  Options: TMoveOptionSet);
var Reg: TCPUReg;
  Lab: String;
begin
  Lab := Value.ToLabel;
  Reg := rHL; //The only reg we can use :sigh:

  //Is the value already in a register?
  if not RegStateEqualsLabel(Reg, Lab) then
  begin
    Assert(not (moPreserveHL in Options));

    OpLD(Reg, Lab);
    RegStateSetLabel(Reg, Lab);
  end;

  case Variable.AddrMode of
    amStack: OpLD(rIX, Variable, Reg);
    amStatic: OpLD(Variable, Reg);
  else
    Assert(False);
  end;
end;


procedure GenStoreImm(ILItem: PILItem;Options: TMoveOptionSet);
var V: PVariable;
begin
  Assert(ILItem.Param1.Kind = pkImmediate);
  Assert(ILItem.Dest.Kind = pkVarDest);

  V := ILItem.Dest.Variable;
  Assert(Assigned(V));

  if IsPointeredType(ILItem.Param1.Imm.VarType) then
    //Where the type is always referenced by a pointer, 'Immediate' means load
    //the address of the data in the data segment
    GenStoreLiteralPointerToVariable(V, ILItem.Param1.Imm, Options)
  else
    GenStoreLiteralToVariable(V, ILItem.Param1.Imm, Options);
end;

//=================================================== VALIDATE AND STORE RESULTS

//Stores an 8-bit value to a 16-bit variable, either sign extending or zero extending
//as necessary
procedure GenStoreReg8BitToVar16Bit(Reg: TCPUReg;FromType: PUserType;
  Variable: PVariable;VarVersion: Integer;RangeChecked: Boolean;Options: TMoveOptionSet);
var ViaA: Boolean;
begin
  Assert(Reg in CPUReg8Bit);
  Assert(GetTypeSize(Variable.UserType) = 2);

  //Store register to low byte of variable
  ViaA := GenVarStore8(Reg, Variable, VarVersion, Options);
  RegStateSetVariable(Reg, Variable, VarVersion, rskVarValueLow);
  if ViaA then
    RegStateSetVariable(rA, Variable, VarVersion, rskVarValueLow);

  if IsSignedType(FromType) and
  //If we'e been range checked, and the destination is unsigned then zero extend
    not (RangeChecked and not IsSignedType(Variable.UserType)) then
  begin //Sign extend
    Assert(not (moPreserveA in Options));

    if ViaA then
      GenSignExtend(rA, rA, Options)
    else
      GenSignExtend(Reg, rA, Options);
    GenVarStore16High(rA, Variable, VarVersion, Options);
  end
  else
    //Zero extend (store zero to high byte or variable)
    GenStoreLiteralToVariableHigh(Variable, TImmValue.CreateInteger(0), Options);
end;

//Store the variable value from the register into the variable
//If RangeChecked is True it signifies the value has been range check. If so the
//store routine may be able to simplify the conversion (ie a signed value may be able to
//be zero extended rather than requiring sign extending
procedure GenStoreRegVarValue(Reg: TCPUReg;FromType: PUserType;
  Variable: PVariable;VarVersion: Integer;RangeChecked: Boolean;Options: TMoveOptionSet);
begin
  case Reg of
    rA..rL:
      case GetTypeRegSize(Variable.UserType) of
        1: GenVarStore8(Reg, Variable, VarVersion, Options);
        2: GenStoreReg8BitToVar16Bit(Reg, FromType, Variable, VarVersion, RangeChecked, Options);
      else
        Assert(False);
      end;
    rHL..rBC:
      case GetTypeRegSize(Variable.UserType) of
        1: GenVarStore8(CPURegPairToLow[Reg], Variable, VarVersion, Options);
        2: GenVarStore16(Reg, Variable, VarVersion, Options);
      else
        Assert(False);
      end;
    rIX, rIY:
    begin
      Assert(Variable.AddrMode = amStatic, 'Sorry, can''t store index registers to stack variables');

      OpLD(Variable, Reg);
    end;
    //Note: Flags should have been processed before we get here, to convert them to
    //a register
  else
    Assert(False);
  end;
end;

procedure GenPush16(Reg: TCPUReg;FromType, ToType: TVarType;RangeChecked: Boolean;Options: TMoveOptionSet);
begin
//  case GetTypeSize(FromType) of
//    1: Assert(False, 'TODO: Push 8 to 16');
    {2:} OpPUSH(Reg);
//  else
//    Assert(False);
//  end;
end;

procedure GenPush8(Reg: TCPUReg;FromType, ToType: TVarType;RangeChecked: Boolean;Options: TMoveOptionSet);
begin
  //If the value is a boolean, and it's in a flag (or in a register but needs processing)
  //this call will do than and put the value in A
  case Reg of
    rA: Reg := rAF;
    rC, rE, rL:
    begin //Push via A
      GenRegMove(Reg, rA, False, Options);
      Reg := rAF;
    end;
    rBC, rDE, rHL:
    begin //Push low byte via A
      GenRegMove(CPURegPairToLow[Reg], rA, False, Options);
      Reg := rAF;
    end;
    rB, rD, rH:
      Reg := CPUReg8ToPair[Reg];
  else
    Assert(False, 'Invalid register for BytePush');
  end;

  OpPUSH(Reg);
  AsmInstr('inc sp');  //Adjust stack to only push a single byte
end;

//=====================================================================BRANCHING

//Generates a conditional jump, using Zero or Carry flag, to the given block
//If ZeroFlag is true, the jump uses the Zero flag, otherwise the Carry flag
//If Reverse is True the condition is inverted (i.e. Zero becomes Not Zero,
//Carry Set becomse Carry Clear).
procedure GenCondJump(const Param: TILParam;Reverse: Boolean;BlockID: Integer);
var F: String;
begin
  Assert(Param.Kind = pkCondBranch);

  case Param.Reg of
    rA:
    begin
      GenFragmentParamName('atozf', Param, 'd');
      if Reverse then
        F := 'z'
      else
        F := 'nz';
    end;
    rZF, rZFA:
      if Reverse then
        F := 'nz'
      else
        F := 'z';
    rNZF, rNZFA:
      if Reverse then
        F := 'z'
      else
        F := 'nz';
    rCF:
      if Reverse then
        F := 'nc'
      else
        F := 'c';
    rNCF:
      if Reverse then
        F := 'c'
      else
        F := 'nc';
  else
    raise Exception.Create('TODO Flags');
  end;

  AsmOpcode('jp', F, GetCurrProcName + IntToStr(BlockID));
end;

//Generates an unconditional jump to the given Block.
//If the block immediately follows the current code location no code is generated
//(i.e. fall-through)
procedure GenUncondJump(BlockID: Integer);
begin
  if GetCGBlockID <> BlockID - 1 then
    if BlockID = 0 then
      AsmOpcode('jp', GetCurrProcName, '')
    else
      AsmOpcode('jp', GetCurrProcName + IntToStr(BlockID),'');
end;

//Generate code for an ILItem which is an unconditional jump
procedure GenUncondBranch(ILItem: PILItem);
begin
  Assert(ILItem.GetBranchBlockiD <> -1);
  GenUncondJump(ILItem.GetBranchBlockID);
end;

//Generate a branch after the
procedure GenCondBranch(const Param: TILParam);
var CGBlockID: Integer;
begin
  CGBlockID := GetCGBlockID;

  //If True block is following block then we only need generate conditional jump for False
  if Param.TrueBlockID = CGBlockID + 1 then
    GenCondJump(Param, True, Param.FalseBlockID)
  else
    //If False block is following block then we only need generate conditional jump for True
    if Param.FalseBlockID = CGBlockID + 1 then
      GenCondJump(Param, False, Param.TrueBlockID)
    else
    begin //Otherwise generate both. Condition jump for True...
      GenCondJump(Param, False, Param.TrueBlockID);
      //...and False doesn't 'fall though' so an unconditional jump for it.
      if Param.FalseBlockID <> CGBlockID + 1 then
        GenUncondJump(Param.FalseBlockID);
    end;
end;

//========================================================PROCESSING DEST PARAMS

procedure GenDestParam(const Param: TILParam;FromType: PUserType;RangeCheck: Boolean;
  RangeCheckProc: TRangeCheckProc;Options: TMoveOptionSet);
var Reg: TCPUReg;
begin
  //Conditional branches require raw flags (ie no conversion and storing of result)
  //So handle and return
  if Param.Kind = pkCondBranch then
  begin
    GenCondBranch(Param);
    EXIT;
  end;

  //Convert flags to a value in the A register which can be stored
  Reg := Param.Reg;
  if Reg in [rZF, rZFA, rNZF, rNZFA, rCPLA, rCF, rNCF] then
  begin //Storing a flag to a variable, we need to convert it to a value in the A register
    GenToBoolean(Param.Reg, Param, Options);
    Reg := rA;
  end;

  //TODO: Move range check to /after/ storing.
  //TODO: If stored via A reg range check against value already in A
  //Range check
  if RangeCheck then
    if Assigned(RangeCheckProc) then
      RangeCheckProc(Reg, Options)
    else
    case Param.Kind of
      pkNone: ;
      pkVarDest:
        GenRangeCheck(Reg, FromType, Param.Variable.UserType, nil, []);
      pkPush, pkPushByte:
        GenRangeCheck(Reg, FromType, Param.PushType, nil, []);
    else
      Assert(False);
    end;

  case Param.Kind of
    pkNone: ; //No param to load
    pkVarDest:
      //RangeCheck here signifies that the value has /been/ range checked, which
      //may allow simplification of a sign extend operation
      GenStoreRegVarValue(Reg, FromType, Param.Variable, Param.VarVersion, RangeCheck, Options);
    pkPush: GenPush16(Reg, UTToVT(FromType), UTToVT(Param.PushType), RangeCheck, Options);
    pkPushByte: GenPush8(Reg, UTToVT(FromType), UTToVT(Param.PushType), RangeCheck, Options);
  else
    System.Assert(False, 'Invalid param kind for param store');
  end;
end;

//Takes the result from the register specified in ILItem.DestAlloc and stores it
//into the location specified in ILItem.Dest
//If a type conversion is to take place then:
// * if validation is enabled, will generate code to ensure the value will fit into
//the destination type
// * generates code to handle the type conversion (if necessary)
procedure StoreAfterPrim(ILItem: PILItem;Prim: PPrimitive);
var RangeCheckProc: TRangeCheckProc;
  ToType: PUserType;
  FromType: PUserType;
begin
  //Update CPU state but ignore kinds which don't have a Variable
  if (ILItem.Dest.Reg <> rNone) and not (ILItem.Dest.Kind in [pkCondBranch, pkPush, pkPushByte]) then
    RegStateSetVariable(ILItem.Dest.Reg, ILItem.Dest.Variable, ILItem.Dest.VarVersion, rskVarValue);

  //If we have an optimised range check proc for the primitive & type conversion
  //find it and pass to the store routine
  ToType := nil;
  case ILItem.Dest.Kind of
    pkVarDest: ToType := ILItem.Dest.Variable.UserType;
    pkPush, pkPushByte: ToType := ILItem.Dest.PushType;
  end;
  RangeCheckProc := nil;
  if ToType <> nil then
    if not ToType.IsSubRange then
      RangeCheckProc := GetOptimisedRangeCheckProc(Prim, ToType.VarType);

  //We assume the result of an expression is a generic value - NOT a subrange and
  //therefore needs range checking into the DestType (if range checks are on).
  FromType := RemoveSubRange(ILItem.ResultType);
  GenDestParam(ILItem.Dest, FromType, cgRangeCheck in ILItem.Dest.Flags, RangeCheckProc, []);
end;

end.

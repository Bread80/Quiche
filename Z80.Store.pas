(*
* Store values from into variables

All functions within this unit update the data in Z80.CPUState to reflect any code
generated.
*)
unit Z80.Store;

interface
uses Def.IL, Def.Primitives, Def.QTypes,
  Z80.CPU, Z80.Validation;

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
procedure GenDestParam(const Param: TILParam;FromType: TVarType;RangeCheck: Boolean;
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
  Def.Variables,
  CodeGen,
  Z80.CodeGen, Z80.CPUState;

//Generates the code to convert a boolean value from various sources into a boolean
//value in A
procedure GenToBoolean(Reg: TCPUReg;const Param: TILParam;Options: TMoveOptionSet);
var FlagsCorrupt: Boolean;
begin
  FlagsCorrupt := True; //Assume
  case Reg of
    rA, rAF: FlagsCorrupt := False;
    rZF:   GenLibraryParamProc('zftoboolean', Param, 'd');
    rZFA:  GenLibraryParamProc('notatoboolean', Param, 'd');
    rNZF:  GenLibraryParamProc('nzftoboolean', Param, 'd');
    rNZFA: GenLibraryParamProc('atoboolean', Param, 'd');
    rCPLA: GenLibraryParamProc('cpla', Param, 'd');
    rCF:   GenLibraryParamProc('cftoboolean', Param, 'd');
    rNCF:  GenLibraryParamProc('ncftoboolean', Param, 'd');
  else
    Assert(False);
  end;
  if FlagsCorrupt then
  begin
    Assert(Options * [moPreserveCF, moPreserveOtherFlags, moPreserveA] = [],
      'I can''t perform this action whilst preserving A or Flags');
    RegStateSetUnknowns([rCF, rZF, rFlags]);
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

  Result := False;
  case Variable.Storage of
    vsStack:
      OpLD(rIX, Variable, Reg);
    vsStatic:
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

  Result := False;
  case Variable.Storage of
    vsStack:
      OpLD(rIX, Variable, Reg);
    vsStatic:
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

  Result := False;
  case Variable.Storage of
    vsStack:
      OpLD(rIX, Variable, Reg, 1);
    vsStatic:
    begin
      if Reg <> rA then
      begin
        Assert(not (moPreserveA in Options));
        OpLD(rA, Reg);
        RegStateSetVariable(rA, Variable, VarVersion, rskVarValue);
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

  case GetTypeSize(Variable.VarType) of
    1:
    case Variable.Storage of
      vsStack:
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
      vsStatic: //We need to go via A register :(
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

      case Variable.Storage of
        vsStack: OpLD(rIX, Variable, Reg);
        vsStatic: OpLD(Variable, Reg);
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
  Assert(GetTypeSize(Variable.VarType) = 2);

  case Variable.Storage of
    vsStack:
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
    vsStatic: //We need to go via A register :(
    begin
      Assert(not (moPreserveA in Options));
      GenLoadRegLiteral(rA, Value, []);
      OpLD(Variable, 1, rA);
    end;
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

  GenStoreLiteralToVariable(V, ILItem.Param1.Imm, Options);
end;

//=================================================== VALIDATE AND STORE RESULTS

//Stores an 8-bit value to a 16-bit variable, either sign extending or zero extending
//as necessary
procedure GenStoreReg8BitToVar16Bit(Reg: TCPUReg;FromType: TVarType;
  Variable: PVariable;VarVersion: Integer;RangeChecked: Boolean;Options: TMoveOptionSet);
var ViaA: Boolean;
begin
  Assert(Reg in CPUReg8Bit);
  Assert(GetTypeSize(Variable.VarType) = 2);

  //Store register to low byte of variable
  ViaA := GenVarStore8(Reg, Variable, VarVersion, Options);

  if IsSignedType(FromType) and
  //If we'e been range checked, and the destination is unsigned then zero extend
    not (RangeChecked and not IsSignedType(Variable.VarType)) then
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
procedure GenStoreRegVarValue(Reg: TCPUReg;FromType: TVarType;
  Variable: PVariable;VarVersion: Integer;RangeChecked: Boolean;Options: TMoveOptionSet);
begin
  case Reg of
    rA..rL:
      case GetTypeSize(Variable.VarType) of
        1: GenVarStore8(Reg, Variable, VarVersion, Options);
        2: GenStoreReg8BitToVar16Bit(Reg, FromType, Variable, VarVersion, RangeChecked, Options);
      else
        Assert(False);
      end;
    rHL..rBC:
      case GetTypeSize(Variable.VarType) of
        1: GenVarStore8(CPURegPairToLow[Reg], Variable, VarVersion, Options);
        2: GenVarStore16(Reg, Variable, VarVersion, Options);
      else
        Assert(False);
      end;
    rIX, rIY:
    begin
      Assert(Variable.Storage = vsStatic, 'Sorry, can''t store index registers to stack variables');

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
  Assert((FromType = ToType) or (FromType = vtUnknown), 'PUSHes can''t do type conversions');

//  case GetTypeSize(FromType) of
//    1: Assert(False, 'TODO: Push 8 to 16');
    {2:} OpPUSH(Reg);
//  else
//    Assert(False);
//  end;
end;

procedure GenPush8(Reg: TCPUReg;FromType, ToType: TVarType;RangeChecked: Boolean;Options: TMoveOptionSet);
begin
  Assert((FromType = ToType) or (FromType = vtUnknown), 'PUSHes can''t do type conversions');

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
  Instr('inc sp');  //Adjust stack to only push a single byte
end;

//=====================================================================BRANCHING

//Generates a conditional jump, using Zero or Carry flag, to the given block
//If ZeroFlag is true, the jump uses the Zero flag, otherwise the Carry flag
//If Reverse is True the condition is inverted (i.e. Zero becomes Not Zero,
//Carry Set becomse Carry Clear).
procedure GenCondJump(const Param: TILParam;Reverse: Boolean;BlockID: Integer);
var F: String;
begin
//  Reverse := Reverse xor ILItem.BranchInvert;
  Assert(Param.Kind = pkCondBranch);

  case Param.Reg of
    rA:
    begin
//      if ILItem.OpType <> rtBoolean then
        GenLibraryParamProc('atozf', Param, 'd');
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

  Opcode('jp', F, GetCurrProcName + IntToStr(BlockID));
end;

//Generates an unconditional jump to the given Block.
//If the block immediately follows the current code location no code is generated
//(i.e. fall-through)
procedure GenUncondJump(BlockID: Integer);
begin
  if GetCurrBlockID <> BlockID - 1 then
    Opcode('jp', GetCurrProcName + IntToStr(BlockID),'');
end;

//Generate code for an ILItem which is an unconditional jump
procedure GenUncondBranch(ILItem: PILItem);
begin
  Assert(ILItem.GetBranchBlockiD <> -1);
  GenUncondJump(ILItem.GetBranchBlockID);
end;

//Generate a branch after the
procedure GenCondBranch(const Param: TILParam);
begin
  //If True block is following block then generate conditional jump for False
  if Param.TrueBlockID = GetCurrBlockID + 1 then
    GenCondJump(Param, True, Param.FalseBlockID)
  else
  begin //Otherwise generate condition jump for True...
    GenCondJump(Param, False, Param.TrueBlockID);
    //...and False doesn't 'fall though' then an unconditional jump for it.
    if Param.FalseBlockID <> GetCurrBlockID + 1 then
      GenUncondJump(Param.FalseBlockID);
  end;
end;

//========================================================PROCESSING DEST PARAMS

procedure GenDestParam(const Param: TILParam;FromType: TVarType;RangeCheck: Boolean;
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
        GenRangeCheck(Reg, FromType, Param.Variable.VarType, nil, []);
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
    pkPush: GenPush16(Reg, FromType, Param.PushType, RangeCheck, Options);
    pkPushByte: GenPush8(Reg, FromType, Param.PushType, RangeCheck, Options);
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
  VarType: TVarType;
begin
  //If we have an optimised range check proc for the primitive & type conversion
  //find it and pass to the store routine
  VarType := vtUnknown;
  case ILItem.Dest.Kind of
    pkVarDest: VarType := ILItem.Dest.Variable.VarType;
    pkPush, pkPushByte: VarType := ILItem.Dest.PushType;
  end;
  RangeCheckProc := GetOptimisedRangeCheckProc(Prim, VarType);

  GenDestParam(ILItem.Dest, ILItem.ResultType, cgRangeCheck in ILItem.Dest.Flags, RangeCheckProc, []);
end;

end.

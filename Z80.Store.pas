(*
* Store values from into variables

All functions within this unit update the data in Z80.CPUState to reflect any code
generated.
*)
unit Z80.Store;

interface
uses ILData, PrimitivesEx, QTypes,
  Z80.CPU;

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
procedure GenStoreParam(const Param: TILParam;FromType: TVarType;RangeCheck: Boolean;
  Options: TMoveOptionSet);

//Takes the result from the register specified in ILItem.DestAlloc and stores it
//into the location specified in ILItem.Dest
//If a type conversion is to take place then:
// * if validation is enabled, will generate code to ensure the value will fit into
//the destination type
// * generates code to handle the type conversion (if necessary)
procedure StoreAfterPrimNG(ILItem: PILItem;Prim: PPrimitiveNG);

implementation
uses Classes, SysUtils,
  Variables,
  CodeGen, Z80.CodeGen, Z80.CPUState, Z80.Validation;

//==================================================Store Registers to Variables

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


//===================================================Store literals to Variables

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
  Reg: TCPUReg;
begin
  Assert(ILItem.Param1.Kind = pkImmediate);
  Assert(ILItem.Dest.Kind = pkVarDest);

  V := ILItem.Dest.Variable;
  Assert(Assigned(V));

  GenStoreLiteralToVariable(V, ILItem.Param1.Imm, Options);
end;


//================================ VALIDATE AND STORE RESULTS

//Stores a value in an 8-bit register to an 8-bit variable
function GenStoreReg8BitToVar8Bit(Reg: TCPUReg;Variable: PVariable;VarVersion: Integer;
  FromType: TVarType;Options: TMoveOptionSet): Boolean;
var ViaA: Boolean;
begin
  Assert(Reg in CPUReg8Bit);
  Assert(GetTypeSize(Variable.VarType) = 1);

  ViaA := GenVarStore8(Reg, Variable, VarVersion, Options);
end;

//Stores the low byte of a register pair to an 8-bit variable
function GenStoreReg16BitToVar8Bit(Reg: TCPUReg;Variable: PVariable;VarVersion: Integer;
  FromType: TVarType;Options: TMoveOptionSet): Boolean;
var ViaA: Boolean;
begin
  Assert(Reg in CPURegPairs);
  Assert(GetTypeSize(Variable.VarType) = 1);

  ViaA := GenVarStore8(CPURegPairToLow[Reg], Variable, VarVersion, Options);
end;

//Stores a 16-bit value in registers to a 16-bit variable
function GenStoreReg16BitToVar16Bit(Reg: TCPUReg;Variable: PVariable;VarVersion: Integer;
  FromType: TVarType;Options: TMoveOptionSet): Boolean;
var ViaA: Boolean;
begin
  Assert(Reg in CPURegPairs);
  Assert(GetTypeSize(Variable.VarType) = 2);

  ViaA := GenVarStore16(Reg, Variable, VarVersion, Options);
end;

//Stores an 8-bit value to a 16-bit variable, either sign extending or zero extending
//as necessary
function GenStoreReg8BitToVar16Bit(Reg: TCPUReg;Variable: PVariable;VarVersion: Integer;
  FromType: TVarType;RangeChecked: Boolean;Options: TMoveOptionSet): Boolean;
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

//Stores a 16 bit value in an index register to a 16-bit variable
function GenStoreXYRegToVar16Bit(Reg: TCPUReg;Variable: PVariable;VarVersion: Integer;
  FromType: TVarType;Options: TMoveOptionSet): Boolean;
begin
  Assert(Reg in [rIX, rIY]);
  Assert(GetTypeSize(Variable.VarType) = 2);
  Assert(Variable.Storage = vsStatic, 'Sorry, can''t store index registers to stack variables');

  OpLD(Variable, Reg);
end;


//Store the variable value from the register into the variable
function StoreRegVarValue(Reg: TCPUReg;Variable: PVariable;VarVersion: Integer;
  FromType: TVarType;RangeCheck: Boolean;Options: TMoveOptionSet): Boolean;
begin
  //Range check as we store
  if RangeCheck then
    GenRangeCheck(Reg, FromType, Variable.VarType, nil, []);

  case Reg of
    rA..rL:
      case GetTypeSize(Variable.VarType) of
        1: Result := GenStoreReg8BitToVar8Bit(Reg, Variable, VarVersion, FromType, Options);
        2: Result := GenStoreReg8BitToVar16Bit(Reg, Variable, VarVersion, FromType, RangeCheck, Options);
      else
        Assert(False);
      end;
    rHL..rBC:
      case GetTypeSize(Variable.VarType) of
        1: Result := GenStoreReg16BitToVar8Bit(Reg, Variable, VarVersion, FromType, Options);
        2: Result := GenStoreReg16BitToVar16Bit(Reg, Variable, VarVersion, FromType, Options);
      else
        Assert(False);
      end;
    rIX, rIY: Result := GenStoreXYRegToVar16Bit(Reg, Variable, VarVersion, FromType, Options);
    //Note: Flags should have been processed before we get here, to convert them to
    //a register
  else
    Assert(False);
  end;
  Result := True;
end;

procedure GenStoreParam(const Param: TILParam;FromType: TVarType;RangeCheck: Boolean;
  Options: TMoveOptionSet);
begin
  case Param.Kind of
    pkNone: ; //No param to load
    pkVarDest:
      if Param.Reg in [rZF, rZFA, rNZF, rNZFA, rCPLA, rCF, rNCF] then
      begin //Storing a flag to a variable, we need to convert it to a value in the A register
        GenToBoolean(Param.Reg, Param, Options);
        StoreRegVarValue(rA, Param.Variable, Param.VarVersion, vtBoolean, RangeCheck, Options);
      end
      else
       StoreRegVarValue(Param.Reg, Param.Variable, Param.VarVersion, FromType, RangeCheck, Options);
    //TODO - process more Dest Kinds (move from StoreAfterPrimNG)
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
procedure StoreAfterPrimNG(ILItem: PILItem;Prim: PPrimitiveNG);
var
  DestType: TVarType;
  ValProcName: String;
begin
  case ILItem.Dest.Kind of
    pkNone: ;
    pkVarDest:
    begin
      //NOTE: Do explicit Range checking here because we need to use the Result data in Prim...
      if cgRangeCheck in ILItem.Dest.Flags then
        GenRangeCheck(ILItem.Dest.Reg, ILItem.ResultType, ILItem.Dest.Variable.VarType, Prim, []);

      //...so pass False to RangeCheck parameter
      GenStoreParam(ILItem.Dest, ILItem.ResultType, False, []);
    end;
    pkCondBranch:
      CondBranchAfterPrimNG(ILItem);
    pkPush:
      GenLibraryProc('push_word', ILItem);     //The stack
    pkPushByte:
    begin
      //If the value is a boolean, and it's in a flag (or in a register but needs processing)
      //this call will do than and put the value in A
      GenToBoolean(ILItem.Dest.Reg, ILItem.Dest, []);
      GenLibraryProc('push_byte_a', ILItem);  //Single byte on the stack
    end;
    else
      Assert(False, 'Invalid Dest Location');
  end;
end;

end.

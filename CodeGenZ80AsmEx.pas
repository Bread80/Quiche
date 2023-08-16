unit CodeGenZ80AsmEx;


interface
uses Classes, MErrors;

procedure InitialiseCodeGen(PlatformFile, QuicheLibrary: String);

function CodeGen(ScopeName: String;AnAsmScope: TStringlist): TAssembleError;

procedure SaveAssemblyFile(FileName: String);

var
  CodeGenErrorString: String;
  CurrErrorCount: Integer;  //In current routine
  TotalErrorCount: Integer; //In current build

implementation
uses CodeLibrary, ILData, SysUtils, Variables, ParserBase, QTypes, Operators, PrimitivesEx, Compiler;

var
  AsmFull: TStringList;
  AsmScope: TStringList;

procedure Line(S: String);
begin
  AsmFull.Add(S);
  if Assigned(AsmScope) then
    AsmScope.Add(S);
end;

procedure Lines(S: String);
begin
  Line(S);
end;

procedure SaveAssemblyFile(FileName: String);
begin
  AsmFull.SaveToFile(Filename);
end;


var CurrProcName: String;
var StackFrameSize: Integer;
var CurrBlockID: Integer;
  CurrSourceLineNo: Integer;
  LabelIndex: Integer;  //Used to generate unique labels. Cleared at the start of each routine

procedure Error(Msg: String);
begin
  inc(CurrErrorCount);
  inc(TotalErrorCount);
  Line('ERROR: ' + Msg + ' in ' + CurrProcName);
end;

function GetUniqueLabel: String;
begin
  Result := '.x'+IntToStr(LabelIndex);
  inc(LabelIndex);
end;

function VarToOffset(Variable: PVariable): Integer;
begin
  Result := {StackFrameSize - }Variable.Offset;
end;

function CodeOffset(Offset: Integer): String;
begin
  if Offset < 0 then
    Result := '-$' + IntToHex(-Offset, 2)
  else
    Result := '+$' + IntToHex(Offset, 2);
end;

function HexByte(Value: Byte): String;
begin
  Result := '$' + IntToHex(Value, 2).ToLower;
end;

function ImmLoByte(Param: PILParam): String;
begin
  Result := '$' + IntToHex(lo(Param.ImmValue), 2).ToLower;
end;

function ImmHiByte(Param: PILParam): String;
begin
  Result := '$' + IntToHex(hi(Param.ImmValue), 2).ToLower;
end;

function ImmWord(Param: PILParam): String;
begin
  Result := '$' + IntToHex(Param.ImmValue, 4).ToLower;
end;

function IXOffset(Offset: Integer): String;
begin
  Result := '(ix' + CodeOffset(Offset) + ')';
end;

//Write an entire instruction line
procedure Instr(S: String);
begin
  Line('  '+S);
end;

procedure Opcode(Op,P1,P2: String);
var S: String;
begin
  S := '  ' + Op;
  if P1 <> '' then
  begin
    S := S + ' ' + P1;
    if P2 <> '' then
      S := S + ',' + P2;
  end;
  Line(S);
end;

procedure GenLabel(Name: String);
begin
  Line(Name + ':');
end;

procedure GenLibraryProc(ProcName: String;ILItem: PILItem);
var Code: String;
begin
  if ProcName.Chars[0] = ':' then
    Instr('call ' + ProcName.SubString(1))
  else
  begin
    Code := CodeLibrary.CodeSub(ProcName, ILItem);
    if Code = '' then
      raise Exception.Create('Validation library coed not found: ' + ProcName);
    Lines(Code);
  end;
end;

procedure GenCode(ProcName: String;ILItem: PILItem);
var
  Prim: PPrimitive;
begin
  if ProcName <> '' then
    if ProcName.Chars[0] = ':' then
      Instr('call ' + ProcName.SubString(1))
    else
    begin
      Prim := PrimFindByProcName(ProcName);
      if Assigned(Prim) then
        Prim.Proc(ILItem)
      else
        GenLibraryProc(ProcName, ILItem);
    end;
end;

//Sign extend register RIn to ROut
//Corrupts A and Flags
procedure GenSignExtend(RIn, ROut: Char);
begin
  if RIn <> 'a' then
    Instr('ld a,'+RIn);  //Move value to A
  Instr('rla');       //Move sign bit to Carry
  Instr('sbc a,a');   //If carry we get -1, otherwise 0
  if ROut <> 'a' then
    Instr('ld '+ROut+',a');  //Move result to register
end;

//Branching

//Generates an unconditional jump to the given Block.
//If the block immediately follows the current code location no code is generated
//(i.e. fall-through)
procedure GenUncondJump(BlockID: Integer);
begin
  if CurrBlockID <> BlockID - 1 then
    Opcode('jp', CurrProcName + IntToStr(BlockID),'');
end;

procedure GenUncondBranch(ILItem: PILItem);
begin
  GenUncondJump(ILItem.TrueBlockID);
end;

//Generates a conditional jump, using Zero or Carry flag, to the given block
//If ZeroFlag is true, the jump uses the Zero flag, otherwise the Carry flag
//If Reverse is True the condition is inverted (i.e. Zero becomes Not Zero,
//Carry Set becomse Carry Clear).
procedure GenCondJump(Flags: TAllocLoc;Reverse: Boolean;BlockID: Integer);
var F: String;
begin
  case Flags of
    plZF, plZFA:
      if Reverse then
        F := 'nz'
      else
        F := 'z';
    plNZF, plNZFA:
      if Reverse then
        F := 'z'
      else
        F := 'nz';
    plCF:
      if Reverse then
        F := 'nc'
      else
        F := 'c';
    plNCF:
      if Reverse then
        F := 'c'
      else
        F := 'nc';
    else
    raise Exception.Create('TODO Flags');
  end;

  Opcode('jp', F, CurrProcName + IntToStr(BlockID));
end;

(*
//Generates a conditional jump for the ILItem. The ILItem must have already been
//validated as being a conditional jump (DestType).
//If either destination Block immediately follows the current code location,
//that code-path will 'fall-through' without any code being generated. The jump
//condition for the remaining code will be inverted as necessary.
//Other parameters are as per GenCondJump
procedure GenCondBranch(ILItem: PILItem; ZeroFlag: Boolean;Invert: Boolean);
begin
  if ILItem.TrueBlockID = CurrBlockID + 1 then
    GenCondJump(ZeroFlag, not Invert, ILItem.FalseBlockID)
  else
  begin
    GenCondJump(ZeroFlag, Invert, ILItem.TrueBlockID);
    if ILItem.FalseBlockID <> CurrBlockID + 1 then
      GenUncondJump(ILItem.FalseBlockID);
  end;
end;

procedure GenCondBranch8(ILItem: PILItem);
begin
  GenLoadParam8('a', @ILItem.Param1);
  Instr('and a');
  GenCondBranch(ILItem, True, True);
end;
*)

//Code generators

//====================================

procedure ProcError(ILItem: PILItem);
begin
  Error('No operation specified or illegal operation')
end;

procedure ProcEmpty(ILItem: PILItem);
begin
  //Do nothing
end;

//===================================Validation
//aka overflow checking

//Raise an overflow error if bit 7 of the DestAlloc register is set
//Preserves all registers. Corrupts flags
procedure ProcDestB7SetOverflow(ILItem: PILItem);
var R: Char;
begin
  R := AllocLocToReg8[ILItem.DestAlloc];
  if R = 'a' then
  begin //A reg
    Instr('and a');
    Instr('jp m,raise_overflow');
  end
  else
  begin
    OpCode('bit','7',R);
    Instr('jp nz,raise_overflow');
  end;
end;

//Raise an overflow error if bit 15 of the DestAlloc register is set
//Preserves all registers. Corrupts flags
procedure ProcDestB15SetOverflow(ILItem: PILItem);
var R: Char;
begin
  R := AllocLocToHighReg[ILItem.DestAlloc];
  if R = 'a' then
  begin //A reg
    Instr('and a');
    Instr('jp m,raise_overflow');
  end
  else
  begin
    OpCode('bit','7',R);
    Instr('jp nz,raise_overflow');
  end;
end;

//====================================Assigns

//8 bit immediate value, extended to 16 bit
procedure ProcAssignS8S16NI(ILItem: PILItem);
//var Variable: PVariable;
begin
//  Variable := ILDestToVariable(@ILItem.Dest);

  //Stack Var
//  VarToOffset(Variable);
  case ILItem.Param1.ImmType of
//    vtByte: GenLibraryProc('assignoffset16imm8', ILItem);
    vtInt8:
      if ILItem.Param1.ImmValue >= $80 then
        GenLibraryProc('assignoffset16imm8neg', ILItem)
      else
        GenLibraryProc('assignoffset16imm8', ILItem);
  else
    raise Exception.Create('Invalid Assign: Param1 must be 8 bit type');
  end;
end;



(*
//---------------------------------Branching
procedure ProcBranch(ILItem: PILItem;Prim: PPrimitive);
begin
  if ILItem.DestType = dtBranch then
    GenUncondJump(ILItem.BranchBlockID)
  else if ILItem.DestType = dtCondBranch then
    //A conditional jump with a single boolean value (I.e. no operation)
    GenCondBranch8(ILItem)
  else
    Error('Invalid DestType for a branch Op')
end;
*)
//=====================================Maths

(*
procedure ProcNegateU8(ILItem: PILItem;Prim: PPrimitive);
begin
  GenLoadParam8('a',@ILItem.Param1);
  Instr('neg');     //Sets carry if input <> 0
  Instr('ld l,a');
  Instr('sbc a,a'); //High byte will be zero if input was zero, otherwise -1
  Instr('ld h,a');
  GenStoreDest16(@ILItem.Dest, 'h','l');
end;

procedure ProcNegateU16(ILItem: PILItem;Prim: PPrimitive);
begin
  Line('  ld l,0');
  Line('  ld h,l');
  GenLoadParam16('d','e',@ILItem.Param1);
  Instr('and a');     //Clear carry
  Instr('sbc hl,de');
  if cgOverflowCheck in ILItem.CodeGenFlags then
    //Overflow if result < -32768. I.e if positive
    Instr('jp p,raise_overflow');
  GenStoreDest16(@ILItem.Dest, 'h','l');
end;

procedure ProcNegateS16(ILItem: PILItem;Prim: PPrimitive);
begin
  GenLoadParam16('h','l',@ILItem.Param1);
  if cgOverflowCheck in ILItem.CodeGenFlags then
  begin
    Instr('call negate_HL_and_test_if_INT');
    Instr('jp nc,raise_overflow');
  end
  else
  begin
    Instr('xor a');
    Instr('sub l');
    Instr('ld l,a');
    Instr('sbc a,a');
    Instr('sub h');
    Instr('ld h,a');
  end;

  GenStoreDest16(@ILItem.Dest, 'h','l');
end;


procedure ProcSHL8(ILItem: PILItem;Prim: PPrimitive);
var RType: TVarType;
begin
  GenLoadParam8('c',@ILItem.Param1);

  RType := ILParamToVarType(@ILItem.Param2);
  if TypeSize[RType] = 2 then
  begin
    GenLoadParam16('a','b', @ILItem.Param2);
    Instr('call left_shift_8_by_16');
  end
  else
  begin
    GenLoadParam8('b', @ILItem.Param2);
    Instr('call left_shift_8_by_8');
  end;
  GenStoreILItem8(ILItem, 'a');
end;

procedure ProcSHL16(ILItem: PILItem;Prim: PPrimitive);
var RType: TVarType;
begin
  GenLoadParam16('h','l',@ILItem.Param1);

  RType := ILParamToVarType(@ILItem.Param2);
  if TypeSize[RType] = 2 then
  begin
    GenLoadParam16('a','b', @ILItem.Param2);
    Instr('call left_shift_16_by_16');
  end
  else
  begin
    GenLoadParam8('b', @ILItem.Param2);
    Instr('call left_shift_16_by_8');
  end;
  GenStoreDest16(@ILItem.Dest, 'h','l');
end;

procedure ProcSHR8(ILItem: PILItem;Prim: PPrimitive);
var RType: TVarType;
begin
  GenLoadParam8('c',@ILItem.Param1);

  RType := ILParamToVarType(@ILItem.Param2);
  if TypeSize[RType] = 2 then
  begin
    GenLoadParam16('a','b', @ILItem.Param2);
    Instr('call right_shift_8_by_16');
  end
  else
  begin
    GenLoadParam8('b', @ILItem.Param2);
    Instr('call right_shift_8_by_8');
  end;
  GenStoreILItem8(ILItem, 'a');
end;

procedure ProcSHR16(ILItem: PILItem;Prim: PPrimitive);
var RType: TVarType;
begin
  GenLoadParam16('h','l',@ILItem.Param1);

  RType := ILParamToVarType(@ILItem.Param2);
  if TypeSize[RType] = 2 then
  begin
    GenLoadParam16('a','b', @ILItem.Param2);
    Instr('call right_shift_16_by_16');
  end
  else
  begin
    GenLoadParam8('b', @ILItem.Param2);
    Instr('call right_shift_16_by_8');
  end;
  GenStoreDest16(@ILItem.Dest, 'h','l');
end;

//------------------------------Comparisons
{Comparisons
Test Actual         Load    Assign  Jump (beyond code!)
A= B A-B=zero       Either  z=1     jp nz
A<>B A-B=not zero   Either  z=0     jp z
A< B A-B=carry      Normal  c=0     jp nc              1<2  t 1-2 c    1<1  1-1 nc
A>=B A-B=not carry  Normal  c=1     jp c               1>=2 f 1-2 c    1<=1 1-1 nc
A> B B-A=carry      Reverse c=1     jp c               1>2  f 2-1 nc   1>1  1-1 nc
A<=B B-A=not carry  Reverse c=0     jp nc              1<=2 t 2-1 nc   1<=1 1-1 nc
}
//The Dest part of an equality operation
//Either a store or conditional jump
//If Invert is True the test will be inverted
//The code at this point has:
//  For a store: A=0 or non zero (False or True)
//  For a branch: Result in Zero flag
procedure GenEqualDest8(ILItem: PILItem;Invert: Boolean);
begin
  case ILItem.DestType of
  dtData:
    begin
      GenAToBool(not Invert);       //Convert A to a boolean (0,1)
      GenStoreILItem8(ILItem, 'a');
    end;
  dtCondBranch:
    GenCondBranch(ILItem, True, Invert);
  else
    Line('Unsupported DestType for Equal');
  end;
end;

//If Invert the test result will be inverted
procedure GenEqual8(ILItem: PILItem;Invert: Boolean);
var Op: String;
  Variable: PVariable;
const opCP= 'cp';
  opSUB= 'sub';
begin
  //Store result or conditional branch?
  //If we're storing the result we'll use SUB to give A=0 or A<>0
  //for a jump we'll use a CP so we can use the Z flag without trashing A
  if ILItem.DestType = dtData then
    Op := opSUB
  else
    Op := opCP;

  GenLoadParam8('a', @ILItem.Param1);
  if ILItem.Param2.Loc = locImmediate then
    Opcode(Op, ImmByte(@ILItem.Param2), '')
  else
  begin
    Variable := ILParamToVariable(@ILItem.Param2);
    case ILItem.Param2.Loc of
      locVar, locTemp:
        Opcode(Op, IXOffset(VarToOffset(Variable)), '');
    else
      raise exception.Create('Equal8 ToDo');
    end;
  end;

  GenEqualDest8(ILItem, Invert);
end;

procedure ProcEqual8(ILItem: PILItem;Prim: PPrimitive);
begin
  GenEqual8(ILItem, False);
end;

procedure ProcNotEqual8(ILItem: PILItem;Prim: PPrimitive);
begin
  GenEqual8(ILItem, True);
end;

//Convert ZF to a boolean (0,1), to conditional jump
//On entry if converting to a bool A must be zero
procedure GenEqualDest16(ILItem: PILItem;Invert: Boolean);
var Lab: String;
begin
  case ILItem.DestType of
  dtData:
    begin
      Lab := GetUniqueLabel;
      if Invert then
        Opcode('jr','z',Lab)
      else
        Opcode('jr','nz',Lab);
      Instr('inc a');
      GenLabel(Lab);
      GenStoreILItem8(ILItem, 'a');
    end;
  dtCondBranch:
    GenCondBranch(ILItem, True, Invert);
  else
    Line('Unsupported DestType for Equal');
  end;
end;

//If Invert the test result will be inverted
procedure GenEqual16(ILItem: PILItem;Invert: Boolean);
begin
  GenLoadParam16('h','l', @ILItem.Param1);
  GenLoadParam16('d','e',@ILItem.Param2);

  //Operation
  if ILItem.DestType = dtData then
    Instr('xor a')    //A=0; clear carry
  else
    Instr('and a');   //Clear carry
  Instr('sbc hl,de');

  GenEqualDest16(ILItem, Invert);
end;

procedure ProcEqual16(ILItem: PILItem;Prim: PPrimitive);
begin
  GenEqual16(ILItem, False);
end;

procedure ProcNotEqual16(ILItem: PILItem;Prim: PPrimitive);
begin
  GenEqual16(ILItem, True);
end;

//Convert ZF to a boolean (0,1)
procedure GenEqualDestM16(ILItem: PILItem;Invert: Boolean);
begin
  if ILItem.DestType = dtData then
    Instr('ld a,0');

  GenEqualDest16(ILItem, Invert);
end;

procedure GenEqualM16(ILItem: PILItem;Invert: Boolean);
var LType, RType: TVarType;
  LParam, RParam: PILParam;
begin
  //We MUST have one signed and one unsigned parameter
  //Assign parameters so the signed one is on the 'left' (HL)
  LType := ILParamToVarType(@ILItem.Param1);
  RType := ILParamToVarType(@ILItem.Param2);
  if LType in SignedTypes then
  begin
    Assert(not (RType in SignedTypes));
    LParam := @ILItem.Param1;
    RParam := @ILItem.Param2;
  end
  else
  begin
    Assert(RType in SignedTypes);
    LParam := @ILItem.Param2;
    RParam := @ILItem.Param1;
  end;

  GenLoadParam16('h','l', LParam);
  GenLoadParam16('d','e', RParam);
  Instr('call equal_m16');
  GenEqualDestM16(ILItem, Invert);
end;

procedure ProcEqualM16(ILItem: PILItem;Prim: PPrimitive);
begin
  GenEqualM16(ILItem, False);
end;

procedure ProcNotEqualM16(ILItem: PILItem;Prim: PPrimitive);
begin
  GenEqualM16(ILItem, True);
end;


//The Dest part of a comparison operation
//Either a store or conditional jump
//If Invert is True the test will be inverted
//The code at this point has the Result in the Carry flag
procedure GenCompDest(ILItem: PILItem;Invert: Boolean);
begin
  case ILItem.DestType of
  dtData:
    begin
      if not Invert then
        Line('  ccf');
      Line('  sbc a,a');
      Line('  inc a');

      GenStoreILItem8(ILItem, 'a');
    end;
  dtCondBranch:
    GenCondBranch(ILItem, False, Invert);
  else
    Line('Unsupported DestType for Comparison');
  end;
end;

//Swap = swap order of parameters
//Invert = invert (logical NOT) result
procedure GenComp8(ILItem: PILItem;Swap, Invert: Boolean);
var
  Left: PILParam;
  Right: PILParam;
  Variable: PVariable;
begin
  if Swap then
  begin
    Left := @ILItem.Param2;
    Right := @ILItem.Param1;
  end
  else
  begin
    Left := @ILItem.Param1;
    Right := @ILItem.Param2;
  end;

  GenLoadParam8('a', Left);
  if Right.Loc = locImmediate then
    Opcode('cp', ImmByte(Right), '')
  else
  begin
    Variable := ILParamToVariable(Right);
    case Right.Loc of
      locVar, locTemp:
        Opcode('cp', IXOffset(VarToOffset(Variable)), '');
    else
      raise exception.Create('Comp8 ToDo');
    end;
  end;

  GenCompDest(ILItem, Invert);
end;


procedure ProcLessU8(ILItem: PILItem;Prim: PPrimitive);
begin
  GenComp8(ILItem, False, False);
end;

procedure ProcGreaterEqualU8(ILItem: PILItem;Prim: PPrimitive);
begin
  GenComp8(ILItem, False, True);
end;

procedure ProcGreaterU8(ILItem: PILItem;Prim: PPrimitive);
begin
  GenComp8(ILItem, True, False);
end;

procedure ProcLessEqualU8(ILItem: PILItem;Prim: PPrimitive);
begin
  GenComp8(ILItem, True, True);
end;

//Swap = swap order of parameters
//Invert = invert (logical NOT) result
procedure GenComp16(ILItem: PILItem;Swap, Invert: Boolean);
var
  Left: PILParam;
  Right: PILParam;
  LType: TVarType;
  RType: TVarType;
begin
  if Swap then
  begin
    Left := @ILItem.Param2;
    Right := @ILItem.Param1;
  end
  else
  begin
    Left := @ILItem.Param1;
    Right := @ILItem.Param2;
  end;

  GenLoadParam16('h','l', Left);
  GenLoadParam16('d','e', Right);

  //Operation
  case ILItem.OpType of
    rtU16:
    begin
      Instr('and a');   //Clear carry
      Instr('sbc hl,de');
    end;
    rtS16:
        Instr('call comp_s16');
    rtM16:
    begin
      LType := ILParamToVarType(Left);
      RType := ILParamToVarType(Right);
      Assert(((LType in [vtInt8, vtInt16, vtInteger]) and (RType in [vtByte, vtWord])) or
        ((LType in [vtByte, vtWord]) and (RType in [vtInt8, vtInt16, vtInteger])), 'Invalid types for GenCompM16');

      if LType in [vtByte, vtWord] then
        Instr('call comp_u16_s16')
      else
        Instr('call comp_s16_u16');
    end;
  else
    raise Exception.Create('Invalid OpTYype for GenComp16');
  end;

  GenCompDest(ILItem, Invert);
end;

procedure ProcLess16(ILItem: PILItem;Prim: PPrimitive);
begin
  GenComp16(ILItem, False, False);
end;

procedure ProcGreaterEqual16(ILItem: PILItem;Prim: PPrimitive);
begin
  GenComp16(ILItem, False, True);
end;

procedure ProcGreater16(ILItem: PILItem;Prim: PPrimitive);
begin
  GenComp16(ILItem, True, False);
end;

procedure ProcLessEqual16(ILItem: PILItem;Prim: PPrimitive);
begin
  GenComp16(ILItem, True, True);
end;

//------------------------Boolean bitwise and logical
procedure ProcBool8(ILItem: PILItem;Prim: PPrimitive);
var
  Variable: PVariable;
  Op: POperator;
begin
  Op := OpIndexToData(ILItem.OpIndex);

  GenLoadParam8('a', @ILItem.Param1);
  if ILItem.Param2.Loc = locImmediate then
    Opcode(Op.Symbol, ImmByte(@ILItem.Param2), '')
  else
  begin
    Variable := ILParamToVariable(@ILItem.Param2);
    case ILItem.Param2.Loc of
      locVar, locTemp:
        Opcode(Op.Symbol, IXOffset(VarToOffset(Variable)), '');
    else
      raise exception.Create('Comp8 ToDo');
    end;
  end;

  GenStoreILItem8(ILItem, 'a');
end;

procedure ProcBool16(ILItem: PILItem;Prim: PPrimitive);
var
  Op: POperator;
begin
  Op := OpIndexToData(ILItem.OpIndex);

  GenLoadParam16('h','l', @ILItem.Param1);
  GenLoadParam16('d','e',@ILItem.Param2);

  //Lazy code. Doing this properly is tooo complex!
  //(At least until I have register allocation working)
  Instr('ld a,l');
  Opcode(Op.Symbol, 'e', '');
  Instr('  ld l,a');
  Instr('  ld a,h');
  Opcode(Op.Symbol, 'd', '');
  Instr('ld h,a');

  GenStoreDest16(@ILItem.Dest, 'h','l');
end;

procedure ProcNot8(ILItem: PILItem;Prim: PPrimitive);
begin
  GenLoadParam8('a', @ILItem.Param1);
  instr('cpl');
  //Low byte

  GenStoreILItem8(ILItem, 'a');
  //High byte - always $ff
end;

procedure ProcNotBoolean(ILItem: PILItem;Prim: PPrimitive);
begin
  GenLoadParam8('a', @ILItem.Param1);
  Instr('xor $01');

  GenStoreILItem8(ILItem, 'a');
end;

procedure ProcNot16(ILItem: PILItem;Prim: PPrimitive);
var
  Op: POperator;
begin
  Op := OpIndexToData(ILItem.OpIndex);

  GenLoadParam16('h','l', @ILItem.Param1);

  //Lazy code. Doing this properly is too complex!
  //(At least until I have register allocation working)
  Instr('ld a,l');
  Instr('cpl');
  Instr('ld l,a');
  Instr('ld a,h');
  Instr('cpl');
  Instr('ld h,a');

  GenStoreDest16(@ILItem.Dest, 'h','l');
end;
*)
//----------------------------------------------------------

//Allocate which registers (if any) parameters need to be loaded (or moved) into
//before a primitive can be generated
procedure TEMPRegAlloc(var ILItem: PILItem;const Prim: PPrimitive);
var Loc: TAllocLoc;
begin
  //If we have an immediate value and the primitive can handle an immediate
  //then we're done...
  if (ILItem.Param1.Loc = locImmediate) and (plImm in Prim.Param1Loc) then
    ILItem.Param1Alloc := plImm
  else  //...otherwise we'll need to load the param into a register
  begin
    Loc := Pred(plA);
    ILItem.Param1Alloc := plNone;
    repeat
      Loc := Succ(Loc);
      if Loc in Prim.Param1Loc then
        ILItem.Param1Alloc := Loc;
    until (ILItem.Param1Alloc <> plNone) or (Loc = high(TAllocLoc));
    if ILItem.Param1Alloc = plNone then
      raise Exception.Create('TEMNRegAlloc couldn''t find suitable Param1 Alloc');
  end;

  if Prim.Param2Loc = [] then
    ILItem.Param2Alloc := plNone
  else if (ILItem.Param2.Loc = locImmediate) and (plImm in Prim.Param2Loc) then
    ILItem.Param2Alloc := plImm
  else
  begin
    Loc := Pred(plA);
    ILItem.Param2Alloc := plNone;
    repeat
      Loc := Succ(Loc);
      if (Loc <> ILItem.Param1Alloc) and (Loc in Prim.Param2Loc) then
        ILItem.Param2Alloc := Loc;
    until (ILItem.Param2Alloc <> plNone) or (Loc = high(TAllocLoc));
    if ILItem.Param2Alloc = plNone then
      raise Exception.Create('TEMNRegAlloc couldn''t find suitable Param2 Alloc');
  end;

  case Prim.DestLoc of
    plP1: ILItem.DestAlloc := ILItem.Param1Alloc;
    plNone: ILItem.DestAlloc := plNone;
  else
    ILItem.DestAlloc := Prim.DestLoc;
  end;
end;

procedure LoadParam(ILItem: PILItem;ParamNo: Integer);
var
  Param: PILParam;
  AllocLoc: TAllocLoc;
  Variable: PVariable;
  Prefix: String;
begin
  Prefix := 'ldp' + IntToStr(ParamNo);
  if ParamNo = 1 then
  begin
    Param := @ILItem.Param1;
    AllocLoc := ILItem.Param1Alloc;
  end
  else
  begin
    Param := @ILItem.Param2;
    AllocLoc := ILItem.Param2Alloc;
  end;

  case AllocLoc of
    plNone, plImm: ;  //Nothing to do
    plA..plL:
      if Param.Loc = locImmediate then
        GenLibraryProc(Prefix + 'r8imm', ILItem)
      else
        GenLibraryProc(Prefix + 'r8offset', ILItem);
    plHL..plBC:
      if Param.Loc = locImmediate then
        GenLibraryProc(Prefix + 'r16imm', ILItem)
      else
      begin
        Variable := ILParamToVariable(Param);
        case TypeSize[Variable.VarType] of
        1:
        begin
          GenLibraryProc(Prefix + 'r16lowoffsetlow', ILItem);
          if Variable.VarType = vtInt8 then
            GenSignExtend(AllocLocToLowReg[AllocLoc], AllocLocToHighReg[AllocLoc])
          else
            GenLibraryProc(Prefix + 'r16highzero', ILItem);
        end;
        2:
          GenLibraryProc(Prefix + 'r16offset', ILItem);
        else
          raise Exception.Create('Invalid type size for parameter');
        end;
      end;
  else
    raise Exception.Create('Invalid Alloc Param');
  end;
end;

//Loads parameters from memory into registers as needed.
//Data is loaded from the locations specified by the ILItem parameters into the
//registers specified by ILItem.Param1Alloc and ILItem.Param2Alloc, if any registers
//are specified there.
procedure LoadBeforePrim(ILItem: PILItem);
begin
  LoadParam(ILItem, 1);
  LoadParam(ILItem, 2);
end;


//If validation is enabled, applies such validation.
//This routine is called after the primitive has executed.
//The validation routine (if there is one) is specifiied in the 'Validate' column
//of the Primitives table (spreadsheet)
procedure ValidateAfterPrim(ILItem: PILItem;Prim: PPrimitive);
begin
  if cgOverflowCheck in ILItem.CodeGenFlags then
    //Validation for the operation itself
    GenCode(Prim.ValProcName, ILItem);
end;


//Specifies the routine to use to validate a convetsion from the type given in the row
//to the type given in the column.
//err: invalid conversion - generates an error
//'' (empty): no validation is necessary for this conversion
//Note: the following types are invalid as a destination: Unknown, M16S16, M16U16, X8, X16
//If a more optimised routine is available this can be specified in the primitives
//table ('Special validations on type conversion'). If so that routine will be used
//in preference to this table.
const ConversionMatrix: array[low(TOpType)..high(TOpType),low(TOpType)..high(TOpType)] of String = (
//Unkn  U8      U16       S8        S16       M16S16  M16U16   X8      X16     Real    Bool  <- Destination (ResultType)
('err', 'err',  'err',    'err',    'err',    'err',  'err',  'err',  'err',  'err',  'err'), //Unkown
('err', '',     '',       'b7sov',  '',       'err',  'err',  'err',  'err',  'err',  'err'), //U8
('err', 'hnzov','',       'h9nzov', 'b15sov', 'err',  'err',  'err',  'err',  'err',  'err'), //U16
('err', 'b7sov','b7sov',  '',       '',       'err',  'err',  'err',  'err',  'err',  'err'), //S8
('err', 'hnzov','b15sov', 'h9neov', 'b15sov', 'err',  'err',  'err',  'err',  'err',  'err'), //S16
('err', 'err',  'err',    'err',    'err',    'err',  'err',  'err',  'err',  'err',  'err'), //M16S16
('err', 'err',  'err',    'err',    'err',    'err',  'err',  'err',  'err',  'err',  'err'), //M16U16
('err', 'err',  'err',    'err',    'err',    'err',  'err',  'err',  'err',  'err',  'err'), //X8
('err', 'err',  'err',    'err',    'err',    'err',  'err',  'err',  'err',  'err',  'err'), //X16
('err', 'err',  'err',    'err',    'err',    'err',  'err',  'err',  'err',  '',     'err'), //Real
('err', 'err',  'err',    'err',    'err',    'err',  'err',  'err',  'err',  'err',  ''));   //Boolean
                                                                                //^^ Source (OpType)
//b7sov: test if bit 7 set. If so, overflow
//b15sov: test if bit 15 is set. If so, overflow
//h9neov: overflow unless all of the highest 9 bits are equal (ie. all set or all clear)
//h9nzov: raise an overflow error unless the highest 9 bits are zero
//hnzov: overflow if the high register is non-zero

//Takes the result from the register specified in ILItem.DestAlloc and stores it
//into the location specified in ILItem.Dest
//If a type conversion is to take place then:
// * if validation is enabled, will generate code to ensure the value will fit into
//the destination type
// * generates code to handle the type conversion (if necessary)
procedure StoreAfterPrim(ILItem: PILItem;Prim: PPrimitive);
var
  OpType: TOpType;
  ValProcName: String;
  R: Char;
begin
  case ILItem.OpType of
    rtM16S16: OpType := rtS16;
    rtM16U16: OpType := rtU16;
  else
    OpType := ILItem.OpType;
  end;

  //Validation for type conversions, if needed. No conversion needed if operator has fixed result type
  if (OpType <> ILItem.ResultType) and (OpIndexToData(ILItem.OpIndex).ResultType = vtUnknown) then
    if cgOverflowCheck in ILItem.CodeGenFlags then
    begin
      ValProcName := '';
      //Do we have a special case validation routine for conversion to said type?
      case ILItem.ResultType of
        rtS8: ValProcName := Prim.ValProcS8;
        rtU8: ValProcName := Prim.ValProcU8;
        rtS16: ValProcName := Prim.ValProcS16;
        rtU16: ValProcName := Prim.ValProcU16;
      else
        raise Exception.Create('Unhandled type in GenValidation');
      end;
      if ValProcName = '' then
        //No special case so use validation matrix!!
        ValProcName := ConversionMatrix[OpType, ILItem.ResultType];

      GenCode(ValProcName, ILItem);
    end;

  //Store the output to the appropriate destination
  case ILItem.DestAlloc of
    plNone: ;
    plA..plL:
      //Are we storing to a 1 or two byte destination?
      case OpTypeSize[ILItem.ResultType] of
        1: GenLibraryProc('storeoffset8r8',ILItem);
        2:
          //Do we need to sign extend?
          if (OpType in [rtS8, rtX8]) and
            ((ILItem.ResultType = rtS16) or
            ((ILItem.ResultType = rtU16) and not (cgOverflowCheck in ILItem.CodeGenFlags))) then
          begin
            GenLibraryProc('storeoffset16lowr8',ILItem);
            R := AllocLocToReg8[ILItem.DestAlloc];
            GenSignExtend(R, 'a');  ///'a'!!!
            GenLibraryProc('storeoffset16higha',ILItem);
          end
          else
            GenLibraryProc('storeoffset16r8',ILItem);
      else
        raise Exception.Create('Invalid type size in StoreAfterPrim');
      end;
    plHL..plBC:
      //Are we storing to a 1 or two byte destination?
      case OpTypeSize[ILItem.ResultType] of
        //When shortening, any validation should have been done above
        1: GenLibraryProc('storeoffset8r16low', ILItem);
        2: GenLibraryProc('storeoffset16r16',ILItem);
      else
        raise Exception.Create('Invalid type size in StoreAfterPrim');
      end;
    plZF:
      begin //For assignments
        GenLibraryProc('zftoboolean', ILItem);
        GenLibraryProc('storeoffset8a', ILItem);
      end;
    plZFA:
      begin //For assignments
        GenLibraryProc('notatoboolean', ILItem);
        GenLibraryProc('storeoffset8a', ILItem);
      end;
    plNZF:
      begin //For assignments
        GenLibraryProc('nzftoboolean', ILItem);
        GenLibraryProc('storeoffset8a', ILItem);
      end;
    plNZFA:
      begin //For assignments
        GenLibraryProc('atoboolean', ILItem);
        GenLibraryProc('storeoffset8a', ILItem);
      end;
    plCPLA:
      begin //For assignments
        GenLibraryProc('cpla', ILItem);
        GenLibraryProc('storeoffset8a', ILItem);
      end;
    plCF:
      begin //For assignments
        GenLibraryProc('cftoboolean', ILItem);
        GenLibraryProc('storeoffset8a', ILItem);
      end;
    plNCF:
      begin //For assignments
        GenLibraryProc('ncftoboolean', ILItem);
        GenLibraryProc('storeoffset8a', ILItem);
      end;
    else
    raise Exception.Create('Illegal DestAlloc in AllocAfterPrim');
  end;
end;

//Generate a branch after the
procedure CondBranchAfterPrim(ILItem: PILItem;Prim: PPrimitive);
begin
  //If True block is following block then generate conditional jump for False
  if ILItem.TrueBlockID = CurrBlockID + 1 then
    GenCondJump(ILItem.DestAlloc, True, ILItem.FalseBlockID)
  else
  begin //Otherwise generate condition jump for True...
    GenCondJump(ILItem.DestAlloc, False, ILItem.TrueBlockID);
    //...and False doesn't 'fall though' then an unconditional jump for it.
    if ILItem.FalseBlockID <> CurrBlockID + 1 then
      GenUncondJump(ILItem.FalseBlockID);
  end;
end;

procedure DoCodeGenItem(ILIndex: Integer);
var ILItem: PILItem;
  Prim: PPrimitive;
begin
  ILItem := ILIndexToData(ILIndex);
  if (ILItem.SourceLineNo <> -1) and (ILItem.SourceLineNo <> CurrSourceLineNo) then
  begin //Output sourrce code line
    CurrSourceLineNo := ILItem.SourceLineNo;
    Line(';' + IntToStr(CurrSourceLineNo) + ': ' + Parser.Source[CurrSourceLineNo].Trim);
  end;  //Output block ID
  if ILItem.BlockID >= 0 then
  begin
    CurrBlockID := ILItem.BlockID;
    Line(CurrProcName + IntToStr(ILItem.BlockID) + ':');
  end;


  if ILItem.OpIndex = OpIndexPhi then
    //Ignore
  else if ILItem.OpIndex = OpIndexBranch then
    GenUncondBranch(ILItem)
//  else if ILItem.OpIndex = OpIndexConstBranch then
//    ConstBranch(ILItem)
//  else if ILItem.OpIndex = OpIndexCondBranch then
//    CondBranchAfterPrim(ILItem, nil)
  else
  begin
    Prim := ILItemToPrimitive(ILItem);
    if Assigned(Prim) then
    begin
      //Temp
      TEMPRegAlloc(ILItem, Prim);
      LoadBeforePrim(ILItem);
      Line(';Gen: '+Prim.ProcName);
      if Assigned(Prim.Proc) then
        Prim.Proc(ILItem)
      else
        GenLibraryProc(Prim.ProcName, ILItem);
      ValidateAfterPrim(ILItem, Prim);
      case ILItem.DestType of
        dtData: StoreAfterPrim(ILItem, Prim);
        dtCondBranch: CondBranchAfterPrim(ILItem, Prim);
      else
        raise Exception.Create('Invalid destination after Primitive');
      end;
    end
    else
      Error('No primitive found:'#13#10 + ILItemToString(ILItem));
  end;
end;

procedure DoCodeGen;
var I: Integer;
begin
  for I := 0 to ILGetCount-1 do
    DoCodeGenItem(I);
end;

function CodeGen(ScopeName: String;AnAsmScope: TStringlist): TAssembleError;
begin
  try
    CodeGenErrorString := '';

    CurrErrorCount := 0;
    AsmScope := AnAsmScope;

    Line(';=========='+ScopeName);
    Line('');

    CurrProcName := '_'+ScopeName.ToLower;
    CurrBlockID := -1;
    CurrSourceLineNo := -1;
    StackFrameSize := VarsStackFrameSize;
    LabelIndex := 1;

    //Calc variables size/offsets
    VarUpdateLocalOffsets;

    GenLabel(CurrProcName);

    DoCodeGen;

    GenLabel(CurrProcName+IntToStr(CurrBlockID+1));
    Instr('ret');
    Line('');
    Line(';----------'+ScopeName);
    Line('');

    Result := errNone;
  except
    on E:Exception do
    begin
      CodeGenErrorString := E.Message;
      Line('CODEGEN ERROR: ' + E.Message);
      Result := errCodeGen;
    end;
  end;
end;

procedure InitPrimitives;
begin
  //Assignment
  PrimSetProc('error', ProcError);
  PrimSetProc('empty',ProcEmpty);

  PrimSetProc('procassignS8S16NI',ProcAssignS8S16NI);


  PrimSetProc('b7sov',ProcDestB7SetOverflow);
  PrimSetProc('b15sov',ProcDestB15SetOverflow);


//  PrimSetProc('branch',ProcBranch);
//  PrimSetProc('phi',ProcPhi);


  //Maths
(*

  PrimSetProc('shl',rtU8,ProcSHL8);
  PrimSetProc('shl',rtS8,ProcSHL8);
  PrimSetProc('shl',rtU16,ProcSHL16);
  PrimSetProc('shl',rtS16,ProcSHL16);
  PrimSetProc('shr',rtU8,ProcSHR8);
  PrimSetProc('shr',rtS8,ProcSHR8);
  PrimSetProc('shr',rtU16,ProcSHR16);
  PrimSetProc('shr',rtS16,ProcSHR16);

  //Comparisons
  PrimSetProc('equal',rtU8,ProcEqual8);
  PrimSetProc('equal',rtBoolean,ProcEqual8);
  PrimSetProc('equal',rtU16,ProcEqual16);
  PrimSetProc('equal',rtS16,ProcEqual16);
  PrimSetProc('equal',rtM16,ProcEqualM16);
  PrimSetProc('notequal',rtU8,ProcNotEqual8);
  PrimSetProc('notequal',rtBoolean,ProcNotEqual8);
  PrimSetProc('notequal',rtU16,ProcNotEqual16);
  PrimSetProc('notequal',rtS16,ProcNotEqual16);
  PrimSetProc('notequal',rtM16,ProcNotEqualM16);

  PrimSetProc('less',rtU8,ProcLessU8);
  PrimSetProc('less',rtBoolean,ProcLessU8);
  PrimSetProc('less',rtU16,ProcLess16);
  PrimSetProc('less',rtS16,ProcLess16);
  PrimSetProc('less',rtM16,ProcLess16);
  PrimSetProc('greaterequal',rtU8,ProcGreaterEqualU8);
  PrimSetProc('greaterequal',rtBoolean,ProcGreaterEqualU8);
  PrimSetProc('greaterequal',rtU16,ProcGreaterEqual16);
  PrimSetProc('greaterequal',rtS16,ProcGreaterEqual16);
  PrimSetProc('greaterequal',rtM16,ProcGreaterEqual16);
  PrimSetProc('greater',rtU8,ProcGreaterU8);
  PrimSetProc('greater',rtBoolean,ProcGreaterU8);
  PrimSetProc('greater',rtU16,ProcGreater16);
  PrimSetProc('greater',rtS16,ProcGreater16);
  PrimSetProc('greater',rtM16,ProcGreater16);
  PrimSetProc('lessequal',rtU8,ProcLessEqualU8);
  PrimSetProc('lessequal',rtBoolean,ProcLessEqualU8);
  PrimSetProc('lessequal',rtU16,ProcLessEqual16);
  PrimSetProc('lessequal',rtS16,ProcLessEqual16);
  PrimSetProc('lessequal',rtM16,ProcLessEqual16);

  //Boolean
  PrimSetProc('bool',rtX8,ProcBool8);
  PrimSetProc('bool',rtX16,ProcBool16);
//  PrimSetProc('bool',rtBoolean,ProcBool8);
  PrimSetProc('not', rtX8,ProcNot8);
  PrimSetProc('not', rtX16,ProcNot16);
  PrimSetProc('not', rtBoolean,ProcNotBoolean);
*)


//  PrimSetProc('',rtUnknown,Proc);
end;

procedure InsertPreamble(PlatformFile, QuicheLibrary: String);
begin
  AsmFull.Add(';Quiche object code');
  AsmFull.Add(';Auto-created. Will be overwritten!');
  AsmFull.Add(';Designed for RASM assembler');
  AsmFull.Add('');
  AsmFull.Add(';Insert platform specific code');
  AsmFull.Add('include "' + PlatformFile + '"');
  AsmFull.Add('');
  AsmFull.Add(';Insert Quiche libraries');
  AsmFull.Add('include "' + QuicheLibrary + '"');
  AsmFull.Add('');
  AsmFull.Add(';Generated code starts here');
  AsmFull.Add('quiche:');
end;

procedure InitialiseCodeGen(PlatformFile, QuicheLibrary: String);
begin
  InitPrimitives;
  CurrErrorCount := 0;
  TotalErrorCount := 0;

  AsmFull := TStringList.Create;
  AsmScope := nil;
  InsertPreAmble(PlatformFile, QuicheLibrary);
end;

end.

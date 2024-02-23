unit CodeGenZ80AsmEx;


interface
uses Classes, Scopes, Globals;

procedure InitialiseCodeGen(PlatformFile, QuicheLibrary: String);

//BlockType can be used to specify whether the root/global block should
//use stack vars or static vars.
//For functions (Scope.Func <> nil) BlockType MUST be btDefault
function CodeGen(Scope: PScope;BlockType: TBlockType): Boolean;

procedure SaveAssemblyFile(FileName: String);

var
  CodeGenErrorString: String;
  CurrErrorCount: Integer;  //In current routine
  TotalErrorCount: Integer; //In current build


//Option for testing
//If True every primitive used will be logged
var LogPrimitives: Boolean;
  PrimitiveLog: TStringList;
function UsesPrimitive(const Name: String): Boolean;

implementation
uses Fragments, ILData, SysUtils, Variables, ParserBase, QTypes, Operators,
  PrimitivesEx, Compiler, Functions;

var
  CodeGenScope: PScope;
  AsmCodeFull: TStringList;
  AsmCodeScope: TStringList;
  AsmDataFull: TStringList;
  AsmDataScope: TStringList;


procedure DataGen;
var I: Integer;
  V: PVariable;
  S: String;
  C: Integer;
begin
  for I := 0 to VarGetCount-1 do
  begin
    V := VarIndexToData(I);
    if V.Storage = vsStatic then
    begin
      S := V.GetAsmName + ': ';
      case GetTypeSize(V.VarType) of
        1: S := S + 'db 0';
        2: S := S + '  dw 0';
      else
        S := 'db ';
        for C := 1 to GetTypeSize(V.VarType) do
        begin
          if C <> 1 then
            S := S + ',';
          S := S + '0';
        end;
      end;
      AsmDataScope.Append(S);
      AsmDataFull.Append(S);
    end;
  end;
end;






procedure Line(S: String);
begin
  AsmCodeFull.Add(S);
  if Assigned(AsmCodeScope) then
    AsmCodeScope.Add(S);
end;

procedure Lines(S: String);
begin
  Line(S);
end;

procedure SaveAssemblyFile(FileName: String);
begin
  AsmCodeFull.Append(AsmDataFull.Text);
  AsmCodeFull.Add('__quiche_end:');

  AsmCodeFull.SaveToFile(Filename);

end;


var CurrProcName: String;
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
    Instr('call ' + ProcName.SubString(1) + ' ;Call')
  else
  begin
    Code := Fragments.FragmentSub(ProcName, ILItem, CodeGenScope);
    if Code = '' then
      raise Exception.Create('Validation library code not found: ' + ProcName);
    Line('                     ;Fragment: ' + ProcName);
    Lines(Code);
  end;
end;

procedure GenCode(ProcName: String;ILItem: PILItem);
var
  Prim: PPrimitive;
begin
  if ProcName <> '' then
    if ProcName.Chars[0] = ':' then
      GenLibraryProc(ProcName, ILItem)
    else
    begin
      Prim := PrimFindByProcName(ProcName);
      if Assigned(Prim) then
      begin
        Line('                     ;Prim: ' + ProcName);
        Prim.Proc(ILItem);
      end
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
  GenUncondJump(ILItem.BranchBlockID);
end;

//Generates a conditional jump, using Zero or Carry flag, to the given block
//If ZeroFlag is true, the jump uses the Zero flag, otherwise the Carry flag
//If Reverse is True the condition is inverted (i.e. Zero becomes Not Zero,
//Carry Set becomse Carry Clear).
procedure GenCondJump(ILItem: PILItem;Reverse: Boolean;BlockID: Integer);
var F: String;
begin
//  Reverse := Reverse xor ILItem.BranchInvert;

  case ILItem.BranchReg of
    rA:
    begin
      if ILItem.OpType <> rtBoolean then
        GenLibraryProc('atozf', ILItem);
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

  Opcode('jp', F, CurrProcName + IntToStr(BlockID));
end;

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
var Reg: Char;
begin
  Reg := CPUReg8ToChar[ILItem.Dest.Reg];
  if Reg = 'a' then
  begin //A reg
    Instr('and a');
    Instr('jp m,raise_overflow');
  end
  else
  begin
    OpCode('bit','7',Reg);
    Instr('jp nz,raise_overflow');
  end;
end;

//Raise an overflow error if bit 15 of the DestAlloc register is set
//Preserves all registers. Corrupts flags
procedure ProcDestB15SetOverflow(ILItem: PILItem);
var Reg: Char;
begin
  Reg := CPURegHighToChar[ILItem.Dest.Reg];
  if Reg = 'a' then
  begin //A reg
    Instr('and a');
    Instr('jp m,raise_overflow');
  end
  else
  begin
    OpCode('bit','7',Reg);
    Instr('jp nz,raise_overflow');
  end;
end;

//====================================Assigns

procedure ProcAssignAbsS16Imm8(ILItem: PILItem);
var Variable: PVariable;
begin
  Assert(ILItem.Param1.Kind = pkImmediate);
  Assert(ILItem.Param1.ImmType in [vtInt8, vtByte]);
  Assert(ILItem.Dest.Kind = pkVar);

  Variable := ILItem.Dest.ToVariable;
  Assert(Variable.Storage = vsStatic);

  if (ILItem.Param1.ImmType = vtInt8) and (ILItem.Param1.ImmValueInt < 0) then
    GenLibraryProc('assign_abs16_imm8_neg', ILItem)
  else
    GenLibraryProc('assign_abs16_imm8', ILItem);
end;

//8 bit immediate value, extended to 16 bit
procedure ProcAssignRelS16Imm8(ILItem: PILItem);
var Variable: PVariable;
begin
  Assert(ILItem.Param1.Kind = pkImmediate);
  Assert(ILItem.Param1.ImmType = vtInt8);
  Assert(ILItem.Dest.Kind = pkVar);

  Variable := ILItem.Dest.ToVariable;
  Assert(Variable.Storage = vsStack);

  if ILItem.Param1.ImmValueInt < 0 then
    GenLibraryProc('assign_rel16_imm8_neg', ILItem)
  else
    GenLibraryProc('assign_rel16_imm8', ILItem);
end;


//=====================================Maths

procedure ProcDec8Reg(ILItem: PILItem);
var Count: Integer;
  I: Integer;
begin
  if ILItem.Param2.Kind = pkNone then
    Count := 1
  else
    Count := ILItem.Param2.ImmToInteger;

  for I := 1 to abs(Count) do
    if Count > 0 then
      GenLibraryProc('dec8_reg', ILItem)
    else
      GenLibraryProc('inc8_reg', ILItem);
end;

procedure ProcDec16Reg(ILItem: PILItem);
var Count: Integer;
  I: Integer;
begin
  if ILItem.Param2.Kind = pkNone then
    Count := 1
  else
    Count := ILItem.Param2.ImmToInteger;

  for I := 1 to abs(Count) do
    if Count > 0 then
      GenLibraryProc('dec16_reg', ILItem)
    else
      GenLibraryProc('inc16_reg', ILItem);
end;

procedure ProcInc8Reg(ILItem: PILItem);
var Count: Integer;
  I: Integer;
begin
  if ILItem.Param2.Kind = pkNone then
    Count := 1
  else
    Count := ILItem.Param2.ImmToInteger;

  for I := 1 to abs(Count) do
    if Count > 0 then
      GenLibraryProc('inc8_reg', ILItem)
    else
      GenLibraryProc('dec8_reg', ILItem);
end;

procedure ProcInc16Reg(ILItem: PILItem);
var Count: Integer;
  I: Integer;
begin
  if ILItem.Param2.Kind = pkNone then
    Count := 1
  else
    Count := ILItem.Param2.ImmToInteger;

  for I := 1 to abs(Count) do
    if Count > 0 then
      GenLibraryProc('inc16_reg', ILItem)
    else
      GenLibraryProc('dec16_reg', ILItem);
end;

procedure ProcCall(ILItem: PILItem);
var Code: String;
begin
  Assert(ILItem.Func <> nil);
  Code := ILItem.Func.GetCallInstruction;
  Instr(Code);
end;




//----------------------------------------------------------

//Allocate which registers (if any) parameters need to be loaded (or moved) into
//before a primitive can be generated
procedure TEMPRegAlloc(var ILItem: PILItem;const Prim: PPrimitive);
var Reg: TCPUReg;
  Regs: TCPURegSet;
begin
  if ILItem.Param1.Reg = rNone then
  begin
    //Prim doesn't use this parameter?
    if Prim.Param1Regs = [] then
      ILItem.Param1.Reg := rNone
    //Param is immediate and Prim can handle an immediate value?
    else if (ILItem.Param1.Kind = pkImmediate) and (rImm in Prim.Param1Regs) then
      ILItem.Param1.Reg := rImm
    else  //...otherwise assign a register for the parameter to be loaded into
    begin
      //Get allowable registers
      if ILItem.Op = opMove then
      begin
        if ILItem.Param1.GetRawType in [rtX8, rtS8, rtU8] then
          if ILItem.Dest.Kind = pkStackByte then
            Regs := [rA]
          else if ILItem.Dest.Kind = pkStack then
            Regs := [rHL, rDE, rBC]
//            Regs := [rA, rB, rD, rH]
          else
            Regs := [rA, rB, rC, rD, rE, rH, rL]
        else
          Regs := [rHL, rDE, rBC]
      end
      else
        Regs := Prim.Param1Regs;

      //Find an available register
      Reg := Pred(rA);
      ILItem.Param1.Reg := rNone;
      repeat
        Reg := Succ(Reg);
        if Reg in Regs then
          ILItem.Param1.Reg := Reg;
      until (ILItem.Param1.Reg <> rNone) or (Reg = high(TCPUReg));
      if ILItem.Param1.Reg = rNone then
        raise Exception.Create('TEMNRegAlloc couldn''t find suitable Param1 register');
    end;
  end;

  if ILItem.Param2.Reg = rNone then
  begin
    if Prim.Param2Regs = [] then
      ILItem.Param2.Reg := rNone
    else if (ILItem.Param2.Kind = pkImmediate) and (rImm in Prim.Param2Regs) then
      ILItem.Param2.Reg := rImm
    else
    begin
      //Get allowable registers
      //TODO: Needs to be more flexible!
      Regs := Prim.Param2Regs - [ILItem.Param1.Reg];

      Reg := Pred(rA);
      ILItem.Param2.Reg := rNone;
      repeat
        Reg := Succ(Reg);
        if Reg in Regs then
          ILItem.Param2.Reg := Reg;
      until (ILItem.Param2.Reg <> rNone) or (Reg = high(TCPUReg));
      if ILItem.Param2.Reg = rNone then
        raise Exception.Create('TEMNRegAlloc couldn''t find suitable Param2 register');
    end;
  end;

  if ILItem.Dest.Reg = rNone then
  begin
    case Prim.DestReg of
      rP1:
        if ILItem.Dest.Kind in [pkStackByte, pkStack] then
          case ILItem.Param1.Reg of
            rA: ILItem.Dest.Reg := rAF;
            rB: ILItem.Dest.Reg := rBC;
            rD: ILItem.Dest.Reg := rDE;
            rH: ILItem.Dest.Reg := rHL;
          else
            ILItem.Dest.Reg := ILItem.Param1.Reg;
//            Assert(False, 'Invalid Byte register for stack value');
          end
        else
          ILItem.Dest.Reg := ILItem.Param1.Reg;
      rNone: ILItem.Dest.Reg := rNone;
    else
      ILItem.Dest.Reg := Prim.DestReg;
    end;
  end;
end;

procedure LoadParam(ILItem: PILItem;ParamNo: Integer;Prim: PPrimitive);
var
  Param: PILParam;
  Variable: PVariable;
  V: PVariable;

  Prefix: String;
  StoreStr: String;
  Suffix: String;
begin
  Prefix := 'load_p' + IntToStr(ParamNo);
  if ParamNo = 1 then
    Param := @ILItem.Param1
  else
    Param := @ILItem.Param2;

  case Param.Kind of
    pkNone: ; //No param to load
    pkImmediate:
      case Param.Reg of
        rNone, rImm: ; //Nothing to do. Imm is handled by Primitive itself
        rA..rL:         GenLibraryProc(Prefix + 'r8_imm', ILItem);
        rHL, rDE, rBC:  GenLibraryProc(Prefix + 'r16_imm', ILItem);
      else
        Assert(False, 'Invalid Reg for load');
      end;
    pkVar:
    begin
      V := Param.ToVariable;
      case V.Storage of
        vsStatic: StoreStr := '_abs';
        vsStack: StoreStr := '_rel';
      end;
      //Suffix only used for 8 bit loads
      if V.Storage = vsStack then
        Suffix := ''
      else if Param.Reg = rA then
        Suffix := '_a'
      else
        Suffix := '_via_a';
      case Param.Reg of
        rNone, rImm: ;  //Nothing to do. Imm is handled by Primitive itself
        rA..rL:
          begin


            if pfLoadRPHigh in Prim.Flags then
              GenLibraryProc(Prefix + 'r8' + StoreStr + 'high' + Suffix, ILItem)
            else if pfLoadRPLow in Prim.Flags then
              GenLibraryProc(Prefix + 'r8' + StoreStr + 'low' + Suffix, ILItem)
            else
              GenLibraryProc(Prefix + 'r8' + StoreStr + Suffix, ILItem);
          end;
        rHL..rBC:
          begin
            Variable := Param.ToVariable;
            case GetTypeSize(Variable.VarType) of
              1:
              begin
                GenLibraryProc(Prefix + 'r16low' + StoreStr + 'low' + Suffix, ILItem);
                if Variable.VarType = vtInt8 then
                  GenSignExtend(CPURegLowToChar[Param.Reg], CPURegHighToChar[Param.Reg])
                else
                  GenLibraryProc(Prefix + 'r16high_zero', ILItem);
              end;
              2: GenLibraryProc(Prefix + 'r16'+StoreStr, ILItem);
            else
              raise Exception.Create('Invalid type size for parameter');
            end;
          end;
      else
        raise Exception.Create('Invalid Reg for load');
      end;
    end;
  else
    Assert(False, 'Invalid param kind for param load');
  end;
end;

//Loads parameters from memory into registers as needed.
//Data is loaded from the locations specified by the ILItem parameters into the
//registers specified by ILItem.Param1Alloc and ILItem.Param2Alloc, if any registers
//are specified there.
procedure LoadBeforePrim(ILItem: PILItem; Prim: PPrimitive);
var Swap: Boolean;
begin
  //Select a register loading order so the second load won't trash the first.
  //A will get trashed by an 8-bit load into a register other than A
  Swap := ILItem.Param1.Reg = rA;

  if Swap then
  begin
    LoadParam(ILItem, 2, Prim);
    LoadParam(ILItem, 1, Prim);
  end
  else
  begin
    LoadParam(ILItem, 1, Prim);
    LoadParam(ILItem, 2, Prim);
  end;
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


procedure StoreToVariable(ILItem: PILItem;OpType: TOpType);
var
  V: PVariable;
  Reg: Char;
  StoreStr: String;
  Suffix: String;
begin
  V := ILItem.Dest.ToVariable;
  case V.Storage of
    vsStatic: StoreStr := 'store_abs';
    vsStack: StoreStr := 'store_rel';
  end;

  //Store the output to the appropriate destination
  case ILItem.Dest.Reg of
    rNone: ;
    rA..rL:
    begin
      if V.Storage = vsStack then
        Suffix := ''
      else if ILItem.Dest.Reg = rA then
        Suffix := '_a'
      else
        Suffix := '_via_a';

      //Are we storing to a 1 or two byte destination?
      case OpTypeSize[ILItem.ResultType] of
        1:
          case GetTypeSize(V.VarType) of
            1: GenLibraryProc(StoreStr + '8_r8' + Suffix,ILItem);
            //If destiniation is 16 bit, zero extend data
            2: GenLibraryProc(StoreStr + '16_r8' + Suffix, ILItem);
          else
            raise Exception.Create('Invalid variable size in StoreAfterPrim');
          end;
        2:
          //Do we need to sign extend?
          if (OpType in [rtS8, rtX8]) and
            ((ILItem.ResultType = rtS16) or
            ((ILItem.ResultType = rtU16) and not (cgOverflowCheck in ILItem.CodeGenFlags))) then
          begin
            GenLibraryProc(StoreStr + '16low_r8' + Suffix,ILItem);
            Reg := CPUReg8ToChar[ILItem.Dest.Reg];
            GenSignExtend(Reg, 'a');  ///'a'!!!
            GenLibraryProc(StoreStr + '16high_a',ILItem);
          end
          else
            GenLibraryProc(StoreStr + '16_r8' + Suffix,ILItem);
      else
        raise Exception.Create('Invalid type size in StoreAfterPrim');
      end;
    end;
    rHL..rBC:
    begin
      if V.Storage = vsStack then
        Suffix := ''
      else if ILItem.Dest.Reg = rA then
        Suffix := '_a'
      else
        Suffix := '_via_a';

      //Are we storing to a 1 or two byte destination?
      case OpTypeSize[ILItem.ResultType] of
        //When shortening, any validation should have been done above
        1: GenLibraryProc(StoreStr + '8_r16low' + Suffix, ILItem);
        2: GenLibraryProc(StoreStr + '16_r16',ILItem);
      else
        raise Exception.Create('Invalid type size in StoreAfterPrim');
      end;
    end;
    rZF:
      begin //For assignments
        GenLibraryProc('zftoboolean', ILItem);
        GenLibraryProc(StoreStr + '8_a', ILItem);
      end;
    rZFA:
      begin //For assignments
        GenLibraryProc('notatoboolean', ILItem);
        GenLibraryProc(StoreStr + '8_a', ILItem);
      end;
    rNZF:
      begin //For assignments
        GenLibraryProc('nzftoboolean', ILItem);
        GenLibraryProc(StoreStr + '8_a', ILItem);
      end;
    rNZFA:
      begin //For assignments
        GenLibraryProc('atoboolean', ILItem);
        GenLibraryProc(StoreStr + '8_a', ILItem);
      end;
    rCPLA:
      begin //For assignments
        GenLibraryProc('cpla', ILItem);
        GenLibraryProc(StoreStr + '8_a', ILItem);
      end;
    rCF:
      begin //For assignments
        GenLibraryProc('cftoboolean', ILItem);
        GenLibraryProc(StoreStr + '8_a', ILItem);
      end;
    rNCF:
      begin //For assignments
        GenLibraryProc('ncftoboolean', ILItem);
        GenLibraryProc(StoreStr + '8_a', ILItem);
      end;
    else
      raise Exception.Create('Illegal DestAlloc in AllocAfterPrim');
  end;
end;

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
begin
  case ILItem.OpType of
    rtM16S16: OpType := rtS16;
    rtM16U16: OpType := rtU16;
  else
    OpType := ILItem.OpType;
  end;

  //Validation for type conversions, if needed. No conversion needed if operator has fixed result type
  if (OpType <> ILItem.ResultType) and (Operations[ILItem.Op].ResultType = teUnknown) then
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

  case ILItem.Dest.Kind of
    pkNone, pkImmediate, pkPhiVar: Assert(False, 'Invalid destination location');
    pkVar:        StoreToVariable(ILItem, OpType);
    pkStack:      GenLibraryProc('push_word', ILItem);     //The stack
    pkStackByte:  GenLibraryProc('push_byte_a', ILItem);  //Single byte on the stack
  else
    Assert(False, 'Invalid Dest Location');
  end;
end;


//Generate a branch after the
procedure CondBranchAfterPrim(ILItem: PILItem;Prim: PPrimitive);
begin
  //If True block is following block then generate conditional jump for False
  if ILItem.TrueBlockID = CurrBlockID + 1 then
    GenCondJump(ILItem, True, ILItem.FalseBlockID)
  else
  begin //Otherwise generate condition jump for True...
    GenCondJump(ILItem, False, ILItem.TrueBlockID);
    //...and False doesn't 'fall though' then an unconditional jump for it.
    if ILItem.FalseBlockID <> CurrBlockID + 1 then
      GenUncondJump(ILItem.FalseBlockID);
  end;
end;

//======================

//----------------------------------------------------------

//Allocate which registers (if any) parameters need to be loaded (or moved) into
//before a primitive can be generated
procedure TEMPRegAllocNG(var ILItem: PILItem;const Prim: PPrimitiveNG);
var Reg: TCPUReg;
  Regs: TCPURegSet;
begin
  if ILItem.Param1.Reg = rNone then
  begin
    //Prim doesn't use this parameter?
    if Prim.LRegs = [] then
      ILItem.Param1.Reg := rNone
    //Param is immediate and Prim can handle an immediate value?
    else if (ILItem.Param1.Kind = pkImmediate) and (rImm in Prim.LRegs) then
      ILItem.Param1.Reg := rImm
    else  //...otherwise assign a register for the parameter to be loaded into
    begin
      //Get allowable registers
      if ILItem.Op = opMove then
      begin
        if ILItem.Param1.GetRawType in [rtX8, rtS8, rtU8] then
          if ILItem.Dest.Kind = pkStackByte then
            Regs := [rA]
          else if ILItem.Dest.Kind = pkStack then
            Regs := [rHL, rDE, rBC]
//            Regs := [rA, rB, rD, rH]
          else
            Regs := [rA, rB, rC, rD, rE, rH, rL]
        else
          Regs := [rHL, rDE, rBC]
      end
      else
        Regs := Prim.LRegs;

      //Find an available register
      Reg := Pred(rA);
      ILItem.Param1.Reg := rNone;
      repeat
        Reg := Succ(Reg);
        if Reg in Regs then
          ILItem.Param1.Reg := Reg;
      until (ILItem.Param1.Reg <> rNone) or (Reg = high(TCPUReg));
      if ILItem.Param1.Reg = rNone then
        raise Exception.Create('TEMNRegAlloc couldn''t find suitable Param1 register');
    end;
  end;

  if ILItem.Param2.Reg = rNone then
  begin
    if Prim.RRegs = [] then
      ILItem.Param2.Reg := rNone
    else if (ILItem.Param2.Kind = pkImmediate) and (rImm in Prim.RRegs) then
      ILItem.Param2.Reg := rImm
    else
    begin
      //Get allowable registers
      //TODO: Needs to be more flexible!
      Regs := Prim.RRegs - [ILItem.Param1.Reg];

      Reg := Pred(rA);
      ILItem.Param2.Reg := rNone;
      repeat
        Reg := Succ(Reg);
        if Reg in Regs then
          ILItem.Param2.Reg := Reg;
      until (ILItem.Param2.Reg <> rNone) or (Reg = high(TCPUReg));
      if ILItem.Param2.Reg = rNone then
        raise Exception.Create('TEMNRegAlloc couldn''t find suitable Param2 register');
    end;
  end;

  if ILItem.DestType = dtData then
  begin
    if ILItem.Dest.Reg = rNone then
    begin
      if Prim.ResultInLReg then
      begin
        if ILItem.Dest.Kind in [pkStackByte, pkStack] then
          case ILItem.Param1.Reg of
            rA: ILItem.Dest.Reg := rAF;
            rB: ILItem.Dest.Reg := rBC;
            rD: ILItem.Dest.Reg := rDE;
            rH: ILItem.Dest.Reg := rHL;
          else
            ILItem.Dest.Reg := ILItem.Param1.Reg;
          end
        else
          ILItem.Dest.Reg := ILItem.Param1.Reg;
      end
      else
        ILItem.Dest.Reg := Prim.ResultReg;
    end;
  end
  else if ILItem.DestType = dtCondBranch then
    ILItem.BranchReg := Prim.ResultReg;

end;

procedure LoadParamNG(ILItem: PILItem;ParamNo: Integer;Prim: PPrimitiveNG);
var
  Param: PILParam;
  Variable: PVariable;
  V: PVariable;

  Prefix: String;
  StoreStr: String;
  Suffix: String;
begin
  Prefix := 'load_p' + IntToStr(ParamNo);
  if ParamNo = 1 then
    Param := @ILItem.Param1
  else
    Param := @ILItem.Param2;

  case Param.Kind of
    pkNone: ; //No param to load
    pkImmediate:
      case Param.Reg of
        rNone, rImm: ; //Nothing to do. Imm is handled by Primitive itself
        rA..rL:         GenLibraryProc(Prefix + 'r8_imm', ILItem);
        rHL, rDE, rBC:  GenLibraryProc(Prefix + 'r16_imm', ILItem);
      else
        Assert(False, 'Invalid Reg for load');
      end;
    pkVar:
    begin
      V := Param.ToVariable;
      case V.Storage of
        vsStatic: StoreStr := '_abs';
        vsStack: StoreStr := '_rel';
      end;
      //Suffix only used for 8 bit loads
      if V.Storage = vsStack then
        Suffix := ''
      else if Param.Reg = rA then
        Suffix := '_a'
      else
        Suffix := '_via_a';
      case Param.Reg of
        rNone, rImm: ;  //Nothing to do. Imm is handled by Primitive itself
        rA..rL:
          begin
            if pfnLoadRPHigh in Prim.Flags then
              GenLibraryProc(Prefix + 'r8' + StoreStr + 'high' + Suffix, ILItem)
            else if pfnLoadRPLow in Prim.Flags then
              GenLibraryProc(Prefix + 'r8' + StoreStr + 'low' + Suffix, ILItem)
            else
              GenLibraryProc(Prefix + 'r8' + StoreStr + Suffix, ILItem);
          end;
        rHL..rBC:
          begin
            Variable := Param.ToVariable;
            case GetTypeSize(Variable.VarType) of
              1:
              begin
                GenLibraryProc(Prefix + 'r16low' + StoreStr + 'low' + Suffix, ILItem);
                if Variable.VarType = vtInt8 then
                  GenSignExtend(CPURegLowToChar[Param.Reg], CPURegHighToChar[Param.Reg])
                else
                  GenLibraryProc(Prefix + 'r16high_zero', ILItem);
              end;
              2: GenLibraryProc(Prefix + 'r16'+StoreStr, ILItem);
            else
              raise Exception.Create('Invalid type size for parameter');
            end;
          end;
      else
        raise Exception.Create('Invalid Reg for load');
      end;
    end;
  else
    Assert(False, 'Invalid param kind for param load');
  end;
end;

//Loads parameters from memory into registers as needed.
//Data is loaded from the locations specified by the ILItem parameters into the
//registers specified by ILItem.Param1Alloc and ILItem.Param2Alloc, if any registers
//are specified there.
procedure LoadBeforePrimNG(ILItem: PILItem; Prim: PPrimitiveNG);
var Swap: Boolean;
begin
  //Select a register loading order so the second load won't trash the first.
  //A will get trashed by an 8-bit load into a register other than A
  Swap := ILItem.Param1.Reg = rA;

  if Swap then
  begin
    LoadParamNG(ILItem, 2, Prim);
    LoadParamNG(ILItem, 1, Prim);
  end
  else
  begin
    LoadParamNG(ILItem, 1, Prim);
    LoadParamNG(ILItem, 2, Prim);
  end;
end;


//If validation is enabled, applies such validation.
//This routine is called after the primitive has executed.
//The validation routine (if there is one) is specifiied in the 'Validate' column
//of the Primitives table (spreadsheet)
procedure ValidateAfterPrimNG(ILItem: PILItem;Prim: PPrimitiveNG);
begin
  if cgOverflowCheck in ILItem.CodeGenFlags then
    //Validation for the operation itself
    GenCode(Prim.ValidateProcName, ILItem);
end;
(*
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

procedure StoreToVariable(ILItem: PILItem;OpType: TOpType);
var
  V: PVariable;
  Reg: Char;
  StoreStr: String;
  Suffix: String;
begin
  V := ILItem.Dest.ToVariable;
  case V.Storage of
    vsAbsolute: StoreStr := 'store_abs';
    vsRelative: StoreStr := 'store_rel';
  end;

  //Store the output to the appropriate destination
  case ILItem.Dest.Reg of
    rNone: ;
    rA..rL:
    begin
      if V.Storage = vsRelative then
        Suffix := ''
      else if ILItem.Dest.Reg = rA then
        Suffix := '_a'
      else
        Suffix := '_via_a';

      //Are we storing to a 1 or two byte destination?
      case OpTypeSize[ILItem.ResultType] of
        1: GenLibraryProc(StoreStr + '8_r8' + Suffix,ILItem);
        2:
          //Do we need to sign extend?
          if (OpType in [rtS8, rtX8]) and
            ((ILItem.ResultType = rtS16) or
            ((ILItem.ResultType = rtU16) and not (cgOverflowCheck in ILItem.CodeGenFlags))) then
          begin
            GenLibraryProc(StoreStr + '16low_r8' + Suffix,ILItem);
            Reg := CPUReg8ToChar[ILItem.Dest.Reg];
            GenSignExtend(Reg, 'a');  ///'a'!!!
            GenLibraryProc(StoreStr + '16high_a',ILItem);
          end
          else
            GenLibraryProc(StoreStr + '16_r8' + Suffix,ILItem);
      else
        raise Exception.Create('Invalid type size in StoreAfterPrim');
      end;
    end;
    rHL..rBC:
      //Are we storing to a 1 or two byte destination?
      case OpTypeSize[ILItem.ResultType] of
        //When shortening, any validation should have been done above
        1: GenLibraryProc(StoreStr + '8_r16low_via_a', ILItem);
        2: GenLibraryProc(StoreStr + '16_r16',ILItem);
      else
        raise Exception.Create('Invalid type size in StoreAfterPrim');
      end;
    rZF:
      begin //For assignments
        GenLibraryProc('zftoboolean', ILItem);
        GenLibraryProc(StoreStr + '8_a', ILItem);
      end;
    rZFA:
      begin //For assignments
        GenLibraryProc('notatoboolean', ILItem);
        GenLibraryProc(StoreStr + '8_a', ILItem);
      end;
    rNZF:
      begin //For assignments
        GenLibraryProc('nzftoboolean', ILItem);
        GenLibraryProc(StoreStr + '8_a', ILItem);
      end;
    rNZFA:
      begin //For assignments
        GenLibraryProc('atoboolean', ILItem);
        GenLibraryProc(StoreStr + '8_a', ILItem);
      end;
    rCPLA:
      begin //For assignments
        GenLibraryProc('cpla', ILItem);
        GenLibraryProc(StoreStr + '8_a', ILItem);
      end;
    rCF:
      begin //For assignments
        GenLibraryProc('cftoboolean', ILItem);
        GenLibraryProc(StoreStr + '8_a', ILItem);
      end;
    rNCF:
      begin //For assignments
        GenLibraryProc('ncftoboolean', ILItem);
        GenLibraryProc(StoreStr + '8_a', ILItem);
      end;
    else
      raise Exception.Create('Illegal DestAlloc in AllocAfterPrim');
  end;
end;
*)
//Takes the result from the register specified in ILItem.DestAlloc and stores it
//into the location specified in ILItem.Dest
//If a type conversion is to take place then:
// * if validation is enabled, will generate code to ensure the value will fit into
//the destination type
// * generates code to handle the type conversion (if necessary)
procedure StoreAfterPrimNG(ILItem: PILItem;Prim: PPrimitiveNG);
var
  OpType: TOpType;
  ValProcName: String;
begin
  case ILItem.OpType of
    rtM16S16: OpType := rtS16;
    rtM16U16: OpType := rtU16;
  else
    OpType := ILItem.OpType;
  end;

  //Validation for type conversions, if needed. No conversion needed if operator has fixed result type
  if (OpType <> ILItem.ResultType) and (Operations[ILItem.Op].ResultType = teUnknown) then
    if cgOverflowCheck in ILItem.CodeGenFlags then
    begin
      ValProcName := '';
      //Do we have a special case validation routine for conversion to said type?
      case ILItem.ResultType of
        rtS8: ValProcName := Prim.ValidateToS8;
        rtU8: ValProcName := Prim.ValidateToU8;
        rtS16: ValProcName := Prim.ValidateToS16;
        rtU16: ValProcName := Prim.ValidateToU16;
      else
        raise Exception.Create('Unhandled type in StoreAfterPrimNG');
      end;
      if ValProcName = '' then
        //No special case so use validation matrix!!
        ValProcName := ConversionMatrix[OpType, ILItem.ResultType];

      GenCode(ValProcName, ILItem);
    end;

  case ILItem.Dest.Kind of
    pkNone, pkImmediate, pkPhiVar: Assert(False, 'Invalid destination location');
    pkVar:        StoreToVariable(ILItem, OpType);
    pkStack:      GenLibraryProc('push_word', ILItem);     //The stack
    pkStackByte:  GenLibraryProc('push_byte_a', ILItem);  //Single byte on the stack
  else
    Assert(False, 'Invalid Dest Location');
  end;
end;


//Generate a branch after the
procedure CondBranchAfterPrimNG(ILItem: PILItem;Prim: PPrimitiveNG);
begin
  //If True block is following block then generate conditional jump for False
  if ILItem.TrueBlockID = CurrBlockID + 1 then
    GenCondJump(ILItem, True, ILItem.FalseBlockID)
  else
  begin //Otherwise generate condition jump for True...
    GenCondJump(ILItem, False, ILItem.TrueBlockID);
    //...and False doesn't 'fall though' then an unconditional jump for it.
    if ILItem.FalseBlockID <> CurrBlockID + 1 then
      GenUncondJump(ILItem.FalseBlockID);
  end;
end;

//=========================

procedure DoCodeGenItem(ILIndex: Integer);
var ILItem: PILItem;
  Prim: PPrimitive;
  PrimNG: PPrimitiveNG;
  SwapParams: Boolean;
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


  Prim := nil;
  if Operations[ILItem.Op].IsNG then
  begin
    PrimNG := ILItemToPrimitiveNG(ILItem^, SwapParams);
    if not assigned(PrimNG) then
      Error('No primitiveNG found:'#13#10 + ILItem.ToString);
    if SwapParams then
      ILItem.SwapParams;

    if Assigned(PrimNG) then
    begin
      if LogPrimitives then
        PrimitiveLog.Add(PrimNG.ProcName);

      //Temp
      TEMPRegAllocNG(ILItem, PrimNG);
      LoadBeforePrimNG(ILItem, PrimNG);
      if Assigned(PrimNG.Proc) then
        PrimNG.Proc(ILItem)
      else
        GenLibraryProc(PrimNG.ProcName, ILItem);
      ValidateAfterPrimNG(ILItem, PrimNG);
      case ILItem.DestType of
        dtNone, dtDataLoad: ;
        dtData: StoreAfterPrimNG(ILItem, PrimNG);
        dtCondBranch: CondBranchAfterPrimNG(ILItem, PrimNG);
      else
        raise Exception.Create('Invalid destination after PrimitiveNG');
      end;
    end;
//Assert(False, 'TODO');
  end
  else  //OG Operators/Primitives
  begin
    case ILItem.Op of
      OpPhi:      ; //Ignore
      OpBranch:   GenUncondBranch(ILItem);
      OpFuncCall: Prim := PrimFindByProcName('proccall');
      OpDataLoad, OpMove: Prim := PrimFindByProcName('empty');
    else
      Prim := ILItemToPrimitive(ILItem^);
    if not assigned(Prim) then
      Error('No primitive found:'#13#10 + ILItem.ToString);
    end;

    if Assigned(Prim) then
    begin
      if LogPrimitives then
        PrimitiveLog.Add(Prim.ProcName);

      //Temp
      TEMPRegAlloc(ILItem, Prim);
      LoadBeforePrim(ILItem, Prim);
      if Assigned(Prim.Proc) then
        Prim.Proc(ILItem)
      else
        GenLibraryProc(Prim.ProcName, ILItem);
      ValidateAfterPrim(ILItem, Prim);
      case ILItem.DestType of
        dtNone, dtDataLoad: ;
        dtData: StoreAfterPrim(ILItem, Prim);
        dtCondBranch: CondBranchAfterPrim(ILItem, Prim);
      else
        raise Exception.Create('Invalid destination after Primitive');
      end;
    end
  end;
end;

procedure DoCodeGen;
var I: Integer;
begin
  for I := 0 to ILGetCount-1 do
    DoCodeGenItem(I);
end;

procedure GenPreamble(Scope: PScope;BlockType: TBlockType);
begin
  if Scope.Func = nil then
  begin
    if BlockType = btStack then
      GenLibraryProc('stacklocal_enter', nil)
  end
  else //Scope.Func <> nil
  begin
    Assert(BlockType = btDefault, 'Can''t override block type for functions');
    case Scope.Func.CallingConvention of
      ccStackLocal:
//        if VarGetLocalByteSize > 0 then
          GenLibraryProc('stacklocal_enter', nil)
//        else
;//          ??
      //TODO: More calling conventions
    else
      Assert(False, 'Unknown Calling Convention');
    end;
  end;
end;

procedure GenPostAmble(Scope: PScope;BlockType: TBlockType);
begin
  if Scope.Func = nil then
  begin
    if BlockType = btStack then
      GenLibraryProc('stacklocal_exit', nil)
    else
      Instr('ret')
  end
  else //Scope.Func <> nil
  begin
     case Scope.Func.CallingConvention of
      ccStackLocal: GenLibraryProc('stacklocal_exit', nil);
      //TODO: More calling conventions
    else
      Assert(False, 'Unknown Calling Convention');
    end;
  end;
end;

function CodeGen(Scope: PScope;BlockType: TBlockType): Boolean;
begin
  CodeGenScope := Scope;
  try
    CodeGenErrorString := '';

    CurrErrorCount := 0;
    AsmCodeScope := Scope.AsmCode;
    AsmDataScope := Scope.AsmData;

    //Generate any global data
    DataGen;

    Line(';=========='+Scope.Name);
    Line('');

    CurrProcName := '_'+Scope.Name.ToLower;
    CurrBlockID := -1;
    CurrSourceLineNo := -1;
    LabelIndex := 1;

    //Calc variables size/offsets
    VarSetOffsets;

    GenLabel(CurrProcName);

    GenPreamble(Scope, BlockType);

    DoCodeGen;

    GenLabel(CurrProcName+IntToStr(CurrBlockID+1));

    GenPostamble(Scope, BlockType);
    Line('');
    Line(';----------'+Scope.Name);
    Line('');

    Result := True;
  except
    on E:Exception do
    begin
      CodeGenErrorString := E.Message;
      Line('CODEGEN ERROR: ' + E.Message);
      Result := False;
    end;
  end;
end;

procedure InitPrimitives;
begin
  //Assignment
  PrimSetProc('error', ProcError);
  PrimSetProc('empty',ProcEmpty);

  PrimSetProc('proc_assign_relS16_imm8',ProcAssignRelS16Imm8);
  PrimSetProc('proc_assign_absS16_imm8',ProcAssignAbsS16Imm8);

  PrimSetProc('b7sov',ProcDestB7SetOverflow);
  PrimSetProc('b15sov',ProcDestB15SetOverflow);

  PrimSetProc('proc_dec8_reg',ProcDec8Reg);
  PrimSetProc('proc_dec16_reg',ProcDec16Reg);
  PrimSetProc('proc_inc8_reg',ProcInc8Reg);
  PrimSetProc('proc_inc16_reg',ProcInc16Reg);

  PrimSetProc('proccall',ProcCall);

  ValidatePrimitives;

//  PrimSetProc('proctypecastX8X16R',ProcTypecastX8X16R);
end;

procedure InsertPreamble(PlatformFile, QuicheLibrary: String);
begin
  AsmCodeFull.Add(';Quiche object code');
  AsmCodeFull.Add(';Auto-created. Will be overwritten!');
  AsmCodeFull.Add(';Designed for RASM assembler');
  AsmCodeFull.Add('');
  AsmCodeFull.Add(';Insert platform specific code');
  AsmCodeFull.Add('include "' + PlatformFile + '"');
  AsmCodeFull.Add('');
  AsmCodeFull.Add(';Insert Quiche libraries');
  AsmCodeFull.Add('include "' + QuicheLibrary + '"');
  AsmCodeFull.Add('');
  AsmCodeFull.Add(';Generated code starts here');
  AsmCodeFull.Add('quiche:');
end;

procedure InitialiseCodeGen(PlatformFile, QuicheLibrary: String);
begin
  if PrimitiveLog = nil then
    PrimitiveLog := TStringList.Create
  else
    PrimitiveLog.Clear;

  InitPrimitives;
  CurrErrorCount := 0;
  TotalErrorCount := 0;

  if AsmCodeFull = nil then
    AsmCodeFull := TStringList.Create
  else
    AsmCodeFull.Clear;
  AsmCodeScope := nil;
  if AsmDataFull = nil then
    AsmDataFull := TStringList.Create
  else
    AsmDataFull.Clear;
  AsmDataScope := nil;
  InsertPreAmble(PlatformFile, QuicheLibrary);
end;

function UsesPrimitive(const Name: String): Boolean;
var S: String;
begin
  for S in PrimitiveLog do
    if CompareText(S, Name) = 0 then
      EXIT(True);
  Result := False;
end;

initialization
  LogPrimitives := False;

  PrimitiveLog := nil;
  AsmCodeFull := nil;
  AsmDataFull := nil;
finalization
  PrimitiveLog.Free;
  AsmCodeFull.Free;
  AsmDataFull.Free;
end.

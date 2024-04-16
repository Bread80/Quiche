unit Z80.CodeGen;

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
  PrimitivesEx, Compiler, Functions,
  Z80.CPU, Z80.Optimise, Z80.CPUState;

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

procedure GenLibraryParamProc(ProcName: String;const Param: TILParam;const Prefix: String);
var Code: String;
begin
{  if ProcName.Chars[0] = ':' then
    Instr('call ' + ProcName.SubString(1) + ' ;Call')
  else
}  begin
    Code := Fragments.FragmentParamSub(ProcName, Param, Prefix);
    if Code = '' then
      raise Exception.Create('Validation library code not found: ' + ProcName);
    Line('                     ;Fragment: ' + ProcName);
    Lines(Code);
  end;
end;

procedure GenCode(ProcName: String;ILItem: PILItem);
var
  Prim: PPrimitiveNG;
  Proc: TCodeGenProc;
begin
  if ProcName = 'empty' then
    EXIT;
  if ProcName = '' then
    EXIT;

  if ProcName.Chars[0] = ':' then
    GenLibraryProc(ProcName, ILItem)
  else
  begin
    Prim := PrimFindByProcNameNG(ProcName);
    if Assigned(Prim) then
    begin
      Line('                     ;Prim: ' + ProcName);
      Prim.Proc(ILItem);
    end
    else
    begin
      Proc := FindCodeGenProc(ProcName);
      if Assigned(Proc) then
        Proc(ILItem)
      else
        GenLibraryProc(ProcName, ILItem);
    end;
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
  Assert(ILItem.GetBranchBlockiD <> -1);
  GenUncondJump(ILItem.GetBranchBlockID);
end;

//Generates a conditional jump, using Zero or Carry flag, to the given block
//If ZeroFlag is true, the jump uses the Zero flag, otherwise the Carry flag
//If Reverse is True the condition is inverted (i.e. Zero becomes Not Zero,
//Carry Set becomse Carry Clear).
procedure GenCondJump(ILItem: PILItem;Reverse: Boolean;BlockID: Integer);
var F: String;
begin
//  Reverse := Reverse xor ILItem.BranchInvert;
  Assert(ILItem.Dest.Kind = pkCondBranch);
  case ILItem.Dest.Reg of
    rA:
    begin
//      if ILItem.OpType <> rtBoolean then
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

//Generate code to move FromReg to ToReg
//Currently only allows 'main' registers (ABCDEHL)
procedure GenRegMove(FromReg, ToReg: TCPUReg;Signed: Boolean);
begin
  Assert(FromReg <> ToReg);
  if FromReg in [rA, rB, rC, rD, rE, rH, rL] then
  begin
    if ToReg in [rA, rB, rC, rD, rE, rH, rL] then
      //8-bit to 8-bit
      Opcode('ld',CPUReg8ToChar[ToReg], CPUReg8ToChar[FromReg])
    else if ToReg in [rBC, rHL, rDE] then
    begin //8-bit to 16-bit
      if FromReg <> CPURegPairToLow[ToReg] then
        Opcode('ld',CPURegLowToChar[ToReg], CPUReg8ToChar[FromReg]);
      if Signed then  //Sign extend (if needed)
        if FromReg = rA then
          GenSignExtend(CPUReg8ToChar[FromReg], CPURegHighToChar[ToReg])
        else
          GenSignExtend(CPURegLowToChar[ToReg], CPURegHighToChar[ToReg])
      else
      Opcode('ld',CPURegHighToChar[ToReg], '$00');
    end
    else
      Assert(False);
  end
  else if (FromReg in [rHL, rDE]) and (ToReg in [rHL, rDE]) then
  begin
    Assert(FromReg <> ToReg);
    Opcode('ex','hl','de');
  end
  else if FromReg in [rHL, rDE, rBC] then
  begin
    Assert(ToReg in [rHL, rDE, rBC]);
    Opcode('ld',CPURegLowToChar[ToReg],CPURegLowToChar[FromReg]);
    Opcode('ld',CPURegHighToChar[ToReg],CPURegHighToChar[FromReg]);
  end;
end;


//Generates the code to convert a boolean value from various sources into a boolean
//value in A
procedure GenToBoolean(Reg: TCPUReg;const Param: TILParam);
begin
  case Reg of
    rA, rAF: ;
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
  Assert(ILItem.Dest.Kind = pkVarDest);

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
  Assert(ILItem.Dest.Kind = pkVarDest);

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



//Register allocation (temporary)
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
        if ILItem.Dest.Kind = pkPushByte then
          Regs := [rA]
        else if ILItem.Dest.Kind = pkPush then
          Regs := [rHL, rDE, rBC]
//            Regs := [rA, rB, rD, rH]
        else if GetTypeSize(ILItem.Param1.GetVarType) = 1 then
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

  case ILItem.Op of
    opBranch, opDataLoad: Reg := rA;  //Dummy value - The rest of this code only operates if = rNone
  else
    case ILItem.Dest.Kind of
      pkNone: Reg := rA;  //Dummy
      pkCondBranch, pkVarDest, pkPush, pkPushByte:
        Reg := ILItem.Dest.Reg
    else
      Assert(False);
    end;
  end;

  if Reg = rNone then
  begin
    if ILItem.Op = opFuncCall then
    begin
      if ILItem.Func.ResultCount > 0 then
        if GetTypeSize(ILItem.Func.FindResult.VarType) = 1 then
          Reg := rA
        else
          Reg := rHL
    end
    else  //Use result data from Primitive
      if Prim.ResultInLReg then
      begin
        if ILItem.Dest.Kind in [pkPushByte, pkPush] then
          case ILItem.Param1.Reg of
            rA: Reg := rAF;
            rB: Reg := rBC;
            rD: Reg := rDE;
            rH: Reg := rHL;
          else
            Reg := ILItem.Param1.Reg;
          end
        else
          Reg := ILItem.Param1.Reg;
      end
      else
        Reg := Prim.ResultReg;

    case ILItem.Dest.Kind of
      pkCondBranch, pkVarDest, pkPush, pkPushByte:
        ILItem.Dest.Reg := Reg
    else
      Assert(False);  //Other options should have been filtered out above
    end;
  end;
end;

procedure LoadParamNG(const Param: TILParam;Prim: PPrimitiveNG);
var
  Variable: PVariable;
  V: PVariable;

  Prefix: String;
  StoreStr: String;
  Suffix: String;
begin
  Prefix := 'load_';

  case Param.Kind of
    pkNone: ; //No param to load
    pkImmediate:
      case Param.Reg of
        rNone, rImm: ; //Nothing to do. Imm is handled by Primitive itself
        rA..rL:         GenLibraryParamProc(Prefix + 'r8_imm', Param, 'p');
        rHL, rDE, rBC:  GenLibraryParamProc(Prefix + 'r16_imm', Param, 'p');
      else
        Assert(False, 'Invalid Reg for load');
      end;
    pkVarSource:
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
              GenLibraryParamProc(Prefix + 'r8' + StoreStr + 'high' + Suffix, Param, 'p')
            else if pfnLoadRPLow in Prim.Flags then
              GenLibraryParamProc(Prefix + 'r8' + StoreStr + 'low' + Suffix, Param, 'p')
            else
              GenLibraryParamProc(Prefix + 'r8' + StoreStr + Suffix, Param, 'p');
          end;
        rHL..rBC:
          begin
            Variable := Param.ToVariable;
            case GetTypeSize(Variable.VarType) of
              1:
              begin
                GenLibraryParamProc(Prefix + 'r16low' + StoreStr + 'low' + Suffix, Param, 'p');
                if Variable.VarType = vtInt8 then
                  GenSignExtend(CPURegLowToChar[Param.Reg], CPURegHighToChar[Param.Reg])
                else
                  GenLibraryParamProc(Prefix + 'r16high_zero', Param, 'p');
              end;
              2: GenLibraryParamProc(Prefix + 'r16'+StoreStr, Param, 'p');
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

//Sub to LoadBeforePrim
//Loads the Param in the register specified in the Param whilst preserving the value
//in the register named in Reg. (Reg can either not be touched, or can be moved
//elsewhere (e.g. another register or the stack) and move back before the function
//returns.
procedure LoadPreservingNG(const Param: TILParam;Reg: TCPUReg;Prim: PPrimitiveNG);
var
  V: PVariable;
  PreserveIn: TCPUReg;
begin
  PreserveIn := rNone;
  //Do we need to move P1 to avoid it getting trashed during the load?
  //Trashing only happens if we load an 8-bit from static address (which
  //needs to go via A)
  if (Reg = rA) and (Param.Kind = pkVarSource) then
  begin
    V := Param.Variable;
    if (V.Storage = vsStatic) and (GetTypeSize(V.VarType) = 1) then
    begin //Find a register to preserve A into (which is neither A or the Param.Reg)
      PreserveIn := CPUStateAllocReg8([rA, Param.Reg]);
      GenRegMove(Reg, PreserveIn, False);
    end;
  end;

  LoadParamNG(Param, Prim);

  if PreserveIn <> rNone then
    GenRegMove(PreserveIn, Reg, False);
end;

procedure LoadWithMoveNG(const Param: TILParam; FromReg, ToReg: TCPUReg;
  Prim: PPrimitiveNG);
var
  MoveBefore: Boolean;  //Do the move before or after the load?
begin
  //Move before if we're loading into FromReg, or if we're using FromReg during the load
  //(only A can be trashed by a load. It's easier to always move before if FromReg is A)
  MoveBefore := (FromReg = Param.Reg) or (FromReg = rA);
  if MoveBefore then
    GenRegMove(FromReg, ToReg, False);
  LoadParamNG(Param, Prim);
  if not MoveBefore then
    GenRegMove(FromReg, ToReg, False);
end;

procedure LoadBothNG(ILItem: PILItem; Prim: PPrimitiveNG);
begin
  //Select a register loading order so the second load won't trash the first.

  //A will get trashed by an 8-bit load into a register other than A, so
  //swap load order just in case
  if ILItem.Param1.Reg = rA then
  begin
    LoadParamNG(ILItem.Param2, Prim);
    LoadParamNG(ILItem.Param1, Prim);
  end
  else
  begin
    LoadParamNG(ILItem.Param1, Prim);
    LoadParamNG(ILItem.Param2, Prim);
  end;
end;

//Loads parameters from memory* into registers as needed.
//Data is loaded from the locations specified by the ILItem parameters into the
//registers specified by ILItem.Param1Alloc and ILItem.Param2Alloc, if any registers
//are specified there.
//* - can also handle parameters which are already in registers
procedure LoadBeforePrimNG(ILItem: PILItem; Prim: PPrimitiveNG);
var Swap: Boolean;
  P1Reg: TCPUReg; //If P1 is already in a register
  P2Reg: TCPUReg; //If P2 is already in a register
  P1Move: Boolean;  //P1 is already in a register but not the required one
  P2Move: Boolean;  //P2 is already in a register but not the required one
begin
//TODO: opDataLoad (not on NG yet): We can have third param!
//      ...and we could have multiple DataMoves
//  if (ILItem.Op = opDataLoad) and (ILItem.Param3.Kind <> pkNone) then
//    Assert(False, 'Can''t handle DataLoad with three parameters yet');

  if ILItem.Param1.Kind = pkVarSource then
    P1Reg := RegStateFindVariable(ILItem.Param1.Variable, ILItem.Param1.VarVersion)
  else
    P1Reg := rNone;
  P1Move := P1Reg <> ILItem.Param1.Reg;

  if ILItem.Param2.Kind = pkVarSource then
    P2Reg := RegStateFindVariable(ILItem.Param2.Variable, ILItem.Param2.VarVersion)
  else
    P2Reg := rNone;
  P2Move := P2Reg <> ILItem.Param2.Reg;

  if P1Reg = rNone then
  begin //Load P1 data
    if P2Reg = rNone then
      //Load P2 data
      LoadBothNG(ILItem, Prim)
    else if P2Move then
      //P2 is in a Reg but needs moving
      LoadWithMoveNG(ILItem.Param1, P2Reg, ILItem.Param2.Reg, Prim)
    else //P2 is in the correct Reg
      LoadPreservingNG(ILItem.Param1, P2Reg, Prim)
  end
  else if P1Move then
  begin //P1 is in a Reg but needs moving
    if P2Reg = rNone then
      //Load P2 data
      LoadWithMoveNG(ILItem.Param2, P1Reg, ILItem.Param1.Reg, Prim)
    else if P2Move then
    begin //P2 is in a Reg but needs moving
      Assert(False);
    end
    else
    begin //P2 is in the correct Reg
      Assert(False);
    end
  end
  else
  begin //P1 is in the correct Reg
    if P2Reg = rNone then
      //Load P2 data
      LoadPreservingNG(ILItem.Param2, P1Reg, Prim)
    else if P2Move then
    begin //P2 is in a Reg but needs moving
      Assert(False);
    end
    else
    begin //P2 is in the correct Reg
      //Do nothing
    end
  end
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

const //Routine names to keep the table source code size reasonable
  b7s = 'bit7_set_overflow';    //test if bit 7 set. If so, overflow
  b15s = 'bit15_set_overflow';  //test if bit 15 is set. If so, overflow
  h9neq = 'high9_neq_overflow'; //overflow unless all of the highest 9 bits are equal (ie. all set or all clear)
  h9nz = 'high9_nz_overflow';   //raise an overflow error unless the highest 9 bits are zero
  hbnz = 'high_nz_overflow';    //overflow if the high register is non-zero

//Specifies the routine to use to validate a conversion from the type given in the row
//to the type given in the column.
//'' (empty): no validation is necessary for this conversion
//If a more optimised routine is available this can be specified in the primitives
//table ('Special validations on type conversion'). If so that routine will be used
//in preference to this table.
const ValidationMatrix: array[vtInt8..vtPointer,vtInt8..vtPointer] of String =
//  To:
//From    Int8  Integer Byte  Word  Pointer
{Int8}    (('',   '',   b7s,  b7s,  b7s),
{Integer} (h9neq, '',   h9nz, b15s, b15s),
{Byte}    (b7s,   '',   '',   '',   ''),
{Word}    (h9nz,  b15s, hbnz,  '',  ''),
{Pointer} (h9nz,  b15s, hbnz, '',   ''));

//Generate a branch after the
procedure CondBranchAfterPrimNG(ILItem: PILItem;Prim: PPrimitiveNG);
begin
  //If True block is following block then generate conditional jump for False
  if ILItem.Param3.TrueBlockID = CurrBlockID + 1 then
    GenCondJump(ILItem, True, ILItem.Param3.FalseBlockID)
  else
  begin //Otherwise generate condition jump for True...
    GenCondJump(ILItem, False, ILItem.Param3.TrueBlockID);
    //...and False doesn't 'fall though' then an unconditional jump for it.
    if ILItem.Param3.FalseBlockID <> CurrBlockID + 1 then
      GenUncondJump(ILItem.Param3.FalseBlockID);
  end;
end;

//Store a value into a variable. Param contains the data for the variable including the
//Reg which currently contains the data.
//FromType is the current type of the data. This routine will arrange for it to be
//extended (or shortened) as necessary. No validation is performed (that should have
//been done by the caller)
procedure StoreToVariable(const Param: TILParam;FromType: TVarType);
var
  Reg: Char;
  StoreStr: String;
  Suffix: String;
begin
  Assert(Param.Kind = pkVarDest);

  case Param.Variable.Storage of
    vsStatic: StoreStr := 'store_abs';
    vsStack: StoreStr := 'store_rel';
  end;

  //Store the output to the appropriate destination
  case Param.Reg of
    rNone: ;
    rA..rL: //We have data in an 8-bit register
    begin
      SetRegStateVariable(Param.Reg, Param.Variable, Param.VarVersion);

      if Param.Variable.Storage = vsStack then
        Suffix := ''
      else if Param.Reg = rA then
        Suffix := '_a'
      else
        Suffix := '_via_a';

      //Are we storing to a 1 or two byte destination?
      case GetTypeSize(Param.Variable.VarType) of
        1:
          case GetTypeSize(FromType) of
            1: GenLibraryParamProc(StoreStr + '8_r8' + Suffix, Param, 'd');
            //If destination is 16 bit, zero extend data
//            2: GenLibraryParamProc(StoreStr + '16_r8' + Suffix, ILItem);
          else
            raise Exception.Create('Invalid variable size in StoreAfterPrim');
          end;
        2:
          //Do we need to sign extend?
          if IsSignedType(FromType) then
          begin //Sign extend
            GenLibraryParamProc(StoreStr + '16low_r8' + Suffix, Param, 'd');
            Reg := CPUReg8ToChar[Param.Reg];
            GenSignExtend(Reg, 'a');  ///'a'!!!
            GenLibraryParamProc(StoreStr + '16high_a', Param, 'd');
          end
          else //Zero extend
            GenLibraryParamProc(StoreStr + '16_r8' + Suffix, Param, 'd');
      else
        raise Exception.Create('Invalid type size in StoreAfterPrim');
      end;
    end;
    rHL..rBC:
    begin
      SetRegStateVariable(Param.Reg, Param.Variable, Param.VarVersion);

      if Param.Variable.Storage = vsStack then
        Suffix := ''
      else if Param.Reg = rA then
        Suffix := '_a'
      else
        Suffix := '_via_a';

      //Are we storing to a 1 or two byte destination?
      case GetTypeSize(Param.Variable.VarType) of
        //When shortening, any validation should have been done above
        1: GenLibraryParamProc(StoreStr + '8_r16low' + Suffix, Param, 'd');
        2: GenLibraryParamProc(StoreStr + '16_r16', Param, 'd');
      else
        raise Exception.Create('Invalid type size in StoreAfterPrim');
      end;
    end;
    rZF, rZFA, rNZF, rNZFA, rCPLA, rCF, rNCF:
    begin //For assignments
      GenToBoolean(Param.Reg, Param);
      GenLibraryParamProc(StoreStr + '8_a', Param, 'd');
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
procedure StoreAfterPrimNG(ILItem: PILItem;Prim: PPrimitiveNG);
var
  DestType: TVarType;
  ValProcName: String;
begin
  case ILItem.Op of
    opDataLoad: ;
  else
    case ILItem.Dest.Kind of
      pkNone: EXIT;
      pkVarDest:
      begin
        DestType := ILItem.Dest.Variable.VarType;

        //Validation for type conversions, if needed. No conversion needed if operator has fixed result type
        if IsNumericType(DestType) then
          if (DestType <> vtUnknown) and (ILItem.ResultType <> DestType) then
            if cgOverflowCheck in ILItem.CodeGenFlags then
            begin
              ValProcName := '';
              //Do we have a special case validation routine for conversion to said type?
              case ILItem.ResultType of
                vtInt8: ValProcName := Prim.ValidateToS8;
                vtByte: ValProcName := Prim.ValidateToU8;
                vtInteger: ValProcName := Prim.ValidateToS16;
                vtWord, vtPointer: ValProcName := Prim.ValidateToU16;
              else
                raise Exception.Create('Unhandled type in StoreAfterPrimNG');
              end;
              if ValProcName = '' then
                //No special case so use validation matrix!!
                ValProcName := ValidationMatrix[ILItem.ResultType, DestType];

              if ValProcName <> '' then
                GenCode(ValProcName, ILItem);
            end;

        StoreToVariable(ILItem.Dest, ILItem.ResultType);
      end;
      pkCondBranch:
        CondBranchAfterPrimNG(ILItem, Prim);
      pkPush:
        GenLibraryProc('push_word', ILItem);     //The stack
      pkPushByte:
      begin
        GenToBoolean(ILItem.Dest.Reg, ILItem.Dest);
        GenLibraryProc('push_byte_a', ILItem);  //Single byte on the stack
      end;
    else
      Assert(False, 'Invalid Dest Location');
    end;
  end;
end;

//=========================

procedure DoCodeGenItem(ILIndex: Integer);
var ILItem: PILItem;
  PrimNG: PPrimitiveNG;
  SwapParams: Boolean;
begin
  ILItem := ILIndexToData(ILIndex);

  if (ILItem.SourceLineNo <> -1) and (ILItem.SourceLineNo <> CurrSourceLineNo) then
  begin //Output source code line
    CurrSourceLineNo := ILItem.SourceLineNo;
    Line(';' + IntToStr(CurrSourceLineNo) + ': ' + Parser.Source[CurrSourceLineNo].Trim);
  end;  //Output block ID

  Line(';IL-' + ILIndex.ToString +': ' + ILItem.ToString);

  if ILItem.BlockID >= 0 then
  begin
    CurrBlockID := ILItem.BlockID;
    Line(CurrProcName + IntToStr(ILItem.BlockID) + ': ;' + ILItem.Comments);
    //Phi means we are the destination for multiple jumps. Assume nothing
    if ILItem.Op = opPhi then
      RegStateInitialise;
  end;


  PrimNG := nil;
  case ILItem.Op of
    opPhi: ; //Do nothing - ignore
    opBranch: GenUncondBranch(ILItem);
    opMove: PrimNG := PrimFindByProcNameNG('empty');
    opDataLoad: PrimNG := PrimFindByProcNameNG('empty');
    opFuncCall: PrimNG := PrimFindByProcNameNG('proccall');
    opStoreImm:
    begin
      Assert(ILItem.Dest.Kind = pkVarDest);
      PrimNG := PrimFindStoreImm(ILItem.Dest.Variable);
      Assert(PrimNG <> nil, 'StoreImm primitive not found' + ILItem.ToString);
    end;
  else
    //Find the Prim based on the operation and parameter data type(s)
    PrimNG := ILItemToPrimitiveNG(ILItem^, SwapParams);
    if not assigned(PrimNG) then
      Error('No primitiveNG found:'#13#10 + ILItem.ToString);
    if SwapParams then
      ILItem.SwapParams;
  end;

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
    RegStateInitialise;
    ValidateAfterPrimNG(ILItem, PrimNG);
    //Also generates branches
    StoreAfterPrimNG(ILItem, PrimNG);
  end;

  Line(';'+CPUStateToString);
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
  begin //We're at global scope
    if BlockType = btStack then
      GenLibraryProc('stacklocal_enter', nil)
  end
  else //Scope.Func <> nil - we're generating function code
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
  RegStateInitialise;
  //TODO: For Register calling convention, set CPUState any parameter

  CodeGenScope := Scope;
  try
    CodeGenErrorString := '';

    CurrErrorCount := 0;
    AsmCodeScope := Scope.AsmCode;
    AsmDataScope := Scope.AsmData;

    CreateVarMap;

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
    Line(VarMapToString);
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
  //Assign Procs used by Primitives table
  //PrimSetProc will raise an error if the Proc is not used anywhere
  //Unused Proc probably ought to be removed
  PrimSetProc('error', ProcError);
  PrimSetProc('empty',ProcEmpty);

  PrimSetProc('proc_assign_relS16_imm8',ProcAssignRelS16Imm8);
  PrimSetProc('proc_assign_absS16_imm8',ProcAssignAbsS16Imm8);

  PrimSetProc('bit7_set_overflow',ProcDestB7SetOverflow);
  PrimSetProc('bit15_set_overflow',ProcDestB15SetOverflow);

  PrimSetProc('proc_dec8_reg',ProcDec8Reg);
  PrimSetProc('proc_dec16_reg',ProcDec16Reg);
  PrimSetProc('proc_inc8_reg',ProcInc8Reg);
  PrimSetProc('proc_inc16_reg',ProcInc16Reg);

  PrimSetProc('proccall',ProcCall);

  //Verifies that all fragments, library routines(??), used by primitives
  //are available, and any Proc which need to be assigned have been assigned
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

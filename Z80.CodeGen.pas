unit Z80.CodeGen;

interface
uses ILData, PrimitivesEx, Scopes, Globals, Functions,
  Z80.CPU, Variables, Z80.Load;



//Generates the code to convert a boolean value from various sources into a boolean
//value in A
procedure GenToBoolean(Reg: TCPUReg;const Param: TILParam;Options: TMoveOptionSet);

//Insert code from a fragment or a library call
procedure GenLibraryProc(ProcName: String;ILItem: PILItem);

//Generate code from a fragment which is specific to a parameter
procedure GenLibraryParamProc(ProcName: String;const Param: TILParam;const Prefix: String);

procedure GenCode(ProcName: String;ILItem: PILItem);

//Generate the code form the start of a function (ie. generate a stack frame, if needed)
procedure GenFunctionPreamble(Scope: PScope;BlockType: TBlockType);

//Generate code at function end (ie. stack teardown, if needed)
procedure GenFunctionPostAmble(Scope: PScope;BlockType: TBlockType);

//Generate the code to call a function
procedure ProcCall(Func: PFunction);

//----------------

//Allocate which registers (if any) parameters need to be loaded (or moved) into
//before a primitive can be generated
procedure TEMPRegAllocNG(var ILItem: PILItem;const Prim: PPrimitiveNG);

//Allocate registers for functions using Register calling convention
procedure TEMPRegAllocFunc(Func: PFunction);

//------------------

procedure InitPrimitives;

implementation
uses Classes, SysUtils,
  CodeGen, Fragments, QTypes, Operators,
  IDE.Compiler, //<-- Included here ONLY to access assembler output meta-data options
  Z80.CPUState, Z80.LoadStoreMove;

//================================== LIBRARY CODE


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

//Insert code from a fragment or a library call
procedure GenLibraryProc(ProcName: String;ILItem: PILItem);
var Code: String;
begin
  if ProcName.Chars[0] = ':' then
    Instr('call ' + ProcName.SubString(1) + ' ;Call')
  else
  begin
    Code := Fragments.FragmentSub(ProcName, ILItem, GetCodeGenScope);
    if Code = '' then
      raise Exception.Create('Validation library code not found: ' + ProcName);
    if IDE.Compiler.Config.CodeGen.FragmentNames then
      Line('                     ;Fragment: ' + ProcName);
    Lines(Code);
  end;
end;

//Generate code from a fragment which is specific to a parameter
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
    if IDE.Compiler.Config.CodeGen.FragmentNames then
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
    if IDE.Compiler.Config.CodeGen.PrimitiveNames then
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

//======================= PROGRAMMATIC CODE GENERATION

//Generate the code form the start of a function (ie. generate a stack frame, if needed)
procedure GenFunctionPreamble(Scope: PScope;BlockType: TBlockType);
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
      ccRegister:
      begin
        //Allocate registers to parameters (unless they've already been allocated,
        //either in the declaration or by the register allocator)
        TEMPRegAllocFunc(Scope.Func);

        //Copy registers to global storage (unless the global storage has been
        //optimised away)
        GenFuncArgStore(Scope.Func);
      end;
      ccStack:
          //TODO: Add register allocation for Result
//        TODO: if VarGetLocalByteSize > 0 then
          GenLibraryProc('stacklocal_enter', nil)
//        else
;//          ??
    else
      Assert(False, 'Unknown Calling Convention');
    end;
  end;
end;

//Generate code at function end (ie. stack teardown, if needed)
procedure GenFunctionPostAmble(Scope: PScope;BlockType: TBlockType);
begin
  Line(';Postamble');
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
      ccRegister: //Load any return parameters into registers
      begin
        GenFuncReturnLoad(Scope.Func);
        Instr('ret')
      end;
      ccStack:
        //TODO: Use GenFuncParamLoad as for Register. We'll need to add the register
        //allocation to the preamble code
         GenLibraryProc('stacklocal_exit', nil);
    else
      Assert(False, 'Unknown Calling Convention');
    end;
  end;
end;

//====================================

procedure ProcError(ILItem: PILItem);
begin
  Error('No operation specified or illegal operation')
end;

procedure ProcEmpty(ILItem: PILItem);
begin
  //Do nothing
end;

//=====================================Maths

procedure ProcDec8Reg(ILItem: PILItem);
var Count: Integer;
  I: Integer;
begin
  if ILItem.Param2.Kind = pkNone then
    Count := 1
  else
    Count := ILItem.Param2.Imm.ToInteger;

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
    Count := ILItem.Param2.Imm.ToInteger;

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
    Count := ILItem.Param2.Imm.ToInteger;

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
    Count := ILItem.Param2.Imm.ToInteger;

  for I := 1 to abs(Count) do
    if Count > 0 then
      GenLibraryProc('inc16_reg', ILItem)
    else
      GenLibraryProc('dec16_reg', ILItem);
end;

procedure ProcCall(Func: PFunction);
var Code: String;
begin
  Assert(Func <> nil);
  Code := Func.GetCallInstruction;
  Instr(Code);
end;

//============================== REGISTER ALLOCATION

//Register allocation (temporary)
//----------------------------------------------------------

//Allocate which registers (if any) parameters need to be loaded (or moved) into
//before a primitive can be generated
procedure TEMPRegAllocNG(var ILItem: PILItem;const Prim: PPrimitiveNG);
var Reg: TCPUReg;
  Regs: TCPURegSet;
begin
  //These items don't need registers to be allocated
  Assert(not (ILItem.Op in [opUnknown, opPhi, opBranch,
    opRegLoad, opRegLoadExtended, opRegStore, opRegStoreExtended, opFuncCall, opFuncCallExtended]));

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

  if ILItem.Dest.Kind = pkNone then
    //Nothing to do
  else if not (ILItem.Dest.Kind in [pkCondBranch, pkVarDest, pkPush, pkPushByte]) then
    Assert(False) //Invalid/unknown ParamKind
  else
  begin
    //Use result data from Primitive
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

    ILItem.Dest.Reg := Reg
  end;
end;

procedure TEMPRegAllocFunc(Func: PFunction);

  procedure AddUsedReg(var Used: TCPURegSet;Reg: TCPUReg);
  begin
    case Reg of
      rA..rL: Used := Used + [Reg, CPUReg8ToPair[Reg]];
      rHL..rBC,rIX,rIY: Used := Used + [Reg, CPURegPairToHigh[Reg], CPURegPairToLow[Reg]];
    else  //Eg Flags
      Used := Used + [Reg];
    end;
  end;

var
  Results: Boolean;
  I: Integer;
  UsedRegs: TCPURegSet;
  Reg8: TCPUReg;  //Next 8 bit to be allocated
  Reg16: TCPUReg; //Next 16 bit to be allocated
begin
  Assert(Func.CallingConvention = ccRegister, 'We can only allocate registers when ccRegister is the calling convention');

  //Repeat for both entry and then exit parameters
  for Results := False to True do
  begin
    //Find any registers already allocated and mark as 'used'
    UsedRegs := [];
    for I := 0 to Func.ParamCount-1 do
      if Func.Params[I].Reg <> rNone then
        if (Results and (Func.Params[I].Access in [vaVal, vaVar, vaConst])) or
          //TODO: Var is Entry and exit where value passed (VarTypeSize <= 2)
          //                   but not where address is passed (VarType >= 2)
          (not Results and (Func.Params[I].Access in [vaVar, vaOut, vaResult])) then
          AddUsedReg(UsedRegs, Func.Params[I].Reg);

    //For each Parameter
    Reg8 := rA;
    Reg16 := rHL;

    for I := 0 to Func.ParamCount + Func.ResultCount -1 do
      if Func.Params[I].VarType <> vtUnknown then
        if Func.Params[I].Reg = rNone then
          if (Results and (Func.Params[I].Access in [vaVal, vaVar, vaConst])) or
            (not Results and (Func.Params[I].Access in [vaOut, vaResult])) then
            case GetTypeSize(Func.Params[I].VarType) of
              1:
              begin
                while (Reg8 in UsedRegs) and (Reg8 in CPUReg8Bit) do
                  inc(Reg8);
                Assert(Reg8 in CPUReg8Bit, 'Unable to allocate a register :(');
                Func.Params[I].Reg := Reg8;
                AddUsedReg(UsedRegs, Reg8);
              end;
              2:
              begin
                while (Reg16 in UsedRegs) and (Reg16 in CPUReg16Bit) do
                  inc(Reg16);
                Assert(Reg16 in CPUReg16Bit, 'Unable to allocate a register :(');
                Func.Params[I].Reg := Reg16;
                AddUsedReg(UsedRegs, Reg16);
              end;
            else
              Assert(False);
            end;
  end;
end;

//========================= CODE GENERATOR

procedure InitPrimitives;
begin
  //Assignment
  //Assign Procs used by Primitives table
  //PrimSetProc will raise an error if the Proc is not used anywhere
  //Unused Proc probably ought to be removed
  PrimSetProc('error', ProcError);
  PrimSetProc('empty',ProcEmpty);

(*  PrimSetProc('proc_assign_relS16_imm8',ProcAssignRelS16Imm8);
  PrimSetProc('proc_assign_absS16_imm8',ProcAssignAbsS16Imm8);

  PrimSetProc('bit7_set_overflow',ProcDestB7SetOverflow);
  PrimSetProc('bit15_set_overflow',ProcDestB15SetOverflow);
*)
  PrimSetProc('proc_dec8_reg',ProcDec8Reg);
  PrimSetProc('proc_dec16_reg',ProcDec16Reg);
  PrimSetProc('proc_inc8_reg',ProcInc8Reg);
  PrimSetProc('proc_inc16_reg',ProcInc16Reg);

//  PrimSetProc('proccall',ProcCall);

  //Verifies that all fragments, library routines(??), used by primitives
  //are available, and any Proc which need to be assigned have been assigned
  ValidatePrimitives;

//  PrimSetProc('proctypecastX8X16R',ProcTypecastX8X16R);
end;

end.

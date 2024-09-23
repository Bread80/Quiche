unit Z80.CodeGen;

interface
uses Def.Functions, Def.Globals, Def.IL, Def.Primitives, Def.Scopes, Def.Variables,
  Z80.CPU, Z80.Load;

//Insert code from a fragment or a library call
procedure GenLibraryProc(ProcName: String;ILItem: PILItem);

//Generate code from a fragment which is specific to a parameter
procedure GenLibraryParamProc(ProcName: String;const Param: TILParam;const Prefix: String);

procedure GenCode(ProcName: String;ILItem: PILItem);

//Generate the code form the start of a function (ie. generate a stack frame, if needed)
procedure GenFunctionPreamble(Scope: PScope;BlockType: TBlockType);

//Generate code at function end (ie. stack teardown, if needed)
procedure GenFunctionPostAmble(Scope: PScope;BlockType: TBlockType);

//------------------

procedure InitPrimitives;

implementation
uses Classes, SysUtils,
  Def.Operators, Def.QTypes,
  CodeGen, CG.Fragments,
  Z80.CPUState, Z80.LoadStoreMove, Z80.RegAlloc,
  IDE.Compiler; //<-- Included here ONLY to access assembler output meta-data options

//================================== LIBRARY CODE


//Insert code from a fragment or a library call
procedure GenLibraryProc(ProcName: String;ILItem: PILItem);
var Code: String;
begin
  if ProcName.Chars[0] = ':' then
    Instr('call ' + ProcName.SubString(1) + ' ;Call')
  else
  begin
    Code := CG.Fragments.FragmentSub(ProcName, ILItem, GetCodeGenScope);
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
    Code := CG.Fragments.FragmentParamSub(ProcName, Param, Prefix);
    if Code = '' then
      raise Exception.Create('Validation library code not found: ' + ProcName);
    if IDE.Compiler.Config.CodeGen.FragmentNames then
      Line('                     ;Fragment: ' + ProcName);
    Lines(Code);
  end;
end;

procedure GenCode(ProcName: String;ILItem: PILItem);
var
  Prim: PPrimitive;
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
    Prim := PrimFindByProcName(ProcName);
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
        TEMPRegAllocRegisterFunc(Scope.Func);

        //Copy registers to global storage (unless the global storage has been
        //optimised away)
        GenFuncArgStore(Scope.Func);
      end;
      ccStack:
      begin
        TEMPRegAllocStackFunc(Scope.Func);
          //TODO: Add register allocation for Result
//        TODO: if VarGetLocalByteSize > 0 then
        GenLibraryProc('stacklocal_enter', nil)
//        else
;//          ??
      end;
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

//========================= CODE GENERATOR

procedure InitPrimitives;
begin
  //PrimSetProc will raise an error if the Proc is not used anywhere
  //Unused Proc probably ought to be removed
  PrimSetProc('error', ProcError);
  PrimSetProc('empty', ProcEmpty);

  PrimSetProc('proc_dec8_reg',ProcDec8Reg);
  PrimSetProc('proc_dec16_reg',ProcDec16Reg);
  PrimSetProc('proc_inc8_reg',ProcInc8Reg);
  PrimSetProc('proc_inc16_reg',ProcInc16Reg);

  //Verifies that all fragments, library routines(??), used by primitives
  //are available, and any Proc which need to be assigned have been assigned
  ValidatePrimitives;
end;

end.

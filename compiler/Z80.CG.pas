unit Z80.CG;

interface
uses Def.Functions, Def.Globals, Def.IL, Def.Scopes, Def.Variables,
  Lib.Primitives,
  Z80.Load;

//Generate the code form the start of a function (ie. generate a stack frame, if needed)
procedure GenFunctionPreamble(Scope: PScope;BlockType: TBlockType);

//Generate code at function end (ie. stack teardown, if needed)
procedure GenFunctionPostAmble(Scope: PScope;BlockType: TBlockType);

//------------------

procedure InitPrimitives;

implementation
uses Classes, SysUtils,
  Def.Operators, Def.VarTypes, Def.UserTypes,
  Lib.GenFragments,
  CG.Data,
  Z80.LoadStoreMove, Z80.RegAlloc,
  Z80.CPUState,
  IDE.Compiler; //<-- Included here ONLY to access assembler output meta-data options

//======================= PROGRAMMATIC CODE GENERATION

//Generate the code form the start of a function (ie. generate a stack frame, if needed)
procedure GenFunctionPreamble(Scope: PScope;BlockType: TBlockType);
var InitLocalVarsLabel: String;
  LocalVarsToInit: Boolean;
  I: Integer;
  V: PVariable;
  TypeCode: Integer;

const
  tcEndOfData = 1;
  tcShortVector = 2;
  tcShortList = 3;
  tcLongVector = 4;
  tcLongList = 5;

begin
  if Scope.Func = nil then
  begin //We're at global scope
    if BlockType = btStack then
      GenFragmentName('stacklocal_enter')
  end
  else //Scope.Func <> nil - we're generating function code
  begin
    Assert(BlockType = btDefault, 'Can''t override block type for functions');
    InitLocalVarsLabel := '';
    case Scope.Func.CallingConvention of
      ccRegister:
      begin
        //Allocate registers to parameters (unless they've already been allocated,
        //either in the declaration or by the register allocator)
        TEMPRegAllocRegisterFunc(Scope.Func);

        //Copy registers to global storage (unless the global storage has been
        //optimised away)
        GenFuncArgStore(Scope.Func);

        InitLocalVarsLabel := '_q_initlocalvars_static';
      end;
      ccStack:
      begin
        TEMPRegAllocStackFunc(Scope.Func);
          //TODO: Add register allocation for Result
//        TODO: if VarGetLocalByteSize > 0 then
        GenLibraryProc('stacklocal_enter', nil);

        InitLocalVarsLabel := '_q_initlocalvars_stack';

        //TODO: Initialise Vectors and Lists
        //CALL _q_initlocalvars_stack
        //List of data - VarType and Offset
        //Vector: Address, Length
        //List: Address, Capacity (Length initialised to zero)

      end;
    else
      raise ECallingConvention.Create;
    end;

    //Types which have meta data need to initialised.
    //Currently that means vectors and lists.
    LocalVarsToInit := False;
    for I := 0 to Scope.VarList.GetCount-1 do
    begin
      V := Scope.VarList.IndexToData(I);
      Assert(V <> nil);
      if (V.VarType = vtArrayType) and (V.UserType.ArrayDef.ArrayType in [atVector, atList]) then
        if not V.IsParam then
        begin //Variable needs initialising
          //TODO: NOT if it's a CopyDataIn variable - but how to know?
          if not LocalVarsToInit then
          begin
            AsmLine('call ' + InitLocalVarsLabel);
            LocalVarsToInit := True;
          end;
          case V.UserType.ArrayDef.ArrayType of
            atVector:
              case V.UserType.ArrayDef.ArraySize of
                asShort: TypeCode := tcShortVector;
                asLong: TypeCode := tcLongVector;
              else
                raise EVarType.Create;
              end;
            atList:
              case V.UserType.ArrayDef.ArraySize of
                asShort: TypeCode := tcShortList;
                asLong: TypeCode := tcLongList;
              else
                raise EVarType.Create;
              end;
          else
            raise EVarType.Create;
          end;
          //TypeCode
          AsmLine('db ' + ByteToStr(TypeCode));

          //Address or Offset
          case V.AddrMode of
            amStack: AsmLine('dw ' + (*V.GetAsmOffset*) WordToStr(V.Offset));
            amStatic: AsmLine('dw ' + V.GetAsmName);
          else
            raise EAddrMode.Create;
          end;
          //Vector length or List Capacity
          case TypeCode of
            tcShortVector: AsmLine('db ' + ByteToStr(V.UserType.VectorLength));
            tcLongVector: AsmLine('dw ' + ByteToStr(V.UserType.VectorLength));
            tcShortList: AsmLine('db ' + ByteToStr(V.UserType.ListCapacity));
            tcLongList: AsmLine('dw ' + WordToStr(V.UserType.ListCapacity));
          else
            raise EVarType.Create;
          end;
        end;
    end;
    if LocalVarsToInit then
      AsmLine('db ' + ByteToStr(tcEndOfData));
  end;
end;

//Generate code at function end (ie. stack teardown, if needed)
procedure GenFunctionPostAmble(Scope: PScope;BlockType: TBlockType);
begin
  AsmLine(';Postamble');
  if Scope.Func = nil then
  begin
    if BlockType = btStack then
      GenLibraryProc('stacklocal_exit', nil)
    else
      AsmInstr('ret')
  end
  else //Scope.Func <> nil
  begin
    case Scope.Func.CallingConvention of
      ccRegister: //Load any return parameters into registers
      begin
        GenFuncReturnLoad(Scope.Func);
        AsmInstr('ret')
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
(*
procedure ProcError(ILItem: PILItem);
begin
  AsmError('No operation specified or illegal operation')
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
*)
//========================= CODE GENERATOR

procedure InitPrimitives;
begin
(*
  //PrimSetProc will raise an error if the Proc is not used anywhere
  //Unused Proc probably ought to be removed
  PrimSetProc('error', ProcError);
//  PrimSetProc('empty', ProcEmpty);

  PrimSetProc('proc_dec8_reg',ProcDec8Reg);
  PrimSetProc('proc_dec16_reg',ProcDec16Reg);
  PrimSetProc('proc_inc8_reg',ProcInc8Reg);
  PrimSetProc('proc_inc16_reg',ProcInc16Reg);

  //Verifies that all fragments, library routines(??), used by primitives
  //are available, and any Proc which need to be assigned have been assigned
  ValidatePrimitives;
*)end;

end.

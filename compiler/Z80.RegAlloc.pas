unit Z80.RegAlloc;

interface
uses Def.IL, Def.Functions,
  Lib.Data;

//----------------

//Allocate which registers (if any) parameters need to be loaded (or moved) into
//before a primitive can be generated
procedure TEMPRegAllocPrim(var ILItem: TILItem;const Prim: PPrimitive);

//Allocate which registers (if any) parameters need to be loaded (or moved) into
//for an opMove operation
procedure TEMPRegAllocMove(var ILItem: PILItem);

procedure TEMPRegAllocTypecast(var ILItem: PILItem);

procedure TEMPRegAllocBlockCopy(var ILItem: PILItem);
procedure TEMPRegAllocParamCopyToStack(var ILItem: PILItem);

procedure TEMPRegAllocPtrLoad(var ILItem: PILItem);

procedure TEMPRegAllocPtrStore(var ILItem: PILItem);

//Allocate registers (flags) for testing a boolean variable
procedure TEMPRegAllocBoolVarBranch(var ILItem: PILItem);

//Allocate registers for functions using Register calling convention
procedure TEMPRegAllocRegisterFunc(Func: PFunction);

//Allocate registers for functions using Stack calling convention
procedure TEMPRegAllocStackFunc(Func: PFunction);


implementation
uses SysUtils, Classes,
  Def.Operators, Def.VarTypes, Def.Variables, Def.UserTypes,
  Z80.Hardware;

//============================== REGISTER ALLOCATION

//Register allocation (temporary)
//----------------------------------------------------------

//Allocate which registers (if any) parameters need to be loaded (or moved) into
//before a primitive can be generated
procedure TEMPRegAllocPrim(var ILItem: TILItem;const Prim: PPrimitive);
var Reg: TCPUReg;
  Regs: TCPURegSet;
begin
  //These items don't need registers to be allocated
  Assert(not (ILItem.Op in [opUnknown, opPhi, opBranch, opMove,
    opRegLoad, opRegLoadExtended, opRegStore, opRegStoreExtended, opFuncCall, opFuncCallExtended]));

  Assert(Assigned(Prim), 'No primitive assign for:'#13#10 + ILItem.ToString);

  if (ILItem.Param1.Reg = rNone) and (ILItem.Param1.Kind <> pkNone) then
    //Prim doesn't use this parameter?
    if Prim.ProcMeta.LLoc = plRegister then
    begin
      //Assign a register for the parameter to be loaded into
      Regs := Prim.ProcMeta.LRegs;
      if Regs <> [] then
      begin
        //Find an available register
        Reg := Pred(rA);
        ILItem.Param1.Reg := rNone;
        repeat
          Reg := Succ(Reg);
          if Reg in Regs then
            ILItem.Param1.Reg := Reg;
        until (ILItem.Param1.Reg <> rNone) or (Reg = high(TCPUReg));
        if ILItem.Param1.Reg = rNone then
          raise Exception.Create('TEMPRegAllocPrim couldn''t find suitable Param1 register');
      end;
    end;

  if (ILItem.Param2.Reg = rNone) and (ILItem.Param2.Kind <> pkNone) then
    if Prim.ProcMeta.RLoc = plRegister then
    begin
      //Get allowable registers
      //TODO: Needs to be more flexible!
      Regs := Prim.ProcMeta.RRegs - [ILItem.Param1.Reg];
      Reg := Pred(rA);
      repeat
        Reg := Succ(Reg);
        if Reg in Regs then
          ILItem.Param2.Reg := Reg;
      until (ILItem.Param2.Reg <> rNone) or (Reg = high(TCPUReg));
      if ILItem.Param2.Reg = rNone then
        raise Exception.Create('TEMPRegAllocPrim couldn''t find suitable Param2 register');
    end;

  if (ILItem.Dest.Reg = rNone) and (ILItem.Dest.Kind <> pkNone) then
    if Prim.ProcMeta.ResultLoc = plRegister then
    begin
      if not (ILItem.Dest.Kind in [pkCondBranch, pkVarDest, pkPush, pkPushByte]) then
        Assert(False); //Invalid/unknown ParamKind

      if Prim.ProcMeta.ResultInLReg then
      begin //Use result data from Primitive
        if ILItem.Dest.Kind in [pkPushByte, pkPush] then
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
      else  //TODO: Flags vs A (ZFA etc). Dest not in a register.
      begin //Choose a register
        Regs := Prim.ProcMeta.ResultRegs;
        Reg := Pred(rA);
        repeat
          Reg := Succ(Reg);
          if Reg in Regs then
            ILItem.Dest.Reg := Reg;
        until (ILItem.Dest.Reg <> rNone) or (Reg = high(TCPUReg));
        if ILItem.Dest.Reg = rNone then
          raise Exception.Create('TEMPRegAllocPrim couldn''t find suitable Dest register');
      end;
    end;
end;

procedure TEMPRegAllocMove(var ILItem: PILItem);
begin
  Assert(ILItem.Op = opMove);
  Assert(ILItem.Param1.Kind in [pkImmediate, pkVarSource, pkVarRef, pkPop, pkPopByte]);
  Assert(ILItem.Param2.Kind = pkNone);
  Assert(ILItem.Param3.Kind in [pkVarDest, pkVarRef, pkPush, pkPushByte]);

  if ILItem.Param1.Reg = rNone then
    if ILItem.Param1.Kind = pkVarRef then
      //A pointer
      ILItem.Param1.Reg := rHL
    else
      //If the Dest is 8-bit load it into A, if 16-bit load it into HL
      case GetTypeRegSize(ILItem.Dest.GetUserType) of
        1: ILItem.Param1.Reg := rA;
        2: ILItem.Param1.Reg := rHL;
      else
        Assert(False);
      end;

  if ILItem.Dest.Reg = rNone then
    ILItem.Dest.Reg := ILItem.Param1.Reg;
end;

procedure TEMPRegAllocTypecast(var ILItem: PILItem);
var FromSize: Integer;
  ToSize: Integer;
begin
  Assert(ILItem.Op = opTypecast);
  Assert(ILItem.Param1.Kind = pkVarSource);
  Assert(ILItem.Param2.Kind = pkNone);
  Assert(ILItem.Dest.Kind = pkVarDest);
  FromSize := GetTypeRegSize(ILItem.Param1.Variable.UserType);
  ToSize := GetTypeRegSize(ILItem.ResultType);
  case FromSize of
    1:
      case ToSize of
        1:
        begin
          ILItem.Param1.Reg := rA;
          ILItem.Dest.Reg := rA;
        end;
        2:
        begin
          ILItem.Param1.Reg := rL;
          ILItem.Dest.Reg := rHL;
        end;
      else
        raise Exception.Create('Unknown type size');
      end;
    2:
      case ToSize of
        1:
        begin
          ILItem.Param1.Reg := rHL;
          ILItem.Dest.Reg := rL;
        end;
        2:
        begin
          ILItem.Param1.Reg := rHL;
          ILItem.Dest.Reg := rHL;
        end;
      else
        raise Exception.Create('Unknown type size');
      end;
  else
    raise Exception.Create('Unknown type size');
  end;
end;

procedure TEMPRegAllocBlockCopy(var ILItem: PILItem);
begin
  Assert(ILItem.Op = opBlockCopy);
  Assert(ILItem.Param1.Kind in [pkVarRef, pkVarSource]);
  Assert(ILItem.Param2.Kind = pkImmediate);
  Assert(ILItem.Dest.Kind = pkVarRef);
  ILItem.Param1.Reg := rHL;
  ILItem.Param2.Reg := rBC;
  ILItem.Dest.Reg := rDE;
end;

procedure TEMPRegAllocParamCopyToStack(var ILItem: PILItem);
begin
  Assert(ILItem.Op = opParamCopyToStack);
  Assert(ILItem.Param1.Kind = pkVarRef);
  Assert(ILItem.Param2.Kind = pkImmediate);
  Assert(ILItem.Dest.Kind = pkVarRef);
  ILItem.Param1.Reg := rHL;
  ILItem.Param2.Reg := rBC;
  ILItem.Param3.Reg := rDE;
end;

procedure TEMPRegAllocPtrLoad(var ILItem: PILItem);
var ByteCount: Integer;
begin
  Assert(ILItem.Op = opPtrLoad);
  Assert(ILItem.Param1.Kind = pkVarPtr);
  Assert(ILItem.Param1.Variable.VarType in [vtPointer, vtTypedPointer]);
  Assert(ILItem.Param2.Kind = pkNone);
  Assert(ILItem.Param3.Kind in [pkVarDest{, pkPush, pkPushByte}]);

  ByteCount := GetTypeDataSize(ILItem.Dest.Variable.UserType);

  //pkVarPtr param requires a pointer
  if ILItem.Param1.Reg = rNone then
    ILItem.Param1.Reg := rHL;

  //Dest requires it's natural size
  case ByteCount of
    1: ILItem.Dest.Reg := rA;
    2: ILItem.Dest.Reg := rDE;
  else
    Assert(False, 'TODO');
  end;
end;

procedure TEMPRegAllocPtrStore(var ILItem: PILItem);
var ByteCount: Integer;
begin
  Assert(ILItem.Op = opPtrStore);
  Assert(ILItem.Param1.Kind in [pkVarPtr, pkVarSource]);
  Assert(ILItem.Param1.Variable.VarType in [vtPointer, vtTypedPointer]);
  Assert(ILItem.Param2.Kind <> pkNone);
  Assert(ILItem.Param3.Kind = pkNone);

  ByteCount := GetTypeDataSize(ILItem.Param1.Variable.UserType.OfType);

  //VarPtr param requires a pointer
  if ILItem.Param1.Reg = rNone then
    ILItem.Param1.Reg := rHL;

  //Param2 requires it's natural size - which is the same as the item pointed to
  case ByteCount of
    1: ILItem.Param2.Reg := rA;
    2: ILItem.Param2.Reg := rDE;
  else
    Assert(False, 'TODO');
  end;
end;

procedure TEMPRegAllocBoolVarBranch(var ILItem: PILItem);
begin
  Assert(ILItem.Op = opBoolVarBranch);
  Assert(ILItem.Param1.Kind in [pkImmediate, pkVarSource, pkPop, pkPopByte]);
  Assert(ILItem.Param2.Kind = pkNone);
  Assert(ILItem.Param3.Kind = pkCondBranch);

  Assert(ILItem.Param1.GetVarType = vtBoolean, 'TODO: Add code for Flag type variables');

  if ILItem.Param1.Reg = rNone then
    //For now we'll assume we want this in the zero flag. Full featured reg allocator
    //will take account of whether the value is already in a CPU flag
    //TODO: If the Param is a Flag type variable
    ILItem.Param1.Reg := rNZF;

  if ILItem.Dest.Reg = rNone then
    ILItem.Dest.Reg := ILItem.Param1.Reg;
end;

procedure TEMPRegAllocRegisterFunc(Func: PFunction);

  procedure AddUsedReg(var Used: TCPURegSet;Reg: TCPUReg);
  begin
    case Reg of
      rA..rL: Used := Used + [Reg, CPUReg8ToPair[Reg]];
      rHL..rBC,rIX,rIY: Used := Used + [Reg, CPURegPairToHigh[Reg], CPURegPairToLow[Reg]];
    else  //Eg Flags
      Used := Used + [Reg];
    end;
  end;

  //If Entry is True we process Entry parameters (those which require data to be
  //passed in). If False we process Exit/Result paramaters (those which return data)
  procedure ProcessParams(Entry: Boolean);
  var
    I: Integer;
    UsedRegs: TCPURegSet;
    Reg8: TCPUReg;  //Next 8 bit to be allocated
    Reg16: TCPUReg; //Next 16 bit to be allocated
    ByteSize: Integer;
  begin
    Assert(Func.CallingConvention = ccRegister, 'We can only allocate registers when ccRegister is the calling convention');

    //Find any registers already allocated and mark as 'used'
    UsedRegs := [];
    for I := 0 to Func.ParamCount + Func.ResultCount - 1 do
      if Func.Params[I].Reg <> rNone then
        if (Entry and Func.Params[I].PassDataIn) or
          (not Entry and not Func.Params[I].ReturnsData) then
          AddUsedReg(UsedRegs, Func.Params[I].Reg);

    //For each Parameter
    Reg8 := rA;
    Reg16 := rHL;

    for I := 0 to Func.ParamCount + Func.ResultCount -1 do
      if Func.Params[I].UserType <> nil then
        if Func.Params[I].Reg = rNone then
          if (Entry and Func.Params[I].PassDataIn) or
            (not Entry and Func.Params[I].ReturnsData) then
          begin
            if Func.Params[I].IsByRef then
              ByteSize := 2
            else
              ByteSize := GetTypeRegSize(Func.Params[I].UserType);

            case ByteSize of
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

begin
  ProcessParams(True);
  ProcessParams(False);
end;

procedure TEMPRegAllocStackFunc(Func: PFunction);
var Param: PParameter;
begin
  Assert(Func.CallingConvention = ccStack);

  if Func.ResultCount = 0 then
    EXIT;

  Assert(Func.ResultCount = 1);

  Param := @Func.Params[Func.ParamCount];
  if Param.ReturnsData then
  begin
    Assert(Param.Reg = rNone, 'Already allocated!');
    case GetTypeRegSize(Param.UserType) of
      1: Param.Reg := rA;
      2: Param.Reg := rHL;
    else
      Assert(False);
    end;
  end;
end;

end.

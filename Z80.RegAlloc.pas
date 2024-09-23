unit Z80.RegAlloc;

interface
uses Def.IL, Def.Primitives, Def.Functions;

//----------------

//Allocate which registers (if any) parameters need to be loaded (or moved) into
//before a primitive can be generated
procedure TEMPRegAllocPrim(var ILItem: PILItem;const Prim: PPrimitive);

//Allocate which registers (if any) parameters need to be loaded (or moved) into
//for an opMove operation
procedure TEMPRegAllocMove(var ILItem: PILItem);

//Allocate registers for functions using Register calling convention
procedure TEMPRegAllocRegisterFunc(Func: PFunction);

//Allocate registers for functions using Stack calling convention
procedure TEMPRegAllocStackFunc(Func: PFunction);


implementation
uses SysUtils, Classes,
  Def.Operators, Def.QTypes, Def.Variables,
  Z80.CPU;

//============================== REGISTER ALLOCATION

//Register allocation (temporary)
//----------------------------------------------------------

//Allocate which registers (if any) parameters need to be loaded (or moved) into
//before a primitive can be generated
procedure TEMPRegAllocPrim(var ILItem: PILItem;const Prim: PPrimitive);
var Reg: TCPUReg;
  Regs: TCPURegSet;
begin
  //These items don't need registers to be allocated
  Assert(not (ILItem.Op in [opUnknown, opPhi, opBranch, opMove,
    opRegLoad, opRegLoadExtended, opRegStore, opRegStoreExtended, opFuncCall, opFuncCallExtended]));

  Assert(Assigned(Prim));

  if ILItem.Param1.Reg = rNone then
  begin
    Regs := [];
    //Prim doesn't use this parameter?
    if Prim.LRegs = [] then
      ILItem.Param1.Reg := rNone
    //Param is immediate and Prim can handle an immediate value?
    else if (ILItem.Param1.Kind = pkImmediate) and (rImm in Prim.LRegs) then
      ILItem.Param1.Reg := rImm
    else  //...otherwise assign a register for the parameter to be loaded into
      Regs := Prim.LRegs;

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
        raise Exception.Create('TEMNRegAlloc couldn''t find suitable Param1 register');
    end;
  end;

  if (ILItem.Param2.Reg = rNone) and (ILItem.Op <> opMove) then
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

procedure TEMPRegAllocMove(var ILItem: PILItem);
var Reg: TCPUReg;
  Regs: TCPURegSet;
begin
  Assert(ILItem.Op = opMove);
  Assert(ILItem.Param1.Kind in [pkImmediate, pkVarSource, pkPop, pkPopByte]);
  Assert(ILItem.Param2.Kind = pkNone);
  Assert(ILItem.Param3.Kind in [pkVarDest, pkPush, pkPushByte]);

  //If the Dest is 8-bit load it into A, if 16-bit load it into HL
  if ILItem.Param1.Reg = rNone then
    if GetTypeSize(ILItem.Dest.GetVarType) = 1 then
      ILItem.Param1.Reg := rA
    else
      ILItem.Param1.Reg := rHL;

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

procedure TEMPRegAllocStackFunc(Func: PFunction);
var Param: PParameter;
begin
  Assert(Func.CallingConvention = ccStack);

  if Func.ResultCount = 0 then
    EXIT;

  Assert(Func.ResultCount = 1);

  Param := @Func.Params[Func.ParamCount];
  Assert(Param.Reg = rNone, 'Already allocated!');
  case GetTypeSize(Param.VarType) of
    1: Param.Reg := rA;
    2: Param.Reg := rHL;
  else
    Assert(False);
  end;
end;

end.

(*
Routines to generate code for the Z80
(Ie where code is generated procedurally, rather than using a code fragment or
library subroutine.
(Also, code for Load/Store/Moves is currently located in the relevant units)
*)
unit Z80.GenProcs;

interface
uses Def.Consts, Def.IL, Def.VarTypes,
  Lib.Data, Lib.GenFragments,
  Z80.Hardware, Z80.Algos;

function GetOverflowAlgoForType(VarType: TVarType): TAlgo;

//NEW for CleverPuppy
//Load any parameters given in the Index'th ILItem.
//Returns the ILIndex of the last ILItem which contains loaded items.
//(I.e if the Op is DataMoveExtended)
procedure GenSourceParams(ILIndex: Integer);

//As GenSourceParams, but for Dest params
procedure GenDestParams(ILIndex: Integer);

//============Code generation 'helpers'

//Used to specify limitations when loading, storing, converting, validating, etc. parameters etc.
type TMoveOptions = (
  moPreserveA,    //Preserve the A register
  moPreserveHL,   //Preserve the HL register
  moPreserveHLDE, //Preserve either HL or DE. Instructs the codegen to NOT use EX HL,DE
  moPreserveCF,   //Preserve the carry flag
  moPreserveOtherFlags //The (testable) flags other than the Carry flag (ie. Sign, PV, Zero)
  );
  TMoveOptionSet = set of TMoveOptions;

//Sign extends an 8-bit value in RIn to a 16-bit value in ROutRIn
//Updates RegState as required
procedure GenSignExtend(RIn, ROut: TCPUReg;Options: TMoveOptionSet);

//Generate code to perform an optimised load of a literal value into a register.
//Various optimisations are available including copying a value from another register
//and 'modifying' the existing value, eg. by INCrementing it.
//The available optimisations depend on both register, the current CPPU state and
//whether CPU flags need to be preserved.
procedure GenLoadRegLiteral(Reg: TCPUReg;const Value: TImmValue;Options: TMoveOptionSet);

//Generate code to move FromReg to ToReg
//Currently only allows 'main' registers (ABCDEHL)
//Can sign extend a value moving from an 8-bit to a 16-bit register. When doing so
//  the A register will be trashed (and the Flags register cannot be preserved)
//Updates RegState
procedure GenRegMove(FromReg, ToReg: TCPUReg;Signed: Boolean;Options: TMoveOptionSet);

//Generate opTypecast
procedure GenTypecast(ILItem: PILItem);

//Generate code for opBlockCopy - copy data for a pointered type from one variable
//to another.
procedure GenBlockCopy(ILItem: PILItem);

//A procedure to generate code for an ILItem's Op
type
  TCodeGenProc = procedure(ILItem: PILItem);

function FindCodeGenProc(const AName: String): TCodeGenProc;

//Where the side-effects of a library routine are deeper than simply corrupting registers
//or setting constant values in registers, a StateProc can be used to procedurally
//update the code generators CPU State data for the operation
type
  TStateProc = procedure(Param: PILParam);

//Find a StateProc from the name of a fragment.
//Raises an exception if no routine is found.
function FindStateProc(const AName: String): TStateProc;

procedure Initialise;

implementation
uses Generics.Collections, SysUtils,
  Def.Globals, Def.Operators, Def.Variables, Def.UserTypes,
  CG.Data,
  Z80.CPUState, Z80.Assembler, Z80.AlgoData, Z80.Validation;

function GetOverflowAlgoForType(VarType: TVarType): TAlgo;
begin
  Assert(False, 'TODO');
end;

procedure GenAlgoParam(Algo: TAlgo;Param: PILParam);
var Fragment: PFragment;
begin
  if Algo = agNone then
    EXIT;

  Assert(Algo <> agUnspecified);
  Fragment := AlgoData[Algo].Fragment;
  Assert(Fragment <> nil);
  GenFragmentParam(Fragment, Param^, '');//TODO: Prefix???
end;

const MaxParamCount = 15;

var OrderedParams: array[0..MaxParamCount-1] of PILParam;

procedure LoadParams(ILIndex: Integer;Source: Boolean);
type TOrdering = (ordUnknown, ordNatural, ordForced);

  function GetOrdering(const Param: TILParam;Source: Boolean): TOrdering;
  begin
    if Source then
      if Param.Kind in [pkVarSource, pkImmediate] then
        if Param.GenOrder <> 0 then
          Result  := ordForced
        else
          Result := ordNatural
      else
        Result := ordUnknown
    else
      if Param.Kind in [pkVarDest] then
        if Param.GenOrder <> 0 then
          Result  := ordForced
        else
          Result := ordNatural
      else
        Result := ordUnknown;
  end;

var I: Integer;
  Item: PILItem;
  ParamNo: Integer;
  Param: PILParam;
  Ordering: TOrdering;
  OrderIndex: Integer;
begin
  for I := 0 to MaxParamCount-1 do
    OrderedParams[I] := nil;
  Ordering := ordUnknown;
  OrderIndex := 0;

  ParamNo := 1;
  Item := ILIndexToData(ILIndex);
  while True do
  begin
    if Ordering = ordUnknown then
    begin
      Ordering := GetOrdering(Item.Param1, Source);
      if Ordering = ordUnknown then
        Ordering := GetOrdering(Item.Param2, Source);
      if Ordering = ordUnknown then
        Ordering := GetOrdering(Item.Param3, Source);
    end;

    Param := Item.GetParam(ParamNo);
    if (Source and (Param.Kind in [pkVarSource, pkImmediate])) or
      (not Source and (Param.Kind in [pkVarDest])) then
    begin
      Assert(Ordering <> ordUnknown);
      if Ordering = ordNatural then
      begin
        Assert(OrderIndex < MaxParamCount);
        Assert(Param.GenOrder = 0);
        OrderedParams[OrderIndex] := Param;
        inc(OrderIndex);
      end
      else
      begin
        Assert((Param.GenOrder > 0) and (Param.GenOrder < MaxParamCount));
        OrderedParams[Param.GenOrder-1] := Param;
      end;
    end;

    if ParamNo = 3 then
      if not (Item.Op in ExtendedOps) then
        EXIT
      else
      begin
        inc(ILIndex);
        Item := ILIndexToData(ILIndex);
        ParamNo := 1;
      end
    else
      inc(ParamNo)
  end;
end;

procedure GenParams(ILIndex: Integer);
var ParamIndex: Integer;
  Param: PILParam;
begin
  ParamIndex := 0;
  while (ParamIndex < MaxParamCount) and (OrderedParams[ParamIndex] <> nil) do
  begin
    Param := OrderedParams[ParamIndex];
    if Param.OverflowAlgo <> agNone then
      GenAlgoParam(Param.OverflowAlgo, Param);
    if Param.Algo <> agNone then
      GenAlgoParam(Param.Algo, Param);

    inc(ParamIndex);
  end;
end;

procedure GenSourceParams(ILIndex: Integer);
begin
  LoadParams(ILIndex, True);
  GenParams(ILIndex);
end;

procedure GenDestParams(ILIndex: Integer);
begin
  LoadParams(ILIndex, False);
  GenParams(ILIndex);
end;

//Sign extends an 8-bit value in RIn to a 16-bit value in ROutRIn
//Updates RegState as required
procedure GenSignExtend(RIn, ROut: TCPUReg;Options: TMoveOptionSet);
begin
  System.Assert(([moPreserveA, moPreserveCF, moPreserveOtherFlags] * Options) = [],
    'Unable to extend whilst preserving flags');

  Assert(RIn in CPUReg8Bit);
  Assert(ROut in CPUReg8Bit);
  if RIn <> rA then
    OpMOV(rA, RIn);  //Move value to A
  AsmOpcode('rla');       //Move sign bit to Carry
  AsmInstr('sbc a,a');   //If carry we get -1, otherwise 0
  if ROut <> rA then
    OpMOV(ROut, rA);  //Move result to register
  RegStateSetUnknowns([rA, rOut, rFlags, rCF]);
end;


//====================================SCAVENGING LITERALS

//Sub of LoadRegLiteral
//Attempt to find an optimised way to load the given value into the given register
function TryLoadReg8Optimised(Reg: TCPUReg;Value: Integer;Options: TMoveOptionSet): Boolean;

  //Does a left rotation match the desired results
  //Value is the original register value
  //Bit0 is the new bit 0 (0 or <> 0)
  //NewValue is the required result
  //NewCF is the required new carry flag zero (0 or <> 0) (ir PreserveCarry)
  //if PreserveCarry is True the new value of Carry must match NewCF
  function LeftRotMatch(Value, Bit0, NewValue, NewCF: Integer;PreserveCarry: Boolean): Boolean;
  var Calc: Integer;
  begin
    Calc := (Value and $fe) shl 1;
    if Bit0 <> 0 then
      Calc := Calc or 1;
    Result := ((Calc and $ff) = NewValue);
    if PreserveCarry then
      Result := Result and ((NewCF <> 0) = ((Value and $80) <> 0));
  end;

  //As above but Bit7 is the new bit 7
  function RightRotMatch(Value, Bit7, NewValue, NewCF: Integer;PreserveCarry: Boolean): Boolean;
  var Calc: Integer;
  begin
    Calc := (Value and $fe) shr 1;
    if Bit7 <> 0 then
      Calc := Calc or $80;
    Result := ((Calc and $ff) = NewValue);
    if PreserveCarry then
      Result := Result and ((NewCF <> 0) = ((Value and 1) <> 0));
  end;

var R: TCPUReg;
  CalcValue: Integer;
  LitA: Integer;  //The current value of the register
  LitCF: Integer; //The current value of Carry Flag
  PreserveCF: Boolean;
  PreserveOtherFlags: Boolean;
begin //We only want optimisations which are faster/smaller than loading a literal (1B/1M/4T)
  System.Assert(Reg in CPUReg8Bit);

  PreserveCF := moPreserveCF in Options;
  PreserveOtherFlags := moPreserveOtherFlags in Options;

  Result := False;
  //If the register already contains a literal value - we may be able to transform
  //it into the value we want
  if RegStateIsLiteral(Reg) then
    //Optimisation for any register
    if not PreserveOtherFlags then
    begin
      //These trash all flags except CF
      if Value = (RegStateGetLiteral(Reg) + 1) and $ff then
      begin
        AsmOpcode('inc',CPURegStrings[Reg]); //1/1/4
        Result := True;
      end
      else if Value = (RegStateGetLiteral(Reg) - 1) and $ff then
      begin
        AsmOpcode('dec',CPURegStrings[Reg]); //1/1/4
        Result := True;
      end;
      if Result then
      begin
        //Trash flags except CF
        RegStateSetUnknown(rFlags); //Other than CF
        EXIT;
      end;
    end;


  //A register specific optimisations
  if Reg = rA then
  begin
    if RegStateIsLiteral(rA) then
    begin
      LitA := RegStateGetLiteral(rA);

      if LitA = Value xor $ff then
      begin
        AsmOpcode('cpl');  //1/1/4. Only changes H and N flags
        EXIT(True);
      end;

      //If both A and Carry are literals we can look for rotations
      if RegStateIsLiteral(rCF) then
        LitCF := RegStateGetLiteral(rCF)
      else
        LitCF := -1;

      if RegStateIsLiteral(rCF) then
      begin
        Result := True;
        if LeftRotMatch(LitA, LitCF, Value, LitCF, PreserveCF) then
        begin
          AsmOpcode('rla'); //1/1/4
          RegStateSetLiteral(rCF, LitA and $80);
        end
        else if RightRotMatch(LitA, LitCF, Value, LitCF, PreserveCF) then
        begin
          AsmOpcode('rra');
          RegStateSetLiteral(rCF, LitA and 1);
        end
        else
          Result := False;
        if Result then
          EXIT;
      end;
      if RegStateIsLiteral(rCF) or not PreserveCF then
      begin
        Result := True;

        if LeftRotMatch(LitA, LitA and $80, Value, LitCF, PreserveCF) then
        begin
          AsmOpcode('rlca');  //1/1/4
          RegStateSetLiteral(rCF, LitA and $80);
        end
        else if RightRotMatch(LitA, LitA and 1, Value, LitCF, PreserveCF) then
        begin
          AsmOpcode('rrca');
          RegStateSetLiteral(rCF, LitA and 1);
        end
        else
          Result := False;
        if Result then
          EXIT;
      end;

      //A register optimisations which can trash flags
      if not PreserveCF and not PreserveOtherFlags then
      begin //Can we add or subtract another register?
        for R := rA to rL do
          if RegStateIsLiteral(R) then
          begin
            CalcValue := RegStateGetLiteral(rA) + RegStateGetLiteral(R);
            if Value = CalcValue and $ff then
            begin
              AsmOpcode('add','a',CPURegStrings[R]); //1B/1M/4T
              RegStateSetUnknown(rFlags);
              RegStateSetLiteral(rCF, CalcValue and $100);
              EXIT(True);
            end;

            CalcValue := RegStateGetLiteral(rA) - RegStateGetLiteral(R);
            if Value = CalcValue and $ff then
            begin
              AsmOpcode('sub',CPURegStrings[R]);
              RegStateSetUnknown(rFlags);
              RegStateSetLiteral(rCF, CalcValue and $100);
              EXIT(True);
            end;

            //Carry flag is set - we can try ADC and SBC
            if RegStateIsLiteral(rCF) and (RegStateGetLiteral(rCF) <> 0) then
            begin
              CalcValue := RegStateGetLiteral(rA) + RegStateGetLiteral(R) + 1;
              if Value = CalcValue and $ff then
              begin
                AsmOpcode('adc','a',CPURegStrings[R]); //1B/1M/4T
                RegStateSetUnknown(rFlags);
                RegStateSetLiteral(rCF, CalcValue and $100);
                EXIT(True);
              end;

              CalcValue := RegStateGetLiteral(rA) - RegStateGetLiteral(R) - 1;
              if Value = CalcValue and $ff then
              begin
                AsmOpcode('sbc','a',CPURegStrings[R]);
                RegStateSetUnknown(rFlags);
                RegStateSetLiteral(rCF, CalcValue and $100);
                EXIT(True);
              end;
            end;
          end;  ///Loop
      end;
    end
    //Set A to zero
    else if not PreserveOtherFlags then
      if (Value = 0) and not PreserveCF then
        begin
          AsmOpcode('xor','a');
          RegStateSetUnknown(rFlags);
          RegStateSetLiteral(rCF, 0);
          EXIT(True);
        end;
  end;
end;

//Sub of LoadRegLiteral
//Attempt to find an optimisaed way to load the given value into the given register pair
function TryLoadRegPairOptimised(Reg: TCPUReg;Value: Integer;Options: TMoveOptionSet): Boolean;
var
  Test: Integer;
  LowIncDec: Boolean;
  HighIncDec: Boolean;
  RLow: TCPUReg;
  RHigh: TCPUReg;
begin
  //LD dd,nn is 3/3/10. We only want optimisations which are faster or smaller
  //LD r,n is 2/2/7 so it's not effective to only optimise one half and LD immediate the other
  //If we need to update both then BOTH must be optimised.
  //If we only need to update one then that can be treated as a single byte load
  //(and optimised or not as such)
  System.Assert(Reg in CPURegPairs);

  Result := False;

  if not (moPreserveHLDE in Options) then
    if ((Reg = rHL) and RegStateEqualsLiteral(rDE, Value)) or
      ((Reg = rDE) and RegStateEqualsLiteral(rHL, Value)) then
    begin
      AsmOpcode('ex','hl','de');
      RegStateEXHLDE;
      EXIT(True);
    end;


  //Are high and/or low bytes already in registers?
  RLow := RegStateFindLiteral8(Value and $ff);
  RHigh := RegStateFindLiteral8((Value shr 8) and $ff);

  //Can we do a 16-bit INC or DEC? (we've already established that both bytes
  //need to change)
  if RegStateIsLiteral(Reg) then
    if Value = (RegStateGetLiteral(Reg) + 1) and $ffff then
    begin
      //This is the best option if:
      //  We need to preserve flags (can't use 8-bit INC) and
      //    We can't copy low byte from another register
      //  -or -
      //  We need to INC both bytes
      if ((RLow = rNone) and (moPreserveOtherFlags in Options)) or ((Value and $ff00) <> (RegStateGetLiteral(Reg) and $ff00)) then
      begin
        AsmOpcode('inc',CPURegStrings[Reg]); //1/1/6
        EXIT(True);
      end;
    end
    else if Value = (RegStateGetLiteral(Reg) - 1) and $ffff then
      if ((RLow = rNone) and (moPreserveOtherFlags in Options)) or ((Value and $ff00) <> (RegStateGetLiteral(Reg) and $ff00)) then
      begin
        AsmOpcode('dec',CPURegStrings[Reg]); //1/1/6
        EXIT(True);
      end;


  //Is the low byte value already in place?
  if RegStateEqualsLiteral(CPURegPairToLow[Reg], Value and $ff) then
  begin //If so optimised load the high byte
    GenLoadRegLiteral(CPURegPairToHigh[Reg], TImmValue.CreateInteger((Value shr 8) and $ff), Options);
    EXIT(True);
  end
    //Is the high byte value already in place?
  else if RegStateEqualsLiteral(CPURegPairToHigh[Reg], (Value shr 8) and $ff) then
  begin //If so optimised load the low byte
    GenLoadRegLiteral(CPURegPairToLow[Reg], TImmValue.CreateInteger(Value and $ff), Options);
    EXIT(True);
  end
  else  //Can both be optimised??
  begin
    LowIncDec := False; //Can we INC or DEC either byte?
    HighIncDec := False;
    if not (moPreserveOtherFlags in Options) then
    begin
      if RegStateIsLiteral(CPURegPairToLow[Reg]) then
      begin
        Test := RegStateGetLiteral(CPURegPairToLow[Reg]);
        LowIncDec := (lo(Value) = (Test + 1) and $ff) or (lo(Value) = (Test - 1) and $ff);
      end;
      if RegStateIsLiteral(CPURegPairToHigh[Reg]) then
      begin
        Test := RegStateGetLiteral(CPURegPairToHigh[Reg]);
        HighIncDec := (hi(Value)  = (Test + 1) and $ff) or (hi(Value) = (Test - 1) and $ff);
      end;
    end;

    //Edge case: we need to swap both halves!
    if (RHigh = CPURegPairToLow[Reg]) and (RLow = CPURegPairToHigh[Reg]) then
      EXIT(False);

    //Can we optimised both bytes?
    if ((RLow <> rNone) or LowIncDec) and ((RHigh <> rNone) or HighIncDec) then
    begin
      //If the new high byte is the old low byte we need to swap the load order
      if RHigh <> CPURegPairToLow[Reg] then
      begin
        if RLow <> rNone then
        begin
          OpMOV(CPURegPairToLow[Reg], RLow);
          Result := True;
        end
        else
          Result := TryLoadReg8Optimised(CPURegPairToLow[Reg], Value and $ff, Options);

        if RHigh <> rNone then
        begin
          OpMOV(CPURegPairToHigh[Reg], RHigh);
          Result := Result and True;
        end
        else
          Result := Result and TryLoadReg8Optimised(CPURegPairToHigh[Reg], (Value shr 8) and $ff, Options);
      end
      else  //Swapped order
      begin
        if RHigh <> rNone then
        begin
          OpMOV(CPURegPairToHigh[Reg],RHigh);
          Result := True;
        end
        else
          Result := TryLoadReg8Optimised(CPURegPairToHigh[Reg], (Value shr 8) and $ff, Options);

        if RLow <> rNone then
        begin
          OpMOV(CPURegPairToLow[Reg],RLow);
          Result := Result and True;
        end
        else
          Result := Result and TryLoadReg8Optimised(CPURegPairToLow[Reg], Value and $ff, Options);
      end;
    end;
  end;
end;

//Loads a literal value into any register, register pair or the Carry Flag
//Includes IX and IY
//If not true the operation failed (ie. one or more of the conditions specified
//in Options was not met. Ie. the load couldn't be completed without trashing
//something which needed to be kept)
procedure GenLoadRegLiteral(Reg: TCPUReg;const Value: TImmValue;Options: TMoveOptionSet);
var R: TCPUReg;
begin
  //If Reg aleady holds target value then EXIT
  if RegStateEqualsLiteral(Reg, Value.ToInteger) then
    EXIT;

  case Reg of
    rA,rB,rC,rD,rE,rH,rL:
    begin
      //If the literal value is already in a register...
      R := RegStateFindLiteral8(Value.ToInteger and $ff);
      if R <> rNone then
        //...then copy it
        OpMOV(Reg,R) //1/1/4 (Bytes/M Cycles/T States)
      else
      begin
        //Look for an optimised way to load the value
        if not TryLoadReg8Optimised(Reg, Value.ToInteger, Options) then
          //Otherwise load the literal
          OpMOV(Reg, Value) //2B/2M/7T
      end;
    end;
    rHL, rDE, rBC:
    begin
      //Can we do an optimised load?
      if not TryLoadRegPairOptimised(Reg, Value.Tointeger, Options) then
        //We've gotten here without loading either half, so load the pair together
        OpMOV(Reg, Value)
    end;
    rIX,rIY:
      OpMOV(Reg, Value);
    rCF:
    begin
      if Value.ToInteger <> 0 then
        AsmOpcode('scf')
      else  //Set to 0
        if RegStateIsLiteral(rCF) then
        begin
          System.Assert(RegStateGetLiteral(rCF) <> 0, 'Setting to current value. Should have been filtered out already');
          AsmOpcode('ccf');  //1/1/4
        end
        else if moPreserveOtherFlags in Options then
        begin
          AsmOpcode('scf');  //1/1/4
          AsmOpcode('ccf');  //1/1/4
        end
        else  //Can trash flags
        begin
          AsmOpcode('and','a');  //1/1/4
          RegStateSetUnknown(rFlags);
        end;
    end
  else
    System.Assert(False, 'Unable to handle given register');
  end;

  RegStateSetLiteral(Reg, Value.ToInteger);
end;

//===========================REGISTER MOVING

//Generate code to move FromReg to ToReg
//Currently only allows 'main' registers (ABCDEHL)
//Can sign extend a value moving from an 8-bit to a 16-bit register. When doing so
//  the A register will be trashed (and the Flags register cannot be preserved)
procedure GenRegMove(FromReg, ToReg: TCPUReg;Signed: Boolean;Options: TMoveOptionSet);
begin
  System.Assert(FromReg <> ToReg);

  if FromReg in CPUReg8Bit then
  begin //From 8 bit
    if ToReg in CPUReg8Bit then
    begin //8-bit to 8-bit
      OpMOV(ToReg, FromReg);
      RegStateCopy(ToReg, FromReg);
    end

    else if ToReg in [rBC, rHL, rDE] then
    begin //8-bit to 16-bit
      //Low byte. Ignore if we're just extending (FromReg is low of ToReg)
      if FromReg <> CPURegPairToLow[ToReg] then
      begin
        OpMOV(CPURegPairToLow[ToReg], FromReg);
        RegStateCopy(CPURegPairToLow[ToReg], FromReg);
      end;

      //Sign extend?
      if Signed then  //Sign extend (if needed)
      begin
        if FromReg = rA then
          GenSignExtend(FromReg, CPURegPairToHigh[ToReg], Options)
        else  //TODO: Preserve A?
          GenSignExtend(CPURegPairToLow[ToReg], CPURegPairToHigh[ToReg], Options)
      end
      else
        //otherwise zero extend
        GenLoadRegLiteral(CPURegPairToHigh[ToReg],TImmValue.CreateInteger(0),Options);
    end
    else
      System.Assert(False);

  end
  else if (FromReg in [rHL, rDE]) and (ToReg in [rHL, rDE]) and not (moPreserveHLDE in Options) then
  begin //EX HL,DE - TODO: Do we need to preserve the other value?
        //If so we'll need to use move option below
    System.Assert(FromReg <> ToReg);
    AsmOpcode('ex','hl','de');
    RegStateEXHLDE;
  end
  else if (FromReg in [rHL, rDE, rBC]) and (ToReg in [rHL, rDE, rBC]) then
  begin //16-bit load
    //TODO: From 16 bit to 8 bit
    //TODO: Validate (optional) if down-sizing the value
    System.Assert(ToReg in [rHL, rDE, rBC]);
    OpMOV(CPURegPairToLow[ToReg],CPURegPairToLow[FromReg]);
    OpMOV(CPURegPairToHigh[ToReg],CPURegPairToHigh[FromReg]);

    RegStateCopy(ToReg, FromReg);
  end
  else if (FromReg in (CPUReg16Bit + CPURegPairs)) and (ToReg in (CPUReg16Bit + CPURegPairs)) then
  begin
    OpPUSH(FromReg);
    OpPOP(ToReg);

    RegStateCopy(ToReg, FromReg);
  end
  else
    System.Assert(False, 'Sorry, unable to handle that register move');
end;

procedure GenTypecast(ILItem: PILItem);
begin
  Assert(ILItem.Op = opTypecast);
  Assert(ILItem.Param1.Kind = pkVarSource);

  if ILItem.Dest.Reg <> ILItem.Param1.Reg then
    if ILItem.Param1.Reg in CPURegPairs then
    begin
      if ILItem.Dest.Reg <> CPURegPairToLow[ILItem.Param1.Reg] then
        Assert(False);  //Invalid register allocations
    end
    else if ILItem.Param1.Reg in CPUReg8Bit then
      if ILItem.Dest.Reg = CPUReg8ToPair[ILItem.Param1.Reg] then
      begin
        //If we're casting from a signed type then signed extend it
        if IsSignedType(ILItem.Param1.Variable.UserType) then
          GenSignExtend(ILItem.Param1.Reg, CPURegPairToHigh[ILItem.Dest.Reg], [])
        else
          //Otherwise zero extend
          GenLoadRegLiteral(CPURegPairToHigh[ILItem.Dest.Reg], TImmValue.CreateInteger(0), []);
      end
      else
        Assert(False); //Invalid register allocations
end;

procedure GenBlockCopy(ILItem: PILItem);
begin
  Assert(ILItem.Op = opBlockCopy);
  OpLDIR;
  RegStateSetUnknowns([rHL, rDE]);
  RegStateSetLiteral(rBC, 0);
end;

//================================CODEGENPROCS

var CodeGenProcs: TDictionary<String, TCodeGenProc>;

//Do nothing
procedure Proc_Empty(ILItem: PILItem);
begin
end;

//====================================

procedure Proc_Error(ILItem: PILItem);
begin
  AsmError('No operation specified or illegal operation')
end;

//=====================================Maths

procedure Proc_Dec8Reg(ILItem: PILItem);
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

procedure Proc_Dec16Reg(ILItem: PILItem);
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

procedure Proc_Inc8Reg(ILItem: PILItem);
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

procedure Proc_Inc16Reg(ILItem: PILItem);
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

//=================================== ARRAYS

//Calculate the addr of an array element where:
// - the array is a static variable with addrmode of amStatic
// - the index is an immediate value
procedure Proc_AddrOfArrayElemStaticImm(ILItem: PILItem);
var V: PVariable; //The array
  ElementSize: Integer;
  Index: Integer; //of the item we want
  FirstIndex: Integer;  //Low of first item in array
begin
  Assert(ILItem.Param1.Kind = pkVarRef);
  Assert(ILItem.Param2.Kind = pkImmediate);
  Assert(IsOrdinalType(UTToVT(ILItem.Param2.Imm.UserType)));

  V := ILItem.Param1.Variable;
  Assert(UTToVT(V.UserType) in [vtArray, vtVector, vtList]);
  Assert(Assigned(V.UserType.OfType));

  ElementSize := GetTypeSize(V.UserType.OfType);
  Index := ILItem.Param2.Imm.ToInteger;

  case UTToVT(V.UserType) of
    vtArray:
    begin
      FirstIndex := V.UserType.BoundsType.Low;
      //TODO: Validate array size
      //TODO: Check Reg State: is value already loaded?

      case V.AddrMode of

        amStatic: //Direct load of calculated address into register
          OpMOV(rHL, V.GetAsmName + ' + ' + WordToStr(ElementSize * (Index - FirstIndex)));
          //TODO: Update Reg State
        amStaticRef, amStackRef:
        begin
          if V.AddrMode = amStaticRef then
            OpLOAD(rDE, V.GetAsmName)
          else  //amStackRef
            OpLOAD(rDE, rIX, V);

          OpMOV(rHL, WordToStr(ElementSize * (Index - FirstIndex))); //Offset to element
          OpADD(rHL,rDE);           //Element address onto HL
          //TODO: Update Reg State
        end;
      else
        raise EAddrMode.Create;
      end;
    end;
//    vtVector:
{    vtList:
}  else
    Assert(False);  //Must be array type
  end;
end;

//Calc address of array element where:
// - the array is in a static variable
// - the index has been pre-loaded into the
//   - HL register (16-bit value),
//   - L register (unsigned 8-bit value)
procedure Proc_AddrOfArrayElemStaticVarSource(ILItem: PILItem);
var V: PVariable; //The array
  ArrayType: PUserType; //Array type
  AddrMode: TAddrMode;
//  IsPointerTo: Boolean; //If True V is a pointer to the actual data (static pointer)
                        //If False V is the actual data (static)
  ElementSize: Integer;
  IndexVT: TVarType;    //Index type
  FirstIndex: Integer;  //First item of array
begin
  Assert(ILItem.Param1.Kind = pkVarRef);
  Assert(ILItem.Param2.Kind = pkVarSource);
  Assert(IsOrdinalType(UTToVT(ILItem.Param2.Variable.UserType)));

  V := ILItem.Param1.Variable;
  ArrayType := V.UserType;

  AddrMode := V.AddrMode;
//  IsPointerTo := (ArrayType.VarType = vtTypedPointer) or
//    (V.AddrMode = amStaticRef);
  if ArrayType.VarType = vtTypedPointer then
  begin
    ArrayType := ArrayType.OfType;
    case AddrMode of
      amStatic: AddrMode := amstaticRef;
      amStack: AddrMode := amStackRef;
    else
      raise EAddrMode.Create;
    end;
  end;
  Assert(UTToVT(ArrayType) in [vtArray, vtVector, vtList]);
  Assert(Assigned(ArrayType.OfType));

  //TODO: Only for vtArray - others use dynamic bounds checking
  if cgRangeCheck in ILItem.Param2.Flags then
    GenRangeCheck(ILItem.Param2.Reg, ILItem.Param2.GetUserType, ArrayType.BoundsType, nil, []);

  ElementSize := GetTypeSize(ArrayType.OfType);
  FirstIndex := ArrayType.BoundsType.Low;

  //Index will be in L (for unsigned 8-bit) or HL (for 16-bit)
  //If 8-bit then extend to 16-bit
  IndexVT := UTToVT(GetBaseType(ILItem.Param2.Variable.UserType));
  if IndexVT = vtInt8 then
    GenSignExtend(rL, rH, [])
  else if GetVarTypeSize(IndexVT) = 1 then
    GenLoadRegLiteral(rH, TImmValue.CreateInteger(0), []);

  //Multiply index by element size
  case ElementSize of
    0: Assert(False);
    1: ;  //Nothing to do
    2:
    begin
      OpADD(rHL, rHL); //Multiply by two
      RegStateSetUnknowns([rFlags, rCF, rZF]);
    end;
  else
    GenLoadRegLiteral(rDE, TImmValue.CreateInteger(ElementSize), []);
    //HL := HL * DE
    GenLibraryProc(':mult16_u_u__u', nil);
    RegStateSetUnknowns([rFlags, rCF, rZF]);
  end;
  //HL -> Index * ElementSize (Offset from array base (ignoring low bound))

  //Get array base address
  //Now add base address of the variable, and any offsets for Vector, List etc
  case UTToVT(ArrayType) of
    vtArray:
      //TODO: Validate array size
      //TODO: Check Reg State: is value already loaded?
    case AddrMode of
      amStatic:
      begin //V is static data
        //Get static address of array less (or plus) offset of first element (due
        //to Low bounds of array)
        if FirstIndex > 0 then
          OpMOV(rDE, V.GetAsmName + ' - ' + WordToStr(ElementSize * FirstIndex))
        else
          OpMOV(rDE, V.GetAsmName + ' + ' + WordToStr(ElementSize * -FirstIndex))
        //TODO: Update Reg State
      end;  //HL -> Offset, DE -> array base
      amStaticRef, amStackRef:
      begin
        //Add/Subtract low bounds from offset
        if ArrayType.BoundsType.Low > 0 then
        begin
          GenLoadRegLiteral(rDE, TImmValue.CreateInteger(abs(ArrayType.BoundsType.Low * ElementSize)), []);
          GenLoadRegLiteral(rCF, TImmValue.CreateInteger(0), []);  //Clear carry
          OpSBC(rHL, rDE);
        end
        else if ArrayType.BoundsType.Low < 0 then
        begin
          GenLoadRegLiteral(rDE, TImmValue.CreateInteger(abs(ArrayType.BoundsType.Low * ElementSize)), []);
          OpADD(rHL, rDE);
        end;
        //HL -> Offset from array base

        //DE->Base of array
        if AddrMode = amStaticRef then
          OpLOAD(rDE, V)
        else
          OpLOAD(rDE, rIX, V, 0);
        //TODO: Update Reg State
      end; //HL -> Offset. DE -> array base
    else
      raise EAddrMode.Create;
    end;
//    vtVector:
{    vtList:
} else
    Assert(False);  //Must be array type
  end;

  //Add array base and offset
  //(ordering may be swapped depending on the path we took)
  OpADD(rHL, rDE);
  RegStateSetVariable(rHL, ILItem.Dest.Variable, ILItem.Dest.VarVersion, rskVarValue);
  RegStateSetUnknowns([rDE, rFlags, rCF, rZF]);
end;

(*
//Calculate the address of an array element where the index is a literal
//Result := BaseAddr + ElementSize * Index
//Generated code
//  static var: a load
//  stack var: an addition (or an optimisation thereof)
procedure Proc_AddrOfArrayElemStaticImm(ILItem: PILItem);
var V: PVariable; //The array
  ElementSize: Integer;
  Index: Integer;
begin
  Assert(ILItem.Param1.Kind = pkVarSource);
  Assert(ILItem.Param2.Kind = pkImmediate);
  Assert(IsEnumerable(UTToVT(ILItem.Param2.Imm.UserType)));

  V := ILItem.Param1.Variable;
  case UTToVT(V.UserType) of
    vtArray:
    begin
      //TODO: Validate array size
      Assert(Assigned(V.UserType.BaseType));  //BaseType is the element type
      ElementSize := GetUserTypeSize(V.UserType.BaseType);
      Index := ILItem.Param2.Imm.ToInteger;
      case V.Storage of
        vsStatic:
          OpLD(rHL, V.GetAsmName + ' + ' + WordToStr(ElementSize * Index));
//          GenLoadRegLiteral(rHL, TImmValue.CreateInteger(V.Offset + ElementSize * Index), []);
        vsStack:
        begin
          if Index < 0 then
            //TODO: Index from end of array
          else if Index = 0 then
            //Nothing to do!!
{          else if Index * ElementSize < 3 then
            //Generate INCs
}          else //Generate ADD
          begin
            GenLoadRegLiteral(rDE, TImmValue.CreateInteger(Index * ElementSize), []);
            OpAdd(rHL, rDE);
            //TODO: UpdateRegState
          end;
        end;
      else
        Assert(False);
      end;
    end;
//    vtVector:
{    vtList:
}  else
    Assert(False);  //Must be array type
  end;
end;
*)
procedure InitCodeGenProcs;
begin
  CodeGenProcs := TDictionary<String, TCodeGenProc>.Create;
  CodeGenProcs.Add('empty',Proc_Empty);
  CodeGenProcs.Add('error',Proc_Error);
  CodeGenProcs.Add('proc_dec8_reg',Proc_Dec8Reg);
  CodeGenProcs.Add('proc_dec16_reg',Proc_Dec16Reg);
  CodeGenProcs.Add('proc_inc8_reg',Proc_Inc8Reg);
  CodeGenProcs.Add('proc_inc16_reg',Proc_Inc16Reg);
  CodeGenProcs.Add('proc_addrof_arrayelem_static_imm',Proc_AddrOfArrayElemStaticImm);
  CodeGenProcs.Add('proc_addrof_arrayelem_static_varsource',Proc_AddrOfArrayElemStaticVarSource);
  //NOTE: Also add line to Initialise
end;

function FindCodeGenProc(const AName: String): TCodeGenProc;
begin
  if not CodeGenProcs.TryGetValue(AName.ToLower, Result) then
    raise Exception.Create('Unable to find CodeGenProc ''' + AName + '''');
end;

//================================STATEPROCS

procedure StateProc_LoadR16Imm(Param: PILParam);
begin
  Assert(Param.Kind = pkImmediate);
  RegStateSetLiteral(Param.Reg, Param.Imm.ToInteger);
end;

procedure StateProc_EXHLDE(Param: PILParam);
begin
  RegStateEXHLDE;
end;

procedure StateProc_LoadVar16(Param: PILParam);
begin
  Assert(Param.Kind = pkVarSource);
  RegStateSetVariable(Param.Reg, Param.Variable, Param.VarVersion, rskVarValue);
end;

procedure StateProc_MoveFromPair(Param: PILParam);
begin
  RegStateCopy(Param.Reg, Param.AlgoReg);
end;

var StateProcs: TDictionary<String, TStateProc>;

procedure InitStateProcs;
begin
  StateProcs := TDictionary<String, TStateProc>.Create;
  StateProcs.Add('load_r16_imm',StateProc_LoadR16Imm);
  StateProcs.Add('ex_hl_de',StateProc_EXHLDE);
  //These algos have different functionality but the end result is the same so
  //we can use the same StateProc
  StateProcs.Add('load_hl_static',StateProc_LoadVar16);
  StateProcs.Add('load_r16_static',StateProc_LoadVar16);
  StateProcs.Add('load_r16_stack',StateProc_LoadVar16);


  StateProcs.Add('move_from_pair',StateProc_MoveFromPair);
end;

function FindStateProc(const AName: String): TStateProc;
begin
  if not StateProcs.TryGetValue(AName.ToLower, Result) then
    raise Exception.Create('Unable to find StateProc ''' + AName + '''');
end;

procedure Initialise;
begin
  AddGenProcFragment('empty','-preservesall -bytes 0 -cycles 0');
  AddGenProcFragment('error','-preservesall -bytes 0 -cycles 0');

  //TODO: Replace these with something we can add meta to
  AddGenProcFragment('proc_dec8_reg','');
  AddGenProcFragment('proc_dec16_reg','');
  AddGenProcFragment('proc_inc8_reg','');
  AddGenProcFragment('proc_inc16_reg','');

  AddGenProcFragment('proc_addrof_arrayelem_static_imm','');
  AddGenProcFragment('proc_addrof_arrayelem_static_varsource','');
end;

initialization
  InitStateProcs;
  InitCodeGenProcs;
finalization
  CodeGenProcs.Free;
  CodeGenProcs := nil;
  StateProcs.Free;
  StateProcs := nil;
end.

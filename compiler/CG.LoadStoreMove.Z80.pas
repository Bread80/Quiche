(*
* Handles ILItem operations opDataMove, opDataMoveExtended, opFuncCall, opFuncCallExtended
* to generate code to:
*   load variables and literals into registers,
*     either for data move or before function calls (register calling convention)
*   call functions
*   store results back to variables after function calls
*)

unit CG.LoadStoreMove.Z80;

interface
uses Def.Functions, Def.IL, Def.QTypes,
  Lib.Data;


//Generates data moves (opDataMove, opDataMoveExtended), which includes doing function dispatch
//Data moves can spread across multiple ILItems. On entry ILIndex is the index of
//the first such ILItem, on exit it is the index of the last.
// (The last ILItem will be opDataMove, the preceding ones opDataMoveExtended)
// 1) Loads any data required into registers
// 2) Calls the function (if there is one)
// 3) Stores any results back into variables
(*procedure GenDataMove(ILIndex: Integer);
*)

//Load any parameters given in the Index'th ILItem.
//Returns the ILIndex of the last ILItem which contains loaded items.
//(I.e if the Op is DataMoveExtended)
function GenRegLoad(ILIndex: Integer;Prim: PPrimitive): Integer;

//Stores data after a function call, or in a data move
function GenRegStore(ILIndex: Integer): Integer;

//Dispatches a function call
procedure GenFuncCall(ILIndex: Integer);

//To be called at the pre-amble of a function call for functions which use
//Register calling convention.
//For each parameter which has been passed in a register (Func.Reg <> rNone),
//and for which the Variable storage has not been optimised away,
//generates code to store the registers value into memory storage.
procedure GenFuncArgStore(Func: PFunction);

//As above but for use at the end of a function which uses the Register calling
//convention.
//Any values which are to be returned in registers are loaded into them
procedure GenFuncReturnLoad(Func: PFunction);


implementation
uses SysUtils,
  Def.Operators, Def.Variables,
  CG.Data,
  CG.Z80, CG.CPUState.Z80, CG.Load.Z80, CG.Store.Z80,
  Z80.Hardware, Z80.GenProcs;

//=======================================DATA TYPES

  //For each parameter:
  //  Move type: static16/static8/stack/addr/imm/imm-copy/copy (value already in another reg (for loads only - not imm)
  //  Process type: none/sign extend/range check8/range check16
type
  TMoveType = (
    mtUnused,   //This register is not used
    mtKeep,     //The value is already loaded, just preserve it
    mtStatic16, //No side effects
    mtStatic8,  //Requires A reg (or HL)
    mtStack,    //No side effects
{TODO}  mtAddrStatic, //Static address - easy peasy
    mtAddrStack,  //Probably requires HL, DE and Flags. Possibly more!
    mtCopy,     //Value is already in a register and just needs moving
    mtImm,      //Literal value. No side effects
    mtImmKeep,  //LiteralValue already in register
    mtImmCopy, //Literal value already in a(nother) register
    mtLabel);   //A label (to static data)
  TMoveTypeSet = set of TMoveType;

  TProcessType = (
      ptNone,
      ptSignExtend, //Requires A and F registers
      ptShrink,     //Shrink/range check a 16-bit value into an 8-bit value
{TODO}      ptRangeCheck8,  //?? Probably A and F
{TODO}      ptRangeCheck16); //?? HL? DE?
  TProcessTypeSet = set of TProcessType;

  TMoveStateItem = record
    Done: Boolean;  //Set to True once param has been loaded/stored
    //Parameter data
    Param: PILParam;
    CheckType: TVarType;  //Set if we need to range check or overflow check the
                          //move (otherwise vtUnknown).
                          //For a load this is the target type (parameter, operand)
                          //For a store this is the source type (result parameter, operation)

    //Meta data about the move
    MoveType: TMoveType;
    ProcessType: TProcessType;
  end;

type TMoveState = array[low(TCPUReg)..high(TCPUReg)] of TMoveStateItem;

type
  TMoveAnalysis = record
    ToMoveCount: Integer; //The number of values still to be loaded/stored
                          //When this reaches zero we are done!!
    ToKeepCount: Integer; //The number of values already loaded in their target register

    //Registers that must be preserved
    PreserveA: Boolean;   //A must be preserved
    PreserveCF: Boolean;  //Carry flag must be preserved
    PreserveOtherFlags: Boolean;  //Other flags must be preserved

    //Registers that will be corrupted
    RequiresA: Integer;   //Number of moves requiring use of A register (decrement as moves are made)
    RequiresFlags: Integer; //Number of moves requiring use of Flags register (decrement as moves are made)

    //If the move will be followed by a conditional branch (eg after a function call)
    //it will be stored here. If not, this will be nil.
    CondBranchParam: PILParam;
  end;

var
  //Define the final state we want when loading or saving registers
  MoveState: TMoveState;
  MoveAnalysis: TMoveAnalysis;

procedure InitMoveState;
var R: TCPUReg;
begin
  //Init the StoreState
  for R := low(TCPUReg) to high(TCPUReg) do
  begin
    MoveState[R].Done := True;
    MoveState[R].Param := nil;
    MoveState[R].CheckType := vtUnknown;
    MoveState[R].MoveType := mtUnused;
    MoveState[R].ProcessType := ptNone;
  end;
end;

procedure InitMoveAnalysis;
begin
  MoveAnalysis.ToMoveCount := 0;
  MoveAnalysis.ToKeepCount := 0;

  MoveAnalysis.PreserveA := False;
  MoveAnalysis.PreserveCF := False;
  MoveAnalysis.PreserveOtherFlags := False;

  MoveAnalysis.RequiresA := 0;
  MoveAnalysis.RequiresFlags := 0;

  MoveAnalysis.CondBranchParam := nil;
end;

//================================ANALYSIS

//Assesses the Param and returns a suitable TMoveType
function AssessMoveType(Param: PILParam): TMoveType;
var R: TCPUReg;
  V: PVariable;
begin
  Result := mtUnused;
  R := Param.Reg;

  //Analyse what move is needed (if any)
  case Param.Kind of
    pkImmediate:
    begin
      if IsPointeredType(Param.Imm.VarType) then
      begin
        //TODO - Is the label already in the register?
        if RegStateEqualsLabel(R, Param.Imm.ToLabel) then
          Result := mtImmKeep
        else
        begin
          Result := mtLabel;
          inc(MoveAnalysis.ToMoveCount);
        end
      end
      else
      begin
        Result := mtImm;  //Default

        //Is the value already in the reg?
        if RegStateEqualsLiteral(R, Param.Imm.ToInteger) then
        begin
          Result := mtImmKeep;
          inc(MoveAnalysis.ToKeepCount);
        end

        //Is it in another reg?
        else
        begin
          inc(MoveAnalysis.ToMoveCount);
          if R in CPUReg8Bit then
            if RegStateFindLiteral8(Param.Imm.ToInteger) <> rNone then
              Result := mtImmCopy;
          //(We could do this again for 16-bit values but that's too much heavy lifting. <sigh>
        end;
      end;
    end;
    pkVarSource, pkVarDest:
    begin
      V := Param.ToVariable;
      if RegStateEqualsVariable(R, V, Param.VarVersion, rskVarValue) then
      begin
        Result  := mtKeep;
        inc(MoveAnalysis.ToKeepCount);
      end
      else
      begin
        inc(MoveAnalysis.ToMoveCount);
        if RegStateFindVariable(V, Param.VarVersion, rskVarValue) <> rNone then
          Result := mtCopy
        else
        case V.Storage of
          vsStatic:
            case GetTypeSize(V.VarType) of
              1: Result := mtStatic8;
              2: Result := mtStatic16;
            else
              System.Assert(False);
            end;
          vsStack:
            Result := mtStack;
        else
          System.Assert(False);
        end;
      end;
    end;
  else
    System.Assert(False);
  end;
end;

//Assesses the Param and returns a suitable TProcessType
function AssessProcessType(Param: PILParam): TProcessType;
var R: TCPUReg;
  V: PVariable;
begin
  R := Param.Reg;

  //Default;
  Result := ptNone;

  case Param.Kind of
    pkImmediate: ;  //Nothing
    pkVarSource:
    begin
      V := Param.ToVariable;
      case GetTypeSize(V.VarType) of
        1:  //8-bit to ??
          if (V.VarType = vtInt8) and (R in CPUReg16Bit) then
            Result := ptSignExtend;
        2:  //16-bit to ??
          if R in CPUReg8Bit then
            Result := ptShrink;
      else
        System.Assert(False);
      end;
      //TODO: Range check other values/type conversions
    end;
    pkVarDest:
    begin
      V := Param.ToVariable;
      case GetTypeSize(V.VarType) of
        1: //?? to 8-bit
          if R in CPUReg8Bit then
            Result := ptShrink;
        2:  //?? to 16-bit
          if (V.VarType = vtInteger) and (R in CPUReg16Bit) then
            Result := ptSignExtend;
      else
        System.Assert(False);
      end;
      //TODO: Range check other values/type conversions
    end;
  else
    System.Assert(False);
  end;
end;

//======================================LOAD DATA

procedure CopyParamToMoveState(Param: PILParam;CheckType: TVarType);
begin
  //Verify we're not double-loading any registers.
  //(If the reg isnn't part of a pair etc the lookup will return rNone,
  //And we shouldn't be loading anything into rNone!)
  System.Assert(Param.Reg <> rNone);
  System.Assert(MoveState[rNone].Param = nil);
  System.Assert(MoveState[Param.Reg].Param = nil);
  System.Assert(MoveState[CPURegPairToLow[Param.Reg]].Param = nil);
  System.Assert(MoveState[CPURegPairToHigh[Param.Reg]].Param = nil);
  System.Assert(MoveState[CPUReg8ToPair[Param.Reg]].Param = nil);

  MoveState[Param.Reg].Done := False;
  MoveState[Param.Reg].Param := Param;
  MoveState[Param.Reg].CheckType := CheckType;
  //TODO: Set CheckType if cgRangeCheck, cgOverflowCheck in CodeGenFlags

  MoveState[Param.Reg].MoveType := AssessMoveType(Param);
  if MoveState[Param.Reg].MoveType in [mtKeep] then
    MoveState[Param.Reg].Done := True;
  MoveState[Param.Reg].ProcessType := AssessProcessType(Param);
end;

//Sets the CheckType field in MoveState from the Function parameter
//For each parameter in Func, copies that Parameter to the relevant record in
//MoveState (ie the state for the register to be loaded or stored)
procedure SetFuncDataInMoveState(Func: PFunction;Loading: Boolean);
var Param: PParameter;
  I: Integer;
begin
  for I := 0 to MaxFunctionParams-1 do
    //Parameter is passed or returned in a register
    if Func.Params[I].Reg <> rNone then
    begin
      Param := @Func.Params[I];
      case Param.Access of
        vaVar:  //Input and output
          MoveState[Param.Reg].CheckType := Param.VarType;
        vaVal, vaConst:  //Send access types
          if Loading then
          begin
            Assert(Assigned(MoveState[Param.Reg].Param),
              'Function parameter specifies a register not included in Load ILItem');
            MoveState[Param.Reg].CheckType := Param.VarType;
          end;
        vaOut, vaResult: //Return access types
          if not Loading then
            //(Might not be an error if we're not saving the value)
            MoveState[Param.Reg].CheckType := Param.VarType;
      else
        Assert(False, 'Unknown Function parameter VarAccess type');
      end;
    end;
end;

//Copies any relevant Params into EndState. If we're Loading copies any
//params which load data. If storing, (ie. not Loading copies any params
//which store data)
//Prim should be set it we're loading a primitive, otherwise it must be nil
function SetMoveState(ILIndex: Integer;Prim: PPrimitive;Loading: Boolean): Integer;
var
  ParamIndex: Integer;
  Param: PILParam;
  CheckType: TVarType;
  ILItem: PILItem;
  Func: PFunction;  //If this is a function call. Otherwise nil
begin
  InitMoveState;

  ParamIndex := 1;
  Func := nil;
  Param := nil;
  ILItem := nil;
  while True do
  begin
    if ParamIndex = 1 then
    begin
      ILItem := ILIndexToData(ILIndex);
      if ILItem.Op in [opFuncCall, opFuncCallExtended] then
        if ILItem.Func <> nil then
        begin
          Assert(Func = nil, 'Multiple functions specified');
          Func := ILItem.Func;
        end;
    end;

    CheckType := vtUnknown;
    case ParamIndex of
      1:
      begin
        Param := @ILItem.Param1;
        if Loading and (Prim <> nil) then
          CheckType := Prim.LType
      end;
      2:
      begin
        Param := @ILItem.Param2;
        if Loading and (Prim <> nil) then
          CheckType := Prim.RType;
      end;
      3:
      begin
        Param := @ILItem.Param3;
        if Loading and (Prim <> nil) then
          CheckType := Prim.ResultType;
      end;
    else
      System.Assert(False);
    end;

    case Param.Kind of
      pkNone: ;
      pkPhiVarSource,pkPhiVarDest: ;
      pkVarAddr: ;  //Handled by the primitive

      //Loading
      pkImmediate:
        if Loading and (Param.Reg <> rNone) then
          CopyParamToMoveState(Param, CheckType);
      pkVarSource,pkPop,pkPopByte:
        if Loading then
          CopyParamToMoveState(Param, CheckType);

      //Storing/After
      pkVarDest,pkPush,pkPushByte:
        if not Loading then
          CopyParamToMoveState(Param, CheckType);
//      pkBranch: ;
      pkCondBranch:
        if not Loading then
        begin
          Assert(MoveAnalysis.CondBranchParam = nil, 'Multiple pkCondBranch in a move');
          MoveAnalysis.CondBranchParam := Param;
        end;
    else
      System.Assert(False);
    end;

    //Update result now we've found a usable parameter
    Result := ILIndex;
    inc(ParamIndex);
    if ParamIndex > 3 then
      if ILItem.Op in [opRegLoadExtended, opRegStoreExtended, opFuncCallExtended] then
      begin
        inc(ILIndex);
        ParamIndex := 1;
      end
      else
      begin
        if Assigned(Func) then
          SetFuncDataInMoveState(Func, Loading);
          //Assign Func Parameters to MoveState
        EXIT;
      end;
  end;
end;

//As above but loads values from a function parameter list.
//Beware Loading terminology: Params are Loaded (into registers) at the end of
//the function, they are Stored (from registers) at the beginning of the function
//NOTE: ILParams allocated here must be disposed (See DisposeMoveStateParams)
procedure SetFuncMoveState(Func: PFunction;Loading: Boolean);

  procedure SetParam(Index: Integer;FuncParam: PParameter;Loading: Boolean);
  var ILParam: PILParam;
  begin
    New(MoveState[FuncParam.Reg].Param);
    ILParam := MoveState[FuncParam.Reg].Param;
    ILParam.Initialise;
    ILParam.Reg := FuncParam.Reg;
    if Loading then
      ILParam.Kind := pkVarSource
    else
      ILParam.Kind := pkVarDest;
    ILParam.Variable := VarFindByFuncParamIndex(Index); //Func param to variable???
    Assert(Assigned(ILParam.Variable), 'Unable to find variable for function parameter');
    ILParam.VarVersion := 0;

    MoveState[FuncParam.Reg].Done := False;
    MoveState[FuncParam.Reg].CheckType := FuncParam.VarType;
    MoveState[FuncParam.Reg].MoveType := AssessMoveType(ILParam);
    MoveState[FuncParam.Reg].ProcessType := AssessProcessType(ILParam);
  end;

var I: Integer;
begin
  InitMoveState;

  for I := Low(Func.Params) to High(Func.Params) do
    case Func.Params[I].Access of
      vaNone: ;
      vaVar:  //In and Out
        SetParam(I, @Func.Params[I], Loading);
      vaVal, vaConst:   //Input params
        if not Loading then
          SetParam(I, @Func.Params[I], Loading);
      vaOut, vaResult:  //Output params
        if Loading then
          SetParam(I, @Func.Params[I], Loading);
    else
      Assert(False);
    end;
end;

//To be called if the Params records within the MoveState have been allocated
//by the move routine (eg. LoadFuncMoveState).
//Disposes any any all Param records in the MoveState
procedure DisposeMoveStateParams;
var Reg: TCPUReg;
begin
  //Dispose any ILParams allocated in MoveState
  for Reg := low(TCPUReg) to high(TCPUReg) do
    if Assigned(MoveState[Reg].Param) then
      Dispose(MoveState[Reg].Param);
end;

//Update the CPUState/RegState to the state in MoveState.
//(Ie. Set the register state to that of the variables (and literals?) in the
//MoveState).
//This routine should be called when Storing data, after a function call or
//in the pre-amble to a function body with Register calling convention
procedure UpdateCPUStateFromMoveState;
var Reg: TCPUReg;
begin
  for Reg := low(TCPUReg) to high(TCPUReg) do
    if Assigned(MoveState[Reg].Param) then
      case MoveState[Reg].Param.Kind of
        pkVarDest:
          RegStateSetVariable(Reg, MoveState[Reg].Param.Variable, MoveState[Reg].Param.VarVersion, rskVarValue);
      else
        Assert(False);
      end;
end;

//=============================================MORE ANALYSIS

//Loading is True to analyse for a Load, False to analyse for a Store
procedure AnalyseMove(Loading: Boolean);
var R: TCPUReg;
begin
  MoveAnalysis.PreserveA := MoveState[rA].MoveType = mtKeep;
  MoveAnalysis.PreserveCF := MoveState[rCF].MoveType = mtKeep;  {??}
  MoveAnalysis.PreserveOtherFlags := MoveState[rFlags].MoveType = mtKeep; {??}

  for R := rA to high(TCPUReg) do
  begin
    if MoveState[R].MoveType in [mtStatic8] then
      inc(MoveAnalysis.RequiresA);  //TODO: Addr()

    if Loading then
    begin
      if MoveState[R].ProcessType in [ptSignExtend] then
        inc(MoveAnalysis.RequiresFlags)
    end
    else {Storing}
      if MoveState[R].ProcessType in [ptRangeCheck8, ptRangeCheck16] then //TODO
        inc(MoveAnalysis.RequiresFlags);
  end;
end;

//====================================LOAD, CALL, STORE

//Have all the values been loaded or stored?
function DataMoveDone: Boolean;
var Reg: TCPUReg;
begin
  for Reg := low(MoveState) to high(TMoveState) do
    if not MoveState[Reg].Done then
      case MoveState[Reg].MoveType of
        mtImm, mtImmKeep, mtImmCopy:
          //Immediate value could be sacrificed to load data. If so check the
          //value is still there
          if not RegStateEqualsLiteral(Reg, MoveState[Reg].Param.Imm.ToInteger) then
            EXIT(False);
        mtLabel:
          if not RegStateEqualsLabel(Reg, MoveState[Reg].Param.Imm.ToLabel) then
            EXIT(False);
      else
        EXIT(False);
      end;
  Result := True;
end;

//Store selected registers into variables if either MoveType or ProcessType applies
//to that parameter
procedure GenDataLoadParams(Regs: TCPURegSet;MoveTypes: TMoveTypeSet;
  ProcessTypes: TProcessTypeSet;Options: TMoveOptionSet);
var Reg: TCPUReg;
  ToType: TVarType;
begin
  for Reg := low(MoveState) to High(MoveState) do
    if not MoveState[Reg].Done then
      if Reg in Regs then
        if MoveState[Reg].Param <> nil then
          if (MoveState[Reg].MoveType in MoveTypes) or (MoveState[Reg].ProcessType in ProcessTypes) then
          begin
            //If we need to range check the load
            if MoveState[Reg].CheckType <> vtUnknown then
              ToType := MoveState[Reg].CheckType
            else //no type conversion to validate
              ToType := MoveState[Reg].Param.GetVarType;

            GenLoadParam(MoveState[Reg].Param^, ToType, Options);
            MoveState[Reg].Done := True;
          end;
end;

function GenRegLoad(ILIndex: Integer;Prim: PPrimitive): Integer;
begin
  InitMoveAnalysis;

  //Initialise the MoveState array with the parameters we need to load into each
  //register, where that value can be sourced, and whether any range checking or
  //extending will be needed (and wether doing so will affect A or Flags
  Result := SetMoveState(ILIndex, Prim, True);

  if DataMoveDone then
    EXIT;

  //Initialise the MoveAnalysis record - whether we need to keep values in any registers
  //which might get trashed, and whether those registers will get trashed.
  AnalyseMove(True);

    if DataMoveDone then
    EXIT;
  //Load any stack address params - these usually require HL, DE and flags
  GenDataLoadParams([rBC, rDE, rHL, rIX, rIY],
    [mtAddrStack], [],
    []); //<--- Mostly just error checking here

  //Load values which will trash A or flags for all registers except A and Flags
  GenDataLoadParams([rB, rC, rD, rE, rH, rL, rBC, rDE, rHL, rIX, rIY],
    [mtStatic8], [ptSignExtend, ptShrink, ptRangeCheck8, ptRangeCheck16],
    [moPreserveHLDE]);  //<-- we'll need more analysis before allowing EX HL,DE
                        //(it might Undo a move we've already done!)

  if DataMoveDone then
    EXIT;
  //Load all values into registers other than A
  GenDataLoadParams([rB, rC, rD, rE, rH, rL, rBC, rDE, rHL, rIX, rIY],
    [mtStatic16, mtStack, mtAddrStatic, mtCopy, mtImm, mtImmCopy, mtLabel], [],
    [moPreserveHLDE{, moPreserveA, moPreserveCF, moPreserveOtherFlags}]); //<--- Mostly just error checking here

  if DataMoveDone then
    EXIT;
  //Load values which will trash A register into A register
  GenDataLoadParams([rA], [mtStatic16, mtStatic8, mtStack, mtCopy, mtImm, mtImmCopy, mtLabel],
    [ptSignExtend, ptShrink, ptRangeCheck8, ptRangeCheck16], [moPreserveHLDE]);

  Assert(DataMoveDone, 'Data Load failed to load everything :(');


  //Preserve strategy
//  Scavenge()  - Move any values between registers
//  DoDataLoad()
//LoadLiterals()
  //Close preserve strategy
  //DoDataLoad for preserved registers (eg A)


  //===PRESERVATION

  //Quick analysis 1/ strategy 1:
  //  a) Do we need to preserve flags?
  //  b) Do we need to preserve A (and it's not a literal - we can reload that)
  //  If either a) or b) we can PUSH AF, load/move everything, the POP AF to finish
  //  Also, if we need to preserve AF then we need the PUSH/POP thing anyway

  //Quick analysis 2:
  //  a) Are we doing anything which trashes A?
  //  b) If so, is there a spare register we can pop it into?
  //  If not just do the loads anyway

  //PreserveAFStrategies:
  //  1. PUSH/POP AF
  //  2. MOVE A (into B,C,D,E,H,L)
  //  3. Trash AF
(*
  if not (PreserveA or PreserveFlags) then
    //No preserving needed
    DoLoads([])  // - All
  else if not (RequiresA or RequiresFlags) then
    //No corrupting
    DoLoads([])  // - All
  else if RequiresFlags then
  begin //Requires flags (and possibly A). A may or may not need to be preserved
    Code('push','af');
    DoLoads([rA,rCF])  // - Except into A and CF
    Code('pop','af');
    //!!! Below might trash Flags. What if we need to preserve them??
    // (If so we need to load the value into another register before the POP, then
    // move it across after the POP).
    if Value to be loaded into A then
      DoLoad(rA)
    if Value to be loaded into CF then
      DoLoadCF([PreserveA]);
  end
  else  //Need to preserve A
*)

  //AnalyseLoads
  //  Do we need to preserve A?
  //  Do we need to preserve Flags?
  //A and F will be corrupted if:
  //  Any param to be sign extended
  //  Any param to be Range checked
  //A (but not F) will be corrupted if:
  //  Any param is an 8-bit static
  //
  //If there's a value in A, to move to another Reg:
  //  Can it move before (ie other Reg is free)?
  //    If not can other Reg move before?
  //      (Recurse? in till loop)
  //...
  //If Preserve A and F (or just F):
  //  Push AF (do this early in case any moves trash A or F)

  //Start by moving any values which can move **except**
  //  If we need A, don't move data into A register

  //Load Static8Bits (Trashes A!)
  //Load Static16Bits
  //Load Stack data
  //If Preserve A and F (or just F):
  //  Pop AF
  //Load A register
  //Load Literals

  //=== REGISTER MOVING
  //Prepare:
  //   For CPUState: any regs which require a value to be loaded into,
  //     and which no not yet have the target value in them,
  //     (also any which will be corrupted by the Primitive/function)
  //       to be marked as 'Unused'
  //       This signals to the code below that they containn nothing useful
  //       and can be used.
  //1. Do not move values into A yet (unless allowed to)
  //  Scan register table for any values yet to be move. If the target register
  //    is available, move it and mark as done.
  //  Repeat this process until no more moves are required, or no more moves can
  //    be made.
  //  If not moves can be made: do we have a reg pair to move?
  //    If so PUSH it
  //    Repeat process above
  //  (Not reg pairs).
  //.loop
  //  If we have a free 8-bit,
  //    move any 8-bit into it, and repeat
  //  else if we're allowed to use A,
  //    move any 8-bit into A and repeat
  //    ** unless we need to use A for a load or process!
  //  else
  //    PUSH AF and repeat
  //  POP AF or move A to a free reg
  //  If we still fail, repeat .loop onwards using different registers or reg pairs
  //    until we either succeed or all options have been exhausted.
end;

procedure GenFuncCall(ILIndex: Integer);
var ILItem: PILItem;
  Code: String;
begin
  while True do
  begin
    ILItem := ILIndexToData(ILIndex);
    Assert(ILItem.Op in [opFuncCall, opFuncCallExtended]);
    if Assigned(ILItem.Func) then
    begin
      Code := ILItem.Func.GetCallInstruction;
      AsmInstr(Code);

      //TEMP: Clear register state. Update to depend on functions Corrupt's data
      RegStateInitialise;
//TODO      RegStatePreserves(ILItem.Func.Preserves);

      EXIT;
    end;

    Assert(ILItem.Op = opFuncCallExtended, 'Op = opFuncCall but no Func given in ILItem');

    inc(ILIndex);
  end;
end;

//Store selected registers into variables if either MoveType or ProcessType applies
//to that parameter
procedure GenDataStoreParams(Regs: TCPURegSet;MoveTypes: TMoveTypeSet;
  ProcessTypes: TProcessTypeSet;
  Options: TMoveOptionSet);
var Reg: TCPUReg;
  FromType: TVarType;
begin
  for Reg := low(MoveState) to High(MoveState) do
    if not MoveState[Reg].Done then
      if MoveState[Reg].Param <> nil then
        if (MoveState[Reg].MoveType in MoveTypes) or (MoveState[Reg].ProcessType in ProcessTypes) then
        begin
          //If we need to range check the store
          if MoveState[Reg].CheckType <> vtUnknown then
              FromType := MoveState[Reg].CheckType
          else //no converting happening, set FromType to the parameters type
            FromType := MoveState[Reg].Param.GetVarType;

          GenDestParam(MoveState[Reg].Param^, FromType,
            cgRangeCheck in MoveState[Reg].Param.Flags, nil, Options);
          MoveState[Reg].Done := True;
        end;
end;

function GenRegStore(ILIndex: Integer): Integer;
begin
  InitMoveAnalysis;

  Result := SetMoveState(ILIndex, nil, False);

  UpdateCPUStateFromMoveState;

  //TODO: If Function call, update RegState
  //(Func return parameters (registers) => variables given in Params)

  //Initialise the MoveAnalysis record - whether we need to keep values in any registers
  //which might get trashed, and whether those registers will get trashed.
  AnalyseMove(False);

  //TODO: Store Flag types (values returned in CPU flags)

  if not DataMoveDone then
    //Store the A register first - other stores might trash it
    GenDataStoreParams([rA], [mtStatic16, mtStatic8, mtStack],
      [ptSignExtend, ptShrink, ptRangeCheck8, ptRangeCheck16], [moPreserveHLDE]);

  if not DataMoveDone then
    //Store values which will trash A or flags for all registers except A and Flags
    GenDataStoreParams([rB, rC, rD, rE, rH, rL, rBC, rDE, rHL, rIX, rIY],
      [mtStatic8], [ptSignExtend, ptShrink, ptRangeCheck8, ptRangeCheck16],
      [moPreserveHLDE]);  //<-- we'll need more analysis before allowing EX HL,DE
                          //(it might Undo a move we've already done!)

  if not DataMoveDone then
    //Store other values
    GenDataStoreParams([rB, rC, rD, rE, rH, rL, rBC, rDE, rHL, rIX, rIY],
      [mtStatic16, mtStack], [],
      [moPreserveHLDE, moPreserveA, moPreserveCF, moPreserveOtherFlags]); //<--- Mostly just error checking here

  Assert(DataMoveDone, 'Data Store failed to store everything :(');

  if MoveAnalysis.CondBranchParam <> nil then
  begin
//    Assert(  Must be a function call!!)
    GenCondBranch(MoveAnalysis.CondBranchParam^);
  end;
end;

procedure GenFuncArgStore(Func: PFunction);
begin
  InitMoveAnalysis;

  SetFuncMoveState(Func, False);

  UpdateCPUStateFromMoveState;

  //Initialise the MoveAnalysis record - whether we need to keep values in any registers
  //which might get trashed, and whether those registers will get trashed.
  AnalyseMove(False);

  if not DataMoveDone then
    //Store the A register first - other stores might trash it
    GenDataStoreParams([rA], [mtStatic16, mtStatic8, mtStack],
      [ptSignExtend, ptShrink, ptRangeCheck8, ptRangeCheck16], [moPreserveHLDE]);

  if not DataMoveDone then
    //Store values which will trash A or flags for all registers except A and Flags
    GenDataStoreParams([rB, rC, rD, rE, rH, rL, rBC, rDE, rHL, rIX, rIY],
      [mtStatic8], [ptSignExtend, ptShrink, ptRangeCheck8, ptRangeCheck16],
      [moPreserveHLDE]);  //<-- we'll need more analysis before allowing EX HL,DE
                          //(it might Undo a move we've already done!)

  if not DataMoveDone then
    //Store other values
    GenDataStoreParams([rB, rC, rD, rE, rH, rL, rBC, rDE, rHL, rIX, rIY],
      [mtStatic16, mtStack], [],
      [moPreserveHLDE, moPreserveA, moPreserveCF, moPreserveOtherFlags]); //<--- Mostly just error checking here

  Assert(DataMoveDone, 'FuncParamStore failed to store everything :(');

  DisposeMoveStateParams;
end;

procedure GenFuncReturnLoad(Func: PFunction);
begin
  InitMoveAnalysis;

  //Initialise the MoveState array with the parameters we need to load into each
  //register, where that value can be sourced, and whether any range checking or
  //extending will be needed (and wether doing so will affect A or Flags
  SetFuncMoveState(Func, True);

  //Initialise the MoveAnalysis record - whether we need to keep values in any registers
  //which might get trashed, and whether those registers will get trashed.
  AnalyseMove(True);

  if not DataMoveDone then
    //Load any stack relative addresses. These usually require HL, DE and flags
    GenDataLoadParams([rBC, rDE, rHL, rIX, rIY],
      [mtAddrStack], [ptSignExtend, ptShrink, ptRangeCheck8, ptRangeCheck16],
      []);  //<-- we'll need more analysis before allowing EX HL,DE
                        //(it might Undo a move we've already done!)
  if not DataMoveDone then
    //Load values which will trash A or flags for all registers except A and Flags
    GenDataLoadParams([rB, rC, rD, rE, rH, rL, rBC, rDE, rHL, rIX, rIY],
      [mtStatic8], [ptSignExtend, ptShrink, ptRangeCheck8, ptRangeCheck16],
      [moPreserveHLDE]);  //<-- we'll need more analysis before allowing EX HL,DE
                        //(it might Undo a move we've already done!)

  if not DataMoveDone then
    //Load all values into registers other than A
    GenDataLoadParams([rB, rC, rD, rE, rH, rL, rBC, rDE, rHL, rIX, rIY],
      [mtStatic16, mtStack, mtAddrStatic, mtCopy, mtImm, mtImmCopy], [],
      [moPreserveHLDE, moPreserveA, moPreserveCF, moPreserveOtherFlags]); //<--- Mostly just error checking here

  if not DataMoveDone then
    //Load values which will trash A register into A register
    GenDataLoadParams([rA], [mtStatic16, mtStatic8, mtStack, mtCopy, mtImm, mtImmCopy],
      [ptSignExtend, ptShrink, ptRangeCheck8, ptRangeCheck16], [moPreserveHLDE]);

  Assert(DataMoveDone, 'Data Load failed to load everything :(');

  DisposeMoveStateParams;
end;

end.

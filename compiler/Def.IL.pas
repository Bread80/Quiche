(*
# Intermediate Language in the Quiche Compiler

The intermediate language (IL) is the core of the Quiche compiler. The parser
takes the source code and converts it to IL. The code generator takes the IL and
converts it to assembly code.

Each IL record stores an operation and up the three parameters. Some operations
extend over multiple IL records, for example when making function calls. The
three parameters are referred to as Param1, Param2 and Dest. Dest can also be
referred to as Param3 when using some of the system operations.

Operations (or operators) divide into two main classes: system operations and
other (non-system) operations. System operations are generally those which move
data without transforming it. For example, initialising variables, branching
and dispatching function calls.

The non-system operations are usually those which do transform data. These are
operations such as maths, comparisons, and logic as well as intrinsics (basic
operations which use function syntax but which (often) result in simple inline
code in the output).

## IL Format for Non-system Operations

The non-system operations are represented in the IL using a common format
whereas the non-system operations use the IL in a more flexible fashion.

The IL for non-system operations takes the form:
Dest Operation Param1 [Param2]
Ie. the Operation takes in one or two parameters - referred to as source
parameters - and stores the result in the dest(ination) parameter.

Thus the souce code:
B := A + 1
is represented in IL as
Operation: Add
Param1: Source the value from the A variable
Param2: An immediate (literal) value of 1
Dest: Store the result to the B variable

## Parameter Kinds

Each parameter has a 'kind' which specifies where the value is being sourced
from or stored to, and what sort of value is required. Some of the key params
kinds are:
Immediate: an immediate (literal) value.
VarSource: Reading value from variable.
VarDest: Storing the result to a variable.
VarAddr: The address of a variable.
VarPtr: The value pointed to by a variable (ie dereferencing a pointer).
Branch: Data for an unconditional branch.
CondBranch: Data for a conditional branch.

Some parameter kinds are only valid when used with specific operations, for
example the Branch and CondBranch can only be used in branching operations.

## Extending IL Items

As mentioned above some operations can extend over multiple IL records. The
operations which allow this come in pairs, one of which has the word Extended
appended. Thus the IL for a function call uses the operations FuncCall and
FuncCallExtended.

The Extended form of the operation indicates that the data extends into the
following IL record. There can be multiple of these extended operation IL items
but they must end with a non extended operation of the same type.

Thus a function call which takes four arguments will require two IL records. The
first will contain data relating to the first three parameters and be of type
FuncCallExtended. The second will contain data for the fourth parameter and be
of type FuncCall.

Note that the exact formatting of the data for a function call within the IL
will depend on the calling convention in use. (And in general the formatting of
IL data for system operations, including extended operations, is specified by
the operations and is not dictated by the IL format).

## Other IL Data

Various other pieces of data are also stored within the intermediate language
record. These include:
* The index of the current code block (a code block is a section of code with no
branches).
* Information about data types.
* Data relating to the source code (eg line number).
* Compiler flags in effect at that position in the source code (eg overflow
and range checking status).

Parameters include extra information such as the CPU register which the data
needs to be placing in (or in which the result will be found). Much
of this data is added by, and used by, the code generator.

## IL Data Storage

IL data is stored in a list format. This allows any IL item to be referenced
through it's index position in the list and allows the IL data to be easily
traversed. The index position is the data stored by the Branch and CondBranch
param kinds, with the value pointing to the destination IL item.

## IL Functions

The IL has functions to append a new IL item to the IL list, insert and IL item
into a list (a function which is rarely used), and to retrieve an item given
it's index.

## Links

Given the core nature of the IL it has relevance to large swathes of the code
base. Key areas of relevance would be the Parser, the Operators, Types, and the
code generator.
*)


unit Def.IL;

interface
uses Generics.Collections, Classes,
  Def.Functions, Def.Operators, Def.QTypes, Def.Consts, Def.Variables, Def.UserTypes,
  Lib.Data,
  Z80.Hardware, Z80.Algos;

//Locations where an operation will find and store it's data
type TILParamKind = (
    pkNone,         //No parameter
    pkImmediate,    //Immediate data (literal or result of a constant expression)
                    //For types which are referenced by pointers (strings ...)
                    //The immediate data is the /address/ of the data in the data area
    pkPhiVarSource, //Parameter for a phi function for a variable
    pkPhiVarDest,   // "
    pkVarSource,    //Read from variable
    pkVarDest,      //Write to variable
    pkVarAddr,      //Address of a variable - only valid where Op is OpAddrOf
    pkVarPtr,       //Reference to data pointed /to/ by the value (a pointer dereference)
                    //ONLY for not pointered types
    pkVarRef,       //Address of the data - ONLY for pointered types.
    pkPop,          //The stack
    pkPopByte,      //Single byte on the stack
    pkPush,
    pkPushByte,
    pkBranch,        //Unconditional branch. Only valid as Param1
    pkCondBranch     //Conditional branch. Only valid as Param3
    );

type
  TILParamFlags = (cgRangeCheck);
  TILParamFlagSet = set of TILParamFlags;

type
  PILParam = ^TILParam;
  TILParam = record
    //If the parameter is Sourcing data (ie. before an operation) this is the
    //register the data will be loaded or move into
    //If the parameter is a Deestination/Result this is the register in which the
    //data will found after the operation. The code generator will store the value
    //back into the variable (or other location) specified by the other values
    Reg: TCPUReg;
    Flags: TILParamFlagSet;
    LoadType: TLoadParamType;

    //For CleverPuppy
    CheckType: TVarType;  //Set if we need to range check or overflow check the
                          //move (otherwise vtUnknown).
                          //For a load this is the target type (parameter, operand)
                          //For a store this is the source type (result parameter, operation)
    GenOrder: Integer;    //Order in which params will be loaded (for sources) or
                          //stored (for dests).
                          //If 0, Params will be loaded in natural order.
                          //If non-zero, params will be laoded in GenOrder order.
                          //Either all If so, all params (of their type - load or
                          //store) must have GenOrder 0 or GenOrder set.

    //If Param is Source:
    SourceLoc: TCodeProcLoc;  //Where the Prim sources data from. Usually a Reg.
    SourceRegs: TCPURegSet;   //If SourceLoc is plRegister.
                              //Registers which the Prim can accept for this parameter

{//SooLoadAlgo    FromWhere: TWhere;      //From where will the data be sourced?
//                            //(eg register, static16, static8, stack16, stack8, immediate, pop)
    FromReg: TCPUReg;       //If FromWhere is TRegister/If loading from a register
    SourceRangeCheck: TRangeCheckType;  //Range checking routine to be applied
                            //(includes type (conversion, reduction, expansion) if necessary)
}
    //If Param is Dest:
    ResultLoc: TCodeProcLoc;  //Where the Prim stores data to. Usually a Reg.
    ResultRegs: TCPURegSet; //If ResultLoc is plRegister.
                            //Regs in which the Prim can leave the result
                            //(If there are multiple options CleverPuppy will need
                            //to generate each as a discrete item)
    ResultInLReg: Boolean;  //If Dest depends on source (can be a register or a
                            //memory location, eg INC (HL) )

    Algo: TAlgo;          //Algorithm to be used to load the data (for source params)
                          //or store it (dest params), for example
                          //reading from variable, popping, immediate, EX HL,DE, etc.
                          //If there is a ramge check routine which checks the data
                          //as it is read in then this value will be algoRangeCheck
    AlgoReg: TCPUReg;     //A CPU register to be passed to the Algo. This could reference
                          //a register to be copied from or to, or to be loaded or saved via.
{    ToAlgo: TMoveAlgo;      //Algorithm to be used to move data into ToRegs
    ToRegs: TCPURegSet;     //Set of registers we can copy data to. This is intended to be
                            //used to allow a value to persist beyond the following step(s)
                            //Where a result is used /by/ the following step this should be
                            //specified using the next steps LoadAlgo+FromReg
                            //Set ToRegs to [DestReg] to leave unmoved
}
    OverflowAlgo: TAlgo;    //Overflow checking routine to be applied
                            //ONLY applies to Dest params of Operations (ie Item
                            //must have a Primitive)


{
    DestRangeCheck: TRangeCheckType;    //Range checking required (if a type change is
                            //required. (Could include overflow checks where the two are best
                            combined??)
    }



    procedure Initialise;

    //These routines set the Kind and (usually) the payload. They also perform
    //validation where possible
    procedure SetPhiVarSource(ABlockID: Integer; AVarVersion: Integer);
    procedure SetPhiVarDest(AVar: PVariable;AVersion: Integer);
    procedure SetVarSourceAndVersion(AVariable: PVariable; AVersion: Integer);
    //Obtains Version from AVariable
    procedure SetVarSource(AVariable: PVariable);
    procedure SetVarAddr(AVariable: PVariable);
    procedure SetVarPtr(AVariable: PVariable);
    procedure SetVarRef(AVAriable: PVariable);
    procedure SetVarDestAndVersion(AVariable: PVariable; AVersion: Integer);
    procedure SetCondBranch;

    function ToVariable: PVariable;

    function GetVarType: TVarType;
    function GetUserType: PUserType;

    //For error messages
    function ImmValueToString: String;

    //For debug etc.
    function ToString: String;

    //Returns True if the Kind and payload data are an exact match
    function KindMatch(AParam: TILParam): Boolean;

    //The payload data
    case Kind: TILParamKind of
      pkNone: ();
      pkImmediate: (  //Immediate (constant) data
        Imm: TImmValue );
      pkPhiVarSource: (            //Phi variable (specified in the Dest data)
        PhiBlockID: Integer;  //If we come from this block...
        PhiSourceVersion: Integer; );   //...use this version of the variable
      pkPhiVarDest: (
        PhiVar: PVariable;
        PhiDestVersion: Integer; );
      pkVarSource, pkVarDest, pkVarAddr, pkVarPtr, pkVarRef: (
        Variable: PVariable;  //The variable
        VarVersion: Integer; );   //Current version of the variable
      pkPop: ();           //Currently invalid for input params
      pkPopByte: ();       //Currently invalid for input params
      pkPush, pkPushByte: (
        PushType: PUserType; );  //Type of the value to be pushed
      pkBranch: (           //For unconditional branches.
        BranchBlockID: Integer; );  //Block number to branch to
      pkCondBranch: (        //For conditional branches
        BranchInvert: Boolean;    //Value has been NOTted
        TrueBlockID: Integer;     //Block number to branch to if condition is true
        FalseBlockID: Integer;    //Block number to branch to if condition is false
      );
    end; {TILParam}

type TILDataFlags = (cgOverFlowCheck);
  TILDataFlagSet = set of TILDataFlags;

//Record for an atomic IL item
//The operation combines paramer1 and parameter2 and writes the result (if it has one) to
//the temp location specified in Dest
type
  PILItem = ^TILItem;
  TILItem = record
    BlockID: Integer;       //Contains +ve value on first line of a block, otherwise -1
    SourceLineNo: Integer;  //For commented code generation
    Comments: String;       //For debugging <g>. Current only used if BlockID <> -1

    Op: TOperator;          //The operation
    ResultType: PUserType;   //CURRENT: The type which will be output by the Operation
                            //OLD: If overflow checking is on, specifies the output type
                            //May also be used by typecasts to change/reduce value size
    Func: PFunction;        //If OpIndex is OpFuncCall this contains details of the function
                            //to call
    Flags: TILDataFlagSet;
    Prim: PPrimitive;       //Assigned by Clever Puppy, otherwise nil

    //We can have up to three parameters. Each parameter can be Source or Destination
    //far depending on it's Kind. Dest is a pseudonym for Param3 for times when
    //that is useful. Actual Param usage depends on the operation
    Param1: TILParam;       //Data for the first parameter
    Param2: TILParam;       //Data for the second parameter


    //Return a parameter given it's index.
    //Indexes are 1-based
    function GetParam(Index: Integer): PILParam;

    {For CleverPuppy
    Data relates to both the operation itself plus that required to load or move source data,
    store or move dest data, and range check and overflow check sources and dests
    Cycles: Integer;        //Number of CPU cycles required for execution
    Bytes: Integer;         //Number of bytes of code generated

    CPUState: TCPUState;    //The state of the CPU /after/ this step has been executed
    }

    //Gets the VarType from ResultType
    function ResultVarType: TVarType;

    //Where Op = dtBranch
    function GetBranchBlockID: Integer;
    procedure SetBranchBlockID(BlockID: Integer);

    //Creates a hidden variable of the given type, sets it as the Dest,
    //and sets DestType to dtData using SetDestType
    function AssignToHiddenVar(UserType: PUserType): PVariable;

    procedure SwapParams;   //For Operations. Swap order of Param1 and Param2
    function ToString: String;  //For debugging

    case Integer of       //Third param (usually result or destination)
    1: (Param3: TILParam);
    2: (Dest: TILParam);
  end;

procedure InitialiseILData;

type TILList = TList<PILItem>;

function CreateILList: TILList;
procedure ClearILList(List: TILList);
procedure SetCurrentILList(List: TILList);

//These two functions are used by SkipMode to enable
//unwanted code to be removed

//Sets a marker at the current IL position
procedure ILMark;
//Removes any IL code after the marker set by ILMark
procedure ILRollback;

//If set True the next ILItem to be created will be a new block
var NewBlock: Boolean;
  //Sets the comments field of the next block created (see NewBlock)
  //Used by control structure parsing code to add debugging comments
  NewBlockComment: String;
function GetCurrBlockID: Integer;
function GetNextBlockID: Integer;

//Allocates a new IL item and appends it to the list
function ILAppend(AnOp: TOperator): PILItem;
//Allocates a new IL item and inserts it into the IL list at the given Index
//The new ILItem will be a part of the same block as the existing ILItem at that index
//(Ie. if the existing item is the start of a block it's BlockID will be moved to the
//new item
function ILInsert(Index: Integer;AnOp: TOperator): PILItem;

//Append an ILItem for an unconditional branch
function ILAppendBranch(BranchID: Integer): PILItem;

//Used to add Data Loads and Data Stores (commonly used when calling functions to
//load variables or constants into registers and write results from registers to
//variables).
//ILItem parameter can be nil, or the value returned by a previous call to
//either function. ILItem will return a value which may or may not be the same as
//that passed in.
//Returns the ILParam into which data is to be stored.
//Loads and Stores can be combined, but all Loads must occur before all Stores
//Update: Function calls now use FuncData. Data moves still use DataMove
function ILAppendRegLoad(var ILItem: PILItem): PILParam;
function ILAppendRegStore(var ILItem: PILItem): PILParam;
function ILAppendFuncData(var ILItem: PILItem): PILParam;

//Related to ILAppendDataMove, ILAppendFunctionCall sets
//the data for the function call within the IL. This avoids the caller having to
//understand the internal rules for such calls.
//As above, ILItem can be set to nil of a previously returned value. The return
//value may or may not be the same as that passed in
procedure ILAppendFuncCall(var ILItem: PILItem;Func: PFunction);
//As ILAppendDataMove, but for function result. Function result is assigned by the
//expression parser /after/ the ILCode for the function call itself has been generated.
//For that reason the Result parameter must be in Dest (Param3).
//The code here ensures that Dest (Param3) is empty, allocating a new ILItem if
//ILItem passed in is nil. or Dest (Param3) is already assigned.
function ILAppendFuncResult(var ILItem: PILItem): PILParam;

//Returns the current number of items in the IL list
function ILGetCount: Integer;

//Advances Index to the next ILItem. If the item is extended, steps over the
//one or more extended item.
function ILNextIndex(Index: Integer): Integer;

//Returns the Index'th item in the IL list
function ILIndexToData(Index: Integer): PILItem;

//Returns the nth parameter within the given item. If the Item is 'extended'
//across multiple Items then finds the Param among the extended item.
//If the ParamIndex is invalid, returns nil.
//If the end of the list is exceeded raises an exception
function ILIndexToParam(ILIndex, ParamIndex: Integer): PILParam;



//Utilities for the UI displays
procedure ILToStrings(S: TStrings);

var NewSourceLine: Boolean;

implementation
uses SysUtils,
  Def.Globals,
  Parse.Base, Parse.Source,
  Z80.AlgoData;

const CPURegStrings: array[low(TCPUReg)..high(TCPUReg)] of String = (
    'None',//'Immediate','Indirect','Offset',
//    'Param1',
    'A','B','C','D','E','H','L',
    'AF',
    'HL','DE','BC','IX','IY',
    'ZF','ZFandA','NZF','NZFandA','CF','NCF','CPLofA','Flags');

var
  ILList: TILList;
  CurrBlockID: Integer;
  CurrSourceLineNo: Integer;
  ILMarkPosition: Integer;

function GetCurrBlockID: Integer;
begin
  Result := CurrBlockID;
end;

function GetNextBlockID: Integer;
begin
  inc(CurrBlockID);
  Result := CurrBlockID;
end;

procedure InitialiseILData;
begin
  ILList := nil;
  CurrBlockID := 0;
  ILMarkPosition := -1;
end;

procedure ILMark;
begin
  Assert(ILMarkPosition = -1);
  ILMarkPosition := ILList.Count;
end;

procedure ILRollback;
begin
  Assert(ILMarkPosition <> -1);
  while ILList.Count > ILMarkPosition do
  begin
    Dispose(ILList[ILList.Count-1]);
    ILList.Delete(ILList.Count-1);
  end;
  ILMarkPosition := -1;
end;

function CreateILList: TILList;
begin
  Result := TILList.Create;
  NewBlock := True;
  NewBlockComment := '';
  NewSourceLine := True;

  //We can't SkipMode over a change of scope
  ILMarkPosition := -1;
end;

procedure ClearILList(List: TILList);
var Item: PILItem;
begin
  for Item in List do
    Dispose(Item);
  List.Clear;

  //We can't SkipMode over a change of scope
  ILMarkPosition := -1;
end;

procedure SetCurrentILList(List: TILList);
begin
  ILList := List;
end;

function ILCreate: PILItem;
begin
  New(Result);
  if NewBlock then
  begin
    Result.BlockID := GetNextBlockID;
    NewBlock := False;
  end
  else
    Result.BlockID := -1;
  Result.Comments := NewBlockComment;
  NewBlockComment := '';

  if Parser.LineNo <> CurrSourceLineNo then
  begin
    Result.SourceLineNo := Parser.LineNo;
    CurrSourceLineNo := Parser.LineNo;
  end
  else
    Result.SourceLineNo := -1;
  Result.Op := opUnknown;

  Result.Flags := [];
  if optOverflowChecks then
    Result.Flags := Result.Flags + [cgOverflowCheck];
  Result.Prim := nil;

  Result.ResultType := nil;
  Result.Func := nil;

  Result.Param1.Initialise;
  Result.Param2.Initialise;
  Result.Param3.Initialise;
end;

function ILAppend(AnOp: TOperator): PILItem;
begin
  Result := ILCreate;
  Result.Op := AnOp;
  ILList.Add(Result);
end;

function ILInsert(Index: Integer;AnOp: TOperator): PILItem;
var After: PILItem;
begin
  After := ILIndexToData(Index);

  Result := ILCreate;
  Result.Op := AnOp;
  ILList.Insert(Index, Result);
  if After.BlockID <> -1 then
  begin
    Result.BlockID := After.BlockID;
    After.BlockID := -1;
  end;
  if After.Comments <> '' then
  begin
    Result.Comments := After.Comments;
    After.Comments := '';
  end;
end;

function ILAppendBranch(BranchID: Integer): PILItem;
begin
  Result := ILAppend(opBranch);
  Result.SetBranchBlockID(BranchID);
end;

function ILAppendExtendable(var ILItem: PILItem;Basic, Ext: TOperator): PILParam;
begin
  Result := nil;
  if ILItem <> nil then
  begin
    Assert(ILItem.Op = Basic);
    if ILItem.Param3.Kind <> pkNone then
    begin //Item passed in is full
      ILItem.Op := Ext;
      ILItem := nil;
    end;
  end;

  if ILItem = nil then
  begin
    ILItem := ILAppend(Basic);
    Result := @ILItem.Param1;
  end
  else
  begin
    Assert(ILItem.Param1.Kind <> pkNone);
    if ILItem.Param2.Kind = pkNone then
      Result := @ILItem.Param2
    else if ILItem.Param3.Kind = pkNone then
      Result := @ILItem.Param3;
  end;
end;

function ILAppendRegLoad(var ILItem: PILItem): PILParam;
begin
  Result := ILAppendExtendable(ILItem, opRegLoad, opRegLoadExtended);
end;

function ILAppendRegStore(var ILItem: PILItem): PILParam;
begin
  Result := ILAppendExtendable(ILItem, opRegStore, opRegStoreExtended);
end;

function ILAppendFuncData(var ILItem: PILItem): PILParam;
begin
  Result := ILAppendExtendable(ILItem, opFuncCall, opFuncCallExtended);
end;

procedure ILAppendFuncCall(var ILItem: PILItem;Func: PFunction);
begin
  if ILItem = nil then
    ILItem := ILAppend(opFuncCall)
  else
  begin
    Assert(ILItem.Op in [opFuncCall, opFuncCallExtended]);
    Assert(ILItem.Func = nil);
  end;

  ILItem.Func := Func;
end;

function ILAppendFuncResult(var ILItem: PILItem): PILParam;
begin
  if ILItem <> nil then
  begin
    Assert(ILItem.Op = opFuncCall);
    if ILItem.Param3.Kind <> pkNone then
    begin //Item passed in is full
      ILItem.Op := opFuncCallExtended;
      ILItem := nil;
    end;
  end;

  if ILItem = nil then
    ILItem := ILAppend(opFuncCall)
  else
    Assert(ILItem.Dest.Kind = pkNone);

  Result := @ILItem.Dest;
end;

function ILGetCount: Integer;
begin
  Result := ILList.Count;
end;

function ILNextIndex(Index: Integer): Integer;
begin
  while ILIndexToData(Index).Op in ExtendedOps do
    inc(Index);
  Result := Index + 1;
end;

function ILIndexToData(Index: Integer): PILItem;
begin
  Result := ILList[Index];
end;

function ILIndexToParam(ILIndex, ParamIndex: Integer): PILParam;
var Item: PILItem;
begin
  while ParamIndex > 3 do
  begin
    //Verify we are extendable
    Assert(ILIndexToData(ILIndex).Op in ExtendedOps);

    inc(ILIndex);
    dec(ParamIndex, 3);
  end;
  Item := ILIndexToData(ILIndex);
  Result := Item.GetParam(ParamIndex);
end;

{ TILParam }

procedure TILParam.SetCondBranch;
begin
  Assert(Kind in [pkNone, pkCondBranch]);
  Kind := pkCondBranch;
  TrueBlockID := -1;
  FalseBlockID := -1;
  BranchInvert := False;
end;

procedure TILParam.SetPhiVarDest(AVar: PVariable; AVersion: Integer);
begin
  Assert(Kind = pkNone);
  Kind := pkPhiVarDest;
  PhiVar := AVar;
  PhiDestVersion := AVersion;
end;

procedure TILParam.SetPhiVarSource(ABlockID, AVarVersion: Integer);
begin
  Assert(Kind = pkNone);
  Kind := pkPhiVarSource;
  PhiBlockID := ABlockID;
  PhiSourceVersion := AVarVersion;
end;

procedure TILParam.SetVarAddr(AVariable: PVariable);
begin
  Kind := pkVarAddr;
  Variable := AVariable;
  VarVersion := AVariable.Version;
end;

procedure TILParam.SetVarDestAndVersion(AVariable: PVariable;
  AVersion: Integer);
begin
  Assert(Kind in [pkNone, pkVarDest]);
  Kind := pkVarDest;
  Variable := AVariable;
  VarVersion := AVersion;
end;

procedure TILParam.SetVarPtr(AVariable: PVariable);
begin
  Kind := pkVarPtr;
  Variable := AVariable;
  VarVersion := AVariable.Version;
end;

procedure TILParam.SetVarRef(AVAriable: PVariable);
begin
  Kind := pkVarRef;
  Variable := AVariable;
  VarVersion := AVariable.Version;
end;

procedure TILParam.SetVarSource(AVariable: PVariable);
begin
   SetVarSourceAndVersion(AVariable, AVariable.Version);
end;

procedure TILParam.SetVarSourceAndVersion(AVariable: PVariable;AVersion: Integer);
begin
//  Assert(Kind = pkNone);
  Kind := pkVarSource;
  Variable := AVariable;
  VarVersion := AVersion;
end;

function TILParam.GetUserType: PUserType;
begin
  case Kind of
    pkNone, pkPhiVarSource, pkPhiVarDest: EXIT(nil);
    pkImmediate:
      Result := Imm.UserType;
    pkVarSource, pkVarDest:
      Result := Variable.UserType;
    pkVarAddr: //TODO: Make this a typed pointer
      if IsPointeredType(Variable.VarType) then
        Result := Variable.UserType
      else
        Result := GetPointerToType(Variable.UserType);
    pkVarPtr:
    begin
      case UTToVT(Variable.UserType) of
        vtTypedPointer: Result := Variable.UserType.OfType;
        vtPointer: Result := GetSystemType(vtByte);
      else
        Assert(False);
      end;
      Assert(not IsPointeredType(Result.VarType));
    end;
    pkVarRef:
    begin
      case UTToVT(Variable.UserType) of
        vtTypedPointer: Result := Variable.UserType.OfType;
        vtPointer: Result := GetSystemType(vtByte);
      else
        Result := Variable.UserType;
      end;
      Assert(IsPointeredType(Result.VarType));
    end;
    pkPush, pkPushByte, pkPop, pkPopByte:
      Result := PushType;
  else
    raise Exception.Create('Unknown ParamKind');
  end;
end;

function TILParam.GetVarType: TVarType;
begin
  Result := UTToVT(GetUserType);
end;

function TILParam.ImmValueToString: String;
begin
  Assert(Kind = pkImmediate);
  Result := Imm.ToString;
end;

procedure TILParam.Initialise;
begin
  Reg := rNone;
  Kind := pkNone;

  Flags := [];
  if optRangeChecks then
    Flags := Flags + [cgRangeCheck];
  LoadType := lptNormal;
  CheckType := vtUnknown;

  GenOrder := 0;
  //CleverPuppy
  SourceLoc := plNone;
  ResultLoc := plNone;
  Algo := agUnspecified;
  AlgoReg := rNone;
  OverflowAlgo := agNone;
end;

function TILParam.KindMatch(AParam: TILParam): Boolean;
begin
  if Kind <> AParam.Kind then
    EXIT(False);
  case Kind of
    pkNone: EXIT(True);
    pkImmediate: EXIT(False);   //This is a cop out. Fill out if required
    pkPhiVarSource, pkPhiVarDest: EXIT(False);  //Always False(??)
    pkVarSource, pkVarDest, pkVarAddr, pkVarPtr:
      EXIT((Variable = AParam.Variable) and (VarVersion = AParam.VarVersion));
    pkPop, pkPopByte: EXIT(False);
    pkPush, pkPushByte: EXIT(PushType = AParam.PushType);
    pkBranch, pkCondBranch: EXIT(False);  //It makes no sense to compare these
  else
    Assert(False);
    Result := False;
  end;
end;

function TILParam.ToString: String;
(*
  function VarToString: String;
  begin
    Result := '%' + Variable.Name + '_' + IntToStr(VarVersion) +
      ':' + VarTypeToName(Variable.VarType) + '/' + CPURegStrings[Reg];
    if SourceAlgo <> agUnspecified then
      Result := Result + '<' + AlgoData[SourceAlgo].Name;
    if StoreAlgo <> agUnspecified then
      Result := Result + '>' + AlgoData[StoreAlgo].Name;
  end;
*)
begin
  case Kind of
    pkNone: Result := '_';
    pkImmediate:
    begin
      Result := ImmValueToString + ':' +
        VarTypeToName(Imm.VarType) + '->' + CPURegStrings[Reg];
    end;
    pkPhiVarSource:
      Result := '[%_' + IntToStr(PhiSourceVersion) + ' {' + IntToStr(PhiBlockID) + '}] ';
    pkPhiVarDest:
    begin
      Assert(Assigned(PhiVar));
      Result := '%' + PhiVar.Name + '_' + IntToStr(PhiDestVersion);
    end;
    pkVarSource, pkVarAddr, pkVarPtr, pkVarRef:
    begin
      Assert(Assigned(Variable));
      Result := '%' + Variable.Name + '_' + IntToStr(VarVersion);
      case Kind of
        pkVarAddr: Result := '@' + Result;
        pkVarPtr: Result := Result + '^';
        pkVarRef: Result := Result + '!';
      end;
      Result := Result + ':' + Variable.UserType.Description;
      if Reg <> rNone then
        Result := Result + '->' + CPURegStrings[Reg]
      else if SourceLoc = plRegister then
        Result := Result + '->' + CPURegSetToString(SourceRegs);
    end;
    pkVarDest:
    begin
      Assert(Assigned(Variable));
      Result := '%' + Variable.Name + '_' + IntToStr(VarVersion) +
        ':' + Variable.UserType.Description;

      if Reg <> rNone then
        Result := CPURegStrings[Reg] + '->' + Result
      else if ResultLoc = plRegister then
        Result := CPURegSetToString(ResultRegs) + '->' + Result;
    end;
    pkPush:
      Result := Result + 'PUSH /' + CPURegStrings[Reg] + PushType.Description;
    pkPushByte:
      Result := Result + 'PUSHBYTE /' + CPURegStrings[Reg] + PushType.Description;
    pkBranch:
    begin
//      Assert(Param = @ILItem.Param1, 'pkBranch must be Param1');
      Result := 'Branch ' + '{' + IntToStr(BranchBlockID) + '} ';
    end;
    pkCondBranch:
    begin
//      Assert(Param = @ILItem.Param3, 'pkCondBranch must be Param3');
      Result := 'CondBranch ';
      if FalseBlockID = -1 then
        Result := Result + '{' + IntToStr(TrueBlockID) + '} '
      else
        Result := Result + '{' + IntToStr(TrueBlockID) + ',' + IntToStr(FalseBlockID) + '} ';
      Result := Result + '/' + CPURegStrings[Reg];
    end;
  else
    Assert(False);
  end;
  case LoadType of
    lptNormal: ;
    lptHigh: Result := Result + '.hi';
    lptLow: Result := Result + '.lo';
  else
    Assert(False);
  end;
  if Algo <> agUnspecified then
  begin
    Result := '/' + Result;
    if AlgoReg <> rNone then
      Result := CPURegStrings[AlgoReg] + Result;
    Result := AlgoData[Algo].Name + Result;
  end;
  if GenOrder <> 0 then
    Result := Result + '['+GenOrder.ToString+']';
end;

function TILParam.ToVariable: PVariable;
begin
  case Kind of
    pkVarSource, pkVarDest, pkVarAddr, pkVarPtr:
      Result := Variable;
  else
    Assert(False);
    Result := nil;
  end;
end;


{ TILItem }

function TILItem.AssignToHiddenVar(UserType: PUserType): PVariable;
begin
  Result := VarCreateHidden(UserType);
  Result.IncVersion;
  Dest.SetVarDestAndVersion(Result, Result.Version);
end;

function TILItem.GetParam(Index: Integer): PILParam;
begin
  case Index of
    1: Result := @Param1;
    2: Result := @Param2;
    3: Result := @Param3;
  else
    raise Exception.Create('Invalid index');
  end;
end;

function TILItem.ResultVarType: TVarType;
begin
  Result := UTToVT(ResultType);
end;

function TILItem.GetBranchBlockID: Integer;
begin
  Assert(Op = opBranch);
  Assert(Dest.Kind = pkBranch);
  Result := Dest.BranchBlockID;
end;

procedure TILItem.SetBranchBlockID(BlockID: Integer);
begin
  Assert(Op = opBranch);
  Dest.Kind := pkBranch;
  Dest.BranchBlockID := BlockID;
end;

procedure TILItem.SwapParams;
var Temp: TILParam;
begin
  Temp := Param1;
  Param1 := Param2;
  Param2 := Temp;
end;

function DataMoveToString(const Param: TILParam): String;
begin
  case Param.Kind of
    pkNone: ;
    pkImmediate:
      Result := #13';Imm    ' + CPURegStrings[Param.Reg] + ' := ' + Param.ToString;
    pkVarSource:
      Result := #13';VarSource    ' + CPURegStrings[Param.Reg] + ' := ' + Param.ToString;
    pkVarDest:
      Result := #13';VarDest    ' + Param.ToString + ' := ' + CPURegStrings[Param.Reg];
    pkVarAddr:
      Result := #13';VarAddr    ' + CPURegStrings[Param.Reg] + ' := @' + Param.ToString;
    pkVarPtr:
      Result := #13';VarPtr     ' + CPURegStrings[Param.Reg] + ' := ' + Param.ToString + '^';
    pkCondBranch:
    begin
      Result := 'CondBranch ';
      if Param.FalseBlockID = -1 then
        Result := Result + '{' + IntToStr(Param.TrueBlockID) + '} '
      else
        Result := Result + '{' + IntToStr(Param.TrueBlockID) + ',' + IntToStr(Param.FalseBlockID) + '} ';
      Result := Result + '/' + CPURegStrings[Param.Reg];
    end;
  else
    Assert(False);
  end;
end;

function TILItem.ToString: String;
var FuncToDo: PFunction;
begin
  Result := '';

  case Op of
    //Special case operation types
    opRegLoad, opRegLoadExtended,
    opRegStore, opRegStoreExtended,
    opFuncCall, opFuncCallExtended:
    begin
      Assert(Param1.Kind in [pkNone, pkImmediate, pkVarSource, pkVarDest, pkVarAddr]);
      Assert(Param2.Kind in [pkNone, pkImmediate, pkVarSource, pkVarDest, pkVarAddr]);
      Assert(Param3.Kind in [pkNone, pkImmediate, pkVarSource, pkVarDest, pkVarAddr, pkCondBranch]);
      Result := Result + OpStrings[Op];

      FuncToDo := Func;
      if Assigned(FuncToDo) and (Param1.Kind in [pkVarDest]) then
      begin
        Result := Result + #13';    CALL: ' + Func.ToString;
        FuncToDo := nil;
      end;
      Result := Result + DataMoveToString(Param1);
      if Assigned(FuncToDo) and (Param2.Kind in [pkVarDest]) then
      begin
        Result := Result + #13';    CALL: ' + Func.ToString;
        FuncToDo := nil;
      end;
      Result := Result + DataMoveToString(Param2);
      if Assigned(FuncToDo) and (Param3.Kind in [pkVarDest]) then
      begin
        Result := Result + #13';    CALL: ' + Func.ToString;
        FuncToDo := nil;
      end;
      Result := Result + DataMoveToString(Param3);
      if Assigned(FuncToDo) then
        Result := Result + #13';    CALL: ' + Func.ToString;
    end;
  else //General operation types
    Assert(Param1.Kind in [pkNone, pkImmediate, pkVarSource, pkVarAddr, pkVarPtr, pkVarRef, pkPhiVarSource]);
    Assert(Param2.Kind in [pkNone, pkImmediate, pkVarSource, pkVarAddr, pkPhiVarSource]);
    Assert(Dest.Kind in [pkNone, pkCondBranch, pkBranch, pkVarDest, pkVarAddr, pkPhiVarDest, pkPush, pkPushByte]);
    Result := Result + Param3.ToString;

    if Dest.Kind in [pkVarDest, pkPhiVarDest, pkCondbranch, pkPush, pkPushByte] then
      Result := Result + ' = ';
    if Op <> opUnknown then
    begin
      Result := Result + OpStrings[Op];
      if not (Dest.Kind in [pkNone, pkPhiVarDest, pkBranch, pkCondBranch]) then
        Result := Result + ':' + ResultType.Description;
      Result := Result + ' ';
    end;
    Result := Result + Param1.ToString + ', ';
    Result := Result + Param2.ToString;
  end;
end;

procedure ILToStrings(S: TStrings);
var Item: PILItem;
  I: Integer;
begin
  S.Clear;
  for I := 0 to ILGetCount-1 do
  begin
    Item := ILIndexToData(I);
    if Item.BlockID <> -1 then
      S.Add('   ' + IntToStr(Item.BlockID)+':  ' + Item.Comments);
    S.Add(IntToStr(I)+'- ' + Item.ToString);
  end;
end;

initialization
  ILList := nil;
finalization
end.

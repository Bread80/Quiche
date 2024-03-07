unit ILData;

interface
uses Generics.Collections, Classes, QTypes, Variables, Functions, Operators;

type
  //This list details the CPU-level places where primitives can find input data
  //and place result data.
  //For a primitive this specifies the options available for register selection.
  //For ILData this specifies where a primitive must find or place data. I.e.
  //after primitive selection and register allocation has taken place.
  //Not all values are valid in all situtions (inputs, results, primitives, ILData)
  TCPUReg = (
    rNone,      //No parameter or unassigned
    rImm,       //Immediate (constant) value
    rIndirect,  //Indirect data, i.e. (HL)
    rOffset,    //Offset data, i.e. (IX+d)
    rP1,        //Data is output in the same register as param1
    rA, rB, rC, rD, rE, rH, rL, //8 bit registers
    rAF,        //Only for stack operations
    rHL, rDE, rBC, rIX, rIY,    //16 bit register pairs
    rZF, rZFA,  //Zero flag Set. The second variant also sets A as a boolean
    rNZF, rNZFA,//Zero flag Clear. The second variant also sets A as a boolean
    rCF,        //Carry flag set
    rNCF,       //Carry flag clear
    rCPLA,      //Result is complement of A (for boolean results only)
    rFlags      //Flags (for corrupt only)
    );
  TCPURegSet = set of TCPUReg;

//All 'registers' which are truly 'registers'
const CPURegRegs = [rA..rL,rHL..rIY,rZFA,rNZFA,rCPLA];
  CPURegFlags = [rZF,rNZF,rCF,rNCF,rCPLA];

const
  //Mappings between register and register name
  CPUReg8ToChar: array[low(TCPUReg)..High(TCPUReg)] of Char = (
  #0,#0,#0,#0,#0,
  'a','b','c','d','e','h','l',
  #0,#0,#0,#0,#0,#0,
  #0,#0,#0,#0,#0,#0,#0,#0);
  CPURegPairToString: array[low(TCPUReg)..High(TCPUReg)] of String = (
  #0,#0,#0,#0,#0,
  #0,#0,#0,#0,#0,#0,#0,
  'af','hl','de','bc','ix','iy',
  #0,#0,#0,#0,#0,#0,#0,#0);
  //Low reg of a pair
  CPURegLowToChar: array[low(TCPUReg)..High(TCPUReg)] of Char = (
  #0,#0,#0,#0,#0,
  #0,#0,#0,#0,#0,#0,#0,
  #0,'l','e','c',#0,#0,
  #0,#0,#0,#0,#0,#0,#0,#0);
  //High reg of a pair
  CPURegHighToChar: array[low(TCPUReg)..High(TCPUReg)] of Char = (
  #0,#0,#0,#0,#0,
  #0,#0,#0,#0,#0,#0,#0,
  #0,'h','d','b',#0,#0,
  #0,#0,#0,#0,#0,#0,#0,#0);


//Locations where an operation will find and store it's data
type TILParamKind = (
    pkNone,      //No parameter
    pkImmediate, //Immediate data (constant)
    pkPhiVar,    //Parameter for a phi function for a variable
    pkVar,       //Variable
    pkStack,     //The stack
    pkStackByte  //Single byte on the stack
    );

type
  PILParam = ^TILParam;
  TILParam = record
    Reg: TCPUReg; //Register of other location to place datainto before the primitive

    procedure Initialise;

    procedure SetImmediate(AImmValue: Integer;AImmType: TVarType);
    procedure SetVariableAndSub(AVariable: PVariable; AVarSub: Integer);
    //Obtains VarSub from AVarIndex
    procedure SetVariable(AVariable: PVariable);

    //If a Param has an immediate value, convert that value to a 'modern'
    //(cross-compiler native) Integer value
    function ImmToInteger: Integer;
    function ToVariable: PVariable;

    function GetVarType: TVarType;
    function GetRawType: TOpType;

    //For error messages
    function ImmValueToString: String;

    case Kind: TILParamKind of
      pkNone: ();
      pkImmediate: (
        ImmValueInt: Integer;       //Immediate (constant) data
        ImmType: TVarType; ); //Type of the above value
      pkPhiVar: (            //Phi variable (specified in the Dest data)
        PhiBlockID: Integer;  //If we come from this block...
        PhiSub: Integer; );   //...use this version of the variable
      pkVar: (
        Variable: PVariable;  //The variable
        VarSub: Integer; );   //Current Sub (version) of the variable
      pkStack: ();           //Currently invalid for input params
      pkStackByte: ();       //Currently invalid for input params
    end;

  TDestType = (dtNone, dtData, dtBranch, dtCondBranch,
    dtDataLoad);  // <- this is not a Dest. We use it as a third param when loading data for calls

  PILDest = ^TILDest;
  TILDest = record
    Reg: TCPUReg;           //The register or other location where the result of the
                            //operation will be found. From here it can be used in a branch,
                            //store, moved to enother register etc
    procedure SetVarAndSub(AVariable: PVariable; AVarSub: Integer);
    function CreateAndSetHiddenVar(ResultType: TVarType; Storage: TVarStorage): PVariable;

    function ToVariable: PVariable;

    function GetOpType: TOpType;

    case Kind: TILParamKind of
      pkNone: ();
      pkImmediate: ();       //Invalid for destinations
      pkPhiVar: (
        PhiVar: PVariable;    //The variable
        PhiSub: Integer; );   //Sub (version) of the resulting variable
      pkVar: (
        Variable: PVariable;    //The variable
        VarSub: Integer; );   //Sub of variable assignment
      pkStack: ();
      pkStackByte: ();
    end;


type TCodeGenFlags = (cgOverFlowCheck);
  TCodeGenFlagSet = set of TCodeGenFlags;

{
//POTENTIAL DESIGN OF IL DATA:

type TILItemAction =
  (acOperation,     //Unary or binary operation
  acPhiFunction,    //Phi function
  acBranch,         //Unconditional
  acCondBranch,     //Branch to one of two destination (True/False)
  acFunctionCall,   //Dispatch a function with return value
  acProcedureCall,  //Dispatch a procedure
  acDataLoad        //Load values into registers
//  acDataStore??
  );


//Record for an atomic IL item
//The operation combines paramer1 and parameter2 and writes the result (if it has one) to
//the temp location specified in Dest
type
  PILItem = ^TILItem;
  TILItem = record
    BlockID: Integer;       //Contains +ve value on first line of a block, otherwise -1
    SourceLineNo: Integer;  //For commented code generation

    case Action of
    acOperation: (
      Op: TOperator;       //The operation
?      OpType: TOpType;     //Raw data type
?      ResultType: TOpType; //If overflow checking is on, specifies the output type
                           //May also be used by typecasts to change/reduce value size
      CodeGenFlags: TCodeGenFlagSet;
      Left: TILParam;
      Right: TILParam;
      Dest: TILDest;
    );
    acPhiFunction: (
      Variable: PVariable;  //The variable this function is for
      DestSub: Integer;     //The Sub version of the resulting variable
      FromBlock1: Integer;  //If we come from this block...
      FromSub1: Integer;    //...variable will have this Sub
      FromBlock2: Integer;  //If we come from this block...
      FromSub2: Integer;    //...variable will have this sub
      //Possibly extend for more options, eg after a case statement
    );
    acBranch: (
      ToBlock: Integer;
    );
    acCondBranch: (
//      Condition: ??;
      ToBlock1: Integer;
      ToBlock2: Integer;
      //Possibly more options - e.g. after a case statement
    );
    acFunctionCall: (
      Func: PFunction;
      FParam1: TILParam;
      FParam2: TILParam;
      FResult: TILDest;
    );
    acProcedureCall: (
      Proc: PFunction;
      PParam1: TILParam;
      PParam2: TILParam;
      PParam3: TILParam;
    );
    acDataLoad: (
      LParam1: TILParam;
      LParam2: TILParam;
      LParam3: TILParam;
    );
  end;
}

//Record for an atomic IL item
//The operation combines paramer1 and parameter2 and writes the result (if it has one) to
//the temp location specified in Dest
type
  PILItem = ^TILItem;
  TILItem = record
    BlockID: Integer;       //Contains +ve value on first line of a block, otherwise -1
    SourceLineNo: Integer;  //For commented code generation
    Op: TOperator;          //The operation
    OpType: TOpType;        //Raw data type
    ResultType: TOpType;    //If overflow checking is on, specifies the output type
                            //May also be used by typecasts to change/reduce value size
    Func: PFunction;        //If OpIndex is OpIndexCall this contains details of the function
                            //to call
    CodeGenFlags: TCodeGenFlagSet;

    Param1: TILParam;       //Data for the first parameter
    Param2: TILParam;       //Data for the second parameter

    procedure SwapParams;   //For Operations. Swap order of Param1 and Param2
    function ToString: String;  //For debugging

    case DestType: TDestType of
      dtNone: ();
      dtData: (
        Dest: TILDest;      //Data for the destination
        );
      dtCondBranch:         //For conditional branches
      (
        BranchInvert: Boolean;    //Value has been NOTted
        TrueBlockID: Integer;     //Block number to branch to if condition is true
        FalseBlockID: Integer;    //Block number to branch to if condition is false

        BranchReg: TCPUReg; //Assigned by register allocator
      );
      dtBranch: (           //For unconditional branches
        BranchBlockID: Integer; );  //Block number to branch to
      dtDataLoad: (         //When loading data into registers for Calls
        Param3: TILParam; );
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

var NewBlock: Boolean;
function GetCurrBlockID: Integer;
function GetNextBlockID: Integer;

//Allocates a new IL item and appends it to the list
function ILAppend(DestType: TDestType;AnOp: TOperator): PILItem;
//Allocates a new IL item and inserts it into the IL list at the given Index
function ILInsert(Index: Integer;DestType: TDestType;AnOp: TOperator): PILItem;

//Returns the current number of items in the IL list
function ILGetCount: Integer;

//Returns the Index'th item in the IL list
function ILIndexToData(Index: Integer): PILItem;



//Utilities for the UI displays
procedure ILToStrings(S: TStrings);

var NewSourceLine: Boolean;

implementation
uses SysUtils, SourceReader, ParserBase, Globals;

const CPURegStrings: array[low(TCPUReg)..high(TCPUReg)] of String = (
    'None','Immediate','Indirect','Offset',
    'Param1',
    'A','B','C','D','E','H','L',
    'AF',
    'HL','DE','BC','IX','IY',
    'ZF','ZFandA','NZF','NZFandA','CF','NCF','CPLofA','Flags');

procedure TILParam.SetImmediate(AImmValue: Integer;AImmType: TVarType);
begin
  Kind := pkImmediate;
  ImmValueInt := AImmValue;
  ImmType := AImmType;
end;

procedure TILParam.SetVariableAndSub(AVariable: PVariable;AVarSub: Integer);
begin
  Kind := pkVar;
  Variable := AVariable;
  VarSub := AVarSub;
end;

procedure TILParam.SetVariable(AVariable: PVariable);
begin
   SetVariableAndSub(AVariable, AVariable.WriteCount);
end;

function TILParam.ImmToInteger: Integer;
begin
  Assert(Kind = pkImmediate);

  Result := ImmValueInt;
//  if IsSignedType(ImmType) and (Result >= $8000) then
//    Result := Result or (-1 xor $ffff);
end;


function TILParam.GetVarType: TVarType;
begin
  case Kind of
    pkNone, pkPhiVar: EXIT(vtUnknown);
    pkImmediate:
      Result := ImmType;
    pkVar:
      Result := Variable.VarType;
  else
    raise Exception.Create('Unknown parameter kind');
  end;
end;

function TILParam.GetRawType: TOpType;
begin
  Result := VarTypeToOpType(GetVarType);
end;

function TILParam.ImmValueToString: String;
begin
  Assert(Kind = pkImmediate);
  case ImmType of
    vtByte: Result := '$' + IntToHex(ImmValueInt, 2);
    vtWord, vtPointer: Result := '$' + IntToHex(ImmValueInt, 4);
    vtInt8, vtInteger: Result := ImmValueInt.ToString;
    vtBoolean:
      if ImmValueInt = 0 then
        Result := 'False'
      else
        Result := 'True';
    vtChar: Result := chr(ImmValueInt);
    vtTypeDef: Result := VarTypeToName(ImmValueInt);
  else
    Assert(False);
  end;
end;

procedure TILParam.Initialise;
begin
  Reg := rNone;
  Kind := pkNone;
end;

function TILParam.ToVariable: PVariable;
begin
  Assert(Kind = pkVar);
  Result := Variable;
end;



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

function ILCreate(DestType: TDestType): PILItem;
begin
  New(Result);
  if NewBlock then
  begin
    Result.BlockID := GetNextBlockID;
    NewBlock := False;
  end
  else
    Result.BlockID := -1;

  if Parser.LineNo <> CurrSourceLineNo then
  begin
    Result.SourceLineNo := Parser.LineNo;
    CurrSourceLineNo := Parser.LineNo;
  end
  else
    Result.SourceLineNo := -1;
  Result.Op := opUnknown;
  if optOverflowChecks then
    Result.CodeGenFlags := [cgOverflowCheck]
  else
    Result.CodeGenFlags := [];

  Result.DestType := DestType;
  Result.ResultType := rtUnknown;
  Result.Func := nil;
  if Result.DestType = dtData then
    Result.Dest.Kind := pkNone;
  Result.Param1.Initialise;
  Result.Param2.Initialise;

  if Result.DestType = dtDataLoad then
    Result.Param3.Initialise
  else
    Result.Dest.Reg := rNone;
end;

function ILAppend(DestType: TDestType;AnOp: TOperator): PILItem;
begin
  Result := ILCreate(DestType);
  Result.Op := AnOp;
  ILList.Add(Result);
end;

function ILInsert(Index: Integer;DestType: TDestType;AnOp: TOperator): PILItem;
begin
  Result := ILCreate(DestType);
  Result.Op := AnOp;
  ILList.Insert(Index, Result);
end;

function ILGetCount: Integer;
begin
  Result := ILList.Count;
end;

function ILIndexToData(Index: Integer): PILItem;
begin
  Result := ILList[Index];
end;

procedure TILDest.SetVarAndSub(AVariable: PVariable;AVarSub: Integer);
begin
  Kind := pkVar;
  Variable := AVariable;
  VarSub := AVarSub;
end;

function TILDest.CreateAndSetHiddenVar(ResultType: TVarType; Storage: TVarStorage): PVariable;
begin
  Kind := pkVar;
  Result := VarCreateHidden(ResultType, Storage);
  Variable := Result;
  VarSub := Result.WriteCount;
end;

function TILDest.ToVariable: PVariable;
begin
  Assert(Kind = pkVar);
  Result := Variable;
end;

function TILDest.GetOpType: TOpType;
begin
  Assert(Kind = pkVar, 'ILDestToOpType: Invalid Dest type');

  if Variable = nil then
    EXIT(rtUnknown);

  Result := VarTypeToOpType(Variable.VarType);
end;


function ILParamToString(ILItem: PILItem;Param: PILParam): String;
begin
  case Param.Kind of
    pkNone: ;
    pkImmediate:
    begin
      Result := Param.ImmValueToString + ':' +
        VarTypeToName(Param.ImmType);
    end;
    pkPhiVar:
     Result := '[%' + ILItem.Dest.PhiVar.Name + '_' +
      IntToStr(Param.PhiSub) + ' {' + IntToStr(Param.PhiBlockID) + '}] ';
    pkVar:
    begin
      Result := '%' + Param.Variable.Name + '_' + IntToStr(Param.VarSub) +
        ':' + VarTypeToName(Param.Variable.VarType);
    end;
  else
    Result := '<INVALID PARAM>';
  end;
end;

procedure TILItem.SwapParams;
var Temp: TILParam;
begin
  Temp := Param1;
  Param1 := Param2;
  Param2 := Temp;
end;

function TILItem.ToString: String;
begin
  Result := '';

  case DestType of
    dtNone: Result := Result + '_ ';
    dtData:
    begin
      case Dest.Kind of
        pkNone: ;
        pkImmediate: ;
        pkPhiVar: Result := Result + '%' + Dest.PhiVar.Name + '_' + IntToStr(Dest.PhiSub);
        pkVar:
          try
          if Assigned(Dest.Variable) then
            Result := Result + '%' + Dest.Variable.Name + '_' + IntToStr(Dest.VarSub)
          else
            Result := Result + '<UNASSIGNED>';
          except
            Result := Result + 'ACCESS VIOLATION ON VARIABLE';
          end;
        pkStack: Result := Result + 'PUSH';
        pkStackByte: Result := Result + 'PUSHBYTE';
      else
        Result := Result + 'Invalid Dest Kind';
//    raise Exception.Create('Invalid DestKind');
      end;
      if not (Dest.Kind in [pkNone, pkPhiVar]) then
        Result := Result + ':' + OpTypeNames[ResultType];
    end;
    dtCondBranch:
    begin
      Result := Result + 'CondBranch ';
      if FalseBlockID = -1 then
        Result := Result + '{' + IntToStr(TrueBlockID) + '} '
      else
        Result := Result + '{' + IntToStr(TrueBlockID) + ',' + IntToStr(FalseBlockID) + '} ';
      Result := Result + ':' + OpTypeNames[ResultType] + ' ';
    end;
    dtBranch:
      Result := Result + 'Branch ' + '{' + IntToStr(BranchBlockID) + '} ';
    dtDataLoad:
    begin
      Result := Result + 'DATALOAD: ';
      if Param1.Kind <> pkNone then
        Result := Result + CPURegStrings[Param1.Reg] + ':=' + ILParamToString(@Self, @Param1);
      if Param2.Kind <> pkNone then
        Result := Result + CPURegStrings[Param2.Reg] + ':=' + ILParamToString(@Self, @Param2);
      if Param3.Kind <> pkNone then
        Result := Result + CPURegStrings[Dest.Reg] + ':=' + ILParamToString(@Self, @Param3);
    end
    else
      raise Exception.Create('Unkown block type');
  end;

  //Ignore unconditional branches
  if not (DestType in [dtBranch]) then
  begin
    if DestType = dtData then
      Result := Result + '=';
    if Op <> opUnknown then
      Result := Result + Operations[Op].Name;
    if ((DestType = dtData) and not (Dest.Kind in [pkNone, pkPhiVar])) or
      (DestType = dtCondBranch) then
      Result := Result + ':' + OpTypeNames[OpType];
    Result := Result + ' ';

    //First operand
    Result := Result + ILParamToString(@Self, @Param1);

    //Second operand
    if Dest.Kind <> pkNone then
      Result := Result + ' ' + ILParamToString(@Self, @Param2);
  end;

  if Func <> nil then
    Result := Result + Func.ToString;
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
      S.Add('   ' + IntToStr(Item.BlockID)+':  ');
    S.Add(IntToStr(I)+'- ' + Item.ToString);
  end;
end;

initialization
  ILList := nil;
finalization
end.

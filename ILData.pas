unit ILData;

interface
uses Generics.Collections, Classes, QTypes;

const
  //Lanuage constants
  valueFalse = 0;
  valueTrue = -1;
  BooleanToBinary: array[False..True] of Integer = (valueFalse, valueTrue);

  //Runtime errors
  rerrNone = 0;
  rerrOverflow = 1;
  rerrDivByZero = 2;

//CompilerOptions
var
  //If true, variables can be auto-created by any assignment to an un-declared
  //variable. Explicit type declarations are allowed when this option is used.
  //If not type inference will be used. Note that type declarations are ONLY
  //allowed the first time the variable is assigned.
  optAllowAutoCreation: Boolean;

  //If enable certain maths operations will be checked for overflow
  //Use the {$Q} compiler directive
  optOverflowChecks: Boolean;


  //Code register allocation and code generation
  //These types specify where a parameter must (can) be placed before a primitive can
  //generate code. Also specifies where the result of a primitive must be placed
type
  //Locations where a parameter can be found
  TAllocLoc = (
    plNone, //No parameter
    plImm,  //Immediate (constant) value
    plP1,   //Data is output in the same register as param1
    plA, plB, plC, plD, plE, plH, plL,  //8 bit registers
    plHL, plDE, plBC, plIX, plIY,       //16 bit register pairs
    plZF, plZFA,  //Zero flag Set. A version also sets A to non-zero
    plNZF, plNZFA,//Zero flag Clear. A version also sets A to zero
    plCF,         //Carry flag set
    plNCF,        //Cary flag clear
    plCPLA,       //Result is complement of A (for boolean results only)
    plFlags       //Flags (for corrupt only)
    );
  TAllocLocSet = set of TAllocLoc;

const
  //Mappings between register and register name
  AllocLocToReg8: array[low(TAllocLoc)..High(TAllocLoc)] of Char = (
  #0,#0,#0,
  'a','b','c','d','e','h','l',
  #0,#0,#0,#0,#0,
  #0,#0,
  #0,#0,
  #0,
  #0,
  #0,
  #0);
  AllocLocToRegPAir: array[low(TAllocLoc)..High(TAllocLoc)] of String = (
  #0,#0,#0,
  #0,#0,#0,#0,#0,#0,#0,
  'hl','de','bc','ix','iy',
  #0,#0,
  #0,#0,
  #0,
  #0,
  #0,
  #0);
  AllocLocToLowReg: array[low(TAllocLoc)..High(TAllocLoc)] of Char = (
  #0,#0,#0,
  #0,#0,#0,#0,#0,#0,#0,
  'l','e','c',#0,#0,
  #0,#0,
  #0,#0,
  #0,
  #0,
  #0,
  #0);
  AllocLocToHighReg: array[low(TAllocLoc)..High(TAllocLoc)] of Char = (
  #0,#0,#0,
  #0,#0,#0,#0,#0,#0,#0,
  'h','d','b',#0,#0,
  #0,#0,
  #0,#0,
  #0,
  #0,
  #0,
  #0);


//Locations where an operation will find it's data
type TILLocation = (
    locNone,      //No parameter
    locImmediate, //Immediate data (constant)
    locPhiVar,    //Parameter for a phi function for a variable
    locVar,       //Variable
    locTemp       //Temporary (transient) data
    );

type
  PILParam = ^TILParam;
  TILParam = record
    case Loc: TILLocation of
      locNone: ();
      locPhiVar: (            //Phi variable (specified in the Dest data)
        PhiBlockID: Integer;  //If we come from this block...
        PhiSub: Integer; );   //...use this version of the variable
      locImmediate: (
        ImmValue: Word;       //Immediate (constant) data
        ImmType: TVarType; ); //Type of the above value
      locVar: (
        VarIndex: Integer;    //Index into variable list
        VarSub: Integer; );   //Current Sub (version) of the variable
      locTemp: (
        TempIndex: Integer ); //Inded of temp variable (should be updated to be a standard variable index)
    end;

  TDestType = (dtData, dtBranch, dtCondBranch);

  PILDest = ^TILDest;
  TILDest = record
    case Loc: TILLocation of
      locPhiVar: (
        PhiVarIndex: Integer; //Index into the variable list of the variable
        PhiSub: Integer; );   //Sub (version) of the resulting variable
      locVar: (
        VarIndex: Integer;    //Index into variable list
        VarSub: Integer; );   //Sub of variable assignment
      locTemp: (
        TempIndex: Integer ); //Index of Temp variable (should be updated to use VarIndexes)
    end;


type TCodeGenFlags = (cgOverFlowCheck);
  TCodeGenFlagSet = set of TCodeGenFlags;

//Record for an atomic IL item
//The operation combines paramer1 and parameter2 and writes the result (if it has one) to
//the temp location specified in Dest
type
  PILItem = ^TILItem;
  TILItem = record
    BlockID: Integer;       //Contains +ve value on first line of a block, otherwise -1
    SourceLineNo: Integer;  //For commented code generation
    OpIndex: Integer;       //The operation
    OpType: TOpType;        //Raw data type
    ResultType: TOpType;    //If overflow checking is on, specifies the output type
                            //May also be used by typecasts to change/reduce value size
    CodeGenFlags: TCodeGenFlagSet;

    Param1: TILParam;       //Data for the first parameter
    Param2: TILParam;       //Data for the second parameter

    Param1Alloc: TAllocLoc; //Register of other location to place Param1 into before the primitive
    Param2Alloc: TAllocLoc; //Ditto for Param2
    DestAlloc: TAllocLoc;   //And the register or other location to place/move the result into

    case DestType: TDestType of
      dtData: (
        Dest: TILDest;      //Data for the destination
        );
      dtCondBranch:         //For conditional branches
      (
        TrueBlockID: Integer;     //Block number to branch to if condition is true
        FalseBlockID: Integer;    //Block number to branch to if condition is false
      );
      dtBranch: (           //For unconditional branches
        BranchBlockID: Integer; );  //Block number to branch to
  end;

//If a Param has an immediate value, convert that value to a 'modern'
//(cross-compiler native) Integer value
function ILParamValueToInteger(Param: PILParam): Integer;

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
function ILAppend(DestType: TDestType): PILItem;
//Allocates a new IL item and inserts it into the IL list at the given Index
function ILInsert(Index: Integer;DestType: TDestType): PILItem;

//Returns the current number of items in the IL list
function ILGetCount: Integer;

//Returns the Index'th item in the IL list
function ILIndexToData(Index: Integer): PILItem;

function ILParamToVarType(Param: PILParam): TVarType;
function ILParamToRawType(Param: PILParam): TOpType;
function ILDestToOpType(Dest: PILDest): TOpType;



//Utilities for the UI displays
function ILItemToString(Item: PILItem): String;
procedure ILToStrings(S: TStrings);

var NewSourceLine: Boolean;

implementation
uses SysUtils, Variables, MSourceReader, ParserBase, Operators;

function ILParamValueToInteger(Param: PILParam): Integer;
begin
  Assert(Param.Loc = locImmediate);

  Result := Param.ImmValue;
  if (Param.ImmType in SignedTypes) and (Result >= $8000) then
    Result := Result or (-1 xor $ffff);
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
  VarClearTempList;
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
  Result.OpIndex := -1;
  if optOverflowChecks then
    Result.CodeGenFlags := [cgOverflowCheck]
  else
    Result.CodeGenFlags := [];

  Result.DestType := DestType;
  Result.ResultType := rtUnknown;
  if Result.DestType = dtData then
    Result.Dest.Loc := locNone;
  Result.Param1.Loc := locNone;
  Result.Param2.Loc := locNone;
end;

function ILAppend(DestType: TDestType): PILItem;
begin
  Result := ILCreate(DestType);
  ILList.Add(Result);
end;

function ILInsert(Index: Integer;DestType: TDestType): PILItem;
begin
  Result := ILCreate(DestType);
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


function ILParamToVarType(Param: PILParam): TVarType;
var Variable: PVariable;
begin
  case Param.Loc of
    locNone, locPhiVar: EXIT(vtUnknown);
    locImmediate:
      Result := Param.ImmType;
    locVar:
    begin
      Variable := VarIndexToData(Param.VarIndex);
      Result := Variable.VarType;
    end;
    locTemp:
    begin
      Variable := VarTempToData(Param.TempIndex);
      Result := Variable.VarType;
    end;
  else
    raise Exception.Create('Unknown Location for Parameter');
  end;
end;

function ILParamToRawType(Param: PILParam): TOpType;
begin
  Result := lutVarTypeToOpType[ILParamToVarType(Param)];
end;

function ILDestToOpType(Dest: PILDest): TOpType;
var Variable: PVariable;
begin
  case Dest.Loc of
    locVar: Variable := VarIndexToData(Dest.VarIndex);
    locTemp: Variable := VarTempToData(Dest.TempIndex);
  else
    raise Exception.Create('ILDestToOpType: Invalid Dest type');
  end;

  if Variable = nil then
    EXIT(rtUnknown);

  Result := lutVarTypeToOpType[Variable.VarType];
end;


function ILParamToString(ILItem: PILItem;Param: PILParam): String;
var Variable: PVariable;
begin
  case Param.Loc of
    locNone: ;
    locImmediate: Result := IntToStr(Param.ImmValue) + ':' + VarTypeNames[Param.ImmType];
    locPhiVar:
     Result := '[%' + VarIndexToName(ILItem.Dest.PhiVarIndex) + '_' +
      IntToStr(Param.PhiSub) + ' {' + IntToStr(Param.PhiBlockID) + '}] ';
    locVar:
    begin
      Variable := VarIndexToData(Param.VarIndex);
      Result := '%' + Variable.Name + '_' + IntToStr(Param.VarSub) +
        ':' + VarTypeNames[Variable.VarType];
    end;
    locTemp:
    begin
      Variable := VarTempToData(Param.TempIndex);
       Result := '%' + IntToStr(Param.TempIndex) + ':' + VarTypeNames[Variable.VarType];
    end;
  else
    Result := '<INVALID PARAM>';
  end;
end;

function ILItemToString(Item: PILItem): String;
begin
  Result := '';

  case Item.DestType of
    dtData:
    begin
      case Item.Dest.Loc of
        locNone: ;
        locImmediate: ;
        locPhiVar: Result := Result + '%' + VarIndexToName(Item.Dest.PhiVarIndex) + '_' + IntToStr(Item.Dest.PhiSub);
        locVar: Result := Result + '%' + VarIndexToName(Item.Dest.VarIndex) + '_' + IntToStr(Item.Dest.VarSub);
        locTemp: Result := Result + '%' + IntToStr(Item.Dest.TempIndex);
      else
        Result := Result + 'Invalid DestLoc';
//    raise Exception.Create('Invalid DestLoc');
      end;
      if not (Item.Dest.Loc in [locNone, locPhiVar]) then
        Result := Result + ':' + RawTypeNames[Item.ResultType];
    end;
    dtCondBranch:
    begin
      Result := Result + 'CondBranch ';
      if Item.FalseBlockID = -1 then
        Result := Result + '{' + IntToStr(Item.TrueBlockID) + '} '
      else
        Result := Result + '{' + IntToStr(Item.TrueBlockID) + ',' + IntToStr(Item.FalseBlockID) + '} ';
      Result := Result + ':' + RawTypeNames[Item.ResultType] + ' ';
    end;
    dtBranch:
      Result := Result + 'Branch ' + '{' + IntToStr(Item.BranchBlockID) + '} ';
    else
      raise Exception.Create('Unkown block type');
  end;

  //Ignore unconditional branches
  if Item.DestType <> dtBranch then
  begin
    if Item.DestType = dtData then
      Result := Result + '=';
    if Item.OpIndex <> opIndexNone then
      Result := Result + OpIndexToData(Item.OpIndex).Name;
    if ((Item.DestType = dtData) and not (Item.Dest.Loc in [locNone, locPhiVar])) or
      (Item.DestType = dtCondBranch) then
      Result := Result + ':' + RawTypeNames[Item.OpType];
    Result := Result + ' ';

    //First operand
    Result := Result + ILParamToString(Item, @Item.Param1);

    //Second operand
    if Item.Dest.Loc <> locNone then
      Result := Result + ' ' + ILParamToString(Item, @Item.Param2);
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
      S.Add('   ' + IntToStr(Item.BlockID)+':  ');
    S.Add(IntToStr(I)+'- ' + ILItemToString(Item));
  end;
end;

initialization
  ILList := nil;
finalization
end.

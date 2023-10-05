unit Operators;

interface
uses QTypes;

//Convert Inc to Add and Dec to Sub for cases of Inc(r, n) and Dec(r, n)
//where n > this value
const iConvertIncToAddThreshhold = 5;

type
  TOpGroup = (
    ogSystem,   //Compiler goodness :)
    ogBinary,   //Operators with two parameters - findable by symbol/name
    ogUnary,    //Unary operators
    ogTypecast, //Typecasts
    ogFunc,     //Appears as a function to the user
    ogProc);    //Appears as a procedure to the user

  TOpFuncFlag = (
    opfP1Variable,      //First parameter must be a variable reference
    opfP1BitSize16Only, //First param must be a 16 bit value
    opfP2Optional,      //Second parameter is optional
    opfP2Immediate,     //Second parameter must be an immediate/constant value
    opfNoOverflowChecks //Don't perform any overflow checks
    );
  TOpFuncFlagSet = set of TOpFuncFlag;

  POperator = ^TOperator;
  TOperator = record
    Symbol: String;           //Symbol or keyword
    OpGroup: TOpGroup;        //Operator gouping
    Name: String;             //Internal name of the operation
    OpTypes: TOpTypeSet;      //Set of available primitive types
    Logical: Boolean;         //If true the operator is a boolean logical one (AND, OR, XOR, NOT)
                              //Used for special processing of parameters.
    Precedence: Integer;      //In expressions. Higher equal higher
    LTypes: TTypeEnumSet;     //List of applicable types for the left operand
    RTypes: TTypeEnumSet;     //List of applicable types for the right operand
    Swappable: Boolean;       //Can operators be swapped?
    MatchOperands: Boolean;   //Extend numeric operators to match each other
    FuncFlags: TOpFuncFlagSet;//Flags for function/proecedure intrinsics
    ResultSame: Boolean;      //If true the result will be the same type as the operands
                              //(Left operand, if only one operand)
    ResultType: TTypeEnum;    //The output type. vtUnknown specifies same as input (left operand)
  end;

var
  OpIndexNone: Integer;
  OpIndexAssign: Integer;
  OpIndexBranch: Integer;
  OpIndexConstBranch: Integer;
  OpIndexCondBranch: Integer;
  OpIndexPhi: Integer;
  OpIndexAdd: Integer;
  OpIndexSubtract: Integer;
  OpIndexMultiply: Integer;
//  OpIndexRealDivide: Integer;
  OpIndexIntDivide: Integer;
  OpIndexMod: Integer;
  OpIndexEqual: Integer;
  OpIndexNotEqual: Integer;
  OpIndexLess: Integer;
  OpIndexGreater: Integer;
  OpIndexLessEqual: Integer;
  OpIndexGreaterEqual: Integer;
  OpIndexAND: Integer;
  OpIndexOR: Integer;
  OpIndexXOR: Integer;
  OpIndexSHL: Integer;
  OpIndexSHR: Integer;
  OpIndexNegate: Integer;
  OpIndexNOT: Integer;

  OpIndexWrite: Integer;
  OpIndexWriteLn: Integer;
  OpIndexWriteNewLine: Integer;
  OpIndexWriteChar: Integer;
  OpIndexWriteInteger: Integer;
  OpIndexWriteBoolean: Integer;

  OpIndexInc: Integer;
  OpIndexDec: Integer;

function OpIndexToData(Index: Integer): POperator;

//Finds the first operator with the given symbol
function OpSymbolToData(Symbol: String;out Index: Integer;out Op: POperator): Boolean;
//Finds the operator withe the given Name
function OpNameToIndex(Name: String): Integer;

//Returns only functions
function OpFunctionToIndex(Name: String): Integer;

//Returns procedures and functions (i.e any which can be used as statements)
function OpProcedureToIndex(Name: STring): Integer;

//Updates the OpIndex to one which can handle the given parameters
//If there is no matching operator, returns an error.
//
//NOTE: some operations are available in multiple variants with different
//operand and result types. For example logical operations (OR, AND, XOR) have
//both bitwise and boolean variants. These have multiple entries in the Operators.
//This code will update the OpIndex as necessary.
//function UpdateOpForParams(var OpIndex: Integer;LType, RType: TVarType): Boolean;

procedure InitialiseOperatorList;

//Convert an operator index to a string explaining it's usage
function OpIndexToUsage(OpIndex: Integer): String;

implementation
uses Generics.Collections, Classes, SysUtils;

var OpList : TList<POperator>;

function OpIndexToData(Index: Integer): POperator;
begin
  Result := OpList[Index];
end;

function OpSymbolToData(Symbol: String;out Index: Integer;out Op: POperator): Boolean;
var I: Integer;
begin
  for I := 0 to OpList.Count-1 do
    if (OpList[I].OpGroup = ogBinary) and (CompareText(OpList[I].Symbol, Symbol) = 0) then
    begin
      Index := I;
      Op := OpList[I];
      EXIT(True);
    end;

  Index := OpIndexNone;
  Op := nil;
  EXIT(False);
end;

function OpNameToIndex(Name: String): Integer;
begin
  for Result := 0 to OpList.Count-1 do
    if CompareText(OpList[Result].Name, Name) = 0 then
      EXIT;

  Result := -1;
end;

function OpFunctionToIndex(Name: String): Integer;
begin
  for Result := 0 to OpList.Count-1 do
    if OpList[Result].OpGroup in [ogFunc, ogProc, ogTypecast] then
      if CompareText(OpList[Result].Name, Name) = 0 then
        EXIT;

  Result := -1;
end;

//Returns procedures and functions (i.e any which can be used as statements)
function OpProcedureToIndex(Name: STring): Integer;
begin
  for Result := 0 to OpList.Count-1 do
    if OpList[Result].OpGroup in [ogFunc, ogProc] then
      if CompareText(OpList[Result].Name, Name) = 0 then
        EXIT;

  Result := -1;
end;

function StringToOpGroup(S: String): TOpGroup;
begin
  if S = 'system' then
    EXIT(ogSystem);
  if S = 'binary' then
    EXIT(ogBinary);
  if S = 'unary' then
    EXIT(ogUnary);
  if S = 'typecast' then
    EXIT(ogTypecast);
  if S = 'func' then
    EXIT(ogFunc);
  if S = 'proc' then
    EXIT(ogProc);
  raise Exception.Create('Unknown group');
end;

//Returns True if the first char of S is Y or y.
//Returns false if S is empty or first char is no Y or y
function StringToBoolean(S: String): Boolean;
begin
  Result := (Length(S) > 0) and (S.Chars[0] in ['Y','y']);
end;

function StringToFuncFlagSet(S: String): TOpFuncFlagSet;
var Items: TArray<String>;
  I: String;
begin
  Result := [];
  Items := S.Split([';']);
  for I in Items do
    if CompareText(I, 'p1variable') = 0 then
      Result := Result + [opfP1Variable]
    else if CompareText(I, 'p1Bitsize16Only') = 0 then
      Result := Result + [opfP1BitSize16Only]
    else if CompareText(I, 'p2optional') = 0 then
      Result := Result + [opfP2OPtional]
    else if CompareText(I, 'p2immediate') = 0 then
      Result := Result + [opfP2Immediate]
    else if CompareText(I, 'nooverflowchecks') = 0 then
      Result := Result + [opfNoOverflowChecks]
    else
      raise Exception.Create('Unknown OpFuncFlag: ' + I);
end;

const //Field indexes

  fGroup          = 2;
  fSymbol         = 3;
  fOpName         = 4;
  fOpTypes        = 5;
  fPrecedence     = 6;
  fLeftOp         = 7;
  fRightOp        = 8;
  fSwappable      = 9;
  fMatchOperands  = 10;
  fFuncFlags      = 11;
  fResultType     = 12;

procedure InitialiseOperatorList;
var Data: TStringList;
  Line: String;
  Fields: TArray<String>;
  Op: POperator;
begin
  if OpList <> nil then
    OpList.Free;

  OpList := TList<POperator>.Create;

  Data := TStringList.Create;
  Data.LoadFromFile('..\..\docs\Operators.csv');

  for Line in Data do
    if (Length(Line) > 0) and (Line.Chars[0] <> ';') then
    begin
      Fields := Line.Split([',']);
      if Length(Fields) < 11 then
        raise Exception.Create('Operators line too short: ' + Line);
      Op := New(POperator);
      OpList.Add(Op);

      Op.Symbol := Fields[fSymbol].ToLower;
      Op.OpGroup := StringToOpGroup(Fields[fGroup].ToLower);
      Op.Name := Fields[fOpName];
      Op.OpTypes := StringToOpTypeSet(Fields[fOpTypes]);
      Op.Precedence := StrToInt(Fields[fPrecedence]);
      Op.Logical := CompareText(Fields[fLeftOp], 'logical') = 0;
      Op.LTypes := StringToTypeEnumSet(Fields[fLeftOp]);
      Op.RTypes := StringToTypeEnumSet(Fields[fRightOp]);
      Op.Swappable := StringToBoolean(Fields[fSwappable]);
      Op.MatchOperands := StringToBoolean(Fields[fMatchOperands]);
      Op.FuncFlags := StringToFuncFlagSet(Fields[fFuncFlags]);

      if (CompareText(Fields[fResultType], 'same') = 0) or
        (CompareText(Fields[fResultType], 'left op') = 0) then
      begin
        Op.ResultSame := True;
        Op.ResultType := teUnknown;
      end
      else
      begin
        Op.ResultSame := False;
        Op.ResultType := StringToTypeEnum(Fields[fResultType]);
      end;
    end;

    OpIndexNone := OpNameToIndex('none');
    OpIndexPhi := OpNameToIndex('phi');
    OpIndexBranch := OpNameToIndex('branch');
    OpIndexCondBranch := OpNameToIndex('condbranch');
    OpIndexConstBranch := OpNameToIndex('constbranch');
    OpIndexAssign := OpNameToIndex('assign');

    OpIndexAdd := OpNameToIndex('add');
    OpIndexSubtract := OpNameToIndex('subtract');
    OpIndexMultiply := OpNameToIndex('multiply');
//    OpIndexFloatDivide := OpNameToIndex('floatdiv');
    OpIndexIntDivide := OpNameToIndex('intdiv');
    OpIndexMod := OpNameToIndex('mod');

    OpIndexEqual := OpNameToIndex('equal');
    OpIndexNotEqual := OpNameToIndex('notequal');
    OpIndexLess := OpNameToIndex('less');
    OpIndexGreater := OpNameToIndex('greater');
    OpIndexLessEqual := OpNameToIndex('lessequal');
    OpIndexGreaterEqual := OpNameToIndex('greaterequal');

    OpIndexAND := OpNameToIndex('AND');
    OpIndexOR := OpNameToIndex('OR');
    OpIndexXOR := OpNameToIndex('XOR');
    OpIndexSHL := OpNameToIndex('SHL');
    OpIndexSHR := OpNameToIndex('SHR');

    OpIndexNOT := OpNameToIndex('not');
    OpIndexNegate := OpNameToIndex('negate');

    OpIndexWrite := OpNameToIndex('write');
    OpIndexWriteLn := OpNameToIndex('writeln');
    OpIndexWriteNewLine := OpNameToIndex('writenewline');
    OpIndexWriteChar := OpNameToIndex('writechar');
    OpIndexWriteInteger := OpNameToIndex('writeinteger');
    OpIndexWriteBoolean := OpNameToIndex('writeboolean');

    OpIndexInc := OpNameToIndex('inc');
    OpIndexDec := OpNameToIndex('dec');

    Assert(OpIndexPhi <> 0);
    Assert(OpIndexBranch <> 0);
    Assert(OpIndexConstBranch <> 0);
    Assert(OpIndexCondBranch <> 0);
    Assert(OpIndexAssign <> 0);

    Assert(OpIndexAdd <> 0);
    Assert(OpIndexSubtract <> 0);
    Assert(OpIndexMultiply <> 0);
//    Assert(OpIndexFloatDivide <> 0);
    Assert(OpIndexIntDivide <> 0);
    Assert(OpIndexMod <> 0);

    Assert(OpIndexEqual <> 0);
    Assert(OpIndexNotEqual <> 0);
    Assert(OpIndexLess <> 0);
    Assert(OpIndexGreater <> 0);
    Assert(OpIndexLessEqual <> 0);
    Assert(OpIndexGreaterEqual <> 0);

    Assert(OpIndexAND <> 0);
    Assert(OpIndexOR <> 0);
    Assert(OpIndexXOR <> 0);
    Assert(OpIndexSHL <> 0);
    Assert(OpIndexSHR <> 0);

    Assert(OpIndexNOT <> 0);
    Assert(OpIndexNegate <> 0);

    Assert(OpIndexWrite <> 0);
    Assert(OpIndexWriteLn <> 0);
    Assert(OpIndexWriteChar <> 0);
    Assert(OpIndexWriteInteger <> 0);
    Assert(OpIndexWriteBoolean <> 0);
    Assert(OpIndexWriteNewLine <> 0);

    Assert(OpIndexInc <> 0);
    Assert(OpIndexDec <> 0);
end;

(*
  TOpGroup = (
    ogSystem,   //Compiler goodness :)
    ogBinary,   //Operators with two parameters - findable by symbol/name
    ogUnary,    //Unary operators
    ogTypecast, //Typecasts
    ogFunc,     //Appears as a function to the user
    ogProc);    //Appears as a procedure to the user

  POperator = ^TOperator;
  TOperator = record
    Symbol: String;           //Symbol or keyword
    OpGroup: TOpGroup;        //Operator gouping
    Name: String;             //Internal name of the operation
    OpTypes: TOpTypeSet;      //Set of available primitive types
    Logical: Boolean;         //If true the operator is a boolean logical one (AND, OR, XOR, NOT)
                              //Used for special processing of parameters.
    Precedence: Integer;      //In expressions. Higher equal higher
    LTypes: TTypeEnumSet;     //List of applicable types for the left operand
    RTypes: TTypeEnumSet;     //List of applicable types for the right operand
    Swappable: Boolean;       //Can operators be swapped?
    MatchOperands: Boolean;   //Extend numeric operators to match each other
    Param2Optional: Boolean;  //Parameter 2 is optional. (Only valid for functions)
    ResultSame: Boolean;      //If true the result will be the same type as the operands
                              //(Left operand, if only one operand)
    ResultType: TTypeEnum;    //The output type. vtUnknown specifies same as input (left operand)
  end;
*)
function OpIndexToUsage(OpIndex: Integer): String;
var OpData: POperator;
begin
  OpData := OpIndexToData(OpIndex);
  Result := OpData.Symbol + ' (stuff to do here)';
end;

end.

unit Operators;

interface
uses QTypes;

type
  POperator = ^TOperator;
  TOperator = record
    Symbol: String;        //Symbol or keyword
    Name: String;         //Internal name of the operation
    OpTypes: TOpTypeSet;  //Set of available primitive types
    Logical: Boolean;     //If true the operator is a boolean logical one (AND, OR, XOR, NOT)
                          //Used for special processing of parameters.
    Precedence: Integer;  //In expressions. Higher equal higher
    LTypes: TTypeEnumSet;  //List of applicable types for the left operand
    RTypes: TTypeEnumSet;  //List of applicable types for the right operand
    Swappable: Boolean;   //Can operators be swapped?
    MatchOperands: Boolean; //Extend numeric operators to match each other
    ResultSame: Boolean;    //If true the result will be the same type as the operands
                            //(Left operand, if only one operand)
    ResultType: TTypeEnum;   //The output type. vtUnknown specifies same as input (left operand)
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
//  OpIndexNegate: Integer;
//  OpIndexNOT: Integer;


function OpIndexToData(Index: Integer): POperator;

//Finds the first operator with the given symbol
function OpSymbolToData(Symbol: String;out Index: Integer;out Op: POperator): Boolean;
//Finds the operator withe the given Name
function OpNameToIndex(Name: String): Integer;

//Updates the OpIndex to one which can handle the given parameters
//If there is no matching operator, returns an error.
//
//NOTE: some operations are available in multiple variants with different
//operand and result types. For example logical operations (OR, AND, XOR) have
//both bitwise and boolean variants. These have multiple entries in the Operators.
//This code will update the OpIndex as necessary.
//function UpdateOpForParams(var OpIndex: Integer;LType, RType: TVarType): Boolean;

procedure InitialiseOperatorList;

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
    if CompareText(OpList[I].Symbol, Symbol) = 0 then
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

const //Field indexes
  fSymbol         = 2;
  fOpName         = 3;
  fOpTypes        = 4;
  fPrecedence     = 5;
  fLeftOp         = 6;
  fRightOp        = 7;
  fSwappable      = 8;
  fMatchOperands  = 9;
  fResultType     = 10;

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
      Op.Name := Fields[fOpName];
      Op.OpTypes := StringToOpTypeSet(Fields[fOpTypes]);
      Op.Precedence := StrToInt(Fields[fPrecedence]);
      Op.Logical := CompareText(Fields[fLeftOp], 'logical') = 0;
      Op.LTypes := StringToTypeEnumSet(Fields[fLeftOp]);
      Op.RTypes := StringToTypeEnumSet(Fields[fRightOp]);
      Op.Swappable := Fields[fSwappable].Chars[0] in ['Y','y'];
      Op.MatchOperands := (Length(Fields[fMatchOperands]) > 0) and (Fields[fMatchOperands].Chars[0] in ['Y','y']);
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
end;

end.

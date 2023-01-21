unit ILData;

interface
uses Generics.Collections, Classes;

//Operations within the IL (Intermediate language)
type TILOperation = (
  opNone,
  opAssign,
  opEqual, opNotEqual, opLess, opGreater, opLessEqual, opGreaterEqual, opIn,
  opAdd, opSubtract, opOR, opXOR,
  opMultiply, opDivide, opDiv, opMod,  opAnd, opShr, opShl,
  opUnaryPlus, opNegate, opNot, opAt
  );

//Precedence for the operations
const ILOpPrecedences: array[low(TILOperation)..high(TILOperation)] of Integer = (
  -1,
  -1,
  1,1,1,1, 1,1,1,
  2,2,2,2,
  3,3,3,3, 3,3,3,
  4,4,4,4);

//Friendly names of the operations
const ILOpStrings: array [low(TILOperation)..high(TILOperation)] of String = (
  '<None>',
  '',//'Assign',
  'Equal','NotEqual','LessThan','GreaterThan','LessThanOrEqual','GreaterThanOrEqual','In',
  'Add','Subtract','or','xor',
  'Multiply','Divide','div','mod','and','shr','shl',
  'UnaryPlus','Negate','not','At');

//Locations where an operation will find it's data
type TILLocation = (
    locNone,      //No parameter
    locImmediate, //Immediate data. Parameter is the data.
    locLocal,     //Local variable. Parameter is an index in the variables list
    locTemp       //Temporary (transient) data. Parameter is the temp result index
                  //(i.e. the result of a previous computation).
    );

type TILType = (iltPrimitive, iltBranch);

//Record for an atomic IL item
//The operation combines paramer1 and parameter2 and writes the result (if it has one) to
//the temp location specified in Dest
type
  PILItem = ^TILItem;
  TILItem = record
    BlockID: Integer;       //Contains +ve value on first line of a block, otherwise -1
    case ILType: TILType of
    iltPrimitive:
      (
      Op: TILOperation;       //The operation
      DestLoc: TILLocation;
      DestData: Integer;      //Destination (a temp result index). -1 if operation has no result
      DestSub: Integer;

      Param1Loc: TILLocation; //Location of parameter 1
      Param1Data: Integer;    //Data for parameter 1
      Param1Sub: Integer;     //Subscript for parameter 1 (i.e. the variable write index)

      Param2Loc: TILLocation; //Location for parameter 2
      Param2Data: Integer;    //Data for parameter 2
      Param2Sub: Integer;     //Subscript for parameter 2 (i.e. the variable write index)
      );
    iltBranch:
      (
      TrueBlock: Integer;     //Block number to branch to if condition is true
      FalseBlock: Integer;    //Block number to branch to if condition is false
      )
    end;

//Clears the list of IL instructions and any related data
procedure ClearILList;

//Gets the next temp value index, and increments it (CurrTempIndex) for the next call
function GetNextTempIndex: Integer;

var NewBlock: Boolean;
function GetCurrBlockID: Integer;
function GetNextBlockID: Integer;

//Allocates a new IL item and adds it to the list
function AllocILItem(ILType: TILType): PILItem;

//Returns the current number of items in the IL list
function GetILCount: Integer;

//Returns the Index'th item in the IL list
function GetILItem(Index: Integer): PILItem;



//Utilities for the UI displays
function ILItemToString(Item: PILItem): String;
procedure ILToStrings(S: TStrings);


implementation
uses SysUtils, Variables;

var
  ILList: TList<PILItem>;
  CurrTempIndex: Integer;
  CurrBlockID: Integer;

function GetNextTempIndex: Integer;
begin
  Result := CurrTempIndex;
  inc(CurrTempIndex);
end;

function GetCurrBlockID: Integer;
begin
  Result := CurrBlockID;
end;

function GetNextBlockID: Integer;
begin
  inc(CurrBlockID);
  Result := CurrBlockID;
end;

procedure InitILList;
begin
  ILList := TList<PILItem>.Create;
end;

procedure FreeILList;
begin
  ClearILList;
  ILList.Free;
end;

procedure ClearILList;
var Item: PILItem;
begin
  for Item in ILList do
    Dispose(Item);
  ILList.Clear;

  CurrTempIndex := 1;
  CurrBlockID := 0;
  NewBlock := True;
end;

function AllocILItem(ILType: TILType): PILItem;
begin
  New(Result);
  ILList.Add(Result);
  if NewBlock then
  begin
    Result.BlockID := GetNextBlockID;
    NewBlock := False;
  end
  else
    Result.BlockID := -1;
  Result.ILType := ILType;
end;

function GetILCount: Integer;
begin
  Result := ILList.Count;
end;

function GetILItem(Index: Integer): PILItem;
begin
  Result := ILList[Index];
end;

function ILItemToString(Item: PILItem): String;
begin
  Result := '';

  case Item.ILType of
    iltPrimitive:
    begin
      case Item.DestLoc of
        locNone: ;
        locImmediate: ;
        locLocal: Result := Result + '%' + VarIndexToName(Item.DestData) + '_' + IntToStr(Item.DestSub);
        locTemp: Result := Result + '%' + IntToStr(Item.DestData);
      else
        Result := Result + 'Invalid DestLoc';
//    raise Exception.Create('Invalid DestLoc');
      end;

      Result := Result + ' = ' + ILOpStrings[Item.Op] + ' ';

      case Item.Param1Loc of
        locNone: ;
        locImmediate: Result := Result + IntToStr(Item.Param1Data);
        locLocal: Result := Result + '%' + VarIndexToName(Item.Param1Data) + '_' + IntToStr(Item.Param1Sub);
        locTemp: Result := Result + '%' + IntToStr(Item.Param1Data);
      else
//    raise Exception.Create('Invalid Param1Loc');
      end;

      if Item.Op <> opAssign then
      case Item.Param2Loc of
        locNone: ;
        locImmediate: Result := Result + ', ' + IntToStr(Item.Param2Data);
        locLocal: Result := Result + ', %' + VarIndexToName(Item.Param2Data) + '_' + IntToStr(Item.Param2Sub);
        locTemp: Result := Result + ', %' + IntToStr(Item.Param2Data);
      else
//    raise Exception.Create('Invalid Param2Loc');
      end;
    end;
    iltBranch:
    begin
      Result := Result + 'Branch ';
      if Item.FalseBlock = -1 then
        Result := Result + '{' + IntToStr(Item.TrueBlock) + '}'
      else
        Result := Result + ' T{' + IntToStr(Item.TrueBlock) + '} F{' + IntToStr(Item.FalseBlock) + '}';
    end
    else
      raise Exception.Create('Unkown block type');
    end;
end;

procedure ILToStrings(S: TStrings);
var Item: PILItem;
begin
  S.Clear;
  for Item in ILList do
  begin
    if Item.BlockID <> -1 then
      S.Add(IntToStr(Item.BlockID)+':  ');
    S.Add(ILItemToString(Item));
  end;
end;

initialization
  InitILList;
finalization
  FreeILList;
end.

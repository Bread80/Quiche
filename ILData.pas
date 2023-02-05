unit ILData;

interface
uses Generics.Collections, Classes;

//Operations within the IL (Intermediate language)
type TILOperation = (
  opNone,
  opAssign,
  opPhi,
  opEqual, opNotEqual, opLess, opGreater, opLessEqual, opGreaterEqual, opIn,
  opAdd, opSubtract, opOR, opXOR,
  opMultiply, opDivide, opDiv, opMod,  opAnd, opShr, opShl,
  opUnaryPlus, opNegate, opNot, opAt
  );

//Precedence for the operations
const ILOpPrecedences: array[low(TILOperation)..high(TILOperation)] of Integer = (
  -1,
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
  'phi',
  'Equal','NotEqual','LessThan','GreaterThan','LessThanOrEqual','GreaterThanOrEqual','In',
  'Add','Subtract','or','xor',
  'Multiply','Divide','div','mod','and','shr','shl',
  'UnaryPlus','Negate','not','At');

//Locations where an operation will find it's data
type TILLocation = (
    locNone,      //No parameter
    locOUT,       //For testing!
    locPhiVar,    //Parameter for a phi function for a variable.
    locImmediate, //Immediate data. Parameter is the data.
    locVar,       //Variable. Parameter is an index in the variables list
    locTemp       //Temporary (transient) data. Parameter is the temp result index
                  //(i.e. the result of a previous computation).
    );

type TDestType = (dtData, dtBranch);

//Record for an atomic IL item
//The operation combines paramer1 and parameter2 and writes the result (if it has one) to
//the temp location specified in Dest
type
  PILItem = ^TILItem;
  TILItem = record
    BlockID: Integer;       //Contains +ve value on first line of a block, otherwise -1
    Op: TILOperation;       //The operation

    Param1Loc: TILLocation; //Location of parameter 1
    Param1Data: Integer;    //Data for parameter 1.
    Param1Sub: Integer;     //Subscript for parameter 1 (i.e. the variable write index)

    Param2Loc: TILLocation; //Location for parameter 2
    Param2Data: Integer;    //Data for parameter 2
    Param2Sub: Integer;     //Subscript for parameter 2 (i.e. the variable write index)

    case DestType: TDestType of
      dtData: (
        DestLoc: TILLocation;
        DestData: Integer;      //Destination (a temp result index). -1 if operation has no result
        DestSub: Integer;
      );
      dtBranch:
      (
        TrueBlock: Integer;     //Block number to branch to if condition is true
        FalseBlock: Integer;    //Block number to branch to if condition is false
                                //If FalseBlock is -1 the branch is unconditional
      );
  end;

//Clears the list of IL instructions and any related data
procedure ClearILList;

//Gets the next temp value index, and increments it (CurrTempIndex) for the next call
function GetNextTempIndex: Integer;

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

  CurrTempIndex := 0;
  CurrBlockID := 0;
  NewBlock := True;
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
  Result.DestType := DestType;
  if Result.DestType = dtData then
    Result.DestLoc := locNone;
  Result.Param1Loc := locNone;
  Result.Param2Loc := locNone;
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

function ILItemToString(Item: PILItem): String;
begin
  Result := '';

  case Item.DestType of
    dtData:
    begin
      case Item.DestLoc of
        locNone: ;
        locOUT: Result := Result + 'OUT ';
        locImmediate: ;
        locVar: Result := Result + '%' + VarIndexToName(Item.DestData) + '_' + IntToStr(Item.DestSub);
        locTemp: Result := Result + '%' + IntToStr(Item.DestData);
      else
        Result := Result + 'Invalid DestLoc';
//    raise Exception.Create('Invalid DestLoc');
      end;
    end;
    dtBranch:
    begin
      Result := Result + 'Branch ';
      if Item.FalseBlock = -1 then
        Result := Result + '{' + IntToStr(Item.TrueBlock) + '} '
      else
        Result := Result + '{' + IntToStr(Item.TrueBlock) + ',' + IntToStr(Item.FalseBlock) + '} ';
    end;
    else
      raise Exception.Create('Unkown block type');
  end;

  //Ignore unconditional branches
  if not ((Item.DestType = dtBranch) and (Item.FalseBlock = -1)) then
  begin
    if Item.DestType <> dtBranch then
      Result := Result + ' = ';
    Result := Result + ILOpStrings[Item.Op] + ' ';

    case Item.Param1Loc of
      locNone: ;
      locImmediate: Result := Result + IntToStr(Item.Param1Data);
      locPhiVar: Result := Result + '[%' + VarIndexToName(Item.DestData) + '_' +
        IntToStr(Item.Param1Sub) + ' {' + IntToStr(Item.Param1Data) + '}] ';
      locVar: Result := Result + '%' + VarIndexToName(Item.Param1Data) + '_' + IntToStr(Item.Param1Sub);
      locTemp: Result := Result + '%' + IntToStr(Item.Param1Data);
    else
//    raise Exception.Create('Invalid Param1Loc');
    end;

    if Item.Op <> opAssign then
    case Item.Param2Loc of
      locNone: ;
      locImmediate: Result := Result + ', ' + IntToStr(Item.Param2Data);
      locPhiVar: Result := Result + '[%' + VarIndexToName(Item.DestData) + '_' +
        IntToStr(Item.Param2Sub) + ' {' + IntToStr(Item.Param2Data) + '}]';
      locVar: Result := Result + ', %' + VarIndexToName(Item.Param2Data) + '_' + IntToStr(Item.Param2Sub);
      locTemp: Result := Result + ', %' + IntToStr(Item.Param2Data);
    else
//    raise Exception.Create('Invalid Param2Loc');
    end;
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
  InitILList;
finalization
  FreeILList;
end.

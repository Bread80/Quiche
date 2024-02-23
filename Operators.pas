unit Operators;
(*
Operators/Operations are operations performed on data by a program. These might
be:
* Infix or prefix operators (+ * = shl and in) etc.
* Intrinsics: built in functions such as abs() hi() sizeof() inc() for which the
  code generator outputs 'inline' code directly into the output (contrast with
  'regualar' library functions which are processed by the code generator in the
  same way as a call to a user created function).

Operations on constant values can be evaluated at compile time (parse time) by
the Eval unit.
Operations on variables, or a mix of variables and constants are processed by
the code generator, which uses the Primitives unit and data table. This lists
all available library routines (whether Fragments or Subroutines) along with
available data types and other meta data. The expression parser also ues the
Primitives unit to validate the available data types and ascertain result types
for operations.
*)

{
NextGen operators:
Operators table defines available operations:
  Operation name
  Symbol (?) or keyword
  Precedence
x  ?Left type(s)
x  ?Right type(s)
x  Swappable (mutable?)(numeric only?)
x  Match operators (numeric only?) - un-necessary: just browse primitives list to see
x    what is available and 'up-grade' parameters as necessary

Primitives table defines available primitives:
  Operation
  Procedure identifier (Proc, Fragment or Subroutine)
  Left type (base VarType)
  Right type (base VarType)
  Commutative (operator order can be swapped)
  Destination type (base VarType, or 'same as Left');
  Registers available (for Left, Right and Dest (or Dest same as P1))
  Corrupts (registers)
  Validation (y/n/either)
  Validation routine (if there's an optimised validation routine. I.e. one which
    makes use of flags set by the primitive
  Conversion validation (optimised routines to used for type conversion after the primitive)

At Parse time:
  Search list of available Primitives
  If *any* routine is available which can handle the given types (etc)
    accept a routine is available and move on
  else: see if any routine is available with upscaled parameters
    if so:
      take note of the minimum return type
      proceed with the expression now being of the available return type

At CodeGen time:
  Now is the time to select the optimium routine from those matching the criteria,
  upscaling parameters as needed

Examples:
Addition accepts parameters of the same type, swappable
  Routines are available for:
    Byte, Byte: Byte  - commutative
    Int8, Int8: Int8  - commutative
    Word, Word: Word (aka Pointer)  - commutative
    Integer, Integer: Integer  - commutative
    Real, Real: Real  - commutative
    Char, Char: String - not commutative
    String, Char: String - not commutative
    Char, String: String - not commutative
    String, String: String - not commutative
    Set, Set: Set - commutat
    ive
}

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

  TOperator = (
    opUnknown,    //Unassigned (error if it gets to codegen)
    opMove,       //Move data from one location to another (ie load+store)
    opStoreImm,   //Store an immediate value into a location
//    opAssignX,     //DEPRECATED
    opBranch,     //Unconditional branch
    opCondBranch, //Conditional branch
    opConstBranch,//Conditional branch with constant expression (immediate value)
    opPhi,        //Phi function
    opDataLoad,   //Load data into registers (for a function call)
    opFuncCall,   //Call function
    opFuncReturn, //Return from function

    //Maths
    opAdd, opSubtract, opMultiply, opRealDiv, opIntDivide, opMod,
    opNegate, //Unary minus
    //Note: Unary addition is skipped by parser
    //Logic
    opOR, opAND, opXOR,
    opComplement,    //Unary NOT
    //Comparisons
    opEqual, opNotEqual, opLess, opGreater, opLessEqual, opGreaterEqual,
    //Misc
    opIn, opSHR, opSHL,
    opAddr,  //@ (address of)
    //Typecasts
    opInteger, opInt8, opWord, opByte, opPointer, opBoolean, opChar,
    //Intrinsics
    //Maths
    opAbs, opDec, opInc, opOdd,
    //System
    opHi, opHigh, opInp, opLo, opLow, opOrd, opOut, opPeek, opPoke, opPred,
    opPtr, opSizeof, opSucc, opSwap, opRead, opReadln, opWrite, opWriteln,
    opWriteNewLine, opWriteChar, opWriteInteger, opWriteBoolean,
    //Strings and Chars
    opChr, opDowncase, opLength, opUpcase
    );

  POpData = ^TOpData;
  TOpData = record
    Name: String;         //Internal name of the operation
    Precedence: Integer;  //In expressions. Higher equal higher
    SignCombine: Boolean; //Affects how the primitives table is searched when
                          //either parameter is a numerical constant.
                          //(Only affects numerical operations)
                          //If SignCombine is True: the other (non-constant)
                          //parameter will be examined. A primitive will be chosen
                          //based on whether the non-constant parameter is signed
                          //or not and whether the constant parameter can be
                          //represented within that type, and if not the type(s)
                          //of one or both parameters will be expanded on the basis
                          //of the signedness of both parameters.
                          //If SignCombine is False the size and signedness of
                          //each parameter will be examined separately.
    FirstPrimIndex: Integer;  //Index in PrimListNG of the first entry for this
                          //operator

    IsNG: Boolean;        //True if this operator uses NG methodology

    //All items below are deprecated
    Symbol: String;           //Symbol or keyword
    OpGroup: TOpGroup;        //Operator gouping
    OpTypes: TOpTypeSet;      //Set of available primitive types
    Logical: Boolean;         //If true the operator is a boolean logical one (AND, OR, XOR, NOT)
                              //Used for special processing of parameters.
    LTypes: TTypeEnumSet;     //List of applicable types for the left operand
    RTypes: TTypeEnumSet;     //List of applicable types for the right operand
    Swappable: Boolean;       //Can operators be swapped?
    MatchOperands: Boolean;   //Extend numeric operators to match each other
    FuncFlags: TOpFuncFlagSet;//Flags for function/proecedure intrinsics
    ResultSame: Boolean;      //If true the result will be the same type as the operands
                              //(Left operand, if only one operand)
    ResultType: TTypeEnum;    //The output type. vtUnknown specifies same as input (left operand)
  end;

var Operations : array[low(TOperator)..high(TOperator)] of TOpData;


//Finds the first operator with the given symbol
function SymbolToOperator(const Symbol: String): TOperator;
//Finds the operator with the given Name
//function OpIdentifierToOpData(const Name: String): POpData;
function IdentToOperator(const Name: String): TOperator;

//Returns only functions
function IdentToIntrinsicFunc(const Name: String): TOperator;

//Returns procedures and functions (i.e any which can be used as statements)
function IdentToIntrinsicProc(const Name: String): TOperator;

procedure InitialiseOperators;

procedure LoadOperatorsFile(const Filename: String);
procedure LoadOperatorsFileNG(const Filename: String);

//Convert an operator index to a string explaining it's usage
function OpToUsage(Op: TOperator): String;

function StringToBoolean(S: String): Boolean;

implementation
uses Generics.Collections, Classes, SysUtils;

const OpStrings : array[low(TOperator)..high(TOperator)] of String = (
    'UNKNOWN','Move','StoreImm',
    'Branch','CondBranch','ConstBranch','Phi','DataLoad',
    'FuncCall','FuncReturn',
    'Add', 'Subtract', 'Multiply', 'RealDiv', 'Div', 'Mod','Negate',
    'OR', 'AND', 'XOR','Complement',
    'Equal', 'NotEqual', 'Less', 'Greater', 'LessEqual', 'GreaterEqual',
    'In', 'SHR', 'SHL',
    'Addr',
    'Integer', 'Int8', 'Word', 'Byte', 'Pointer', 'Boolean', 'Char',
    'Abs', 'Dec', 'Inc', 'Odd',
    'Hi', 'High', 'Inp', 'Lo', 'Low', 'Ord', 'Out', 'Peek', 'Poke', 'Pred',
    'Ptr', 'Sizeof', 'Succ', 'Swap', 'Read', 'Readln', 'Write', 'Writeln',
    'WriteNewLine', 'WriteChar', 'WriteInteger', 'WriteBoolean',
    'Chr', 'Downcase', 'Length', 'Upcase'
    );

const
  //Mapping between ASCII and symbolic operators
  SymbolOps: array of TOperator = [opAdd, opSubtract, opMultiply, opRealDiv,
    opEqual,opNotEqual,opLess,opGreater,opLessEqual,opGreaterEqual];
  SymbolStrings: array of String = ['+','-','*','/','=','<>','<','>','<=','>='];

procedure ClearOpList;
var Op: TOperator;
begin
  for Op := low(TOperator) to high(TOperator) do
  begin
    Operations[Op].Name := '';
    Operations[Op].Symbol := '';
  end;
end;

procedure InitialiseOperators;
begin
  ClearOpList;
end;

function SymbolToOperator(const Symbol: String): TOperator;
var I: Integer;
begin
  Assert(Length(SymbolOps) = Length(SymbolStrings));
  for I := 0 to length(SymbolStrings)-1 do
    if Symbol = SymbolStrings[I] then
      EXIT(SymbolOps[I]);

  Result := opUnknown;
end;

function IdentToOperator(const Name: String): TOperator;
begin
  for Result := low(TOperator) to high(TOperator) do
    if CompareText(Name, OpStrings[Result]) = 0 then
      EXIT;

  Result := opUnknown;
end;

function IdentToOpData(const Name: String): POpData;
var Op: TOperator;
begin
  for Op := low(TOperator) to high(TOperator) do
    if CompareText(Name, OpStrings[Op]) = 0 then
      EXIT(@Operations[Op]);

  Result := nil;
end;

function IdentToIntrinsicFunc(const Name: String): TOperator;
begin
  for Result := low(TOperator) to high(TOperator) do
    if Operations[Result].OpGroup in [ogFunc, ogProc, ogTypecast] then
      if CompareText(Operations[Result].Name, Name) = 0 then
        EXIT;

  Result := opUnknown;
end;

//Returns procedures and functions (i.e any which can be used as statements)
function IdentToIntrinsicProc(const Name: String): TOperator;
begin
  for Result := low(TOperator) to high(TOperator) do
    if Operations[Result].OpGroup in [ogFunc, ogProc] then
      if CompareText(Operations[Result].Name, Name) = 0 then
        EXIT;

  Result := opUnknown;
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

procedure LoadOperatorsFile(const Filename: String);
var Data: TStringList;
  Line: String;
  Fields: TArray<String>;
  Op: POpData;
  I: Integer;
begin
  Data := TStringList.Create;
  Data.LoadFromFile(Filename);

  for Line in Data do
    if (Length(Line) > 0) and (Line.Chars[0] <> ';') then
    begin
      Fields := Line.Split([',']);
      if Length(Fields) < 11 then
        raise Exception.Create('Operators line too short: ' + Line);
      for I:=0 to Length(Fields)-1 do
        Fields[I] := Fields[I].Trim;

      Op := IdentToOpData(Fields[fOpName]);
      if Op = nil then
        raise Exception.Create('Operator not found: ' + Fields[fOpName]);
      Op.IsNG := False;

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
end;

const
  fNGOpName = 1;
  fNGPrecedence = 2;
  fNGSignCombine = 3;

procedure LoadOperatorsFileNG(const Filename: String);
var Data: TStringList;
  Line: String;
  Fields: TArray<String>;
  Op: POpData;
  I: Integer;
begin
  Data := TStringList.Create;
  Data.LoadFromFile(Filename);

  for Line in Data do
    if (Length(Line) > 0) and (Line.Chars[0] <> ';') then
    begin
      if Line.StartsWith('END') then
        EXIT;

      Fields := Line.Split([',']);
      if Fields[fNGOpName] <> '' then
      begin
        if Length(Fields) < 11 then
          raise Exception.Create('Operators line too short: ' + Line);
        for I:=0 to Length(Fields)-1 do
          Fields[I] := Fields[I].Trim;

        Op := IdentToOpData(Fields[fNGOpName]);
        if Op = nil then
          raise Exception.Create('Operator not found: ' + Fields[fNGOpName]);
        Op.IsNG := True;

        Op.FirstPrimIndex := -1;
        Op.Name := Fields[fNGOpName];
        Op.Precedence := StrToInt(Fields[fNGPrecedence]);
        Op.SignCombine := StringToBoolean(Fields[fNGSignCombine]);
      end;
    end;
end;

function OpToUsage(Op: TOperator): String;
var OpData: POpData;
begin
  OpData := @Operations[Op];
  Result := OpData.Symbol + ' (stuff to do here)';
end;

end.

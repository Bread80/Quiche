unit Def.Operators;
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
}

interface

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

  //The listing below includes comments on parameter usage within the ILItem record
  //(see ILData unit)
  TOperator = (
    //Special/system operators
    //------------------------
    //Param Kinds for these are varied.
    opUnknown,    //Unassigned (error if it gets to codegen)
    opTypecast,   //A cast from one type to another. Usually programmer driven.
                  //Generally the data is unchanged and no validation is performed.
    opMove,       //Move data from one location to another (ie load+store)
                  //One parameter (Param1) and a Dest
    opBlockCopy,  //Copy a block of data from one location to another
                  //Param1: VarRef Source variable
                  //Param2: Count of bytes to copy
                  //Param3/Dest: VarRef Dest variable
    opParamCopyToStack,
                  //A specialised version of opBlockCopy for copying a function
                  //parameter's data into stack offset memory.
                  //After calculating the address the Dest data this address is also
                  //written back as Param1's value. Thus the Param1 variable us updated
                  //to the data storage location ready for the remainder of the function
                  //to use.
                  //This operation is used for pointered types in the Stack calling
                  //convention.
                  //Param1: VarSource of the Variable (which must be have StackRef
                  //addressing mode.
                  //  On entry to the function variable's value is source address
                  //  of the data to be copied. opParamCopyToStack updates the variables
                  //  value to be the address of the local data (as passed in as Dest)
                  //Param2: Count of bytes to copy
                  //Param3/Dest: VarRef of the variable (the address of the local data)
    opStoreImm,   //Store an immediate value into a location
                  //One parameter (Param1) which must be an pkImmediate and a Dest
    opBranch,     //Unconditional branch
                  //Param1 is a pkBranch (TODO: Move this to Dest for consistency)
    opBoolVarBranch,   //Conditional branch - use for a Branch where the branch depends
                  //on the value of a boolean variable. Ie IF BOOLVAR THEN ...
    opPtrLoad,    //Dereference a pointer (ie. load the data it points at)
    opPtrStore,   //Assign to a derefenced pointer
    opPhi,        //Phi function
                  //Currently Param1 and Param2 are pkPhiVarSource and Dest is pkPhiVarDest.
                  //Needs expanding to allow more sources for code where more than two paths
                  //can converge.
    opRegLoad,    //Load data from variables or constants into registers and/or
                  //from registers into variables.
                  //Used for function calls.
                  //One to three source params (Param1, Param2, Param3)
                  //All Params can be either a load or a store depending on the
                  //parameter Kind. Load MUST always come before Stores.
                  //If more than three moves are required then a DataMoveExtended
                  //items can be added.
                  //NOTE: Code gen can currently only handle two params.
    opRegLoadExtended,
                  //As opDataMove but indicates the data continues into the following
                  //ILItem. The following item must be either a opDataMove or opDataMoveExtended
    opRegStore,
    opRegStoreExtended,
    opFuncCall,   //Call function
    opFuncCallExtended, //As opDataMoveExtended for opFuncCall. Must come before opFuncCall
                  //As a binary operator: zero to two sources and zero or one Dest.
                  //Exact param usage depends on calling convention:
                  //ccStack: Params pushed on the stack, no source params. Dest used for result
                  //          Can be followed by opDataStores to save additional results
                  //          (not currently used by function dispatcher)
                  //ccRegister: source params are used to load registers. Dest is used for result.
                  //            May be supplemented by opDataLoad (before) and opDataStore (after)
                  //            to load and/or save additional registers
    opFuncReturn, //NOTUSED? Return from function
                  //TODO: This needs converting to a opDataStore

    //Binary operators
    //These take two input parameters (Param1, Param2) and an output (Dest)
    //Maths
    opAdd, opSubtract, opMultiply, opRealDiv, opIntDivide, opMod,
    //Logic
    opOR, opAND, opXOR,
    //Comparisons
    opEqual, opNotEqual, opLess, opGreater, opLessEqual, opGreaterEqual,
    //Misc
    opConcat, opIn, opSHR, opSHL,

    //Unary operators
    //These have one input/source parameter (Param1) and an output/dest/result value (Dest/Param3)
    opNegate, //Unary minus
    //Note: Unary addition is skipped by parser
    opComplement,    //Unary NOT
    opAddrOf,       //@ (address of), or address of data
    opAddrElement,  //Address of an element item within an array type. Param2 is the array index.
    opAddrOffset,   //Address of an offset from a pointered item. Used for records and for
      //accessing the meta data of vectors and lists. Param2 is the byte offset from the base.
      //Note that A[0] can be validly referenced by opAddrOf, opAddrElement or opAddrOffset

    //Intrinsics
    //These take one or two input parameters (Param1, Param2) and usually (but not always)
    //an output value (Dest)
    //Maths
    opAbs, opDec, opInc, opOdd,
    //System
    opHi, opHigh, opInp, opLo, opLow, opOrd, opOut, opPeek, opPoke, opPred,
    opPtr, opSizeof, opSucc, opSwap,
    //Keyboard and Screen (and other I/O)
{    opKeyPressed,}
    opRead, {opReadKey,} opReadln, opWrite, opWriteln,
    //Strings and Chars
    opCapacity, opChr, opDowncase, opLength, opSetLength, opUpcase
    );

const
  SystemOps = [opUnknown..opFuncReturn];
  ExtendedOps = [opRegLoadExtended, opRegStoreExtended, opFuncCallExtended];

  InfixOps = [opAdd..opSHL];
  UnaryOps = [opNegate..opAddrOffset];  //This is required for primitive search
  IntrinsicOps = [opAbs..opUpcase];

type
  POpData = ^TOpData;
  TOpData = record
    Name: String;         //Internal name of the operation
    Precedence: Integer;  //In expressions. Higher equal higher.
                          //Only meaningful for binary operators
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
  end;

var Operations : array[low(TOperator)..high(TOperator)] of TOpData;


//Finds the first operator with the given symbol
function SymbolToOperator(const Symbol: String): TOperator;

//Finds the operator with the given Name
function IdentToInfixOperator(const Name: String): TOperator;
function IdentToAnyOperator(const Name: String): TOperator;
function IdentToIntrinsicOperator(const Name: String): TOperator;

procedure InitialiseOperators;

procedure LoadOperatorsFile(const Filename: String);

//Convert an operator index to a string explaining it's usage
function OpToUsage(Op: TOperator): String;

function StringToBoolean(S: String): Boolean;


const OpStrings : array[low(TOperator)..high(TOperator)] of String = (
  //System operators
  'UNKNOWN','Typecast',
  'Move','BlockCopy','ParamCopyToStack','StoreImm',
  'Branch','BoolVarBranch',
  'PtrLoad','PtrStore',
  'Phi',
  'RegLoad','RegLoadEx',
  'RegStore','RegStoreEx',
  'FuncCall','FuncCallEx',
  'FuncReturn',

  //Binary operators
  'Add', 'Subtract', 'Multiply', 'RealDiv', 'Div', 'Mod',
  'OR', 'AND', 'XOR',
  'Equal', 'NotEqual', 'Less', 'Greater', 'LessEqual', 'GreaterEqual',
  'Concat', 'In', 'SHR', 'SHL',

  //Unary operators
  'Negate', 'Complement',
  'AddrOf', 'AddrElement', 'AddrOffset', {'Deref',}

  //Intrinsics
  'Abs', 'Dec', 'Inc', 'Odd',
  'Hi', 'High', 'Inp', 'Lo', 'Low', 'Ord', 'Out', 'Peek', 'Poke', 'Pred',
  'Ptr', 'Sizeof', 'Succ', 'Swap',

  {'KeyPressed',} 'Read', {'ReadKey',} 'Readln', 'Write', 'Writeln',

  'Capacity', 'Chr', 'Downcase', 'Length', 'SetLength', 'Upcase'
  );


implementation
uses Classes, SysUtils;

const
  //Mapping between ASCII and symbolic operators
  SymbolOps: TArray<TOperator> = [opAdd, opSubtract, opMultiply, opRealDiv,
    opEqual,opNotEqual,opLess,opGreater,opLessEqual,opGreaterEqual];
  SymbolStrings: TArray<String> = ['+','-','*','/','=','<>','<','>','<=','>='];

procedure ClearOpList;
var Op: TOperator;
begin
  for Op := low(TOperator) to high(TOperator) do
  begin
    Operations[Op].Name := '';
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

function IdentToInfixOperator(const Name: String): TOperator;
begin
  for Result in InfixOps do
    if CompareText(Name, OpStrings[Result]) = 0 then
      EXIT;

  Result := opUnknown;
end;

function IdentToAnyOperator(const Name: String): TOperator;
begin
  for Result := low(TOperator) to high(TOperator) do
    if CompareText(Name, OpStrings[Result]) = 0 then
      EXIT;

  Result := opUnknown;
end;

function IdentToIntrinsicOperator(const Name: String): TOperator;
begin
  for Result in IntrinsicOps do
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

//Returns True if the first char of S is Y or y.
//Returns false if S is empty or first char is no Y or y
function StringToBoolean(S: String): Boolean;
begin
  Result := (Length(S) > 0) and (CharInSet(S.Chars[0], ['Y','y']));
end;

const
  fNGOpName = 1;
  fNGPrecedence = 2;
  fNGSignCombine = 3;

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
          raise Exception.Create('Operator not found: ' + Fields[fNGOpName] +
            #13#10'On reading the operators file and operator was found which is not definied within Quiche.');

        Op.FirstPrimIndex := -1;
        Op.Name := Fields[fNGOpName];
        Op.Precedence := StrToInt(Fields[fNGPrecedence]);
        Op.SignCombine := StringToBoolean(Fields[fNGSignCombine]);
      end;
    end;
end;

function OpToUsage(Op: TOperator): String;
//var OpData: POpData;
begin
  //TODO
//  OpData := @Operations[Op];
//  Result := OpData.Symbol + ' (stuff to do here)';
end;

end.

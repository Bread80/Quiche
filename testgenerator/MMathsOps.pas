unit MMathsOps;

interface

procedure GenTests;

implementation
uses SysUtils, Classes;

var TestNo: Integer;
{Strategy:
for each operator:
  generate tests for every combination of types for first and second
    parameters, and result type
    For each type combo, generate tests for extremes of range for
      each type (-32768, -32767, -129, -128, -127, -1, 0, +1, 127, 128, 129, 32767, 32768, 65534, 65535);
      and a number of random values
      Calculate result values to expect or, if the result is invalid, error code
      Take into account type priorities in calculations
}

type TOperator = (
  opAdd, opSubtract, opMultiply, opFloatDivide, opIntDivide, opMod,   //Maths
  opEqual, opNotEqual, opLess, opGreater, opLessEqual, opGreaterEqual,//Comparison
  opAND, opOR, opXOR, //Logical
  opSHL, opSHR);  //Shifts

const OpSymbols : array[low(TOperator)..high(TOperator)] of String =
  ('+','-','*','/','div','mod',
  '=','<>','<','>','<=','>=',
  'and','or','xor',
  'shl','shr');
  OpStrings: array[low(TOperator)..high(TOperator)] of String =
  ('add','subtract','multiply','float-divide','int-divide','mod',
  'equal','not-equal','less','greater','less-equal','greater-equal',
  'and','or','xor',
  'shl','shr');

type TType = (tInt8, tInt16, tByte, tWord, tPointer, tBoolean, tChar);
  TTypeSet = set of TType;
const
  AllNumTypes = [tInt8, tInt16, tByte, tWord, tPointer];
  ResultNumTypes = [tInt8, tInt16, tByte, tWord];
  SysNumTypes = [tByte, tWord];
  _8BitTypes = [tInt8, tByte, tChar];

const TypeStrings : array[low(TType)..high(TType)] of String =
  ('Int8','Integer','Byte','Word','Pointer','Boolean','Char');

//List of operations which have Int8 support (others have to convert to Int16)
const Int8CapableOps = [opAdd, opSubtract, opSHL, opSHR, opEqual, opNotEqual];

const
  BFalse = 0;
  BTrue = $ff;

//Minumum and maximum values for each item in NumTypes
const TypeRanges: array[low(TType)..high(TType)] of array[0..1] of Integer =
  ((-128, 127),     //Int8
  (-32768, 32767),  //Int16
  (0, 255),         //Byte
  (0, 65535),       //Word
  (0, 65535),       //Pointer
  (BFalse, BTrue),  //Booleans
  (0, 255));        //Char


//Eval meta data
type TEvalState = (
  esValid,              //We're good :)
  esNotImplementedYet,  //This feature is not yet implemented, so you can't test it!
  esDivideByZero,       //Operation should give a divide by zero error
  esInvalidParams);     //This operation can't be called with these parameter values
                        //This should give a compile error in constant expressions,
                        //and an overflow error at run time

//Evaluate the operator
//Op: The operation
//P1V, P2V: The values of the left and right operands
//EvalState returns the outcome of the operation
//Result is the Eval'ed result
function EvalOp(Op: TOperator;P1V, P2V: Integer;P1T, P2T: TType;IsConstant: Boolean;out EvalState: TEvalState): Integer;
const Bool: array[false..true] of Integer = (BFalse, BTrue);
begin
  EvalState := esValid;
  case Op of
  opAdd: Result := P1V + P2V;
  opSubtract: Result := P1V - P2V;
  opMultiply: Result := P1V * P2V;
  opFloatDivide: //Don't have reals yet
    EvalState := esNotImplementedYet;
  opIntDivide:
    if P2V = 0 then
      EvalState := esDivideByZero
    else
      Result := P1V div P2V;
  opMod:
    if P2V = 0 then
      EvalState := esDivideByZero
    else
      Result := P1V mod P2V;
  opEqual: Result := Bool[P1V = P2V];
  opNotEqual: Result := Bool[P1V <> P2V];
  opLess: Result := Bool[P1V < P2V];
  opGreater: Result := Bool[P1V > P2V];
  opLessEqual: Result := Bool[P1V <= P2V];
  opGreaterEqual: Result := Bool[P1V >= P2V];
  opAND: Result := P1V and P2V;
  opOR: Result := P1V or P2V;
  opXOR: Result := P1V xor P2V;
  opSHL:
  begin
    if (P2V < 0) or (P2V > 32) then
      Result := 0
    else
      Result := P1V shl P2V;
    if not IsConstant then
    begin
      if P1T in _8BitTypes  then
        Result := Result and $ff
      else
        Result := Result and $ffff;
      if P1T = tInt8 then
        if Result and $80 <> 0 then
          Result := Result or -256;
      if P1T = tInt16 then
        if Result and $8000 <> 0 then
          Result := Result or -65536
    end;
  end;
  opSHR:
  begin
    if (P2V < 0) or (P2V > 32) then
      Result := 0
    else
      Result := P1V shr P2V;
    if (P2V <> 0) and not IsConstant then
    begin //We always do a logical right shift, so null the high bit
      if P1T in _8BitTypes then
        Result := Result and $7f
      else
        Result := Result and $7fff;
{      if P1T = tInt8 then
        if Result and $80 <> 0 then
          Result := Result or -256;
      if P1T = tInt16 then
        if Result and $8000 <> 0 then
          Result := Result or -65536
}    end;
  end;
  else
    raise Exception.Create('Unknown operator in Eval');
  end;
end;

//Is the Value valid for the Type?
function IsInRange(T: TType; Value: Integer): Boolean;
begin
  if T = tBoolean then
    Result := (Value = BFalse) or (Value = BTrue)
  else
    Result := (Value >= TypeRanges[T,0]) and (Value <= TypeRanges[T,1]);
end;

//Returns the Type of the operation to be performed when called with parameters
//of the given Types
function GetOpType(Op: TOperator;P1T, P2T: TType): TType;
begin
  if P1T = tBoolean then
    if P2T = tBoolean then
      EXIT(tBoolean)
    else
      raise Exception.Create('Invalid type pairing: Boolean');
  if P1T = tChar then
    if P2T = tChar then
      EXIT(tChar)
    else
      raise Exception.Create('Invalid type pairing: Char');

  if Op in [opSHL, opSHR] then
    EXIT(P1T);

  if (P1T = tPointer) or (P2T = tPointer) then
    EXIT(tPointer);
  if (P1T = tInt16) or (P2T = tInt16) then
    EXIT(tInt16);
  if (P1T = tInt8) or (P2T = tInt8) then
    if (P1T = tInt8) and (P2T = tInt8) and (Op in Int8CapableOps) then
      EXIT(tInt8)
    else
      EXIT(tInt16);
  if (P1T = tWord) or (P2T = tWord) then
    EXIT(tWord);
  Result := tByte;
end;

type TLoc = (ltVar, ltLeftImm, ltRightImm, ltBothImm);
  TLocSet = set of TLoc;
const LocStrings: array[low(TLoc)..high(TLoc)] of String = (
  'Var',              //Both parameters are in variables
  'Left immediate',   //Left hand parameter is immediate
  'Right immediate', //Right hand parameter is immediate
  'Both immediate');  //Both parameters are immediate values (tests the compile time eval!)

//Loc(ation) is where the parameters are being sourced from
//Ths function returns a set of the locations where parameters may be found for
//the given operation and operation type.
//Note: All operations can handle parameters loaded from variables. This
//routine is intended to test operations which can generate code which can generate
//more effictient code by operating parameters directly in inline data, from
//IX-offsetted variables etc. E.g. operations which can use opcodes such as
//ADD A,n
//ADD A,(IX+d)
function GetParamLocList(Op: TOperator;OpType: TType): TLocSet;
begin
  Result := [ltVar, ltBothImm];
  case Op of
    opAdd:
      if OpType in [tInt8, tByte] then
        Result := Result + [ltLeftImm, ltRightImm];
    opSubtract:
      if OpType in [tInt8, tByte] then
        Result := Result + [ltRightImm];
   opEqual, opNotEqual, opLess, opGreater, opLessEqual, opGreaterEqual,
   opAND, opOR, opXOR:
     Result := Result + [ltLeftImm, ltRightImm];
  end;
end;

//Returns the Value when converted to the given Type
function ValueToString(Value: Integer;T: TType): String;
begin
  if T in AllNumTypes then
    Exit(Value.ToString);

  case T of
    tBoolean:
      if Value = BFalse then
        EXIT('False')
      else if Value = BTrue then
        EXIT('True')
      else
        raise Exception.Create('Invalid boolean value in ValueToString' + Value.ToString);
    tChar:
      case Value of
        0..31,127..255: EXIT('#' + Value.ToString);
        32..126: EXIT('''' + chr(Value) + '''');
      else
        raise Exception.Create('Invalid char value in ValueToString' + Value.ToString);
      end;
  else
    raise Exception.Create('Inknown type in ValueToString');
  end;
end;

//Mask a value to fit within the type given type, as it would be if the value where
//not being error checked
function MaskToType(Value: Integer;T1: TType): Integer;
begin
  case T1 of
    tInt8:
    begin
      Result := Value and $ff;
      if Result >= $80 then
        Result := Result or (-1 xor $ff);
    end;
    tInt16:
    begin
      Result := Value and $ffff;
      if Result >= $8000 then
        Result := Result or (-1 xor $ffff);
    end;
    tByte, tChar: Result := Value and $ff;
    tWord, tPointer: Result := Value and $ffff;
    tBoolean: Result := Value;
  else
    raise Exception.Create('Unknown type in MaskToType');
  end;
end;

//Does the operator need separate checking for Overflow and non-Overflow situations
function DoesOpNeedErrorCheck(Op: TOperator): Boolean;
begin
  Result := Op in [opAdd, opSubtract];
end;

//Generate tests for a given operator, parameter types and parameter values
//Test code is appended to the StringList (SL)
//Multiple tests will be generated where the parameters can be loaded from multiple Locations
procedure GenOpTests(SL: TStringList;Op: TOperator; P1T, P2T, RT: TType; P1V, P2V: Integer;
  NeedErrorCheck: Boolean);
var Expected: Integer;
  OpType: TType;
  Loc: TLoc;
  EvalState: TEvalState;
  Test: String;
  ErrorCheck: Boolean;
  Overflow: String;
  VarValue: String;
begin
  OpType := GetOpType(Op, P1T, P2T);
  //Generate declaration and expression code

  for ErrorCheck := not NeedErrorCheck to True do
  begin

    if ErrorCheck then
      Overflow := '$overflow on'
    else
      Overflow := '$overflow off';
    SL.Add(Overflow);

    for Loc in GetParamLocList(Op, OpType) do
    begin
      inc(TestNo);

      Expected := EvalOp(Op, P1V, P2V, P1T, P2T, Loc = ltBothImm, EvalState);
      if not ErrorCheck and (Loc <> ltBothImm) then
        Expected := MaskToType(MaskToType(Expected, OpType), RT);

      Test := 'code auto:' + TestNo.ToString + ' ' + OpStrings[Op] + '[';
      if Loc = ltBothImm then
        Test := Test + 'compile-time'
      else
        Test := Test + TypeStrings[OpType];
      Test := Test + '] ' +
        P1V.ToString + '[' + TypeStrings[P1T] + '], ' +
        P2V.ToString + '[' + TypeStrings[P2T] + '] -> ' +
        '[' + TypeStrings[RT] + '] ' +
        LocStrings[Loc] + ' ' + Overflow + #13#10;

      if Loc in [ltVar, ltRightImm] then
        Test := Test + '  var  a: ' + TypeStrings[P1T] + ' = ' + ValueToString(P1V, P1T) + ';'#13#10;
      if Loc in [ltVar, ltLeftImm] then
        Test := Test + '  var  b: ' + TypeStrings[P2T] + ' = ' + ValueToString(P2V, P1T) + ';'#13#10;
      Test := Test + '  var  r: ' + TypeStrings[RT] + ';'#13#10;

      case Loc of
        ltVar: Test := Test + '  r := a ' + OpSymbols[Op] + ' b;'#13#10;
        ltLeftImm: Test := Test + '  r := ' + ValueToString(P1V, P1T) + ' ' + OpSymbols[Op] + ' b;'#13#10;
        ltRightImm: Test := Test + '  r := a ' + OpSymbols[Op] + ' ' + ValueToString(P2V, P2T) + ';'#13#10;
        ltBothImm: Test := Test + '  r := ' + ValueToString(P1V, P1T) + ' ' + OpSymbols[Op] + ' ' + VAlueToString(P2V, P2T) + ';'#13#10;
      end;

      Test := Test + 'endcode'#13#10;


      case EvalState of
        esValid:
          //If the operator does not need testing with OverflowOff, and Overflow is Off then
          //all we need do is test that the code compiles and runs without error...
          if not ErrorCheck and not DoesOpNeedErrorCheck(Op) then
          begin
            if (Loc = ltBothImm) and not IsInRange(RT, Expected) then
              //...except for compile time overflows, which will fail to compile
              Test := Test + 'compile error'#13#10
            else
              Test := Test + 'runtime noerror'#13#10;
          end
          else
          begin
            VarValue := 'varvalue r ' + ValueToString(Expected, RT) + #13#10;
            if RT in [tBoolean] then
              Test := Test + VarValue
            else
            //Note: Compile time expressions do not have an OpType and are not Range checked against OpType
            if (IsInRange(OpType, Expected) or (Loc = ltBothImm)) and IsInRange(RT, Expected) then
              //Generate tests for expected result
              Test := Test + VarValue
            else if Loc = ltBothImm then
              Test := Test + 'compile error'#13#10
            else if ErrorCheck then
              Test := Test + 'runtime overflow'#13#10
            else
              Test := Test + VarValue;
          end;
        esNotImplementedYet: Test := '';
        esDivideByZero:
          //Generate test for divide by zero
          if Loc = ltBothImm then
            Test := Test + 'compile error'#13#10
          else
            Test := Test + 'runtime dividebyzero'#13#10;
        esInvalidParams:
          //Generate test for compile fail
          Test := Test + 'compile error'#13#10;
      else
        raise Exception.Create('Invalid EvalState');
      end;
      if Test <> '' then
        SL.Add(Test);
    end;
  end;
end;

procedure GenIfTests(SL: TStringList;Op: TOperator; P1T, P2T: TType; P1V, P2V: Integer);
type TIfTestType = (itIfThen, itIfThenElse);
var
  TestType: TIfTestType;
  Expected: Integer; //as Boolean
  OpType: TType;
  Loc: TLoc;
  EvalState: TEvalState;
  Test: String;
begin
  OpType := GetOpType(Op, P1T, P2T);
  //Generate declaration and expression code

  for TestType := low(TIfTestType) to high(TIfTestType) do
  begin
    for Loc in GetParamLocList(Op, OpType) do
    begin
      inc(TestNo);

      Expected := EvalOp(Op, P1V, P2V, P1T, P2T, Loc = ltBothImm, EvalState);

      Test := 'code auto:' + TestNo.ToString + ' if ' + OpStrings[Op] + '[';
      if Loc = ltBothImm then
        Test := Test + 'compile-time'
      else
        Test := Test + TypeStrings[OpType];
      Test := Test + '] ' +
        P1V.ToString + '[' + TypeStrings[P1T] + '], ' +
        P2V.ToString + '[' + TypeStrings[P2T] + '] ' +
        LocStrings[Loc] + #13#10;

      if Loc in [ltVar, ltRightImm] then
        Test := Test + '  var  a: ' + TypeStrings[P1T] + ' = ' + ValueToString(P1V, P1T) + ';'#13#10;
      if Loc in [ltVar, ltLeftImm] then
        Test := Test + '  var  b: ' + TypeStrings[P2T] + ' = ' + ValueToString(P2V, P1T) + ';'#13#10;
      Test := Test + '  var  r: Byte = 0;'#13#10;

      case Loc of
        ltVar: Test := Test + '  if a ' + OpSymbols[Op] + ' b then '#13#10;
        ltLeftImm: Test := Test + '  if ' + ValueToString(P1V, P1T) + ' ' + OpSymbols[Op] + ' b then'#13#10;
        ltRightImm: Test := Test + '  if a ' + OpSymbols[Op] + ' ' + ValueToString(P2V, P2T) + ' then'#13#10;
        ltBothImm: Test := Test + '  if ' + ValueToString(P1V, P1T) + ' ' + OpSymbols[Op] + ' ' + VAlueToString(P2V, P2T) + ' then'#13#10;
      end;

      Test := Test + '    r := 1';
      if TestType = itIfThen then
        Test := Test + ';'#13#10
      else  //ifIfThenElse
        Test := Test + #13#10'  else'#13#10'    r := 2;'#13#10;

      Test := Test + 'endcode'#13#10;

      if TestType = itIfThen then
        if Expected = BFalse then  //False - Then skipped
          Test := Test + 'varvalue r 0'#13#10
        else  //True - Then taken
          Test := Test + 'varvalue r 1'#13#10
      else //itIfThenElse
        if Expected = BFalse then  //False - Else taken
          Test := Test + 'varvalue r 2'#13#10
        else  //True - Then taken
          Test := Test + 'varvalue r 1'#13#10;

      if Test <> '' then
        SL.Add(Test);
    end;
  end;
end;

type TIntArray = array of Integer;
const
  TestValsInt8: TIntArray = [-128, -127, -1, 0, 1, 127];
  TestValsInt16: TIntArray = [-32768, -32767, -129, -128, -1, 0, 1, 127, 128, 32767];
  TestValsByte: TIntArray = [0, 1, 127, 128, 255];
  TestValsWord: TIntArray = [0, 255, 32767, 32768, 65535];
  TestValuesBool: TIntArray = [BFalse, BTrue];
  TestValuesChar: TIntArray = [0, 31, 32, 255];
  TestValuesShift: TIntArray = [0,1,4,8,15,16,255];

//Returns a suitable set of test values for the operator and with the given operato type
//LeftParam is True if the parameter is the left one, false if the right one.
//This can be used to generate different test data for operators with asymettrical parameters
function GetTestValues(Op: TOperator; OpType: TType;LeftParam: Boolean): TIntArray;
begin
  if (Op in [opSHL, opSHR]) and not LeftParam then
    Result := TestValuesShift;
  case OpType of
    tInt8: Result := TestValsInt8;
    tInt16: Result := TestValsInt16;
    tByte: Result := TestValsByte;
    tWord, tPointer: Result := TestValsWord;
    tBoolean: Result := TestValuesBool;
    tChar: Result := TestValuesChar;
  else
    raise Exception.Create('Unknown OpType in GetTestValues');
  end;
end;

//Generate a set of tests for the given operator
procedure GenOperatorInputs(SL: TStringList; Op: TOperator; LeftTypes, RightTypes: TTypeSet);
var P1T, P2T, RT: TType;
  P1V, P2V: Integer;        //Parameter values
begin
  //Iterate each type for Parameter 1...
  for P1T in LeftTypes do
    //Parameter 2...
    for P2T in RightTypes do
    begin
      //and Result
      RT := GetOpType(Op, P1T, P2T);
//      for RT in ResultTypes do
        //Iterate all valid test values
        //for first parameter...
        for P1V in GetTestValues(Op, P1T, True) do
          if IsInRange(P1T, P1V) then
            //and second
            for P2V in GetTestValues(Op, P2T, False) do
              if IsInRange(P2T, P2V) then
                GenOpTests(SL, Op, P1T, P2T, RT, P1V, P2V, DoesOpNeedErrorCheck(Op));
    end;
end;

//Generate a set of tests for the given operator
procedure GenOperatorOutputs(SL: TStringList; Op: TOperator; LeftTypes, ResultTypes: TTypeSet);
var P1T, P2T, RT: TType;
  P1V, P2V: Integer;        //Parameter values
begin
  //Iterate each type for Parameter 1...
  for P1T in LeftTypes do
  begin
    //Parameter 2...
    P2T := P1T;
//    for P2T in RightTypes do
      //and Result
      for RT in ResultTypes do
        //Iterate all valid test values
        //for first parameter...
        for P1V in GetTestValues(Op, P1T, True) do
          if IsInRange(P1T, P1V) then
            //and second
            for P2V in GetTestValues(Op, P2T, False) do
              if IsInRange(P2T, P2V) then
                GenOpTests(SL, Op, P1T, P2T, RT, P1V, P2V, P1T <> RT);
  end;
end;

//Generate a set of tests for the given operator
procedure GenOperator(SL: TStringList; Op: TOperator; LeftTypes, RightTypes, ResultTypes: TTypeSet);
var P1T, P2T, RT: TType;
  P1V, P2V: Integer;        //Parameter values
begin
  //Iterate each type for Parameter 1...
  for P1T in LeftTypes do
  begin
    //Parameter 2...
    for P2T in RightTypes do
      //and Result
      for RT in ResultTypes do
        //Iterate all valid test values
        //for first parameter...
        for P1V in GetTestValues(Op, P1T, True) do
          if IsInRange(P1T, P1V) then
            //and second
            for P2V in GetTestValues(Op, P2T, False) do
              if IsInRange(P2T, P2V) then
                GenOpTests(SL, Op, P1T, P2T, RT, P1V, P2V, DoesOpNeedErrorCheck(Op));
  end;
end;

procedure GenIfOperator(SL: TStringList; Op: TOperator; LeftTypes, RightTypes: TTypeSet);
var P1T, P2T, RT: TType;
  P1V, P2V: Integer;        //Parameter values
begin
  //Iterate each type for Parameter 1...
  for P1T in LeftTypes do
  begin
    //Parameter 2...
    for P2T in RightTypes do
        //Iterate all valid test values
        //for first parameter...
        for P1V in GetTestValues(Op, P1T, True) do
          if IsInRange(P1T, P1V) then
            //and second
            for P2V in GetTestValues(Op, P2T, False) do
              if IsInRange(P2T, P2V) then
                GenIfTests(SL, Op, P1T, P2T, P1V, P2V);
  end;
end;

procedure WriteFileHeader(SL: TStringList);
begin
  SL.Add(';PROCEDURALLY GENERATED FILE');
  SL.Add('');
  SL.Add(';Any modifications will be overwritten!');
  SL.Add(';Generated by the QuicheTestGen utility');
  SL.Add('');
end;

procedure GenTests;
var Op: TOperator;
  SL: TStringList;
begin
  SL := TStringList.Create;
  for Op in [low(TOperator)..high(TOperator)] do
  begin
    SL.Clear;
    TestNo := 1;

    WriteFileHeader(SL);

    case Op of
      opAdd, opSubtract, opMultiply, opFloatDivide, opIntDivide, opMod:
      begin
        GenOperatorInputs(SL, Op, AllNumTypes, AllNumTypes);
        GenOperatorOutputs(SL, Op, AllNumTypes, ResultNumTypes);
      end;
      opEqual, opNotEqual, opLess, opGreater, opLessEqual, opGreaterEqual://Comparison
      begin
        GenOperator(SL, Op, AllNumTypes, AllNumTypes, [tBoolean]);
        GenOperator(SL, Op, [tBoolean], [tBoolean], [tBoolean]);
        GenOperator(SL, Op, [tChar], [tChar], [tBoolean]);
      end;
      opAND, opOR, opXOR: //Logical
      begin
        GenOperatorInputs(SL, Op, SysNumTypes, SysNumTypes);
        GenOperatorOutputs(SL, Op, SysNumTypes, SysNumTypes);
        GenOperator(SL, Op, [tBoolean], [tBoolean], [tBoolean]);
      end;
      opSHL, opSHR:
      begin
        GenOperator(SL, Op, AllNumTypes, AllNumTypes, AllNumTypes);
      end;
      else
      raise Exception.Create('Unknown operator in GetTests');
    end;

    SL.SaveToFile('AutoOp' + OpStrings[Op] + '.tst');
  end;

  //Tests for If
  for Op in [low(TOperator)..high(TOperator)] do
  begin
    SL.Clear;
    TestNo := 1;

    case Op of
      opAdd, opSubtract, opMultiply, opFloatDivide, opIntDivide, opMod: ;
      opEqual, opNotEqual, opLess, opGreater, opLessEqual, opGreaterEqual://Comparison
      begin
        WriteFileHeader(SL);

        GenIfOperator(SL, Op, AllNumTypes, AllNumTypes);
        GenIfOperator(SL, Op, [tBoolean], [tBoolean]);
        GenIfOperator(SL, Op, [tChar], [tChar]);

        SL.SaveToFile('AutoOpIf' + OpStrings[Op] + '.tst');
      end;
      opAND, opOR, opXOR: ;
      opSHL, opSHR: ;
      else
        raise Exception.Create('Unknown operator in GetTests');
    end;

  end;

  SL.Free;
end;

end.

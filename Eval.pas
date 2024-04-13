unit Eval;

interface
uses SysUtils, ILData, ParseErrors, QTypes, Operators;

//Evaulate an operator with two parameters
function EvalBi(Op: TOperator;Param1, Param2: PILParam;
  out Value: Integer;out ResultType: TVarType): TQuicheError;

//Evaluate and operator woth a single parameter
function EvalUnary(Op: TOperator; Param: PILParam;
  out Value: Integer;out ResultType: TVarType): TQuicheError;

//Evaluate and intrinsic with a single parameter
function EvalIntrinsicUnary(Op: TOperator;const Param: TILParam;
  out Value: Integer;out ResultType: TVarType): TQuicheError;

//Evaulate an instrinsic with two parameters
function EvalIntrinsicBi(Op: TOperator;const Param1, Param2: TILParam;
  out Value: Integer;out ResultType: TVarType): TQuicheError;

implementation
uses Globals, System.Character;

function ValueToResultType(Value: Integer;out ResultType: TVarType): TQuicheError;
begin
  if Value < -32768 then
    EXIT(Err(qeConstantExpressionOverflow))
  else if Value < -128 then
    ResultType := vtInteger
  else if Value < 0 then
    ResultType := vtInt8
  else if Value < 256 then
    ResultType := vtByte
  else if Value <= 65535 then
    ResultType := vtWord
  else
    EXIT(Err(qeConstantExpressionOverflow));

  Result := qeNone;
end;


//Eval binary operator when both params are integers
function EvalBiInteger(Op: TOperator;Param1, Param2: PILParam;
  out Value: Integer;out ResultType: TVarType): TQuicheError;
var P1: Integer;  //First parameter value
  P2: Integer;    //Second parameter value
begin
  ResultType := vtUnknown;
  P1 := Param1.ImmToInteger;
  P2 := Param2.ImmToInteger;

  case Op of
    opAdd:            Value := P1 + P2;
    opSubtract:       Value := P1 - P2;
    opMultiply:  Value := P1 * P2;
    opIntDivide:
      if P2 = 0 then
        EXIT(Err(qeDivByZero))
      else
        Value := P1 div P2;
    opMod:
    if P2 = 0 then
      EXIT(Err(qeDivByZero))
    else
      Value := P1 mod P2;
    opEqual:
    begin
      Value := BooleanToBinary[P1 = P2];
      ResultType := vtBoolean;
    end;
    opNotEqual:
    begin
      Value := BooleanToBinary[P1 <> P2];
      ResultType := vtBoolean;
    end;
    opLess:
    begin
      Value := BooleanToBinary[P1 < P2];
      ResultType := vtBoolean;
    end;
    opGreater:
    begin
      Value := BooleanToBinary[P1 > P2];
      ResultType := vtBoolean;
    end;
    opLessEqual:
    begin
      Value := BooleanToBinary[P1 <= P2];
      ResultType := vtBoolean;
    end;
    opGreaterEqual:
    begin
      Value := BooleanToBinary[P1 >= P2];
      ResultType := vtBoolean;
    end;
    opAND: Value := P1 and P2;
    opOR:  Value := P1 or P2;
    opXOR: Value := P1 xor P2;
    opSHL:
      if (P2 < 0) or (P2 > 32) then
        Value := 0
      else
        Value := P1 shl P2;
    opSHR:
      if (P2 < 0) or (P2 > 32) then
        Value := 0
      else
        Value := P1 shr P2;
  else
    raise Exception.Create('Unknown operation in Evaluate');
  end;

  if ResultType = vtUnknown then
    Result := ValueToResultType(Value, ResultType)
  else
    Result := qeNone;
end;

//Eval binary operator when both params are booleans
function EvalBiBoolean(Op: TOperator;Param1, Param2: PILParam;
  out Value: Integer;out ResultType: TVarType): TQuicheError;
var P1: Integer;  //First parameter value
  P2: Integer;    //Second parameter value
begin
  ResultType := vtUnknown;
  P1 := Param1.ImmToInteger;
  P2 := Param2.ImmToInteger;

  case Op of
    opEqual:        Value := BooleanToBinary[P1 = P2];
    opNotEqual:     Value := BooleanToBinary[P1 <> P2];

    //Note: we reverse the tests for booleans when compared as integers
    opLess:         Value := BooleanToBinary[P2 < P1];
    opGreater:      Value := BooleanToBinary[P2 > P1];
    opLessEqual:    Value := BooleanToBinary[P2 <= P1];
    opGreaterEqual: Value := BooleanToBinary[P2 >= P1];
    opAND:          Value := P1 and P2;
    opOR:           Value := P1 or P2;
    opXOR:          Value := P1 xor P2;
  else
    raise Exception.Create('Unknown operation in Evaluate');
  end;

  if ResultType = vtUnknown then
    ResultType := vtBoolean;
  Result := qeNone;
end;

//Eval binary operator when both params are chars
function EvalBiChar(Op: TOperator;Param1, Param2: PILParam;
  out Value: Integer;out ResultType: TVarType): TQuicheError;
var P1: Integer;  //First parameter value
  P2: Integer;    //Second parameter value
begin
  Result := qeNone;
  ResultType := vtUnknown;
  P1 := Param1.ImmToInteger;
  P2 := Param2.ImmToInteger;

  case Op of
    opEqual:
     begin
      Value := BooleanToBinary[P1 = P2];
      ResultType := vtBoolean;
    end;
    opNotEqual:
    begin
      Value := BooleanToBinary[P1 <> P2];
      ResultType := vtBoolean;
    end;
    opLess:
    begin
      Value := BooleanToBinary[P1 < P2];
      ResultType := vtBoolean;
    end;
    opGreater:
    begin
      Value := BooleanToBinary[P1 > P2];
      ResultType := vtBoolean;
    end;
    opLessEqual:
    begin
      Value := BooleanToBinary[P1 <= P2];
      ResultType := vtBoolean;
    end;
    opGreaterEqual:
    begin
      Value := BooleanToBinary[P1 >= P2];
      ResultType := vtBoolean;
    end;
  else
    raise Exception.Create('Unknown operation in Evaluate');
  end;
end;

//If both operand are immediate values, and we have a suitable routine available,
//Evaluate LeftSlug := LeftSlug.Operand <LeftSlug.Operation> RightSlug.Operand
//Returns True if the operation was evaluated.
//If so, RightSlug is now spare
function EvalBi(Op: TOperator;Param1, Param2: PILParam;
  out Value: Integer;out ResultType: TVarType): TQuicheError;
begin
  Result := qeNone;
  if IsNumericType(Param1.ImmType) and IsNumericType(Param2.ImmType) then
    Result := EvalBiInteger(Op, Param1, Param2, Value, ResultType)
  else if (Param1.ImmType = vtBoolean) and (Param2.ImmType = vtBoolean) then
    Result := EvalBiBoolean(Op, Param1, Param2, Value, ResultType)
  else if (Param1.ImmType = vtChar) and (Param2.ImmType = vtChar) then
    Result := EvalBiChar(Op, Param1, Param2, Value, ResultType)
  else
    EXIT(ErrOpUsage('Incompatible types ' +
      VarTypeToName(Param1.ImmType) + ' and ' +
      VarTypeToName(Param2.ImmType), Op));
end;

function LogicValueToType(Value: Integer;VarType: TVarType): Integer;
begin
  case VarType of
    vtByte: Result := Value and $ff;
    vtWord, vtPointer: Result := Value and $ffff;
    vtInt8:
      if Value >= $80 then
        Result := Value or (-1 xor $ff)
      else
        Result := Value;
    vtInteger:
      if Value >= iIntegerMin then
        Result := Value or (-1 xor iCPUWordMask)
      else
        Result := Value;
  else
    Assert(False, 'Invalid type for logic result');
  end;
end;

function EvalUnary(Op: TOperator; Param: PILParam; out Value: Integer;out ResultType: TVarType): TQuicheError;
var P: Integer;  //Parameter value
  PType: TVarType;
begin
  ResultType := vtUnknown;

  PType := Param.ImmType;

  if IsNumericType(PType) and IsNumericType(PType) then
  begin
    P := Param.ImmToInteger;

    case Op of
      opComplement: Value := LogicValueToType(not P, PType);
      opNegate: Value :=  - P
    else
      raise Exception.Create('Unknown operation in Evaluate');
    end;

    if ResultType = vtUnknown then
      Result := ValueToResultType(Value, ResultType)
    else
      Result := qeNone;
  end
  else if PType = vtBoolean then
  begin
    P := Param.ImmToInteger;

    if Op = opComplement then
    begin
      Value := P xor valueTrue;
      ResultType := vtBoolean;
      Result := qeNone;
    end
    else
      raise Exception.Create('Unknown operation in Evaluate');
  end
  else
    Result := qeNone;
end;

//Evaluate and intrinsic with a single parameter
function EvalIntrinsicUnary(Op: TOperator;const Param: TILParam;
  out Value: Integer;out ResultType: TVarType): TQuicheError;
var OpData: POpData;
  P: Integer;
begin
  OpData := @Operations[Op];
//  Assert(OpData <> nil);
  Assert(Param.Kind = pkImmediate);

  if OpData.OpGroup = ogTypecast then
  begin
    ResultType := TypeEnumToVarType[OpData.ResultType];
    case GetTypeSize(ResultType) of
      1:
      begin
        Value := Param.ImmValueInt and $ff;
        if IsSignedType(ResultType) then
          if (Value and $80) <> 0 then
            Value := Param.ImmValueInt or (-1 xor $ff);
      end;
      2:
      begin
        Value := Param.ImmValueInt and $ffff;
        if GetTypeSize(Param.ImmType) = 1 then
          Value := Value and $ff;
      end;
      else
        raise Exception.Create('Unknown type in EvalInstrinsicUnary');
      end;
    Result := qeNone;
  end
  else  //Functions
  begin
    P := Param.ImmToInteger;
    ResultType := vtUnknown;

    //-----Maths functions
    case Op of
      opAbs: Value := abs(P);
     opOdd:
      begin
        Value := BooleanToBinary[odd(P)];
        ResultType := vtBoolean;
      end;

      //-----System functions
      opHi:
      begin
        Assert(GetTypeSize(Param.ImmType) = 2);
        Value := hi(P);
      end;
      opHigh:
      begin
        ResultType := Param.ImmType;
        if ResultType = vtTypeDef then
        begin
          ResultType := Param.ImmValueInt;
          Value := GetMaxValue(ResultType)
        end
        else
        if IsEnumerable(ResultType) then
          Value := GetMaxValue(ResultType)
        else
          EXIT(errMsg(qeExpression, 'Invalid parameter for high()'));
      end;
      opLo:
      begin
        Assert(GetTypeSize(Param.ImmType) = 2);
        Value := lo(P);
      end;
      opLow:
      begin
        ResultType := Param.ImmType;
        if ResultType = vtTypeDef then
        begin
          ResultType := Param.ImmValueInt;
          Value := GetMinValue(ResultType)
        end
        else
        if IsEnumerable(ResultType) then
          Value := GetMinValue(ResultType)
        else
          EXIT(errMsg(qeExpression, 'Invalid parameter for low()'));
      end;
      opOrd:
        Value := P;
    else if (CompareText(OpData.Name, 'pred') = 0) then
    begin
      if Param.ImmType = vtBoolean then
      begin
        if P = valueFalse then
          EXIT(Err(qeConstantExpressionOverflow))
        else
          Value := valueFalse;
        ResultType := vtBoolean;
      end
      else
        Value := P - 1;
    end
    else if CompareText(OpData.Name, 'sizeof') = 0 then
    begin //TODO: type names
      if Param.ImmType = vtTypeDef then
        Value := GetTypeSize(Param.ImmValueInt)
      else
        Value := GetTypeSize(Param.ImmType);
      if Value < 256 then
        ResultType := vtByte
      else
        ResultType := vtWord;
    end
{    else if (CompareText(OpData.Name, 'succ') = 0) then
    begin
      if Param.ImmType = vtBoolean then
      begin
        if P = valueTrue then
          EXIT(Err(qeConstantExpressionOverflow))
        else
          Value := valueTrue;
        RType := vtBoolean;
      end
      else
        Value := P + 1;
    end
    else if CompareText(OpData.Name, 'swap') = 0 then
    begin
      Assert(GetTypeSize(Param.ImmType) = 2);
      Value := swap(P);
    end

    //----- Char/String functions
    else if CompareText(OpData.Name, 'chr') = 0 then
    begin
      if not (P in [0..255]) then
        EXIT(Err(qeConstantExpressionOverflow));
      Value := P;
      ResultType := vtChar;
    end
    else if CompareText(OpData.Name, 'downcase') = 0 then
    begin
      Value := ord(chr(P).ToLower);
      ResultType := vtChar;
    end
    else if CompareText(OpData.Name, 'upcase') = 0 then
    begin
      Value := ord(chr(P).ToUpper);
      ResultType := vtChar;
    end
}    //----End
    else
      EXIT(qeIntrinsicCantBeEvaluatedAtCompileTime);
  end;

    if ResultType = vtUnknown then
      Result := ValueToResultType(Value, ResultType)
    else
      Result := qeNone;
  end;
end;

//Evaulate an instrinsic with two parameters
function EvalIntrinsicBi(Op: TOperator;const Param1, Param2: TILParam;
  out Value: Integer;out ResultType: TVarType): TQuicheError;
var P1, P2: Integer;
begin
  Assert(Param1.Kind = pkImmediate);
  Assert(Param2.Kind = pkImmediate);

  P1 := Param1.ImmToInteger;
  P2 := Param2.ImmToInteger;
  ResultType := vtUnknown;

{  case Op of

  if CompareText(OpData.Name, 'inc') = 0 then
      Value := P1 + P2
  else
}    EXIT(qeIntrinsicCantBeEvaluatedAtCompileTime);

  if ResultType = vtUnknown then
    Result := ValueToResultType(Value, ResultType)
  else
    Result := qeNone;
end;

end.

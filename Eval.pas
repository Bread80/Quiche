unit Eval;

interface
uses SysUtils, ILData, ParseErrors, QTypes;

//Evaulate an operator with two parameters
function EvalBi(OpIndex: Integer;Param1, Param2: PILParam;
  out Value: Integer;out RType: TVarType): TQuicheError;

//Evaluate and operator woth a single parameter
function EvalUnary(OpIndex: Integer; Param: PILParam;
  out Value: Integer;out RType: TVarType): TQuicheError;

//Evaluate and intrinsic with a single parameter
function EvalIntrinsicUnary(OpIndex: Integer; Param: PILParam;
  out Value: Integer;out RType: TVarType): TQuicheError;

//Evaulate an instrinsic with two parameters
function EvalIntrinsicBi(OpIndex: Integer; Param1, Param2: TILParam;
  out Value: Integer;out RType: TVarType): TQuicheError;

implementation
uses Operators, Globals, System.Character;

function ValueToRType(Value: Integer;out RType: TVarTYpe): TQuicheError;
begin
  if Value < -32768 then
    EXIT(Err(qeConstantExpressionOverflow))
  else if Value < -128 then
    RType := vtInteger
  else if Value < 0 then
    RType := vtInt8
  else if Value < 256 then
    RType := vtByte
  else if Value <= 65535 then
    RType := vtWord
  else
    EXIT(Err(qeConstantExpressionOverflow));

  Result := qeNone;
end;


//Eval binary operator when both params are integers
function EvalBiInteger(OpIndex: Integer;Param1, Param2: PILParam;
  out Value: Integer;out RType: TVarType): TQuicheError;
var P1: Integer;  //First parameter value
  P2: Integer;    //Second parameter value
begin
  RType := vtUnknown;
  P1 := Param1.ImmToInteger;
  P2 := Param2.ImmToInteger;

  if OpIndex = opIndexAdd then
    Value := P1 + P2
  else if OpIndex = opIndexSubtract then
    Value := P1 - P2
  else if OpIndex = opIndexMultiply then
    Value := P1 * P2
  else if OpIndex = opIndexIntDivide then
    if P2 = 0 then
      EXIT(Err(qeDivByZero))
    else
      Value := P1 div P2
  else if OpIndex = opIndexMod then
    if P2 = 0 then
      EXIT(Err(qeDivByZero))
    else
      Value := P1 mod P2
  else if OpIndex = opIndexEqual then
  begin
    Value := BooleanToBinary[P1 = P2];
    RType := vtBoolean;
  end
  else if OpIndex = opIndexNotEqual then
  begin
    Value := BooleanToBinary[P1 <> P2];
    RType := vtBoolean;
  end
  else if OpIndex = opIndexLess then
  begin
    Value := BooleanToBinary[P1 < P2];
    RType := vtBoolean;
  end
  else if OpIndex = opIndexGreater then
  begin
    Value := BooleanToBinary[P1 > P2];
    RType := vtBoolean;
  end
  else if OpIndex = opIndexLessEqual then
  begin
    Value := BooleanToBinary[P1 <= P2];
    RType := vtBoolean;
  end
  else if OpIndex = opIndexGreaterEqual then
  begin
    Value := BooleanToBinary[P1 >= P2];
    RType := vtBoolean;
  end
  else if OpIndex = opIndexAND then
    Value := P1 and P2
  else if OpIndex = opIndexOR then
    Value := P1 or P2
  else if OpIndex = opIndexXOR then
    Value := P1 xor P2
  else if OpIndex = opIndexSHL then
    Value := P1 shl P2
  else if OpIndex = opIndexSHR then
    Value := P1 shr P2
  else
    raise Exception.Create('Unknown operation in Evaluate');

  if RType = vtUnknown then
    Result := ValueToRType(Value, RType)
  else
    Result := qeNone;
end;

//Eval binary operator when both params are booleans
function EvalBiBoolean(OpIndex: Integer;Param1, Param2: PILParam;
  out Value: Integer;out RType: TVarType): TQuicheError;
var P1: Integer;  //First parameter value
  P2: Integer;    //Second parameter value
begin
  RType := vtUnknown;
  P1 := Param1.ImmToInteger;
  P2 := Param2.ImmToInteger;

  if OpIndex = opIndexEqual then
    Value := BooleanToBinary[P1 = P2]
  else if OpIndex = opIndexNotEqual then
    Value := BooleanToBinary[P1 <> P2]
  else if OpIndex = opIndexLess then
    Value := BooleanToBinary[P1 < P2]
  else if OpIndex = opIndexGreater then
    Value := BooleanToBinary[P1 > P2]
  else if OpIndex = opIndexLessEqual then
    Value := BooleanToBinary[P1 <= P2]
  else if OpIndex = opIndexGreaterEqual then
    Value := BooleanToBinary[P1 >= P2]
  else if OpIndex = opIndexAND then
    Value := P1 and P2
  else if OpIndex = opIndexOR then
    Value := P1 or P2
  else if OpIndex = opIndexXOR then
    Value := P1 xor P2
  else
    raise Exception.Create('Unknown operation in Evaluate');

  if RType = vtUnknown then
    RType := vtBoolean;
  Result := qeNone;
end;

//Eval binary operator when both params are chars
function EvalBiChar(OpIndex: Integer;Param1, Param2: PILParam;
  out Value: Integer;out RType: TVarType): TQuicheError;
var P1: Integer;  //First parameter value
  P2: Integer;    //Second parameter value
begin
  Result := qeNone;
  RType := vtUnknown;
  P1 := Param1.ImmToInteger;
  P2 := Param2.ImmToInteger;

  if OpIndex = opIndexEqual then
  begin
    Value := BooleanToBinary[P1 = P2];
    RType := vtBoolean;
  end
  else if OpIndex = opIndexNotEqual then
  begin
    Value := BooleanToBinary[P1 <> P2];
    RType := vtBoolean;
  end
  else if OpIndex = opIndexLess then
  begin
    Value := BooleanToBinary[P1 < P2];
    RType := vtBoolean;
  end
  else if OpIndex = opIndexGreater then
  begin
    Value := BooleanToBinary[P1 > P2];
    RType := vtBoolean;
  end
  else if OpIndex = opIndexLessEqual then
  begin
    Value := BooleanToBinary[P1 <= P2];
    RType := vtBoolean;
  end
  else if OpIndex = opIndexGreaterEqual then
  begin
    Value := BooleanToBinary[P1 >= P2];
    RType := vtBoolean;
  end
  else
    raise Exception.Create('Unknown operation in Evaluate');
end;

//If both operand are immediate values, and we have a suitable routine available,
//Evaluate LeftSlug := LeftSlug.Operand <LeftSlug.Operation> RightSlug.Operand
//Returns True if the operation was evaluated.
//If so, RightSlug is now spare
function EvalBi(OpIndex: Integer;Param1, Param2: PILParam;
  out Value: Integer;out RType: TVarType): TQuicheError;
begin
  Result := qeNone;
  if IsNumericType(Param1.ImmType) and IsNumericType(Param2.ImmType) then
    EvalBiInteger(OpIndex, Param1, Param2, Value, RType)
  else if (Param1.ImmType = vtBoolean) and (Param2.ImmType = vtBoolean) then
    EvalBiBoolean(OpIndex, Param1, Param2, Value, RType)
  else if (Param1.ImmType = vtChar) and (Param2.ImmType = vtChar) then
    EvalBiChar(OpIndex, Param1, Param2, Value, RType)
  else
    EXIT(ErrOpUsage('Incompatible types ' +
      VarTypeToName(Param1.ImmType) + ' and ' +
      VarTypeToName(Param2.ImmType), OpIndex));
end;

function EvalUnary(OpIndex: Integer; Param: PILParam; out Value: Integer;out RType: TVarType): TQuicheError;
var P: Integer;  //Parameter value
  PType: TVarType;
begin
  RType := vtUnknown;

  PType := Param.ImmType;

  if IsNumericType(PType) and IsNumericType(PType) then
  begin
    P := Param.ImmToInteger;

    if OpIndex = opIndexNOT then
      Value := not P
    else if OpIndex = opIndexNegate then
      Value :=  - P
    else
      raise Exception.Create('Unknown operation in Evaluate');

    if RType = vtUnknown then
      Result := ValueToRType(Value, RType)
    else
      Result := qeNone;
  end
  else if PType = vtBoolean then
  begin
    P := Param.ImmToInteger;

    if OpIndex = opIndexNOT then
    begin
      Value := P xor valueTrue;
      RType := vtBoolean;
      Result := qeNone;
    end
    else
      raise Exception.Create('Unknown operation in Evaluate');
  end
  else
    Result := qeNone;
end;

//Evaluate and intrinsic with a single parameter
function EvalIntrinsicUnary(OpIndex: Integer; Param: PILParam;
  out Value: Integer;out RType: TVarType): TQuicheError;
var OpData: POperator;
  P: Integer;
begin
  OpData := OpIndexToData(OpIndex);
  Assert(OpData <> nil);
  Assert(Param.Loc = locImmediate);

  if OpData.OpGroup = ogTypecast then
  begin
    RType := TypeEnumToVarType[OpData.ResultType];
    case GetTypeSize(RType) of
      1:
      begin
        Value := Param.ImmValue and $ff;
        if IsSignedType(RType) then
          if (Value and $80) <> 0 then
            Value := Param.ImmValue or (-1 xor $ff);
      end;
      2:
      begin
        Value := Param.ImmValue and $ffff;
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
    RType := vtUnknown;

    //-----Maths functions
    if CompareText(OpData.Name, 'abs') = 0 then
      Value := abs(P)
    else if (CompareText(OpData.Name, 'pred') = 0) then
    begin
      if Param.ImmType = vtBoolean then
      begin
        if P = valueFalse then
          EXIT(Err(qeConstantExpressionOverflow))
        else
          Value := valueFalse;
        RType := vtBoolean;
      end
      else
        Value := P - 1;
    end
    else if (CompareText(OpData.Name, 'succ') = 0) then
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
    else if CompareText(OpData.Name, 'odd') = 0 then
    begin
      Value := BooleanToBinary[odd(P)];
      RType := vtBoolean;
    end

    //-----System functions
    else if CompareText(OpData.Name, 'hi') = 0 then
    begin
      Assert(GetTypeSize(Param.ImmType) = 2);
      Value := hi(P);
    end
    else if CompareText(OpData.Name, 'high') = 0 then
    begin
      RType := Param.ImmType;
      if IsEnumerable(Param.ImmType) then
        Value := GetMaxValue(Param.ImmType)
      else
        EXIT(ErrMsg(qeTODO, 'TODO: intrinsic high() with type name as a parameter'))
    end
    else if CompareText(OpData.Name, 'lo') = 0 then
    begin
      Assert(GetTypeSize(Param.ImmType) = 2);
      Value := lo(P);
    end
    else if CompareText(OpData.Name, 'low') = 0 then
    begin
      RType := Param.ImmType;
      if IsEnumerable(Param.ImmType) then
        Value := GetMinValue(Param.ImmType)
      else
        EXIT(ErrMsg(qeTODO, 'TODO: intrinsic low() with type name as a parameter'))
    end
    else if CompareText(OpData.Name, 'ord') = 0 then
      Value := P
    else if CompareText(OpData.Name, 'sizeof') = 0 then
    begin //TODO: type names
      Value := GetTypeSize(Param.ImmType);
      RType := vtInteger;
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
      RType := vtChar;
    end
    else if CompareText(OpData.Name, 'downcase') = 0 then
    begin
      Value := ord(chr(P).ToLower);
      RType := vtChar;
    end
    else if CompareText(OpData.Name, 'upcase') = 0 then
    begin
      Value := ord(chr(P).ToUpper);
      RType := vtChar;
    end
    //----End
    else
      EXIT(ErrMsg(qeExpression, 'Function not valid in constant expressions: ' + OpData.Name));

    if RType = vtUnknown then
      Result := ValueToRType(Value, RType)
    else
      Result := qeNone;
  end;
end;

//Evaulate an instrinsic with two parameters
function EvalIntrinsicBi(OpIndex: Integer; Param1, Param2: TILParam;
  out Value: Integer;out RType: TVarType): TQuicheError;
var OpData: POperator;
  P1, P2: Integer;
begin
  OpData := OpIndexToData(OpIndex);
  Assert(OpData <> nil);
  Assert(Param1.Loc = locImmediate);
  Assert(Param2.Loc = locImmediate);

  P1 := Param1.ImmToInteger;
  P2 := Param2.ImmToInteger;
  RType := vtUnknown;

  if CompareText(OpData.Name, 'inc') = 0 then
      Value := P1 + P2
  else
    EXIT(ErrMsg(qeBUG, 'Intrinsic not implemented: ' + OpData.Name));

  if RType = vtUnknown then
    Result := ValueToRType(Value, RType)
  else
    Result := qeNone;
end;

end.

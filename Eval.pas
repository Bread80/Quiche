unit Eval;

interface
uses SysUtils, ILData, ParseErrors, QTypes, Operators;

//Evaulate an operator with two parameters
function EvalBi(Op: TOperator;Param1, Param2: PILParam;
  out Value: TImmValue): TQuicheError;

//Evaluate and operator woth a single parameter
function EvalUnary(Op: TOperator; Param: PILParam;
  out Value: TImmValue): TQuicheError;

//Evaluate and intrinsic with a single parameter
function EvalIntrinsicUnary(Op: TOperator;const Param: TILParam;
  out Value: TImmValue): TQuicheError;

//Evaulate an instrinsic with two parameters
function EvalIntrinsicBi(Op: TOperator;const Param1, Param2: TILParam;
  out Value: TImmValue): TQuicheError;

implementation
uses Globals, System.Character;

function ValueToVarType(Value: Integer;out VarType: TVarType): TQuicheError;
begin
  if not TryIntegerToVarType(Value, VarType) then
    EXIT(Err(qeConstantExpressionOverflow));

  Result := qeNone;
end;

//Massages the result of a bitwise logical operation to fit within the given VarType
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

//If both operand are immediate values, and we have a suitable routine available,
//Evaluate LeftSlug := LeftSlug.Operand <LeftSlug.Operation> RightSlug.Operand
//Returns True if the operation was evaluated.
//If so, RightSlug is now spare
function EvalBi(Op: TOperator;Param1, Param2: PILParam;
  out Value: TImmValue): TQuicheError;
var P1: TImmValue;  //First parameter value
  P2: TImmValue;    //Second parameter value
  Error: Boolean;
begin
  Error := False;
  Result := qeNone;
  P1 := Param1.Imm;
  P2 := Param2.Imm;
  Value.VarType := vtUnknown;


  case Op of
    opAdd:
      if IsIntegerType(P1.VarType) and IsIntegerType(P2.VarType) then
        Value.IntValue := P1.IntValue + P2.IntValue
      else
        Error := True;
    opSubtract:
      if IsIntegerType(P1.VarType) and IsIntegerType(P2.VarType) then
        Value.IntValue := P1.IntValue - P2.IntValue
      else
        Error := True;
    opMultiply:
      if IsIntegerType(P1.VarType) and IsIntegerType(P2.VarType) then
        Value.IntValue := P1.IntValue * P2.IntValue
      else
        Error := True;
    opIntDivide:
    begin
      if IsIntegerType(P1.VarType) and IsIntegerType(P2.VarType) then
        if P2.IntValue = 0 then
          EXIT(Err(qeDivByZero))
        else
          Value.IntValue := P1.IntValue div P2.IntValue
      else
        Error := True;
    end;
    opMod:
      if IsIntegerType(P1.VarType) and IsIntegerType(P2.VarType) then
        if P2.IntValue = 0 then
          EXIT(Err(qeDivByZero))
        else
          Value.IntValue := P1.IntValue mod P2.IntValue
      else
        Error := True;
    opEqual, opNotEqual:
    begin
      if IsIntegerType(P1.VarType) and IsIntegerType(P2.VarType) then
        Value.BoolValue := P1.IntValue = P2.IntValue
      else if (P1.VarType = vtBoolean) and (P2.VarType = vtBoolean) then
        Value.BoolValue := P1.BoolValue = P2.BoolValue
      else if (P1.VarType = vtChar) and (P2.VarType = vtChar) then
        Value.BoolValue := P1.CharValue = P2.CharValue
      else if (P1.VarType = vtTypeDef) and (P2.VarType = vtTypeDef) then
        Value.BoolValue := P1.TypeValue = P2.TypeValue
      else
        Error := True;
      if Op = opNotEqual then
        Value.BoolValue := not Value.BoolValue;
      Value.VarType := vtBoolean;
    end;
    opLess, opGreaterEqual:
    begin
      if IsIntegerType(P1.VarType) and IsIntegerType(P2.VarType) then
        Value.BoolValue := P1.IntValue < P2.IntValue
      else if (P1.VarType = vtBoolean) and (P2.VarType = vtBoolean) then
        Value.BoolValue := P1.BoolValue < P2.BoolValue
      else if (P1.VarType = vtChar) and (P2.VarType = vtChar) then
        Value.BoolValue := P1.CharValue < P2.CharValue
      else if (P1.VarType = vtTypeDef) and (P2.VarType = vtTypeDef) then
        Value.BoolValue := P1.TypeValue < P2.TypeValue
      else
        Error := True;
      if Op = opGreaterEqual then
        Value.BoolValue := not Value.BoolValue;
      Value.VarType := vtBoolean;
    end;
    opGreater, opLessEqual:
    begin
      if IsIntegerType(P1.VarType) and IsIntegerType(P2.VarType) then
        Value.BoolValue := P1.IntValue > P2.IntValue
      else if (P1.VarType = vtBoolean) and (P2.VarType = vtBoolean) then
        Value.BoolValue := P1.BoolValue > P2.BoolValue
      else if (P1.VarType = vtChar) and (P2.VarType = vtChar) then
        Value.BoolValue := P1.CharValue > P2.CharValue
      else if (P1.VarType = vtTypeDef) and (P2.VarType = vtTypeDef) then
        Value.BoolValue := P1.TypeValue > P2.TypeValue
      else
        Error := True;
      if Op = opLessEqual then
        Value.BoolValue := not Value.BoolValue;
      Value.VarType := vtBoolean;
    end;
    opAND:
      if IsIntegerType(P1.VarType) and IsIntegerType(P2.VarType) then
      begin
        Value.IntValue := P1.IntValue and P2.IntValue;
        Value.IntValue := LogicValueToType(Value.IntValue, P1.VarType);
      end
      else if (P1.VarType = vtBoolean) and (P2.VarType = vtBoolean) then
        Value.BoolValue := P1.BoolValue and P2.BoolValue
      else
        Error := True;
    opOR:
      if IsIntegerType(P1.VarType) and IsIntegerType(P2.VarType) then
      begin
        Value.IntValue := P1.IntValue or P2.IntValue;
        Value.IntValue := LogicValueToType(Value.IntValue, P1.VarType);
      end
      else if (P1.VarType = vtBoolean) and (P2.VarType = vtBoolean) then
        Value.BoolValue := P1.BoolValue or P2.BoolValue
      else
        Error := True;
    opXOR:
      if IsIntegerType(P1.VarType) and IsIntegerType(P2.VarType) then
      begin
        Value.IntValue := P1.IntValue xor P2.IntValue;
        Value.IntValue := LogicValueToType(Value.IntValue, P1.VarType);
      end
      else if (P1.VarType = vtBoolean) and (P2.VarType = vtBoolean) then
        Value.BoolValue := P1.BoolValue xor P2.BoolValue
      else
        Error := True;
    opSHL:
      if IsIntegerType(P1.VarType) and IsIntegerType(P2.VarType) then
        if (P2.IntValue < 0) or (P2.IntValue > 32) then
          Value.IntValue := 0
        else
          Value.IntValue := P1.IntValue shl P2.IntValue
      else
        Error := True;
    opSHR:
      if IsIntegerType(P1.VarType) and IsIntegerType(P2.VarType) then
        if (P2.IntValue < 0) or (P2.IntValue > 32) then
          Value.IntValue := 0
        else
          Value.IntValue := P1.IntValue shr P2.IntValue
      else
        Error := True;
  else
    raise Exception.Create('Unknown operation in Evaluate');
  end;

  if Result <> qeNone then
    EXIT;

  if Value.VarType = vtUnknown then
    if IsNumericType(Value.VarType) then
      Result := ValueToVarType(Value.IntValue, Value.VarType)
    else
    begin
      Value.VarType := P1.VarType;
      Result := qeNone;
    end
  else
    Result := qeNone;

  if Error then
    EXIT(ErrOpUsage('Incompatible types ' +
      VarTypeToName(Param1.Imm.VarType) + ' and ' +
      VarTypeToName(Param2.Imm.VarType), Op));
end;




function EvalUnary(Op: TOperator; Param: PILParam; out Value: TImmValue): TQuicheError;
var P: TImmValue;
begin
  Result := qeNone;
  P := Param.Imm;
  Value.VarType := vtUnknown;

  if IsNumericType(P.VarType) then
  begin
    case Op of
      opComplement:
      begin
        Value.IntValue := not P.IntValue;
        Value.IntValue := LogicValueToType(Value.IntValue, P.VarType);
      end;
      opNegate: Value.IntValue :=  - P.IntValue
    else
      raise Exception.Create('Unknown operation in Evaluate');
    end;

//    Value.VarType := P.VarType;
  end
  else if P.VarType = vtBoolean then
  begin
    if Op = opComplement then
    begin
      Value.BoolValue := not P.BoolValue;
      Value.VarType := vtBoolean;
      Result := qeNone;
    end
    else
      raise Exception.Create('Unknown operation in Evaluate');
  end
  else
    EXIT(ErrOpUsage('Operand type incompatible with operator ' +
      VarTypeToName(Param.Imm.VarType), Op));


  if Result <> qeNone then
    EXIT;

  if Value.VarType = vtUnknown then
    if IsNumericType(Value.VarType) then
      Result := ValueToVarType(Value.IntValue, Value.VarType)
    else
    begin
      Value.VarType := P.VarType;
      Result := qeNone;
    end
  else
    Result := qeNone;

{  if Error then
    EXIT(ErrOpUsage('Incompatible types ' +
      VarTypeToName(Param1.Imm.VarType) + ' and ' +
      VarTypeToName(Param2.Imm.VarType), Op));
}end;

//Evaluate and intrinsic with a single parameter
function EvalIntrinsicUnary(Op: TOperator;const Param: TILParam;
  out Value: TImmValue): TQuicheError;
var OpData: POpData;
  P: TImmValue;
  Error: Boolean;
//const TypeCastToType: array[low(TypecastOps)..high(TypecastOps)] of TVarType =
//  (vtInt8, vtInteger, vtByte, vtWord, vtPointer, vtBoolean, vtChar);
begin
  Error := False;
  OpData := @Operations[Op];
  Assert(Param.Kind = pkImmediate);
  P := Param.Imm;
  Value.VarType := vtUnknown;

  case Op of
    //Typecasts
    opInt8:
    begin
      Value.IntValue := LogicValueToType(Param.Imm.ToInteger and $ff, vtInt8);
      Value.VarType := vtInt8;
    end;
    opInteger:
    begin
      Value.IntValue := Param.Imm.ToInteger;
      Value.VarType := vtInteger;
    end;
    opByte:
    begin
      Value.IntValue := Param.Imm.ToInteger and $ff;
      Value.VarType := vtByte;
    end;
    opWord:
    begin
      Value.IntValue := Param.Imm.ToInteger and iCPUWordMask;
      Value.VarType := vtWord;
    end;
    opPointer:
    begin
      Value.IntValue := Param.Imm.ToInteger and iCPUWordMask;
      Value.VarType := vtPointer;
    end;
//    opReal: Value.VarType := vtReal;
    opBoolean:
    begin
      Value.BoolValue := Param.Imm.ToInteger <> 0;
      Value.VarType := vtBoolean;
    end;
    opChar:
    begin
      Value.CharValue := chr(Param.Imm.ToInteger and $ff);
      Value.VarType := vtChar;
    end;

    //-----Maths functions
    opAbs:
      if IsIntegerType(P.VarType) then
        Value.IntValue := abs(P.IntValue)
      else
        Error := True;
    opOdd:
    begin
      if IsIntegerType(P.VarType) then
        Value.BoolValue := odd(P.IntValue)
      else
        Error := True;
      Value.VarType := vtBoolean;
    end;

    //-----System functions
    opHi:
    begin
      if (GetTypeSize(P.VarType) = 2) and IsIntegerType(P.VarType) then
        Value.IntValue := hi(P.IntValue)
      else
        Error := True;
      Value.VarType := vtByte;
    end;
    opHigh:
    begin
      if P.VarType = vtTypeDef then
        Value.VarType := P.TypeValue
      else
        Value.VarType := P.VarType;
      if IsEnumerable(P.VarType) then
        SetMaxValue(Value)
      else
        Error := True;
    end;
    opLo:
    begin
      if (GetTypeSize(P.VarType) = 2) and IsIntegerType(P.VarType) then
        Value.IntValue := lo(P.IntValue)
      else
        Error := True;
      Value.VarType := vtByte;
    end;
    opLow:
    begin
      if P.VarType = vtTypeDef then
        Value.VarType := P.TypeValue
      else
        Value.VarType := P.VarType;
      if IsEnumerable(P.VarType) then
        SetMinValue(Value)
      else
        Error := True;
    end;
    opOrd:
    begin
      if IsIntegerType(P.VarType) then
        Value := P
      else
      begin
        case P.VarType of
          vtBoolean:
            if P.BoolValue = True then
              Value.IntValue := 1
            else
              Value.IntValue := 0;
          vtChar:
            Value.IntValue := ord(P.CharValue);
          vtTypeDef:
            Value.IntValue := ord(P.TypeValue);
        else
          Error := True;
        end;
        if Value.IntValue <= 255 then
          Value.VarType := vtbyte
        else
          Value.VarType := vtWord;
      end;
    end;
    opPred:
      if IsIntegerType(P.VarType) then
        Value.IntValue := pred(P.IntValue)
      else
      case P.VarType of
        vtBoolean: Value.BoolValue := pred(P.BoolValue);
        vtChar: Value.CharValue := pred(P.CharValue);
      else
        Error := True;
      end;
    opSizeof:
    begin
      if P.VarType = vtTypeDef then
        Value.IntValue := GetTypeSize(P.TypeValue)
      else
        Value.IntValue := GetTypeSize(P.VarType);
      if Value.IntValue < 256 then
        Value.VarType := vtByte
      else
        Value.VarType := vtWord;
    end;
    opSucc:
      if IsIntegerType(P.VarType) then
        Value.IntValue := succ(P.IntValue)
      else
      case P.VarType of
        vtBoolean: Value.BoolValue := succ(P.BoolValue);
        vtChar: Value.CharValue := succ(P.CharValue);
      else
        Error := True;
      end;
    opSwap:
      if (GetTypeSize(P.VarType) = 2) and IsIntegerType(P.VarType) then
      begin
        Value.IntValue := swap(P.IntValue);
        Value.VarType := P.VarType;
      end
      else
        Error := True;

    //----- Char/String functions
    opChr:
    begin
      if IsIntegerType(P.VarType) then
        if not (P.IntValue in [0..255]) then
          EXIT(Err(qeConstantExpressionOverflow))
        else
          Value.CharValue := chr(P.IntValue)
      else
        Error := True;
      Value.VarType := vtChar
    end;
    opDowncase:
      if P.VarType = vtChar then
        Value.CharValue := P.CharValue.ToLower
      else
        Error := True;
    opUpcase:
      if P.VarType = vtChar then
        Value.CharValue := P.CharValue.ToUpper
      else
        Error := True;

    //----End
  else
    EXIT(qeIntrinsicCantBeEvaluatedAtCompileTime);
  end;

  if Value.VarType = vtUnknown then
    if IsIntegerType(P.VarType) then
      Result := ValueToVarType(Value.IntValue, Value.VarType)
    else
    begin
      Value.VarType := P.VarType;
      Result := qeNone
    end
    else
      Result := qeNone;

  if Error then
    EXIT(ErrOpUsage('Parameter type incompatible with function ' +
      VarTypeToName(P.VarType), Op));
end;

//Evaulate an instrinsic with two parameters
function EvalIntrinsicBi(Op: TOperator;const Param1, Param2: TILParam;
  out Value: TImmValue): TQuicheError;
var P1, P2: TImmValue;
  Error: Boolean;
begin
  Assert(Param1.Kind = pkImmediate);
  Assert(Param2.Kind = pkImmediate);
  Error := False;

  P1 := Param1.Imm;
  P2 := Param2.Imm;
  Value.VarType := vtUnknown;

  case Op of
    opPred:
    begin
      if IsIntegerType(P2.VarType) then
        if IsIntegerType(P1.VarType) then
          Value.IntValue := P1.IntValue - P2.IntValue
        else
          case P1.VarType of
            vtBoolean: Value.BoolValue := False;
            vtChar: Value.CharValue := chr(ord(P1.CharValue)-P2.IntValue);
          else
            Error := True;
          end
      else
        Error := True;
    end;
    opSucc:
    begin
      if IsIntegerType(P2.VarType) then
        if IsIntegerType(P1.VarType) then
          Value.IntValue := P1.IntValue + P2.IntValue
        else
          case P1.VarType of
            vtBoolean: Value.BoolValue := True;
            vtChar: Value.CharValue := chr(ord(P1.CharValue)+P2.IntValue);
          else
            Error := True;
          end
      else
        Error := True;
    end;
  else
    EXIT(qeIntrinsicCantBeEvaluatedAtCompileTime);
  end;

  if Value.VarType = vtUnknown then
    if IsNumericType(Value.VarType) then
      Result := ValueToVarType(Value.IntValue, Value.VarType)
    else
    begin
      Value.VarType := P1.VarType;
      Result := qeNone;
    end
  else
    Result := qeNone;

  if Error then
    EXIT(ErrOpUsage('Incompatible parameter types ' +
      VarTypeToName(Param1.Imm.VarType) + ' and ' +
      VarTypeToName(Param2.Imm.VarType), Op));
end;

end.

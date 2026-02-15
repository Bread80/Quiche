unit Parse.Eval;

interface
uses SysUtils,
  Def.IL, Def.Operators, Def.VarTypes, Def.Consts, Def.UserTypes,
  Parse.Errors;

////////////////////////////////////////////////////////////////////////////////
//For all of these functions the types of the parameter(s) in relation to the
//operator *must* have been validated in advance (ie via Lib.Primitives).
//No validation is performed by these routines.
////////////////////////////////////////////////////////////////////////////////

//Evaulate an operator with two parameters
function EvalBi(Op: TOperator;Param1, Param2: PILParam;
  out Value: TImmValue): TQuicheError;

//Evaluate and operator woth a single parameter
function EvalUnary(Op: TOperator; Param: PILParam;
  out Value: TImmValue): TQuicheError;

//Evaluate and intrinsic with a single parameter
//Returns Evalled as False if the op can't be evalled at compile time
function EvalIntrinsicUnary(Op: TOperator;const Param: TILParam;
  out Value: TImmValue;out Evalled: Boolean): TQuicheError;

//Evaulate an instrinsic with two parameters
//Returns RunTimeOnly as True if the op can't be evalled at compile time
function EvalIntrinsicBi(Op: TOperator;const Param1, Param2: TILParam;
  out Value: TImmValue;out Evalled: Boolean): TQuicheError;

//Typecast an ImmValue to the given type
function EvalTypecast(var Value: TImmValue; ToType: PUserType): TQuicheError;

implementation
uses {$ifndef fpc}System.Character,{$endif}
  Def.Globals;

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
    raise Exception.Create('Invalid type for logic result');
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
  IntValue: Integer;
  VarType: TVarType;  //Of result
begin
  Error := False;
  Result := qeNone;
  P1 := Param1.Imm;
  P2 := Param2.Imm;
  VarType := vtUnknown;
  IntValue := 0;

  case Op of
    opAdd:
      if IsIntegerVarType(P1.VarType) and IsIntegerVarType(P2.VarType) then
        IntValue := P1.IntValue + P2.IntValue
      else
        Error := True;
    opSubtract:
      if IsIntegerVarType(P1.VarType) and IsIntegerVarType(P2.VarType) then
        IntValue := P1.IntValue - P2.IntValue
      else
        Error := True;
    opMultiply:
      if IsIntegerVarType(P1.VarType) and IsIntegerVarType(P2.VarType) then
        IntValue := P1.IntValue * P2.IntValue
      else
        Error := True;
    opIntDivide:
      if IsIntegerVarType(P1.VarType) and IsIntegerVarType(P2.VarType) then
        if P2.IntValue = 0 then
          EXIT(Err(qeDivByZero))
        else
          IntValue := P1.IntValue div P2.IntValue
      else
        Error := True;
    opMod:
      if IsIntegerVarType(P1.VarType) and IsIntegerVarType(P2.VarType) then
        if P2.IntValue = 0 then
          EXIT(Err(qeDivByZero))
        else
          IntValue := P1.IntValue mod P2.IntValue
      else
        Error := True;

    opEqual, opNotEqual:
    begin
      if IsIntegerVarType(P1.VarType) and IsIntegerVarType(P2.VarType) then
        Value.CreateBoolean(P1.IntValue = P2.IntValue)
      else if (P1.VarType = vtBoolean) and (P2.VarType = vtBoolean) then
        Value.CreateBoolean(P1.BoolValue = P2.BoolValue)
      else if (P1.VarType = vtChar) and (P2.VarType = vtChar) then
        Value.CreateBoolean(P1.StringValue = P2.StringValue)
      else if (P1.VarType in [vtChar, vtArrayType]) and (P2.VarType in [vtChar, vtArrayType]) then
        Assert(False, 'TODO')
//        Value.CreateBoolean(P1.StringValue = P2.StringValue)
(*      else if (P1.VarType = vtTypeDef) and (P2.VarType = vtTypeDef) then
        Value.CreateBoolean(P1.TypeDefValue = P2.TypeDefValue)
*)      else
        Error := True;
      if Op = opNotEqual then
        Value.CreateBoolean(not Value.BoolValue);
      if not Error then
        EXIT;
    end;
    opLess, opGreaterEqual:
    begin
      if IsIntegerVarType(P1.VarType) and IsIntegerVarType(P2.VarType) then
        Value.CreateBoolean(P1.IntValue < P2.IntValue)
      else if (P1.VarType = vtBoolean) and (P2.VarType = vtBoolean) then
        Value.CreateBoolean(P1.BoolValue < P2.BoolValue)
      else if (P1.VarType = vtChar) and (P2.VarType = vtChar) then
        Value.CreateBoolean(P1.CharValue < P2.CharValue)
      else
        Error := True;
      if Op = opGreaterEqual then
        Value.CreateBoolean(not Value.BoolValue);
      if not Error then
        EXIT;
    end;
    opGreater, opLessEqual:
    begin
      if IsIntegerVarType(P1.VarType) and IsIntegerVarType(P2.VarType) then
        Value.CreateBoolean(P1.IntValue > P2.IntValue)
      else if (P1.VarType = vtBoolean) and (P2.VarType = vtBoolean) then
        Value.CreateBoolean(P1.BoolValue > P2.BoolValue)
      else if (P1.VarType = vtChar) and (P2.VarType = vtChar) then
        Value.CreateBoolean(P1.CharValue > P2.CharValue)
      else
        Error := True;
      if Op = opLessEqual then
        Value.CreateBoolean(not Value.BoolValue);
      if not Error then
        EXIT;
    end;

    opAND:
      if IsIntegerVarType(P1.VarType) and IsIntegerVarType(P2.VarType) then
      begin
        IntValue := P1.IntValue and P2.IntValue;
        IntValue := LogicValueToType(IntValue, P1.VarType);
      end
      else if (P1.VarType = vtBoolean) and (P2.VarType = vtBoolean) then
      begin
        Value.CreateBoolean(P1.BoolValue and P2.BoolValue);
        EXIT;
      end
      else
        Error := True;
    opOR:
      if IsIntegerVarType(P1.VarType) and IsIntegerVarType(P2.VarType) then
      begin
        IntValue := P1.IntValue or P2.IntValue;
        IntValue := LogicValueToType(IntValue, P1.VarType);
      end
      else if (P1.VarType = vtBoolean) and (P2.VarType = vtBoolean) then
      begin
        Value.CreateBoolean(P1.BoolValue or P2.BoolValue);
        EXIT;
      end
      else
        Error := True;
    opXOR:
      if IsIntegerVarType(P1.VarType) and IsIntegerVarType(P2.VarType) then
      begin
        IntValue := P1.IntValue xor P2.IntValue;
        IntValue := LogicValueToType(IntValue, P1.VarType);
      end
      else if (P1.VarType = vtBoolean) and (P2.VarType = vtBoolean) then
      begin
        Value.CreateBoolean(P1.BoolValue xor P2.BoolValue);
        EXIT;
      end
      else
        Error := True;

    opConcat:
      if P1.UserType.IsStringableType and P2.UserType.IsStringableType then
      begin
        Value.CreateString(P1.StringValue + P2.StringValue);
        EXIT;
      end
      else if (P1.VarType = vtArrayType) and (P2.VarType = vtArrayType) then
        Assert(False, 'TODO')
      else
        Error := True;
    opSHL:
      if IsIntegerVarType(P1.VarType) and IsIntegerVarType(P2.VarType) then
        if (P2.IntValue < 0) or (P2.IntValue > 32) then
          IntValue := 0
        else
          IntValue := P1.IntValue shl P2.IntValue
      else
        Error := True;
    opSHR:
      if IsIntegerVarType(P1.VarType) and IsIntegerVarType(P2.VarType) then
        if (P2.IntValue < 0) or (P2.IntValue > 32) then
          IntValue := 0
        else
          IntValue := P1.IntValue shr P2.IntValue
      else
        Error := True;
  else
    raise Exception.Create('Unknown operation in Evaluate');
  end;

  if Result <> qeNone then
    EXIT;

  if VarType = vtUnknown then
    if Operations[Op].SignCombine then
    begin
      VarType := GetImmSignCombineType(IntValue, P1.VarType, P2.VarType);
      if VarType = vtUnknown then
        EXIT(Err(qeConstantExpressionOverflow));
      Value.CreateTyped(VarType, IntValue);
      Result := qeNone;
    end
    else if IsNumericVarType(Value.VarType) then
    begin
      VarType := Value.VarType;
      Result := ValueToVarType(IntValue, VarType);
      Value.CreateTyped(VarType, IntValue)
    end
    else
    begin
      Value.CreateTyped(Value.VarType, IntValue);
      Result := qeNone;
    end
  else
    Result := qeNone;

  if Error then
    EXIT(ErrOpUsageSub2(qeOpIncompatibleTypes, VarTypeToName(Param1.Imm.VarType),
      VarTypeToName(Param2.Imm.VarType), Op));
end;

function EvalUnary(Op: TOperator; Param: PILParam; out Value: TImmValue): TQuicheError;
var P: TImmValue;
  VarType: TVarType;
  IntValue: Integer;
begin
  Result := qeNone;
  P := Param.Imm;
  VarType := vtUnknown;

  if IsNumericVarType(P.VarType) then
  begin
    case Op of
      opComplement:
      begin
        IntValue := not P.IntValue;
        IntValue := LogicValueToType(IntValue, P.VarType);
      end;
      opNegate: IntValue :=  - P.IntValue
    else
      raise Exception.Create('Unknown operation in Evaluate');
    end;

  end
  else if P.VarType = vtBoolean then
  begin
    if Op = opComplement then
    begin
      Value.CreateBoolean(not P.BoolValue);
      EXIT;
    end
    else
      raise Exception.Create('Unknown operation in Evaluate');
  end
  else
    EXIT(ErrOpUsageSub(qeOpIncompatibleType,
      VarTypeToName(Param.Imm.VarType), Op));

  if Result <> qeNone then
    EXIT;

  if VarType = vtUnknown then
    if IsNumericVarType(Value.VarType) then
    begin
      VarType := Value.VarType;
      Result := ValueToVarType(IntValue, VarType);
      Value.CreateTyped(VarType, IntValue);
    end
    else
    begin
      Value.CreateTyped(P.VarType, IntValue);
      Result := qeNone;
    end
  else
    Result := qeNone;
end;

//Evaluate and intrinsic with a single parameter
function EvalIntrinsicUnary(Op: TOperator;const Param: TILParam;
  out Value: TImmValue;out Evalled: Boolean): TQuicheError;
var P: TImmValue;
  Error: Boolean;
  VarType: TVarType;
  IntValue: Integer;
begin
  Error := False;
  Assert(Param.Kind = pkImmediate);
  P := Param.Imm;
  VarType := vtUnknown;
  Result := qeNone;
  IntValue := 0;
  Evalled := True;

  case Op of
    opAbs:
      if IsIntegerVarType(P.VarType) then
      begin
        Value.CreateTyped(vtInteger, abs(P.IntValue));
        EXIT;
      end
      else
        Error := True;
    opChr:
    begin
      if IsIntegerVarType(P.VarType) then
        if P.IntValue in [0..255] then
        begin
          Value.CreateChar(chr(P.IntValue));
          EXIT;
        end
        else
          EXIT(Err(qeConstantExpressionOverflow))
      else
        Error := True;
    end;
    opDowncase:
    begin
      if P.VarType = vtChar then
      {$ifdef fpc}
        Value.CreateChar(Lowercase(P.CharValue))
      {$else}
        Value.CreateChar(P.CharValue.ToLower)
      {$endif}
      else if P.VarType = vtArrayType then
        Assert(False, 'TODO')
//        Value.CreateString(P.StringValue.ToLower)
      else
        Error := True;
      if not Error then
        EXIT;
    end;
    opHi:
      if (GetTypeSize(P.UserType) = 2) and IsIntegerVarType(P.VarType) then
      begin
        Value.CreateTyped(vtByte, hi(Word(P.IntValue and $ffff)));
        EXIT;
      end
      else
        Error := True;
    opHigh:
    begin
      case P.VarType of
        vtTypeDef:
        begin
          if P.TypeDefValue.VarType = vtBoolean then
            Value.CreateBoolean(True)
          else if IsOrdinalType(P.TypeDefValue) then
            Value.CreateTyped(P.TypeDefValue, P.TypeDefValue.High)
          else if P.TypedefValue.VarType = vtArrayType then
          begin
            if P.TypeDefValue.ArrayDef.IsUnbounded then
              Evalled := False
            else
              case P.TypeDefValue.ArrayDef.ArrayType of
                atArray:
                  Value.CreateTyped(GetOfType(P.TypeDefValue.BoundsType), P.TypeDefValue.BoundsType.High);
                atVector: Value.CreateTyped(P.TypeDefValue.ArrayDef.MetaType, P.TypeDefValue.VectorLength-1);
                atList: Evalled := False;
              else
                Error := True;
              end;
          end
          else
            Error := True;
        end;
        vtArrayType:  //Array literal
          case P.UserType.ArrayDef.ArrayType of
            atArray:
              Value.CreateTyped(GetOfType(P.UserType.BoundsType), P.UserType.BoundsType.High);
            atVector, atList:
            begin
              IntValue := P.ArrayLength-1;
              if IntValue > 255 then
                VarType := vtWord
              else
                VarType := vtByte;
              Value.CreateTyped(VarType, IntValue);
            end;
          else
            Error := True;
          end;
      else
        Error := True;
      end;
      if not Error then
        EXIT;
    end;
    opLength:
    begin
      case P.VarType of
        vtTypeDef:
        begin
          if P.TypeDefValue.VarType = vtArrayType then
          begin
            if P.TypeDefValue.ArrayDef.IsUnbounded then
              Evalled := False;
            case P.TypeDefValue.ArrayDef.ArrayType of
              atArray:
                Value.CreateTyped(vtWord, GetTypeItemCount(P.TypeDefValue.BoundsType));
              atVector:
                Value.CreateTyped(P.TypeDefValue.ArrayDef.MetaType, P.TypeDefValue.VectorLength);
              atList: Evalled := False;
            else
              Error := True;
            end
          end
          else
            Error := True;
          if Error then
            EXIT(ErrOpUsageSub(qeOpIncompatibleType, VarTypeToName(P.TypeDefValue.VarType), Op));
          EXIT;
        end;
        vtArrayType:  //Array literal
        begin
          IntValue := P.ArrayLength;
          if IntValue > 255 then
            VarType := vtWord
          else
            VarType := vtByte;
          Value.CreateTyped(VarType, IntValue);
        end;
        //Extend for other array types
      else
        Error := True;
      end;
      if Error then
        EXIT(ErrOpUsageSub(qeOpIncompatibleType, VarTypeToName(P.TypeDefValue.VarType), Op));
    end;
    opLo:
      if (GetTypeSize(P.UserType) = 2) and IsIntegerVarType(P.VarType) then
      begin
        Value.CreateTyped(vtByte, lo(Word(P.IntValue and $ffff)));
        EXIT;
      end
      else
        Error := True;
    opLow:
    begin
      case P.VarType of
        vtTypeDef:
        begin
          if P.TypeDefValue.VarType = vtBoolean then
            Value.CreateBoolean(False)
          else if IsOrdinalType(P.TypeDefValue) then
            Value.CreateTyped(P.TypeDefValue, P.TypeDefValue.Low)
          else if P.TypeDefValue.VarType = vtArrayType then
            case P.TypeDefValue.ArrayDef.ArrayType of
              atArray:
                Value.CreateTyped(GetOfType(P.TypeDefValue.BoundsType), P.TypeDefValue.BoundsType.Low);
              atVector, atList:
                Value.CreateTyped(P.TypeDefValue.ArrayDef.MetaType, 0);
            else
              Error := True;
            end
          else
            Error := True;
        end;
        vtArrayType:  //Array literal
        case P.UserType.ArrayDef.ArrayType of
          atArray:
            Value.CreateTyped(GetOfType(P.UserType.BoundsType), P.UserType.BoundsType.Low);
          atVector, atList:
            Value.CreateTyped(P.UserType.ArrayDef.MetaType, 0);
        else
          Error := True;
        end
      else
        Error := True;
      end;
      if not Error then
        EXIT;
    end;
    opOdd:
      if IsIntegerVarType(P.VarType) then
      begin
        Value.CreateBoolean(odd(P.IntValue));
        EXIT;
      end
      else
        Error := True;
    opOrd:
    begin
      if IsIntegerVarType(P.VarType) then
        Value.CreateTyped(P.VarType, P.IntValue)
      else
      begin
        case P.VarType of
          vtBoolean:
            if P.BoolValue = True then
              Value.CreateTyped(vtByte, 1)
            else
              Value.CreateTyped(vtByte, 0);
          vtChar, vtEnumeration:
            Value.CreateTyped(vtByte, P.ToInteger);
        else
          Error := True;
        end;
      end;
      if not Error then
        EXIT;
    end;
    opPred:
      if IsIntegerVarType(P.VarType) then
        IntValue := pred(P.IntValue)
      else
      begin
        case P.VarType of
          vtBoolean: Value.CreateBoolean(pred(P.BoolValue));
          vtChar, vtEnumeration:
          begin
            Assert(P.UserType <> nil);
            Value.CreateTyped(P.UserType, P.ToInteger -1);
          end;
        else
          Error := True;
        end;
        if not Error then
          EXIT;
      end;
    opSizeof:
    begin
      case P.VarType of
        vtTypeDef:
        begin
          if P.TypeDefValue.VarType = vtArrayType then
            //Unbounded arrays can't be evalled at compile time
            if P.TypeDefValue.ArrayDef.IsUnbounded then
            begin
              Evalled := False;
              EXIT;
            end;
          IntValue := GetTypeSize(P.TypeDefValue);
        end;
        vtArrayType:
          IntValue := P.UserType.ArrayDef.MetaSize + (P.ArrayLength * P.UserType.ArrayDef.ElementSize);
      else
        IntValue := GetTypeSize(P.UserType);
      end;

      if IntValue < 256 then
        Value.CreateTyped(vtByte, IntValue)
      else
        Value.CreateTyped(vtWord, IntValue);
      EXIT;
    end;
    opSucc:
      if IsIntegerVarType(P.VarType) then
        IntValue := succ(P.IntValue)
      else
      begin
        case P.VarType of
          vtBoolean: Value.CreateBoolean(succ(P.BoolValue));
          vtChar, vtEnumeration:
          begin
            Assert(P.UserType <> nil);
            Value.CreateTyped(P.UserType, P.ToInteger + 1);
          end;
        else
          Error := True;
        end;
        if not Error then
          EXIT;
      end;
    opSwap:
      if (GetTypeSize(P.UserType) = 2) and IsIntegerVarType(P.VarType) then
      begin
        Value.CreateTyped(P.VarType, swap(P.IntValue));
        EXIT;
      end
      else
        Error := True;
    opUpcase:
    begin
      if P.VarType = vtChar then
      {$ifdef fpc}
        Value.CreateChar(Upcase(P.CharValue))
      {$else}
      Value.CreateChar(P.CharValue.ToUpper)
      {$endif}
      else if P.VarType = vtArrayType then
        Assert(False, 'TODO')
//        Value.CreateString(P.StringValue.ToUpper)
      else
        Error := True;
      if not Error then
        EXIT;
    end;

    //----End
  else
    Evalled := False;
    EXIT;
  end;

  if VarType = vtUnknown then
    if IsIntegerVarType(P.VarType) then
    begin
      VarType := Value.VarType;
      Result := ValueToVarType(IntValue, VarType);
      Value.CreateTyped(VarType, IntValue);
    end
    else
    begin
      Value.CreateTyped(P.VarType, IntValue);
      Result := qeNone
    end
    else
      Result := qeNone;

  if Error then
    EXIT(ErrOpUsageSub(qeOpIncompatibleType, VarTypeToName(P.VarType), Op));
end;

//Evaulate an instrinsic with two parameters
function EvalIntrinsicBi(Op: TOperator;const Param1, Param2: TILParam;
  out Value: TImmValue;out Evalled: Boolean): TQuicheError;
var P1, P2: TImmValue;
  Error: Boolean;
  VarType: TVarType;
  IntValue: Integer;
begin
  Assert(Param1.Kind = pkImmediate);
  Assert(Param2.Kind = pkImmediate);
  Error := False;
  Result := qeNone;

  P1 := Param1.Imm;
  P2 := Param2.Imm;
  VarType := vtUnknown;
  IntValue := 0;
  Evalled := True;

  case Op of
    opPred:
    begin
      if IsIntegerVarType(P2.VarType) then
        if IsIntegerVarType(P1.VarType) then
          IntValue := P1.IntValue - P2.IntValue
        else
        begin
          case P1.VarType of
            vtBoolean: Value.CreateBoolean(False);
            vtChar, vtEnumeration:
            begin
              Assert(P1.UserType <> nil);
              Value.CreateTyped(P1.UserType, P1.ToInteger - P2.ToInteger);
            end;
          else
            Error := True;
          end;
          if not Error then
            EXIT;
        end
      else
        Error := True;
    end;
    opSucc:
    begin
      if IsIntegerVarType(P2.VarType) then
        if IsIntegerVarType(P1.VarType) then
          IntValue := P1.IntValue + P2.IntValue
        else
        begin
          case P1.VarType of
            vtBoolean: Value.CreateBoolean(True);
            vtChar, vtEnumeration:
            begin
              Assert(P1.UserType <> nil);
              Value.CreateTyped(P1.UserType, P1.ToInteger + P2.ToInteger);
            end;
          else
            Error := True;
          end;
          if not Error then
            EXIT;
        end
      else
        Error := True;
    end;
  else
    Evalled := False;
    EXIT;
  end;

  if VarType = vtUnknown then
    if IsNumericVarType(VarType) then
    begin
      VarType := Value.VarType;
      Result := ValueToVarType(IntValue, VarType);
      Value.CreateTyped(VarType, IntValue);
    end
    else
    begin
      Value.CreateTyped(P1.VarType, IntValue);
      Result := qeNone;
    end
  else
    Result := qeNone;

  if Error then
    EXIT(ErrOpUsageSub2(qeOpIncompatibleTypes, VarTypeToName(Param1.Imm.VarType),
      VarTypeToName(Param2.Imm.VarType), Op));
end;

function EvalTypecast(var Value: TImmValue; ToType: PUserType): TQuicheError;
var NewValue: Integer;
begin
  if IsOrdinalType(ToType) and (IsOrdinalType(Value.UserType)) then
  begin
    case GetTypeDataSize(ToType) of
      1:
      begin
        NewValue := Value.ToInteger and $ff;
        if IsSignedType(ToType) then
          if NewValue >= $80 then
            NewValue := NewValue or (-1 xor $ff);
      end;
      2:
      begin
        NewValue := Value.ToInteger and $ffff;
        if IsSignedType(ToType) then
          if NewValue >= $8000 then
            NewValue := NewValue or (-1 xor $ffff);
      end;
    else
      raise Exception.Create('Unknown type data size in EvalTypecast');
    end;
    Value.CreateTyped(ToType, NewValue);
    Result := qeNone;
  end
  else
    Assert(False, 'TODO: Typecasts for constant expression involving non-ordinal types');
end;

end.

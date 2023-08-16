unit Eval;

interface
uses SysUtils, ILData, MErrors, QTypes;

function EvalBi(OpIndex: Integer;Param1, Param2: PILParam;
  out Value: Integer;out RType: TVarType): TAssembleError;

implementation
uses Operators;

//If both operand are immediate values, and we have a suitable routine available,
//Evaluate LeftSlug := LeftSlug.Operand <LeftSlug.Operation> RightSlug.Operand
//Returns True if the operation was evaluated.
//If so, RightSlug is now spare
function EvalBi(OpIndex: Integer;Param1, Param2: PILParam;
  out Value: Integer;out RType: TVarType): TAssembleError;
var P1: Integer;  //First parameter value
  P1Type: TVarType;
  P2: Integer;    //Second parameter value
  P2Type: TVarType;
begin
  RType := vtUnknown;

  P1Type := Param1.ImmType;
  P2Type := Param2.ImmType;
  RType := vtUnknown;

  if (P1Type in NumericTypes) and (P2Type in NumericTypes) then
  begin
    P1 := ILParamValueToInteger(Param1);
    P2 := ILParamValueToInteger(Param2);

    if OpIndex = opIndexAdd then
      Value := P1 + P2
    else if OpIndex = opIndexSubtract then
      Value := P1 - P2
    else if OpIndex = opIndexMultiply then
      Value := P1 * P2
{    else if OpIndex = opIndexFloatDivide then
//      Value := P1 / P2
}    else if OpIndex = opIndexIntDivide then
      if P2 = 0 then
        EXIT(errDivByZero)
      else
        Value := P1 div P2
    else if OpIndex = opIndexMod then
      if P2 = 0 then
        EXIT(errDivByZero)
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

{
      opNOT: ;
      opNegate: ;
}    else
      raise Exception.Create('Unknown operation in Evaluate');
    if RType = vtUnknown then
      if Value < -32768 then
        EXIT(errConstantExpressionOverflow)
      else if Value < -128 then
        RType := vtInt16
      else if Value < 0 then
        RType := vtInt8
      else if Value < 256 then
        RType := vtByte
      else if Value <= 65535 then
        RType := vtWord
      else
        EXIT(errConstantExpressionOverflow);
  end
  else if (P1Type = vtBoolean) and (P2Type = vtBoolean) then
  begin
//    RType := vtBoolean;
  end
  else if (P1Type = vtChar) and (P2Type = vtChar) then
  begin
//    RType := vtBoolean;
  end;

  Result := errNone;
(*  end;
  Result := True;
  Param1.ImmValue := R and $ffff;
  Param2.Loc := locNone;
*)end;

end.

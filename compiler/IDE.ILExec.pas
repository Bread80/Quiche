unit IDE.ILExec;

interface
uses Classes,
  Def.IL, Def.Variables;

procedure Execute;

var ExecOutput: TStringList;

implementation
uses Generics.Collections, SysUtils,
  Def.Globals, Def.Operators, Def.VarTypes, Def.Consts;

const
  ValueToBool: array[valueTrue..valueFalse] of Boolean = (True, False);

var CurrIL: Integer;
  Trace: Boolean;

procedure RaiseError(Msg: String);
begin
  ExecOutput.Add(IntToStr(CurrIL) + '- ERROR: ' + Msg);
end;

procedure ExecTest(Test: Boolean;Msg: String);
begin
  if not Test then
    RaiseError(Msg);
end;

procedure ExecLog(Msg: String);
begin
  ExecOutput.Add(IntToStr(CurrIL) + '- ' + Msg);
end;

var TempVars: TList<PVariable>;

procedure ClearTempVars;
var V: PVariable;
begin
  for V in TempVars do
    Dispose(V);
  TempVars.Clear;
end;

function TempVarIndexToData(Index: Integer): PVariable;
begin
  if (Index >= TempVars.Count) or (TempVars[Index].VarType = vtUnknown) then
  begin
    RaiseError('TempVar accessed before creation');
    EXIT(nil);
  end
  else
    Result := TempVars[Index];
end;
(*
function TempVarCreate(Index: Integer): PVariable;
begin
  Result := nil;
  if TempVars.Count <= Index then
    while TempVars.Count <= Index do
    begin
      New(Result);
      TempVars.Add(Result);
      Result.Value := TImmValue.CreateTyped(vtInteger, 0);
      Result.VarType := vtUnknown;
      Result.Version := -1;
    end
  else
  begin
    Result := TempVars[Index];
    Result.Value := TImmValue.CreateTyped(vtInteger, 0);
    Result.VarType := vtUnknown;
  end;
end;
*)

function GetValue(ILItem: PILItem;Param: PILParam;var ValueType: TVarType;out SubMismatch: Boolean): Integer;
var Variable: PVariable;
begin
  Result := -1;
  case Param.Kind of
    pkNone: RaiseError('Attempt to read a param which is locNone');
    pkPhiVarSource:
    begin
      Variable := ILItem.Param3.PhiVar;
//      ExecTest(Variable.Sub = Sub, 'Variable Sub doesn''t match param Sub version (Phi node)');
      Result := Variable.Value.ToInteger;
      ValueType := Variable.VarType;
      SubMismatch := Variable.Version <> Param.VarVersion;
    end;
    pkImmediate:
    begin
//      Result := Param.ImmValueInt;
      ValueType := Param.Imm.VarType;
    end;
    pkVarSource:
    begin
      Variable := Param.Variable;
      ExecTest(Variable.Version = Param.VarVersion, 'Variable Sub doesn''t match param Sub version (normal node)');
      Result := Variable.Value.ToInteger;
      ValueType := Variable.VarType;
    end;
  else
    RaiseError('Unknown loc type for Param');
  end;
end;

{function GetValue1(ILItem: PILItem; var ValueType: TVarType;out SubMismatch: Boolean): Integer;
begin
  Result := GetValue(ILItem, ILItem.Param1Loc, ILItem.Param1Data, ILItem.Param1Sub, ValueType, SubMismatch);
end;

function GetValue2(ILItem: PILItem; var ValueType: TVarType;out SubMismatch: Boolean): Integer;
begin
  Result := GetValue(ILItem, ILItem.Param2Loc, ILItem.Param2Data, ILItem.Param2Sub, ValueType, SubMismatch);
end;
}
function BlockIDToILIndex(BlockID: Integer): Integer;
var ILItem: PILItem;
begin
  for Result := 0 to ILGetCount-1 do
  begin
    ILItem := ILIndexToData(Result);
    if ILItem.BlockID = BlockID then
      EXIT;
  end;
  RaiseError('BlockID not found');
  Result := -1;
end;

function BoolNot(Value: Integer): Integer;
begin
  Result := 1-Value;
end;

function ExecILItem(Index: Integer): Integer;
{var ILItem: PILItem;
  PrevBlock: Integer;
  Value: Integer;
  ValueType: TVarType;
  ValueType2: TVarType;
  Variable: PVariable;
  SubMismatch: Boolean;
  Op: POpData;
}begin
{  ILItem := ILIndexToData(Index);
  if ILItem.BlockID >= 0 then
  begin
//    PrevBlock := CurrBlock;
    CurrBlock := ILItem.BlockID;
  end;

}  Result := Index + 1;
{
//  Op := @Operations[ILItem.Op];
  if ILItem.Op = opBranch then
    //Unconditional branch - don't evaluate parameters
  else
  begin
    Value := GetValue(ILItem, @ILItem.Param1, ValueType, SubMismatch);

(*    case Op.Op of
      //System operations
      opNone: ; //Nothing
      opAssign:
        ExecTest(ILItem.Param2.Loc = locNone, 'Assign op but Param2 is not locNone');
      opPhi:
      begin
        ExecTest(ILItem.Param1.Loc in [locPhiVar], 'Param1 of a Phi node must be locPhiVar');
        if PrevBlock = ILItem.Param1.PhiBlockID then
          ExecTest(not SubMismatch, 'Variable Sub doesn''t match param Sub version (Phi node)')
          //(Param1)
        else
        begin
          ExecTest(ILItem.Param2.Loc in [locPhiVar], 'Param2 of a Phi node must be locPhiVar');
          if PrevBlock = ILItem.Param2.PhiBlockID then
          begin
            Value := GetValue(ILItem, @ILItem.Param2, ValueType, SubMismatch);
            ExecTest(not SubMismatch, 'Variable Sub doesn''t match param Sub version (Phi node)');
          end
          else
            RaiseError('Neither block of the Phi node matches the previous Block ID');
        end;
      end;

      //Conditionals
      opEqual, opNotEqual:
      begin
        Value := BoolToValue[Value = GetValue(ILItem, @ILItem.Param2, ValueType2, SubMismatch)];
        ExecTest(ValueType = ValueType2, 'Type mismatch');
        ValueType := vtBoolean;
        if Op.Op = opNotEqual then
          Value := BoolNot(Value);
      end;
      opLess, opGreaterEqual:
      begin
        Value := BoolToValue[Value < GetValue(ILItem, @ILItem.Param2, ValueType2, SubMismatch)];
        ExecTest(ValueType = ValueType2, 'Type mismatch');
        ValueType := vtBoolean;
        if Op.Op = opGreaterEqual then
          Value := BoolNot(Value);
      end;
      opGreater, opLessEqual:
      begin
        Value := BoolToValue[Value > GetValue(ILItem, @ILItem.Param2, ValueType2, SubMismatch)];
        ExecTest(ValueType = ValueType2, 'Type mismatch');
        ValueType := vtBoolean;
        if Op.Op = opLessEqual then
          Value := BoolNot(Value);
      end;

      //Maths
      opAdd:
      begin
        Value := Value + GetValue(ILItem, @ILItem.Param2, ValueType2, SubMismatch);
        ExecTest((ValueType = ValueType2) and (ValueType = vtInteger), 'Type mismatch');
      end;
      opSubtract:
      begin
        Value := Value - GetValue(ILItem, @ILItem.Param2, ValueType2, SubMismatch);
        ExecTest((ValueType = ValueType2) and (ValueType = vtInteger), 'Type mismatch');
      end;
      opMultiply:
      begin
        Value := Value * GetValue(ILItem, @ILItem.Param2, ValueType2, SubMismatch);
        ExecTest((ValueType = ValueType2) and (ValueType = vtInteger), 'Type mismatch');
      end;
      {opDivide, }{opDiv:
      begin
        Value := Value div GetValue(ILItem, @ILItem.Param2, ValueType2, SubMismatch);
        ExecTest((ValueType = ValueType2) and (ValueType = vtInteger), 'Type mismatch');
      end;
      opMod:
      begin
        Value := Value mod GetValue(ILItem, @ILItem.Param2, ValueType2, SubMismatch);
        ExecTest((ValueType = ValueType2) and (ValueType = vtInteger), 'Type mismatch');
      end;

      //Logical/bitwise
      opOR:
      begin
        Value := Value or GetValue(ILItem, @ILItem.Param2, ValueType2, SubMismatch);
        ExecTest((ValueType = ValueType2) and (ValueType in [vtInteger, vtBoolean]), 'Type mismatch');
      end;
      opXOR:
      begin
        Value := Value xor GetValue(ILItem, @ILItem.Param2, ValueType2, SubMismatch);
        ExecTest((ValueType = ValueType2) and (ValueType in [vtInteger, vtBoolean]), 'Type mismatch');
      end;
      opAnd:
      begin
        Value := Value and GetValue(ILItem, @ILItem.Param2, ValueType2, SubMismatch);
        ExecTest((ValueType = ValueType2) and (ValueType in [vtInteger, vtBoolean]), 'Type mismatch');
      end;

      //Misc
      opShr:
      begin
        Value := Value shr GetValue(ILItem, @ILItem.Param2, ValueType2, SubMismatch);
        ExecTest((ValueType = ValueType2) and (ValueType = vtInteger), 'Type mismatch');
      end;
      opShl:
      begin
        Value := Value shl GetValue(ILItem, @ILItem.Param2, ValueType2, SubMismatch);
        ExecTest((ValueType = ValueType2) and (ValueType = vtInteger), 'Type mismatch');
      end;

      opIn, opUnaryPlus, opNegate, opNot, opAt: RaiseError('Operation not implemented'); //Todo
    else
      RaiseError('Unkown Operation');
    end;
*)  end;

  //Assign result to Dest/Do branch
      case ILItem.Dest.Kind of
        pkNone: RaiseError('Dest assignment to <locNone>');
        pkPhiVarDest:
        begin
          Variable := ILItem.Param3.PhiVar;
          Variable.Value := TImmValue.CreateInteger(Value);
          Variable.VarType := ValueType;
          Variable.WriteCount := ILItem.Param3.PhiDestVersion;
        end;
        pkVarDest:
        begin
          Variable := ILItem.Param3.Variable;
          Variable.Value := TImmValue.CreateInteger(Value);
          Variable.VarType := ValueType;
          Variable.WriteCount := ILItem.Param3.VarVersion;
        end;
        pkImmediate: RaiseError('Dest assignment to <locImmediate>');
      else
        RaiseError('Unknown Dest type');
      end;
{    dtCondBranch:
    begin
      if ILItem.FalseBlockID < 0 then
        //Unconditional jump
        Result := BlockIDToILIndex(ILItem.TrueBlockID)
      else if ValueType <> vtBoolean then
        RaiseError('Attempting conditional branch but Value is not vtBoolean')
      else
        if Value = valueTrue then
          Result := BlockIDToILIndex(ILItem.TrueBlockID)
        else
          Result := BlockIDToILIndex(ILItem.FalseBlockID);
    end;
}
end;



procedure Execute;
begin
//  Trace := True;

  ExecOutput.Clear;
  Vars.ExecClear;
  ClearTempVars;

  CurrIL := 0;

  while (CurrIL >= 0) and (CurrIL < ILGetCount) do
  begin
    if Trace then
      ExecLog(IntToStr(CurrIL));
    CurrIL := ExecILItem(CurrIL);
  end;

  //After execution tests
  if CurrIL < 0 then
    RaiseError('Execution failed')
  else
    ExecLog('Execution completed');
end;

initialization
  ExecOutput := TStringList.Create;
  TempVars := TList<PVariable>.Create;
  Trace := False;
end.

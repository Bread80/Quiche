unit ILExec;

interface
uses ILData, Variables, Classes;

const
  valFalse = 0;
  valTrue = 1;
  BoolToValue: array[False..True] of Integer = (valFalse, valTrue);
  ValueToBool: array[valFalse..valTrue] of Boolean = (False, True);

procedure Execute;

var ExecOutput: TStringList;

implementation
uses Generics.Collections, SysUtils;

var CurrBlock: Integer;
  CurrIL: Integer;
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

function TempVarCreate(Index: Integer): PVariable;
begin
  if TempVars.Count <= Index then
    while TempVars.Count <= Index do
    begin
      New(Result);
      TempVars.Add(Result);
      Result.ValueInt := 0;
      Result.VarType := vtUnknown;
      Result.Sub := -1;
    end
  else
  begin
    Result := TempVars[Index];
    Result.ValueInt := 0;
    Result.VarType := vtUnknown;
  end;
end;

function GetValue(ILItem: PILItem;Loc: TILLocation;Data, Sub: Integer;var ValueType: TVarType;out SubMismatch: Boolean): Integer;
var Variable: PVariable;
begin
  case Loc of
    locNone: RaiseError('Attempt to read a param which is locNone');
    locPhiVar:
    begin
      Variable := VarIndexToData(ILItem.DestData);
//      ExecTest(Variable.Sub = Sub, 'Variable Sub doesn''t match param Sub version (Phi node)');
      Result := Variable.ValueInt;
      ValueType := Variable.VarType;
      SubMismatch := Variable.Sub <> Sub;
    end;
    locImmediate:
    begin
      Result := Data;
      ValueType := vtInteger;
    end;
    locVar:
    begin
      Variable := VarIndexToData(Data);
      ExecTest(Variable.Sub = Sub, 'Variable Sub doesn''t match param Sub version (normal node)');
      Result := Variable.ValueInt;
      ValueType := Variable.VarType;
    end;
    locTemp:
    begin
      Variable := TempVarIndexToData(Data);
      ExecTest(Assigned(Variable), 'Temp variable not assigned yet');
      Result := Variable.ValueInt;
      ValueType := Variable.VarType;
    end;
  else
    RaiseError('Unknown loc type for Param');
  end;
end;

function GetValue1(ILItem: PILItem; var ValueType: TVarType;out SubMismatch: Boolean): Integer;
begin
  Result := GetValue(ILItem, ILItem.Param1Loc, ILItem.Param1Data, ILItem.Param1Sub, ValueType, SubMismatch);
end;

function GetValue2(ILItem: PILItem; var ValueType: TVarType;out SubMismatch: Boolean): Integer;
begin
  Result := GetValue(ILItem, ILItem.Param2Loc, ILItem.Param2Data, ILItem.Param2Sub, ValueType, SubMismatch);
end;

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
var ILItem: PILItem;
  PrevBlock: Integer;
  Value: Integer;
  ValueType: TVarType;
  ValueType2: TVarType;
  Variable: PVariable;
  SubMismatch: Boolean;
begin
  ILItem := ILIndexToData(Index);
  if ILItem.BlockID >= 0 then
  begin
    PrevBlock := CurrBlock;
    CurrBlock := ILItem.BlockID;
  end;

  Result := Index + 1;

  if (ILItem.DestType = dtBranch) and (ILItem.FalseBlock = -1) then
    //Unconditional branch - don't evaluate parameters
  else
  begin
    Value := GetValue1(ILItem, ValueType, SubMismatch);

    case ILItem.Op of
      //System operations
      opNone: ; //Nothing
      opAssign:
        ExecTest(ILItem.Param2Loc = locNone, 'Assign op but Param2 is not locNone');
      opPhi:
      begin
        ExecTest(ILItem.Param1Loc in [locPhiVar], 'Param1 of a Phi node must be locPhiVar');
        if PrevBlock = ILItem.Param1Data then
          ExecTest(not SubMismatch, 'Variable Sub doesn''t match param Sub version (Phi node)')
          //(Param1)
        else
        begin
          ExecTest(ILItem.Param2Loc in [locPhiVar], 'Param2 of a Phi node must be locPhiVar');
          if PrevBlock = ILItem.Param2Data then
          begin
            Value := GetValue2(ILItem, ValueType, SubMismatch);
            ExecTest(not SubMismatch, 'Variable Sub doesn''t match param Sub version (Phi node)');
          end
          else
            RaiseError('Neither block of the Phi node matches the previous Block ID');
        end;
      end;

      //Conditionals
      opEqual, opNotEqual:
      begin
        Value := BoolToValue[Value = GetValue2(ILItem, ValueType2, SubMismatch)];
        ExecTest(ValueType = ValueType2, 'Type mismatch');
        ValueType := vtBoolean;
        if ILItem.Op = opNotEqual then
          Value := BoolNot(Value);
      end;
      opLess, opGreaterEqual:
      begin
        Value := BoolToValue[Value < GetValue2(ILItem, ValueType2, SubMismatch)];
        ExecTest(ValueType = ValueType2, 'Type mismatch');
        ValueType := vtBoolean;
        if ILItem.Op = opGreaterEqual then
          Value := BoolNot(Value);
      end;
      opGreater, opLessEqual:
      begin
        Value := BoolToValue[Value > GetValue2(ILItem, ValueType2, SubMismatch)];
        ExecTest(ValueType = ValueType2, 'Type mismatch');
        ValueType := vtBoolean;
        if ILItem.Op = opLessEqual then
          Value := BoolNot(Value);
      end;

      //Maths
      opAdd:
      begin
        Value := Value + GetValue2(ILItem, ValueType2, SubMismatch);
        ExecTest((ValueType = ValueType2) and (ValueType = vtInteger), 'Type mismatch');
      end;
      opSubtract:
      begin
        Value := Value - GetValue2(ILItem, ValueType2, SubMismatch);
        ExecTest((ValueType = ValueType2) and (ValueType = vtInteger), 'Type mismatch');
      end;
      opMultiply:
      begin
        Value := Value * GetValue2(ILItem, ValueType2, SubMismatch);
        ExecTest((ValueType = ValueType2) and (ValueType = vtInteger), 'Type mismatch');
      end;
      opDivide, opDiv:
      begin
        Value := Value div GetValue2(ILItem, ValueType2, SubMismatch);
        ExecTest((ValueType = ValueType2) and (ValueType = vtInteger), 'Type mismatch');
      end;
      opMod:
      begin
        Value := Value mod GetValue2(ILItem, ValueType2, SubMismatch);
        ExecTest((ValueType = ValueType2) and (ValueType = vtInteger), 'Type mismatch');
      end;

      //Logical/bitwise
      opOR:
      begin
        Value := Value or GetValue2(ILItem, ValueType2, SubMismatch);
        ExecTest((ValueType = ValueType2) and (ValueType in [vtInteger, vtBoolean]), 'Type mismatch');
      end;
      opXOR:
      begin
        Value := Value xor GetValue2(ILItem, ValueType2, SubMismatch);
        ExecTest((ValueType = ValueType2) and (ValueType in [vtInteger, vtBoolean]), 'Type mismatch');
      end;
      opAnd:
      begin
        Value := Value and GetValue2(ILItem, ValueType2, SubMismatch);
        ExecTest((ValueType = ValueType2) and (ValueType in [vtInteger, vtBoolean]), 'Type mismatch');
      end;

      //Misc
      opShr:
      begin
        Value := Value shr GetValue2(ILItem, ValueType2, SubMismatch);
        ExecTest((ValueType = ValueType2) and (ValueType = vtInteger), 'Type mismatch');
      end;
      opShl:
      begin
        Value := Value shl GetValue2(ILItem, ValueType2, SubMismatch);
        ExecTest((ValueType = ValueType2) and (ValueType = vtInteger), 'Type mismatch');
      end;

      opIn, opUnaryPlus, opNegate, opNot, opAt: RaiseError('Operation not implemented'); //Todo
    else
      RaiseError('Unkown Operation');
    end;
  end;

  //Assign result to Dest/Do branch
  case ILItem.DestType of
    dtData:
      case ILItem.DestLoc of
        locNone: RaiseError('Dest assignment to <locNone>');
        locOUT: ExecLog('OUT: ' + IntToStr(Value));
        locPhiVar, locVar:
        begin
          Variable := VarIndexToData(ILItem.DestData);
          Variable.ValueInt := Value;
          Variable.VarType := ValueType;
          Variable.Sub := ILItem.DestSub;
        end;
        locImmediate: RaiseError('Dest assignment to <locImmediate>');
        locTemp:
        begin
          Variable := TempVarCreate(ILItem.DestData);
          Variable.ValueInt := Value;
          Variable.VarType := ValueType;
        end;
      else
        RaiseError('Unknown Dest type');
      end;
    dtBranch:
    begin
      if ILItem.FalseBlock < 0 then
        //Unconditional jump
        Result := BlockIDToILIndex(ILItem.TrueBlock)
      else if ValueType <> vtBoolean then
        RaiseError('Attempting conditional branch but Value is not vtBoolean')
      else
        if Value = valTrue then
          Result := BlockIDToILIndex(ILItem.TrueBlock)
        else
          Result := BlockIDToILIndex(ILItem.FalseBlock);
    end;
  else
    RaiseError('Unknown DestType');
  end;
end;



procedure Execute;
begin
//  Trace := True;

  ExecOutput.Clear;
  VarsExecClear;
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

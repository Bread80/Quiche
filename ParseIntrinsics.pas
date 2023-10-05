(*
Intrinsics are functions and procedures which generate inline code or call core
library rouitines, as opposed to those which generate a (stack framed) function call
*)
unit ParseIntrinsics;

interface
uses ParseErrors, ParseExpr;

//Parse an intinsic operator/function once it has been found in the operators table.
//OpIndex is the index identifying the operator.
//If AssignReturn is true then the parser is expecting a return value which it can
//assign or use in an expression etc. If AssignReturn is True and the operation is a procedure
//then an error will be raised.
//If a value is being returned, it must be returned in Slug.
function ParseIntrinsic(OpIndex: Integer;AssignReturn: Boolean;var Slug: TExprSlug): TQuicheError;

implementation
uses SysUtils,
  SourceReader, QTypes, ILData, Variables, Operators, Eval, ParserBase;

//===============================================
//Intrinsic functions

//Write and Writeln are special cases!
function ParseWrite(NewLine: Boolean): TQuicheError;
var Ch: Char;
  Brace: Boolean; //Is parameter list wrapped in braces?
  ILItem: PILItem;
  VType: TVarType;
begin
  //!!Don't skip whitespace: Brace indicating parameter list must come immediately after function
  Ch := Parser.TestChar;
  //Parameters?
  if not Parser.EOS then
  begin
    Brace := Ch = '(';
    if Brace then
      Parser.SkipChar;

    //Empty parameter list?
    Ch := Parser.TestChar;
    if (not Brace) or (Brace and (Ch <> ')')) then
    repeat
      VType := vtUnknown;
      Result := ParseExprToILItem(ILItem, VType);
      if Result <> qeNone then
        EXIT;
      case VType of
        vtChar:
        begin
          ILItem.DestType := dtNone;  //No result
          ILItem.Dest.Loc := locNone; //No result
          ILItem.OpIndex := OpIndexWriteChar;
          ILItem.OpType := VarTypeToOpType(VType);
        end;
        vtInteger, vtInt8, vtByte, vtWord:
        begin
          ILItem.DestType := dtNone;  //No result
          ILItem.Dest.Loc := locNone; //No result
          ILItem.OpIndex := OpIndexWriteInteger;
          ILItem.OpType := VarTypeToOpType(VType);
        end;
        vtBoolean:
        begin
          ILItem.DestType := dtNone;  //No result
          ILItem.Dest.Loc := locNone; //No result
          ILItem.OpIndex := OpIndexWriteBoolean;
          ILItem.OpType := VarTypeToOpType(VType);
        end;
      else
        EXIT(ErrMsg(qeTodo, 'Unhandled parameter type for Write/ln: ' + VarTypeToName(VType)));
      end;

      Parser.SkipWhiteSpace;
      Ch := Parser.TestChar;
      if Ch = ',' then
        Parser.SkipChar;
    until Ch <> ',';

    if Brace then
      if Ch = ')' then
        Parser.SkipChar
      else
        EXIT(ErrMsg(qeSyntax, ermCommaOrCloseParensExpected));
  end;

  if NewLine then
  begin
    ILItem := ILAppend(dtNone);
     ILItem.DestType := dtNone;  //No result
     ILItem.Dest.Loc := locNone; //No result
     ILItem.OpIndex := OpIndexWriteNewLine;
     ILItem.OpType := rtUnknown;
  end;
end;

//Read parameter(s) of an intrinsic
function ParseParams(OpIndex: Integer;OpData: POperator;var HaveParam2: Boolean;var Slug1, Slug2: TExprSlug): TQuicheError;
var Ch: Char;
  Brace: Boolean; //Is parameter list wrapped in braces?
  ExprType1, ExprType2: TVarType;
begin
  //!!Don't skip whitespace. Brace indicating parameter list must come immediate after identifier
  Ch := Parser.TestChar;
  //Parameters?
  if Parser.EOS then
    EXIT(ErrOpUsage(ermParameterListExpected, OpIndex));

  Brace := Ch = '(';
  if Brace then
    Parser.SkipChar;

  //Read first parameter
  ExprType1 := vtUnknown;
  Result := ParseExpressionToSlug(Slug1, ExprType1);
  if Result <> qeNone then
    EXIT;

  Parser.SkipWhiteSpace;

  //Second parameter? - TODO Add support for optional second parameter - OpType.RTypes includes vtUnknown?
  if opfP2Optional in OpData.FuncFlags then
    //Optional second parameter
    HaveParam2 := Parser.TestChar = ','
  else  //Compulsory second parameter
    HaveParam2 := OpData.RTypes <> [];

  if HaveParam2 then
  begin
    if Parser.TestChar <> ',' then
      EXIT(ErrOpUsage(ermIncorrectParameterCount, OpIndex));
    Parser.SkipChar;
    ExprType2 := vtUnknown;
    Result := ParseExpressionToSlug(Slug2, ExprType2);

    Parser.SkipWhiteSpace;
  end;

  if Brace then
    if Parser.TestChar <> ')' then
      EXIT(ErrOpUsage(ermCommaOrCloseParensExpected, OpIndex))
    else
    begin
      Parser.SkipChar;
      Parser.SkipWhiteSpace;
    end
  else
    if Parser.TestChar = ',' then
      EXIT(ErrOpUsage(ermIncorrectParameterCount, OpIndex));
end;

function ValidateParams(OpIndex: Integer;OpData: POperator;HaveParam2: Boolean;const Slug1, Slug2: TExprSlug): TQuicheError;
begin
  //====Validate parameters
  if opfP1Variable in OpData.FuncFlags then
    if (Slug1.ILItem <> nil) or not (Slug1.Operand.Loc in [locVar]) then
      EXIT(ErrOpUsage('First parameter must be a variable reference', OpIndex));
  if HaveParam2 and (opfP2Immediate in OpData.FuncFlags) then
    if (Slug2.ILItem <> nil) or (Slug2.Operand.Loc <> locImmediate) then
      EXIT(ErrOpUsage('Second parameter must be a constant or constant expression', OpIndex));
  if opfP1Bitsize16Only in OpData.FuncFlags then
    if Slug1.Operand.Loc <> locImmediate then
      if GetTypeSize(ILParamToVarType(@Slug1.Operand)) <> 2 then
        EXIT(ErrOpUsage('First parameter must be a 16-bit value', OpIndex));

  //Validate parameter type matches types available for the operation
  if FindAssignmentTypes(OpData.LTypes, ILParamToVarType(@Slug1.Operand)) = vtUnknown then
    EXIT(ErrMsg(qeTypeMismatch, 'Expression type (' + VarTypeToName(ILParamToVarType(@Slug1.Operand)) + ') ' +
      'incompatible with operator (' + TypeEnumSetToString(OpData.LTypes) + ') in first parameter'));
  if HaveParam2 then
    if FindAssignmentTypes(OpData.LTypes, ILParamToVarType(@Slug2.Operand)) = vtUnknown then
      EXIT(ErrMsg(qeTypeMismatch, 'Expression type (' + VarTypeToName(ILParamToVarType(@Slug2.Operand)) + ') ' +
        'incompatible with operator (' + TypeEnumSetToString(OpData.RTypes) + ') in second parameter'));
end;

//Handle generic intrinsics which follow the standard syntax etc rules defined in
//the operators table.
function ParseGenericIntrinsic(OpIndex: Integer;AssignReturn: Boolean;var Slug: TExprSlug): TQuicheError;
var
  OpData: POperator;
  ILItem: PILItem;
  VType: TVarType;
  Slug1, Slug2: TExprSlug;
  HaveParam2: Boolean;
  Value: Integer;
  RType: TVarType;
begin
  Assert(OpIndex <> -1,'Unknown operator');
  OpData := OpIndexToData(OpIndex);

  Result := ParseParams(OpIndex, OpData, HaveParam2, Slug1, Slug2);
  if Result <> qeNone then
    EXIT;

  Result := ValidateParams(OpIndex, OpData, HaveParam2, Slug1, Slug2);
  if Result <> qeNone then
    EXIT;


  //Are (both) parameters immediates? If so evaluate
  if (Slug1.ILItem = nil) and (Slug1.Operand.Loc = locImmediate) then
  begin
    if HaveParam2 then
    begin
      if (Slug2.ILItem = nil) and (Slug2.Operand.Loc = locImmediate) then
        Result := EvalIntrinsicBi(OpIndex, Slug1.Operand, Slug2.Operand,
          Value, RType);
        if Result <> qeNone then
          EXIT;
    end
    else
    begin
      Result := EvalIntrinsicUnary(OpIndex, @Slug1.Operand,
        Value, RType);
      if Result <> qeNone then
        EXIT;
    end;
    if RType <> vtUnknown then
    begin
      //TODO: Assign data to slug
      Slug.Operand.Loc := locImmediate;
      Slug.Operand.ImmValue := Value;
      Slug.Operand.ImmType := RType;
      Slug.OpIndex := OpIndexNone;
      Slug.ResultType := RType;
      Slug.ImplicitType := RType;
      EXIT(qeNone);
    end;
  end;

  //Optimisations for large Inc and Dec values
  if (OpIndex = OpIndexInc) or (OpIndex = OpIndexDec) then
    if Slug2.Operand.Loc = locImmediate then
    begin
      if abs(ILParamValueToInteger(@Slug2.Operand)) >= iConvertIncToAddThreshhold then
        if OpIndex = OpIndexInc then
          OpIndex := OpIndexAdd
        else
          OpIndex := OpIndexSubtract;
        //DO NOT update OpData - we need data from original operation.
        //(but we need OpIndex) from new operation
    end;

  //Create ILData or ExprSlug
  if Slug1.ILItem <> nil then
    SlugAssignToTempVar(Slug1);
  if HaveParam2 and (Slug2.ILItem <> nil) then
    SlugAssignToTempVar(Slug2);
  if AssignReturn or (opfp1Variable in OpData.FuncFlags) then
  begin //Return a slug for assignment/remainder of expression
    ILItem := ILAppend(dtData);
    Slug.Operand.Loc := locNone;
    Slug.ILItem := ILItem;
    Slug.OpIndex := OpIndexNone;
    Slug.OpData := nil;
    Slug.Negate := False;
    Slug.Invert := False;
    //Dest to be assigned by caller
  end
  else
  begin //Append the function call to ILList, nothing to return
    ILItem := ILAppend(dtNone);
  end;
  ILItem.OpIndex := OpIndex;
  if opfNoOverflowChecks in OpData.FuncFlags then
    ILItem.CodeGenFlags := ILItem.CodeGenFlags - [cgOverFlowCheck];

  ILItem.Param1 := Slug1.Operand;
  if HaveParam2 then
    ILItem.Param2 := Slug2.Operand;
  ILItem.OpType := VarTypeToOpType(Slug1.ResultType);
  //TODO: Expand OpoType to suit types available in OpData
  //AssignSlugTypes?? But what about x16??
//  if not AssignSlugTypes(Slug1, Slug2) then
//    EXIT(ErrOpUsage(qeTypeMismatch)

  if OpData.ResultSame then
    ILItem.ResultType := VarTypeToOpType(Slug1.ResultType)
  else
    ILItem.ResultType := TypeEnumToOpType[OpData.ResultType];

  if AssignReturn then
  begin
    if OpData.ResultSame then
      Slug.ResultType := Slug1.ResultType
    else
      Slug.ResultType := TypeEnumToVarType[OpData.ResultType];
    Slug.ImplicitType := Slug.ResultType;
    Slug.OpType := ILItem.OpType;
    Slug.ParamOrigin := poExplicit;
  end
  else if opfp1Variable in OpData.FuncFlags then
  begin //Param1 is a variable and we're assigning the result to it.
    ILItem.Dest.Loc := ILItem.Param1.Loc;
    case ILItem.Param1.Loc of
      locVar:
      begin
        ILItem.Dest.VarIndex := ILItem.Param1.VarIndex;
        ILItem.Dest.VarSub := VarIndexIncWriteCount(ILItem.Param1.VarIndex);
      end;
    else
      raise Exception.Create('Unknown Param Loc in Instrinsic Assign');
    end;
  end;
end;

function ParseIntrinsic(OpIndex: Integer;AssignReturn: Boolean;var Slug: TExprSlug): TQuicheError;
begin
  if OpIndex = OpIndexWrite then
    if AssignReturn then
      EXIT(ErrOpUsage(ermCantAssignProcedure, OpIndex))
    else
      EXIT(ParseWrite(False));
  if OpIndex = OpIndexWriteLn then
    if AssignReturn then
      EXIT(ErrOpUsage(ermCantAssignProcedure, OpIndex))
    else
      EXIT(ParseWrite(True));

  Result := ParseGenericIntrinsic(OpIndex, AssignReturn, Slug);
end;

end.

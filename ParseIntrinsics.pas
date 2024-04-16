(*
Intrinsics are functions and procedures which generate inline code or call core
library rouitines, as opposed to those which generate a (stack framed) function call
*)
unit ParseIntrinsics;

interface
uses ParseErrors, ParseExpr, Operators;

//Parse an intinsic operator/function once it has been found in the operators table.
//OpIndex is the index identifying the operator.
//If AssignReturn is true then the parser is expecting a return value which it can
//assign or use in an expression etc. If AssignReturn is True and the operation is a procedure
//then an error will be raised.
//If a value is being returned, it must be returned in Slug.
function ParseIntrinsic(Op: TOperator;AssignReturn: Boolean;var Slug: TExprSlug): TQuicheError;

implementation
uses SysUtils,
  SourceReader, QTypes, ILData, Variables, Eval, ParserBase;

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
          ILItem.Op := OpWriteChar;
        vtInteger, vtInt8, vtByte, vtWord:
          ILItem.Op := OpWriteInteger;
        vtBoolean:
          ILItem.Op := OpWriteBoolean;
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
    ILItem := ILAppend(OpWriteNewLine);
  Result := qeNone;
end;

//Read parameter(s) of an intrinsic
{function ParseParams(Op: TOperator;var HaveParam2: Boolean;var Slug1, Slug2: TExprSlug): TQuicheError;
var Ch: Char;
  Brace: Boolean; //Is parameter list wrapped in braces?
  ExprType1, ExprType2: TVarType;
  OpData: POpData;
begin
  OpData := @Operations[Op];
  //!!Don't skip whitespace. Brace indicating parameter list must come immediate after identifier
  Ch := Parser.TestChar;
  //Parameters?
  if Parser.EOS then
    EXIT(ErrOpUsage(ermParameterListExpected, Op));

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
      EXIT(ErrOpUsage(ermIncorrectParameterCount, Op));
    Parser.SkipChar;
    ExprType2 := vtUnknown;
    Result := ParseExpressionToSlug(Slug2, ExprType2);

    Parser.SkipWhiteSpace;
  end;

  if Brace then
    if Parser.TestChar <> ')' then
      EXIT(ErrOpUsage(ermCommaOrCloseParensExpected, Op))
    else
    begin
      Parser.SkipChar;
      Parser.SkipWhiteSpace;
    end
  else
    if Parser.TestChar = ',' then
      EXIT(ErrOpUsage(ermIncorrectParameterCount, Op));
end;
}
{function ValidateParams(Op: TOperator;HaveParam2: Boolean;const Slug1, Slug2: TExprSlug): TQuicheError;
var OpData: POpData;
begin
  OpData := @Operations[Op];
  //====Validate parameters
  if opfP1Variable in OpData.FuncFlags then
    if (Slug1.ILItem <> nil) or not (Slug1.Operand.Kind in [pkVar]) then
      EXIT(ErrOpUsage('First parameter must be a variable reference', Op));
  if HaveParam2 and (opfP2Immediate in OpData.FuncFlags) then
    if (Slug2.ILItem <> nil) or (Slug2.Operand.Kind <> pkImmediate) then
      EXIT(ErrOpUsage('Second parameter must be a constant or constant expression', Op));
  if opfP1Bitsize16Only in OpData.FuncFlags then
    if Slug1.Operand.Kind <> pkImmediate then
      if GetTypeSize(Slug1.Operand.GetVarType) <> 2 then
        EXIT(ErrOpUsage('First parameter must be a 16-bit value', Op));

  //Validate parameter type matches types available for the operation
  if FindAssignmentTypes(OpData.LTypes, Slug1.Operand.GetVarType) = vtUnknown then
    EXIT(ErrMsg(qeTypeMismatch, 'Expression type (' + VarTypeToName(Slug1.Operand.GetVarType) + ') ' +
      'incompatible with operator (' + TypeEnumSetToString(OpData.LTypes) + ') in first parameter'));
  if HaveParam2 then
    if FindAssignmentTypes(OpData.LTypes, Slug2.Operand.GetVarType) = vtUnknown then
      EXIT(ErrMsg(qeTypeMismatch, 'Expression type (' + VarTypeToName(Slug2.Operand.GetVarType) + ') ' +
        'incompatible with operator (' + TypeEnumSetToString(OpData.RTypes) + ') in second parameter'));

  Result := qeNone;
end;
}
//Handle generic intrinsics which follow the standard syntax etc rules defined in
//the operators table.
{function ParseGenericIntrinsic(Op: TOperator;AssignReturn: Boolean;var Slug: TExprSlug): TQuicheError;
var
  OpData: POpData;
  ILItem: PILItem;
  Slug1, Slug2: TExprSlug;
  HaveParam2: Boolean;
  Value: Integer;
  RType: TVarType;
begin
  Slug1.Initialise;
  Slug2.Initialise;

//  Assert(Op <> -1,'Unknown operator');
  OpData := @Operations[Op];

  Result := ParseParams(Op, HaveParam2, Slug1, Slug2);
  if Result <> qeNone then
    EXIT;

  Result := ValidateParams(Op, HaveParam2, Slug1, Slug2);
  if Result <> qeNone then
    EXIT;


  //Are (both) parameters immediates? If so evaluate
  if (Slug1.ILItem = nil) and (Slug1.Operand.Kind = pkImmediate) then
  begin
    if HaveParam2 then
    begin
      if (Slug2.ILItem = nil) and (Slug2.Operand.Kind = pkImmediate) then
        Result := EvalIntrinsicBi(Op, Slug1.Operand, Slug2.Operand,
          Value, RType);
        if Result <> qeNone then
          EXIT;
    end
    else
    begin
      Result := EvalIntrinsicUnary(Op, Slug1.Operand, Value, RType);
      if Result <> qeNone then
        EXIT;
    end;
    if RType <> vtUnknown then
    begin
      //TODO: Assign data to slug
      Slug.SetImmediate(Value, RType);
      Slug.Op := OpUnknown;
      Slug.ImplicitType := RType;
      EXIT(qeNone);
    end;
  end;

  //Optimisations for large Inc and Dec values
  if Op in [OpInc, OpDec] then
    if Slug2.Operand.Kind = pkImmediate then
    begin
      if abs(Slug2.Operand.ImmToInteger) >= iConvertIncToAddThreshhold then
        if Op = OpInc then
          Op := OpAdd
        else
          Op := OpSubtract;
        //DO NOT update OpData - we need data from original operation.
        //(but we need OpIndex) from new operation
    end;

  //Create ILData or ExprSlug
  if Slug1.ILItem <> nil then
    Slug1.AssignToHiddenVar;
  if HaveParam2 and (Slug2.ILItem <> nil) then
    Slug2.AssignToHiddenVar;
  if AssignReturn or (opfp1Variable in OpData.FuncFlags) then
  begin //Return a slug for assignment/remainder of expression
    ILItem := ILAppend(dtData, Op);
    Slug.Operand.Kind := pkNone;
    Slug.ILItem := ILItem;
    Slug.Op := OpUnknown;
//    Slug.OpData := nil;
    Slug.Negate := False;
    Slug.Invert := False;
    //Dest to be assigned by caller
  end
  else
  begin //Append the function call to ILList, nothing to return
    ILItem := ILAppend(dtNone, Op);
  end;
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
    Assert(ILItem.Param1.Kind = pkVar);
    ILItem.Dest.SetVarAndSub(ILItem.Param1.Variable, ILItem.Param1.Variable.IncWriteCount);
  end;
end;
}
function ParseIntrinsic(Op: TOperator;AssignReturn: Boolean;var Slug: TExprSlug): TQuicheError;
begin
  if Op = OpWrite then
    if AssignReturn then
      EXIT(ErrOpUsage(ermCantAssignProcedure, Op))
    else
      EXIT(ParseWrite(False));
  if Op = OpWriteLn then
    if AssignReturn then
      EXIT(ErrOpUsage(ermCantAssignProcedure, Op))
    else
      EXIT(ParseWrite(True));

//  Result := ParseGenericIntrinsic(Op, AssignReturn, Slug);
end;

end.

unit Parse.Expr;

interface
uses Def.IL, Def.Operators, Def.VarTypes, Def.UserTypes, Def.Consts, Def.Variables,
  Parse.Errors, Parse.Literals;

//Creates IL to assign the slug to a dest (variable or other)
//  Variable must be assigned,
//  ??? and must be a pointer type
//  If AsType is nil:
//    If the variable is being created it will be assigned the implicit type of the expression
//    Otherwise the variable will be checked for compatibility with the expressions result type
procedure AssignSlugToDest(const Slug: TExprSlug;var Variable: PVariable;
  AsType: PUserType);

function ParseExprToSlug(out Slug: TExprSlug;var ExprType: PUserType): TQuicheError;

//Parses an expression to an ILItem with a single parameter and no operation
//Dest and Operation data *must* be assigned by the caller
function ParseExprToILItem(out ILItem: PILItem;out UType: PUserType): TQuicheError;

//Parses a constant expression and returns it in Value.
//If the expression is not a constant expression, or if the type is incompatible with
//ExprType returns an error.
//On entry a value of vtUnknown may be passed for ExprType. If so ExprType will
//return the type of the expression parsed
function ParseConstantExpr(out Value: TImmValue;var ExprType: PUserType): TQuicheError;

//Parses an expression (After the <varname> := has been parsed) and creates IL
//to assign it to the Variable which has been passed in. VarIndex is the index of
//the Variable
//If Variable is nil and VarIndex is -1, creates a new variable, assigning it a
//type based on the result of the expression. The caller will then need to fill
//in the variables Name, and any other necessary details.
//WARNING: When creating and initialising a variable the variable MUST be created
//*after* the expression has been evaluated. If the variable is created before
//then it will be possible to reference the variable within the expression which
//would, of course, be an bug.
function ParseAssignmentExpr(var Variable: PVariable;AsType: PUserType): TQuicheError;

//Parse an assignment statement
// <varname> := <assignment-expression>
//Variable is the variable being assigned to.
//If Parser is at a suffix - ^ or [ will parse appropriately and store at the
//dereference address
function ParseAssignment(Variable: PVariable): TQuicheError;

//Converts the Expr (or immediate value) contained within Slug and returns it's
//TypeDef. The TypeDef will be an Immediate value and, therefore, any IL generated
//by the expression will be deleted (or can it??)
procedure SlugToTypeDef(var Slug: TExprSlug);

implementation
uses SysUtils,
  Def.Globals, Def.Functions, Def.Scopes,
  Lib.Primitives,
  Parse.Base, Parse.Eval, Parse.FuncCall, Parse.Source, Parse.Pointers,
  Parse.TypeChecker;


procedure AssignSlugToDest(const Slug: TExprSlug;var Variable: PVariable;
  AsType: PUserType);
var VarVersion: Integer;
  ILItem: PILItem;
begin
  if Slug.ILItem <> nil then
  begin
    ILItem := Slug.ILItem;
    if ILItem.Op = OpUnknown then
      if (ILItem.Param1.Kind = pkImmediate) and (ILItem.Param2.Kind = pkNone) then
        ILItem.Op := OpStoreImm
      else
        ILItem.Op := OpMove;
  end
  else
  begin
    if Slug.Operand.Kind = pkImmediate then
      ILItem := ILAppend(OpStoreImm)
    else if IsPointeredType(Slug.ResultType.VarType) then
      ILItem := ILAppend(opBlockCopy)
    else
      ILItem := ILAppend(OpMove);
    ILItem.Param1 := Slug.Operand;
    ILItem.Param2.Kind := pkNone;
    ILItem.ResultType := Slug.ResultType;
  end;

  //Overflows for an immediate assignment must be validated by the parser
  if ILItem.Op = OpStoreImm then
    ILItem.Flags := ILItem.Flags - [cgOverflowCheck];

  if ILItem.Op = opBlockCopy then
  begin
    Assert(ILItem.Param1.Kind = pkVarSource);
    ILItem.Param1.Kind := pkVarRef;
  end;

  if Variable = nil then
  begin
    if Assigned(AsType) then
      Variable := VarCreateUnnamed(AsType)
    else
      Variable := VarCreateUnnamed(Slug.ImplicitType);
  end;

  VarVersion := Variable.IncVersion;
  If Slug.ResultByRefParam <> nil then
  begin
    //We need to modify the IL so the Variable's VarRef gets passed /into/ the function
    //To do this Store the variable's details into the param pointed to by Slug.ResultByRefParam
    Assert(Slug.ResultByRefParam.Kind = pkNone);
    Slug.ResultByRefParam.SetVarRef(Variable);
  end
  else
  begin
    ILItem.Dest.SetVarDestAndVersion(Variable, VarVersion);
    if ILItem.Op = opBlockCopy then
      ILItem.Dest.Kind := pkVarRef;
  end;
end;


//=================================Complex operands, and operators

function ParseOperand(var Slug: TExprSlug;UnaryOp: TUnaryOperator): TQuicheError; forward;

//Parses identifiers as expression parameters.
//Identifiers can be constants, variables or functions or types
function ParseOperandIdentifier(var Slug: TExprSlug;Ident: String): TQuicheError;
var IdentData: TIdentData;
  Scope: PScope;
begin
  //Search everything we can see
  IdentData := SearchScopes(Ident, Scope);
  case IdentData.IdentType of
    itUnknown:
      EXIT(ErrSub(qeUndefinedIdentifier, Ident));
    itConst:
    begin
      Slug.SetImmediate(IdentData.C.UserType);
      Slug.Operand.Imm := IdentData.C.Value;
      Slug.ParamOrigin := poExplicit;
      Slug.ResultType := IdentData.C.UserType;
      Slug.implicitType := IdentData.C.UserType;
      EXIT(qeNone);
     end;
    itVar:
    begin
      if TestForPtrSuffix then
        EXIT(ParsePtrSuffixLoad(Slug, IdentData.V));

      Slug.Operand.SetVarSource(IdentData.V);
      Slug.ParamOrigin := poExplicit;
      Slug.ResultType := IdentData.V.UserType;
      Slug.ImplicitType := IdentData.V.UserType;
      EXIT(qeNone);
    end;
    itType:
      //If it is a typecast it will be followed by open bracket.
      if Parser.TestChar = '(' then
        EXIT(ErrTODO('Typecasts for user types not yet implemented. Type names not allowed in expressions'))
        //Typecast TODO
      else
      begin //Not a typecast - return a TypeDef
        Slug.Operand.Kind := pkImmediate;
        Slug.Operand.Imm.CreateTypeDef(IdentData.T);
        Slug.ParamOrigin := poExplicit;
        Slug.ImplicitType := GetSystemType(vtTypeDef);
        Slug.ResultType := GetSystemType(vtTypeDef);
        EXIT(qeNone);
      end;
    itEnumItem:
    begin
      Slug.SetImmediate(IdentData.T);
      Slug.Operand.Imm := TImmValue.CreateTyped(IdentData.T, IdentData.Index);
      Slug.ParamOrigin := poExplicit;
      Slug.ResultType := IdentData.T;
      Slug.implicitType := IdentData.T;
      EXIT(qeNone);
    end;
    itFunction:
    begin
      Result := DoParseFunctionCall(IdentData.F, Slug);
      if Result <> qeNone then
        EXIT;
    end;
  else
    EXIT(ErrBUG('Invalid/unknown IdentType in ParseOperandIentifier'));
  end;
end;

function DoUnaryOp(UnaryOp: TUnaryOperator;var Slug: TExprSlug): TQuicheError;
var VType: PUserType; //Dummy(??)
  ResultType: PUserType;
begin
  Assert(UnaryOp in UnaryOps);
  Result := qeNone;
  Slug.Op := UnaryOp;

  VType := nil;
  ResultType := nil;
  if not PrimFindParseUnary(Slug.Op, Slug, VType, ResultType) then
  begin
    Assert(Slug.ImplicitType <> nil);
    EXIT(ErrOpUsageSub(qeOpIncompatibleType, Slug.ImplicitType.Description, Slug.Op));
  end;

  if (Slug.ILItem = nil) and (Slug.Operand.Kind = pkImmediate) then
  begin
    Result := EvalUnary(Slug.Op, @Slug.Operand, Slug.Operand.Imm);
    if Result <> qeNone then
      EXIT;
    Slug.Op := OpUnknown;
    Slug.ImplicitType := Slug.Operand.Imm.UserType;
  end
  else  //Parameter is not immediate
  begin
    Slug.ResultType := ResultType;
    Slug.ImplicitType := Slug.ResultType;;

    if Slug.ILItem <> nil then
      //We already have an ILItem created. If so, we need to set the Dest to
      //assign it to a temp var and use that in our calculations
      Slug.AssignToHiddenVar;

    Slug.ILItem := ILAppend(Slug.Op);
    Slug.ILItem.Param1 := Slug.Operand;
    Slug.ILItem.Param2.Kind := pkNone;
    Slug.ILItem.ResultType := Slug.ResultType;
  end;
end;

//Parses the @ operator. Value must be something which is addressable.
function ParseAddrOf(var Slug: TExprSlug): TQuicheError;
var UnaryOp: TUnaryOperator;
begin //Sub-expressions
  Parser.Mark;
  Parser.SkipChar;

  UnaryOp := opUnknown;
  Result := ParseOperand(Slug, UnaryOp);
  if Result <> qeNone then
    EXIT;

  if UnaryOp <> opUnknown then
    EXIT(Err(qeAt));
  if Slug.Op <> opUnknown then
    EXIT(Err(qeAt));
  if Slug.Operand.Kind <> pkVarSource then
     EXIT(Err(qeAt));

  Slug.ILItem := ILAppend(OpAddrOf);
  Slug.ILItem.Param1 := Slug.Operand;
  Slug.ILItem.Param1.Kind := pkVarAddr;
  Slug.ILItem.ResultType := GetPointerToType(Slug.ResultType);
  Slug.Operand.Kind := pkNone;
  Slug.ResultType := Slug.ILItem.ResultType;
  Slug.ImplicitType := Slug.ResultType;

  Result := qeNone;
end;

//Parses an operand (of an expression) and returns the data in Operand
//Also parses sub-expressions (in parenthesis)
//Unary operators ('-' and 'not') result in a recursive call back into ParseOperand
//with the UnaryOp parameter updated appropriately. Multiple prefixes of the same operator
//will be cancelled out by the recursive call returning the UnaryOp to uoNone.
//The unary operator will be inserted into the IL when we reach the end of the chain of
//(identical) unary operators, with tail recursion adding any previous unary operators
function ParseOperand(var Slug: TExprSlug;UnaryOp: TUnaryOperator): TQuicheError;
var
  Ch: Char;
  Ident: String;
begin
  Result := Parser.SkipWhiteNL;
  if Result <> qeNone then
    EXIT;

  Parser.Mark;

  Ch := Parser.TestChar;
  case Ch of
    '(':
      begin //Sub-expressions
        Parser.SkipChar;

        Result := ParseExprToSlug(Slug, Slug.ResultType);
        if Result <> qeNone then
          EXIT;

        if not Parser.SkipCharIf(')') then
          EXIT(Err(qeUnmatchedBrackets));
        Result := qeNone;
      end;
//  csIdentFirst
    'a'..'z','A'..'Z','_': //Identifiers - variables, functions or keyword values
    begin
      Result := ParseIdentifier(#0, Ident);
      if Result <> qeNone then
        EXIT;

      if CompareText(Ident, 'not') = 0 then
      begin
        case UnaryOp of
          opUnknown: Result := ParseOperand(Slug, opComplement);
          opNegate: Result := ParseOperand(Slug, opComplement);
          opComplement: Result := ParseOperand(Slug, opUnknown);
        else
          EXIT(ErrBUG('Unknown/invalid unary operator'));
        end;
        if UnaryOp = opComplement then
          UnaryOp := opUnknown;
      end
      else
        Result := ParseOperandIdentifier(Slug, Ident);

      if Result <> qeNone then
        EXIT;
    end;
//    csDecimalFirst
    '0'..'9': //Numeric constants
      if UnaryOp = opNegate then
      begin
        Result := ParseNegativeDecimal(Slug);
        UnaryOp := opUnknown;
      end
      else
        Result := ParseDecimal(Slug, sgnNone);
    '$': //Hex constant
      Result := ParseHex(Slug);
    '%': //Binary constant
      Result := ParseBinary(Slug);
    '.': //Real constant
      EXIT(ErrTODO('Floating point numbers not yet supported'));
    '''','#': //Char or string
      Result := ParseStringOrChar(Slug);
    '@': //Address prefix
      Result := ParseAddrOf(Slug);
    '+': //Unary plus
    begin
      Parser.SkipChar;
      if CharInSet(Parser.TestChar, ['0'..'9']) then
        Result := ParseDecimal(Slug, sgnPlus)
      else
        EXIT(ParseOperand(Slug, UnaryOp));
    end;
    '-': //Unary subtract/negative
    begin
      Parser.SkipChar;
      if CharInSet(Parser.TestChar, ['0'..'9']) then
        Result := ParseDecimal(Slug, sgnMinus)
      else
      begin
        case UnaryOp of
          opUnknown: Result := ParseOperand(Slug, opNegate);
          opNegate: Result := ParseOperand(Slug, opUnknown);
          opComplement: Result := ParseOperand(Slug, opNegate);
        else
          EXIT(errBUG('Unknown/invalid unary operator'));
        end;
        if Result <> qeNone then
          EXIT;
        if UnaryOp = opNegate then
          UnaryOp := opUnknown;
      end;
    end;
  else
    EXIT(Err(qeOperandExpected));
  end;

  //If Negate or Invert: Do that here
  //NOTE: Recurse for '-' clears Negate, Recurse for 'NOT' clears Invert
  //Also, if constant is negated, or negate is otherwise processed, Negate must be cleared
  //Ditto for Invert/NOT
  if UnaryOp <> opUnknown then
    Result := DoUnaryOp(UnaryOp, Slug);
end;

//Parse and returns a *binary* operator (i.e. not unary ones) and it's precedence
//If at the end of an expression returns opNone.
function ParseInfixOperator(var Slug: TExprSlug): TQuicheError;
var Ident: String;
begin
  Result := Parser.SkipWhite;
  if Result <> qeNone then
    EXIT;
  Parser.Mark;

  Slug.Op := opUnknown;

  //End of (sub)-expression
  if CharInSet(Parser.TestChar, [')',']',';',',',#0]) then
    EXIT(qeNone);
  if Parser.EOLN then
    EXIT(qeNone);

  if TestSymbolFirst then
  begin
    Result := ParseSymbol(Ident);
    Slug.Op := SymbolToOperator(Ident);
  end
  else if TestIdentFirst then
  begin
    Result := ParseIdentifier(#0, Ident);
    Slug.Op := IdentToInfixOperator(Ident);
  end
  else
    EXIT(Err(qeOperatorExpected));

  if Result <> qeNone then
    EXIT;
  if Slug.Op = opUnknown then
  begin
    Parser.Undo;
    EXIT(qeNone);
  end;

  Result := qeNone;
end;

//Parse out and returns a single 'slug' - an operand and the operator following
//it (or opNone)
function ParseExprSlug(out Slug: TExprSlug): TQuicheError;
begin
  Slug.Initialise;
  Result := ParseOperand(Slug, opUnknown);
  if Result <> qeNone then
    EXIT;

  Result := ParseInfixOperator(Slug);
end;

//==============================Expressions

//Sets the types in the left slug (ResultType, OpType fields) for the operation when
//used with the given input operands: Slug.Operand <Slug.Operation> RightSlug.Operand
//Also validates that the operator can be used with the given operand types and
//that there is a suitable primitive available (as noted in the OpTypes field of
//the Operands list.
//Returns False if operands are incompatible with the operator/available primitives
function AssignSlugTypes(var Left, Right: TExprSlug): Boolean;
var
  LType: PUserType;
  RType: PUserType;
  ResultType: PUserType;
begin
  LType := nil;
  RType := nil;
  ResultType := nil;  //Find routines with any result type
  Result := PrimFindParse(Left.Op, Left, Right, LType, RType, ResultType);
  if not Result then
    EXIT;

  //Update types for constants
  if LType <> nil then
    if (Left.ILItem = nil) and (Left.Operand.Kind = pkImmediate) then
      Left.Operand.Imm.UpdateUserType(LType);
  if RType <> nil then
    if (Right.ILItem = nil) and (Right.Operand.Kind = pkImmediate) then
      Right.Operand.Imm.UpdateUserType(RType);

  Left.ResultType := ResultType;
  Left.ImplicitType := Left.ResultType;
  Right.ImplicitType := Left.ResultType;
end;

//Compares the precedences of the passed in slug and the following slug.
//If the following slug(s) is(are) higher precedence then recurses to parse
//that(those) slug(s). If not appends the current data to the IL list and
//returns.
//When it returns the Slug parameter will have been updated to reflect the
//current situation - i.e. next operator (and it's precedence) and the location
//and value of the operand (i.e. the temp index location of the result of the
//deeper operations.
//ILItem returns the final operation of the expression. The caller will need
//to update the Dest info as needed
function ParseSubExpr(var Left: TExprSlug;out ILItem: PILItem): TQuicheError;
var Right: TExprSlug;
  Evalled: Boolean;
begin
  while True do
  begin
    //Next slug
    Result := ParseExprSlug(Right);
    if Result <> qeNone then
      EXIT;

    //If the right slug returned an ILItem then we need to set it's Dest to a temp var
    if Right.ILItem <> nil then
      if Right.ILItem.Dest.Kind = pkNone then
        //We already have an ILItem created. If so, we need to set the Dest to
        //assign it to a temp var and use that in our calculations
        Right.AssignToHiddenVar;

    //While next slug is higher precedence
    while (Right.Op <> OpUnknown) and (Left.OpData.Precedence < Right.OpData.Precedence) do
    begin
      Result := ParseSubExpr(Right, ILItem);
      if Result <> qeNone then
        EXIT;

      if not AssignSlugTypes(Left, Right) then
        EXIT(ErrOpUsageSub2(qeOpIncompatibleTypes,
          VarTypeToName(Left.Operand.GetVarType),
          VarTypeToName(Right.Operand.GetVarType), Left.Op));

      if ILItem <> nil then
        //Add sub-expression to IL list
        //with dest as temp data
        //Update right slug for next iteration
        Right.Operand.SetVarSource(ILItem.AssignToHiddenVar(Left.ResultType));
    end;

    //Evaluate constant expressions
    Evalled := False;
    if (Left.Operand.Kind = pkImmediate) and (Right.Operand.Kind = pkImmediate) then
    begin
      //If possible, evaluate and replace Slug.Operand
      Result := EvalBi(Left.Op, @Left.Operand, @Right.Operand, Left.Operand.Imm);
      if Result <> qeNone then
        EXIT;
(*      if EvalType <> vtUnknown then
      begin
        Left.SetImmediate(EvalResult, EvalType);
*)        Left.ImplicitType := Left.Operand.Imm.UserType;
        Left.ResultType := Left.ImplicitType;
        Left.Op := Right.Op;
        if Right.Op = OpUnknown then
        begin
          ILItem := nil;
          EXIT(qeNone);
        end;
(*      end;
 *)   end;

    //If we didn't evaluate as a constant expression
    if not Evalled then
    begin
      //Modifies operand types and result type based on available primitives
      if not AssignSlugTypes(Left, Right) then
        EXIT(ErrOpUsageSub2(qeOpIncompatibleTypes,
          Left.Operand.GetUserType.Description,
          Right.Operand.GetUserType.Description, Left.Op));

      //Add current operation to list.
      //Dest info will be added by later
      ILItem := ILAppend(Left.Op);
      ILItem.ResultType := Left.ResultType;
      ILItem.Param1 := Left.Operand;
      ILItem.Param2 := Right.Operand;

      //Exit if end of expression or lower precedence
      if (Right.Op = opUnknown) or
        (Left.OpData.Precedence > Right.OpData.Precedence) then
      begin
        //Note: Dest info will be added by the caller
        //Update slug and return
        Left.Op := Right.Op;

        //...and return
        EXIT(qeNone);
      end;

      //Same precedence:
      //Add item to IL list
      //with dest as temp data (hidden variable)
      //Update slug data...
      Left.Operand.SetVarSource(ILItem.AssignToHiddenVar(Left.ResultType));

      //Right slug operation becomes left slug operation
      Left.Op := Right.Op;
    end;
  end;
end;

//For cases where the expression is a single item with no operation, either a
//literal value or a variable/identifier
//Updates the Slug's data as appropriate
function FixupSlugNoOperation(var Slug: TExprSlug;var ExprType: PUserType): TQuicheError;
begin
  if Slug.ResultType = nil then
    Slug.ResultType := GetSystemType(Slug.Operand.GetVarType);

  if Assigned(ExprType) then
  begin
    Result := ValidateAssignment(ExprType, Slug);
    if Result <> qeNone then
      EXIT;
  end
  else
    ExprType := Slug.ResultType;

  Result := qeNone;
end;

function ParseExprToSlug(out Slug: TExprSlug;var ExprType: PUserType): TQuicheError;
var ILItem: PILItem;
begin
  Parser.Mark;

  //Read the first slug of the expression
  Result := ParseExprSlug(Slug);
  if Result <> qeNone then
    EXIT;

  //If no operation then the expression is just a single item - either a literal
  //or variable.
  //We'll populate the operation data, but the dest data will be added by the caller
  if Slug.Op = opUnknown then
    EXIT(FixupSlugNoOperation(Slug, ExprType));

  //If the slug returned an ILItem then we need to set it's Dest to a temp var
  if Slug.ILItem <> nil then
    if Slug.ILItem.Dest.Kind = pkNone then
      //We already have an ILItem created. If so, we need to set the Dest to
      //assign it to a temp var and use that in our calculations
      Slug.AssignToHiddenVar;

  //Loop until end of expression
  repeat
    Result := ParseSubExpr(Slug, ILItem);
    if Result <> qeNone then
      EXIT;

    //Unless end of expression :- assign value to hidden variable
    if Slug.Op <> OpUnknown then
      Slug.Operand.SetVarSource(ILItem.AssignToHiddenVar(Slug.ResultType));
  until Slug.Op = opUnknown;


  if ILItem = nil then
  begin
    if Assigned(ExprType) then
    begin
      Result := ValidateAssignment(ExprType, Slug);
      if Result <> qeNone then
        EXIT;
    end;
  end
  else // ILItem <> nil
//???ValidateExprType??
    Slug.ILItem := ILItem;

  if Assigned(ExprType) then
//    ILItem.ResultType := ExprType
  else
    ExprType := Slug.ImplicitType;

  Result := qeNone;
end;

//Parses an expression to an ILItem
function ParseExprToILItem(out ILItem: PILItem;out UType: PUserType): TQuicheError;
var
  ExprType: PUserType;
  Slug: TExprSlug;
begin
  Slug.Initialise;

  ExprType := nil;
  Result := ParseExprToSlug(Slug, ExprType);
  if Result <> qeNone then
    EXIT;

  UType := Slug.ResultType;
  if Slug.ILItem <> nil then
    ILItem := Slug.ILItem
  else
  begin
    ILItem := ILAppend(OpUnknown);
    ILItem.Param1 := Slug.Operand;
    ILItem.Param2.Kind := pkNone;
  end;

  //Op data and Dest to be assigned by caller
end;

function ParseConstantExpr(out Value: TImmValue;var ExprType: PUSerType): TQuicheError;
var Slug: TExprSlug;
begin
  Result := ParseExprToSlug(Slug, ExprType);
  if Result <> qeNone then
    EXIT;

  if Slug.Operand.Kind <> pkImmediate then
    EXIT(Err(qeConstantExpressionExpected));

  Value := Slug.Operand.Imm;
end;

function ParseAssignmentExpr(var Variable: PVariable;AsType: PUserType): TQuicheError;
var
  ExprType: PUserType;
  Slug: TExprSlug;
begin
  ExprType := AsType;
  Result := ParseExprToSlug(Slug, ExprType);
  if Result <> qeNone then
    EXIT;

  if UTToVT(ExprType) = vtString then
    //TODO: If we are assigning a string literal to a variable (or passing as a var
    //parameter) then we need to get the code generator to generate /modifable/
    //string data (and to make sure that data is stored in RAM).
    EXIT(ErrTODO('Strings variables are not currently supported.'));

  //Verify assignment is in range. Only really required for integers due to
  //implicit typing rules
  if not Assigned(AsType) then
  begin
    Result := ValidateAssignment(ExprType, Slug);
    if Result <> qeNone then
      EXIT;
  end;

  AssignSlugToDest(Slug, Variable, AsType);
end;

//Parse an assignment statement
// <varname> := <assignment-expression>
function ParseAssignment(Variable: PVariable): TQuicheError;
var UserType: PUserType;
begin
  Assert(Assigned(Variable));

  if Variable.IsConst then
    EXIT(Err(qeAssignToCONSTVar));

  if CharInSet(Parser.TestChar, PtrSuffix) then
    EXIT(ParseAssignPtrSuffixStore(Variable));

  //Normal assignment
  UserType := Variable.UserType;

  if not TestAssignment then
    EXIT(Err(qeAssignmentExpected));

  Result := ParseAssignmentExpr(Variable, UserType);
end;

procedure SlugToTypeDef(var Slug: TExprSlug);
begin
  Assert(UTToVT(Slug.ResultType) <> vtTypeDef);

  Slug.Operand.Kind := pkImmediate;
  Slug.Operand.Imm.CreateTypeDef(Slug.ResultType);
  Slug.ResultType := GetSystemType(vtTypeDef);
  Slug.ImplicitType := Slug.ResultType;
  Slug.ILItem := nil;

  //*********************************************************
  //****TODO: Rollback any IL generated by the expression****
  //*********************************************************
end;

end.

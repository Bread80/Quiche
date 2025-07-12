unit Parse.Pointers;

interface
uses Parse.Literals, Parse.Errors,
  Def.Variables, Def.UserTypes;

const PtrSuffix = ['^','['];


//Parses pointer deferences and array references.
//If the Parser is at a ^ or [ symbol
//Generates IL:
// <temp-var> := PtrLoad <variable>
function ParsePtrSuffixLoad(var Slug: TExprSlug;V: PVariable): TQuicheError;

//Parses a pointer suffix in an assign expression and generates
//IL for the assignment
function ParseAssignPtrSuffixStore(V: PVariable): TQuicheError;

//Parse an assignment to a derefereced pointer dereference of the form
// X^ := <expression>
//Parse must be pointing at the terminating ^
//Generates IL code:
// _ := PtrStore <variable> <expression>
//where, for the above expression, <variable> is X of ParamKind pkVarAddr

implementation
uses
  Parse.Expr, Parse.Base,
  Def.QTypes, Def.IL, Def.Operators;

//Parses ^ suffix (pointer dereference)
//Returns an ILItem with opPtrLoad <variable>
//Dest will be assigned by the caller
function ParsePtrLoad(var Slug: TExprSlug;V: PVariable): TQuicheError;
begin
  Assert(Parser.TestChar = '^');
  Parser.SkipChar;

  Slug.Initialise;
  Slug.ILItem := ILAppend(opPtrLoad);
  Slug.ILItem.Param1.SetVarPtr(V);

  //UserType must be something we can dereference
  case UTToVT(V.UserType) of
    vtPointer: Slug.ResultType := GetSystemType(vtByte);
    vtTypedPointer: Slug.ResultType := V.UserType.OfType;
  else
    EXIT(ErrSub(qePointerDerefError, V.UserType.Description));
  end;

  Slug.ILItem.ResultType := Slug.ResultType;
  Slug.ImplicitType := Slug.ResultType;
end;

//Parse array array reference.
//Returns an opAddrOf <array-var> <index>
//<index> may be an immediate or a variable. If the index is a run-time expression
//then <index> will be a temporary variable to which the index expression is
//assigned
//<index> is zero based, no matter what the type or range of the bounds type. (TODO)
//Parse must be positioned at the opening [
//V is the variable being dereferenced, or nil if we're referencing a constant array(???)
//  If V is nil the expression must be a constant one.
//  If the expression is not a constant one it's value will be validated against the
//  type and V (if given)
function ParseArrayIndex(var Slug: TExprSlug;V: PVariable): TQuicheError;
var IndexSlug: TExprSlug; //Slug for teh index expresssion
  ArrayType: PUserType;
begin
  Assert(Assigned(V));
  Assert(Parser.TestChar in ['[',',']);
  Parser.SkipChar;

  ArrayType := V.UserType;
  //Index expression must be of BoundsType
  Result := ParseExprToSlug(IndexSlug, ArrayType.BoundsType);
  if Result <> qeNone then
    EXIT;

  if IndexSlug.ILItem <> nil then
    IndexSlug.AssignToHiddenVar;

  //Get the address of the element into AddrVar as a typed pointer
  //opAddrOf will give us @V + ElementSize * Param2 where V is Param1
  Slug.Initialise;
  Slug.ILItem := ILAppend(opAddrOf);
  Slug.ILItem.Param1.SetVarAddr(V);  //We want the address of the array, not the value
  Slug.ILItem.Param2 := IndexSlug.Operand;
  Slug.ResultType := GetPointerToType(V.UserType.OfType);
  Slug.ImplicitType := Slug.ResultType;
  Slug.ILItem.ResultType := Slug.ResultType;

  Result := Parser.SkipWhite;
  if Result <> qeNone then
    EXIT;

(*  if Parser.TestChar = ',' then
    if IsArrayType(ArrayType.VarType) then
      //TODO: comma separated multi-dimensional array references - recurse back to ourselves
      //(references using multiple square brace syntax will be parsed at a higher level)
      Assert(False, 'TODO')
    else
      EXIT(Err(qeArrayTooManyDimensions))
  else*) if Parser.TestChar <> ']' then
    EXIT(Err(qeCloseSquareBraceExpected));

  Parser.SkipChar;
end;

//Calls ParseArrayIndex to generate an (optional) expression and a opAddrOf
//to generate the address to read from.
//Then generates an opPtrLoad to return the actual data.
function ParseArrayRead(var Slug: TExprSlug;V: PVariable): TQuicheError;
var AddrSlug: TExprSlug;
begin
  Assert(Assigned(V));

  //Get an expression which returns a variable containing the address to read from
  //to read from
  Result := ParseArrayIndex(AddrSlug, V);
  if Result <> qeNone then
    EXIT;

  AddrSlug.AssignToHiddenVar;

  Slug.Initialise;
  Slug.ILItem := ILAppend(opPtrLoad);
  Assert(AddrSlug.Operand.Kind = pkVarSource);
  Slug.ILItem.Param1.SetVarPtr(AddrSlug.Operand.Variable);
  Slug.ResultType := V.UserType.OfType;
  Slug.ImplicitType := Slug.ResultType;
  Slug.ILItem.ResultType := Slug.ResultType;
end;

function ParsePtrSuffixLoad(var Slug: TExprSlug;V: PVariable): TQuicheError;
var Ch: Char;
begin
  Ch := Parser.TestChar;
  case Ch of
    '^': EXIT(ParsePtrLoad(Slug, V));
    '[': EXIT(ParseArrayRead(Slug, V));
  else
    Assert(False);  //No pointer suffix
  end;
end;




function DoAssignPtrStore(AddrVar: PVariable;ExprType: PUserType): TQuicheError;
var ExprSlug: TExprSlug;
  ILItem: PILItem;
begin
  Parser.SkipChar;
  if not TestAssignment then
    EXIT(Err(qeAssignmentExpected));

  //Parse the assignment expression
  Result := ParseExprToSlug(ExprSlug, ExprType);
  if Result <> qeNone then
    EXIT;

  ILItem := ILAppend(opPtrStore);
  ILItem.Param1.SetVarPtr(AddrVar);

  if ExprSlug.ILItem <> nil then
  begin
    ExprSlug.AssignToHiddenVar;
    ILItem.Param2.SetVarSource(ExprSlug.ILItem.Dest.Variable);
  end
  else
    ILItem.Param2 := ExprSlug.Operand;

  ILItem.ResultType := nil;
end;

function ParseAssignPtrStore(V: PVariable): TQuicheError;
var VT: TVarType;
  Dest: PVariable;
  Slug: TExprSlug;
  ILItem: PILItem;
  ExprType: PUserType;
begin
  Assert(Assigned(V));
  Assert(Parser.TestChar = '^');

  Parser.SkipChar;
  if not TestAssignment then
    EXIT(Err(qeAssignmentExpected));

  //Get the type of the pointed to value
  //Type must be something we can dereference
  case UTToVT(V.UserType) of
    vtPointer: ExprType := GetSystemType(vtByte);
    vtTypedPointer: ExprType := V.UserType.OfType;
  else
    EXIT(ErrSub(qePointerDerefError, V.UserType.Description));
  end;

  Result := DoAssignPtrStore(V, ExprType);
end;

function ParseAssignArrayWrite(V: PVariable): TQuicheError;
var AddrSlug: TExprSlug;
  AddrVar: PVariable;
  ExprType: PUserType;
begin
  Assert(Assigned(V));

  //Get an expression which returns a variable containing the address to read from
  //to read from
  Result := ParseArrayIndex(AddrSlug, V);
  if Result <> qeNone then
    EXIT;

  if Assigned(AddrSlug.ILItem) then
    AddrSlug.AssignToHiddenVar;

  AddrVar := AddrSlug.ILItem.Dest.Variable;
  //AddrVar is a pointer to the data
  Assert(UTToVT(AddrVar.UserType) = vtTypedPointer);
  ExprType := AddrVar.UserType.OfType;
  Result := DoAssignPtrStore(AddrVar, ExprType);
end;

function ParseAssignPtrSuffixStore(V: PVariable): TQuicheError;
var Ch: Char;
begin
  Ch := Parser.TestChar;
  case Ch of
    '^': EXIT(ParseAssignPtrStore(V));
    '[': EXIT(ParseAssignArrayWrite(V));
  else
    Assert(False);  //No pointer suffix
  end;
end;

end.

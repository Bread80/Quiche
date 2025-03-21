{$ifdef fpc}
  {$mode delphi}
{$endif}
unit Parse;
{
Signed (or unsigned) propagation in expressions.

This algorithm determines whether an expression is signed or unsigned.

Constants specified as hexadecimal ($ prefix) or binary (%prefix) are always unsigned.
Constants specified as decimal are always signed.
The signedness of function calls is as specified by the function declaration
These types can be overidden by an explicit typecast.

The initial signedness of an expression is determined by the leftmost value. If this
value is given as a hex or binary constant then the expression will be considered to be
unsigned. Thus,
$1000 + 42 gives an unsigned result,
1000 + $42 gives a signed result.
However, when a typecast is used,
word(1000) + $42 results in an unsigned expression
1000 + word(42)  results in a signed expression
}

interface
uses Def.Variables,
  Parse.Errors, Parse.Expr;

type TAssembleCallback = function: Boolean;
var OnScopeDone: TAssembleCallback;

type TEndAt = (eaEOF, eaEND, eaUNTIL);
//Parses a list of statements
//If ToEnd is eaEND or eaUNTIL, parses until END or UNTIL is parsed,
//    If found, the END or UNTIL is consumed
//otherwise (ToEnd is eaEOF) exits at EOF
function ParseStatements(EndAt: TEndAt;Storage: TVarStorage): TQuicheError;


//Parse the declaration part, including Types, Consts, and Vars.
//If AllowFuncs is True then Functions and Procedure declarations are also allowed.
//This enables the distinction between program level decarations and function/procedure
//level declarations (which Quiche doesn't allow)
function ParseDeclarations(IsRoot: Boolean;AllowFuncs: Boolean;Storage: TVarStorage): TQuicheError;

implementation
uses SysUtils, Classes,
  Def.Functions, Def.Globals, Def.IL, Def.Operators, Def.QTypes, Def.Scopes,
  Def.Consts,
  Parse.Base, Parse.Fixups, Parse.FuncCall, Parse.FuncDef, Parse.Source;

//===============================================
//Language syntax

// < boolean-expression> := <expression>
//                       where the result is a Boolean type
//If the expression evaluates to a constant:
//  Returns ILItem as nil and Expression result in ConstExprValue
//otherwise
//  Returns last ILItem of the expression in ILItem and ConstExprValue as junk
//  ILItem will have a CondBranch destination.
function ParseBranchExpr(out ILItem: PILItem;out ConstExprValue: Boolean): TQuicheError;
var ExprType: TVarType;
//  ImplicitType: TVarType; //Dummy
  Slug: TExprSlug;
begin
  Slug.Initialise;

  ExprType := vtUnknown;
  Result := ParseExpressionToSlug(Slug, ExprType);
  if Result <> qeNone then
    EXIT;
  if ExprType <> vtBoolean then
    EXIT(Err(qeBooleanExpressionExpected));

  if Slug.ILItem <> nil then
  begin //Expression
    ILItem := Slug.ILItem;
    //Convert item to a branch
    ILItem.Dest.SetCondBranch;
    if ILItem.Op in [opUnknown, OpMove, OpStoreImm] then
      ILItem.Op := OpBranch;
    Assert(ILItem.ResultType = vtBoolean);
    ILItem.ResultType := vtFlag;  //Required for proper primitive selection
  end
  //ILItem = nil
  else if Slug.Operand.Kind = pkImmediate then
  begin //Constant
    Assert(Slug.Operand.Imm.VarType = vtBoolean);
    ILItem := nil;
    ConstExprValue := Slug.Operand.Imm.BoolValue;//(Slug.Operand.ImmValueInt and $ff) = (valueTrue and $ff);
  end
  else  //Variable
  begin //ILItem = nil and OpIndex <> None
    ILItem := ILAppend(OpBoolVarBranch);
    ILItem.Param1 := Slug.Operand;
    ILItem.Param2.Kind := pkNone;
    ILItem.ResultType := vtBoolean; //(Not technically needed)
  end;
end;

//============================= DECLARATIONS

// <assignment> := <variable-name> := <expression>
//Parses assignment and variable declarations
//VarRead should be True if a 'var' keyword has been parsed
//AllowVar is True if a declaration beginning with 'var' is allowed and the 'var'
//has yet to be parsed
//If the variable name has already been parsed it must be passed in in VarName,
//if not VarName must be empty
//Variable returns the Variable which was either created or assigned to
//VarIndex returns the Index in the variable list of Variable
//Also inspects the optAllowAutoCreation option to determine if a declaration
//requires an explicit 'var' or can be implied by the first assignment to a variable
function ParseAssignment(VarRead, AllowVar, AssignRequired: Boolean;const Ident: String;
  out Variable: PVariable;Storage: TVarStorage): TQuicheError;
var Ch: Char;
  VarName: String;
  VarType: TVarType;  //vtUnknown if we're using type inference
  DoAssign: Boolean;  //True if we're assigning a value
  Creating: Boolean;
  Keyword: TKeyword;
  IdentType: TIdentType;
begin
  Result := Parser.SkipWhiteNL;
  if Result <> qeNone then
    EXIT;

  //VAR keyword or variable name
  if Ident <> '' then
    VarName := Ident
  else
  begin
    Result := ParseIdentifier(#0, VarName);
    if Result <> qeNone then
      EXIT;
  end;

  if optVarAutoCreate and not VarRead and AllowVar then
  begin
    VarRead := CompareText(VarName, 'var') = 0;
    if VarRead then
    begin
      Result := ParseIdentifier(#0,VarName);
      if Result <> qeNone then
        EXIT;
    end;
  end;

  VarType := vtUnknown;
  //Is a there any form of type sepcifier?
  if optVarAutoCreate or VarRead then
  begin
    Keyword := IdentToKeyword(VarName);
    if Keyword <> keyUnknown then
      EXIT(ErrSub(qeReservedWord, VarName));

    Result := TestForTypeSymbol(VarType);
    if Result <> qeNone then
      EXIT;

    if VarType <> vtUnknown then
    begin //<type-symbol> form
      DoAssign := TestAssignment;
      if AssignRequired and not DoAssign then
        EXIT(Err(qeAssignmentExpected));
    end
    else if TestAssignment then
      //Type inference form - nothing to do here
      DoAssign := True
    else
    begin //Type name form
      Result := Parser.SkipWhiteNL;
      if Result <> qeNone then
        EXIT;

      Ch := Parser.TestChar;
      if Ch <> ':' then
        EXIT(Err(qeColonExpectedInVAR));
      Parser.NextChar(Ch);
      Result := Parser.SkipWhiteNL;
      if Result <> qeNone then
        EXIT;

      Result := ParseVarTypeName(VarType);
      if Result <> qeNone then
        EXIT;
      if VarType = vtUnknown then
        EXIT(Err(qeUnknownType));
      if VarType in [vtReal, vtString] then
        EXIT(ErrTODO('Type not yet supported: ' + VarTypeToName(VarType)));

      Result := Parser.SkipWhite;
      if Result <> qeNone then
        EXIT;

      Ch := Parser.TestChar;
      DoAssign := Parser.TestChar = '=';
      if AssignRequired and not DoAssign then
        EXIT(Err(qeEqualExpectedInAssignment));

      if DoAssign then
        Parser.SkipChar;
    end;

    if VarRead then
    begin
      if SearchCurrentScope(VarName, IdentType) <> nil then
        EXIT(ErrSub(qeIdentifierRedeclared, VarName));
      Variable := nil;
    end
    else
      Variable := VarFindByNameAllScopes(VarName);
  end
  else  //We're only doing assignment, no creation allowed
  begin
    DoAssign := TestAssignment;
    if not DoAssign then
      EXIT(Err(qeAssignmentExpected));
    Variable := VarFindByNameAllScopes(VarName);
    if Variable = nil then
      EXIT(ErrSub(qeVariableNotFound, VarName));
    VarType := Variable.VarType;
  end;

  //Are we assigning a value today?
  if DoAssign then
  begin
    Creating := Variable = nil;
    Result := ParseAssignmentExpr(Variable, VarType);
    if Result <> qeNone then
      EXIT;

    if Creating and (VarType <> vtUnknown) then
      Variable.SetType(VarType);

    //Was a type specified, or are we using type inference?
{    if VarType <> vtUnknown then
    begin
      //If a type was specified, is the expression result compatible with that type?
      if not ValidateAssignmentTypes(VarType, Variable.VarType) then
        EXIT(errIncompatibleTypes)
      else  //If so, update the variable to the type specified in the declaration
        VarSetType(Variable, VarType);
    end
    else if Creating then //Use type returned from expression - but extend integers
      if Variable.VarType = vtInt8 then
        VarSetType(Variable, vtInteger);
}
    if Creating then
      Variable.SetName(VarName);
  end
  else
  begin //Otherwise just create it. Meh. Boring
    Variable := VarCreate(VarName, VarType);
    if Variable = nil then
      EXIT(ErrSub(qeIdentifierRedeclared, VarName));
    Result := qeNone;
  end;
end;

//=====================================================
//Keywords

// <variable-declararion> := VAR <identifier>[: <type>] [= <expr>]
//                           (Either <type> or <expr> (or both) must be given
//                        |  VAR <identifier><type-symbol> [:= <expr>]
//                           (no space allowed between <identifier> and <type-symbol>)
//                           VAR <identifier> := <expr>
function DoVAR(const Ident: String;Storage: TVarStorage): TQuicheError;
var Variable: PVariable;
begin
  Result := ParseAssignment(True, False, False, Ident, Variable, Storage);
end;

// <constant-declaration> := CONST <identifier>[: <type>] = <expr>
//                           CONST <identifier><type-symbol> = <expr>
function DoCONST(const Ident: String): TQuicheError;
var ConstName: String;
  Value: TImmValue;
  VarType: TVarType;
begin
  Result := Parser.SkipWhiteNL;
  if Result <> qeNone then
    EXIT;

  //Name
  ConstName := Ident;
  Result := ParseUniqueIdentifierIfNone(ConstName);
  if Result <> qeNone then
    EXIT;

  //Type (if specified)
  Result := ParseTypeSpecifier(VarType);
  if Result <> qeNone then
    EXIT;

  //'='
  Result := Parser.SkipWhite;
  if Result <> qeNone then
    EXIT;
  if Parser.TestChar <> '=' then
    EXIT(Err(qeEqualExpectedInCONST));
  Parser.SkipChar;

  //Value
  Result := ParseConstantExpression(Value, VarType);
  if Result <> qeNone then
    EXIT;

  Consts.Add(ConstName, VarType, Value);
end;


type TBlockState = (bsSingle, bsBeginRead);

//=================================== LOOPS

//Parse a code block. BlockState specifies whether the initial BEGIN keyword
//has already been parsed (If so, nothing following it should have been parsed)
function ParseBlock(BlockState: TBlockState;Storage: TVarStorage): TQuicheError;forward;

// <for-statement> := FOR <identifier> := <expr> TO <expr> [DO]
//                      <block>
//                  | FOR <identifier> := <expr> TO <expr> DO <block>
//IL generated by a FOR loop:
//Block 1 Entry Section (added to pre-existing block):
//  Assign loop control variable
//  Assign hidden loop termination variable (TODO: optimise to constant where possible)
//Block 2 - Header Section (start of looped part)
//  Add phi for loop control variable
//  (Extra phis may be inserted by fixups for other variables)
//  Test loop variable and...
//  ...Add CondBranch to either Body Section or Exit Section
//Block 3 - Body Section
//  Code within the loop
//Block 4 - Latch Section
//  Inc/Dec (etc) loop control Variable
//  Branch to Header Section (Block 2)
//Block 5- Exit Section (code after the loop)
//  Generate fixups (ie. extra phi functions for block 2)
function DoFOR(Storage: TVarStorage): TQuicheError;
var
  VarRead: Boolean;
  LoopVarName: String;
  LoopVar: PVariable;
  Step: Integer;       //TO or DOWNTO?
  ExprType: TVarType;
  Slug: TExprSlug;
  ToVar: PVariable;  //Hidden variable to contain end value for the loop. Nil if value is constant
  ToValue: TImmValue;  //If EndVar is nil, contants constant endvalue
  EntryBlockID: Integer;
  EntryLastItemIndex: Integer;
  HeaderBlockID: Integer;
  PhiItemIndex: Integer;
  LoopVarPhi: PILItem;  //Phi node for the loop variable
  ExitTestItem: PILItem;
  ILItem: PILItem; //Used for various Items
  PhiInsertCount: Integer;
begin
  ScopeIncDepth;  //If loop counter is declared here, ensure it goes out of scope
                  //after the loop

  Result := Parser.SkipWhiteNL;
  if Result <> qeNone then
    EXIT;

  //ENTRY section - prepare for loop
  //-------------
  //Read loop variable name
  Result := ParseIdentifier(#0, LoopVarName);
  if Result <> qeNone then
    EXIT;

  VarRead := CompareText(LoopVarName, 'var') = 0;
  if VarRead then
  begin
    Result := Parser.SkipWhiteNL;
    if Result <> qeNone then
      EXIT;

    Result := ParseIdentifier(#0, LoopVarName);
    if Result <> qeNone then
      EXIT;
  end;

  Result := Parser.SkipWhiteNL;
  if Result <> qeNone then
    EXIT;

  //Insert test here for FOR .. IN form
//  if TestForIdent('in') then
    //

  //(Create and) assign the loop variable
  Result := ParseAssignment(VarRead, false, True, LoopVarName, LoopVar, Storage);
  if Result <> qeNone then
    EXIT;

  if TestForIdent('to') then
    Step := 1
  else if TestForIdent('downto') then
    Step := -1
  else
    EXIT(Err(qeTOorDOWNTOExpected));

  //TO value
  ExprType := LoopVar.VarType;
  Result := ParseExpressionToSlug(Slug, ExprType);
  ToVar := nil;
  if Result <> qeNone then
    EXIT;
  //If the value is a constant
  if (Slug.ILItem = nil) and (Slug.Operand.Kind = pkImmediate) then
    ToValue := Slug.Operand.Imm
  else //if the value is not constant, assign it to a temp variable
    AssignSlugToVariable(Slug, ToVar, ExprType);

  //TODO: Insert code here for Step value

  //Test for DO - optional if there's a new line before code
  if not TestForIdent('do') then
    if Parser.SkipToNextLine <> qeNone then
      EXIT(Err(qeDOExpected));

  //TODO: Test if we should do loop inversion

  //Insert Branch into header
  ILAppendBranch(GetCurrBlockID + 1);
  EntryBlockID := GetCurrBlockID;
  EntryLastItemIndex := ILGetCount - 1;

  //HEADER Block - start of the looping section, Phi nodes and test for end
  //------------
  //Insert Phi for loop variable
  NewBlock := True;
  NewBlockComment := 'Loop header: ' + LoopVarName;
  LoopVarPhi := ILAppend(OpPhi);
  LoopVarPhi.Param1.SetPhiVarSource(EntryBlockID, LoopVar.Version);
  //(Fixup param2 later)

  LoopVarPhi.Dest.SetPhiVarDest(LoopVar, LoopVar.IncVersion);

  HeaderBlockID := GetCurrBlockID;
  PhiItemIndex := ILGetCount - 1; //Needed later so we can Phi any other variables

  ExitTestItem := nil;
  //Test LoopVar and Branch to Body or Exit
  case Step of
    -1: ExitTestItem := ILAppend(OpGreaterEqual);
    1:  ExitTestItem := ILAppend(OpLessEqual);
  else
    Assert(False, 'Invalid Step value');
  end;
  ExitTestItem.ResultType := vtBoolean;
  ExitTestItem.Param1.SetVarSource(LoopVar);
  if ToVar = nil then
  begin //TO value is constant
    ExitTestItem.Param2.Kind := pkImmediate;
    ExitTestItem.Param2.Imm := ToValue;
  end
  else  //To value is variable
    ExitTestItem.Param2.SetVarSource(ToVar);
  ExitTestItem.Dest.SetCondBranch;
  ExitTestItem.Dest.TrueBlockID := GetCurrBlockID + 1; //Body BlockID
  //FalseBlock to be fixed up at end of loop

  //BODY section - the code that's being looped
  //------------
  NewBlock := True;
  NewBlockComment := 'Loop Body: ' + LoopVarName;

  //Parse Loop block
  Result := ParseBlock(bsSingle, Storage);
  if Result <> qeNone then
    EXIT;

  //Insert Branch into Latch section
  if not NewBlock then
    ILAppendBranch(GetCurrBlockID + 1);

  //LATCH section - next LoopVar and branch back to header
  //-------------
  NewBlock := True;
  NewBlockComment := 'Loop Latch: ' + LoopVarName;
  //Next loopvar
  ILItem := nil;
  case Step of
    1:
    if IsEnumerable(LoopVar.VarType) then
      ILItem := ILAppend(opSucc)
    else //Real??
      ILItem := ILAppend(OpAdd);
    -1:
    if IsEnumerable(LoopVar.VarType) then
      ILItem := ILAppend(opPred)
    else //Real??
      ILItem := ILAppend(OpSubtract);
  else
    Assert(False, 'Invalid Step Value');
  end;
  ILItem.ResultType := LoopVar.VarType;
  ILItem.Param1.SetVarSource(LoopVar);
  //(Uncomment to add Step value)
  ILItem.Param2.Kind := pkImmediate;
  ILItem.Param2.Imm.CreateTyped(vtByte, 1);

  ILItem.Dest.SetVarDestAndVersion(LoopVar, LoopVar.IncVersion);

  //Insert Branch back to Header section
  ILAppendBranch(HeaderBlockID);

  //Fixup Phi for Loopvar at start of Header section
  LoopVarPhi.Param2.SetPhiVarSource(GetCurrBlockID, LoopVar.Version);

  //Insert Phis at start of Header (for any variables updated during loop)
  VarClearAdjust; //Prep for branch adjust
  VarClearTouches;
  LoopVar.Touched := True;
  PhiInsertCount := PhiWalkInt(ILGetCount-1, EntryLastItemIndex, -1, EntryLastItemIndex,
    GetCurrBlockID, EntryBlockID, False, PhiItemIndex + 1);
  //We also need to fixup references within the loop to any variables we have phi'd
  BranchFixUpRight(EntryLastItemIndex + PhiInsertCount + 2, ILGetCount - 1);

  //EXIT section
  //--------------
  //Fixup Branch in Header section
  ExitTestItem.Param3.FalseBlockID := GetCurrBlockID + 1;

  //Insert Phis after loop
  NewBlock := True;

  ScopeDecDepth;  //If loop counter was declared take it out of scope
end; //----------------------------------------------------------- /FOR


// <while-statement> := WHILE <boolean-expression> [DO]
//                        <block>
function DoWHILE(Storage: TVarStorage): TQuicheError;
var
  WHILEID: Integer; //Block ID of the WHILE - so we can generate the loop code
  WHILEIndex: Integer;  //Index of the first ILItem (the WHILE)
  Conditional: PILItem;     //The Break out conditional. Nil if expression is a constant
  ConstExprValue: Boolean;  //If branch condition is a constant
  PrevSkipMode: Boolean;  //If we're skipping the loop code (WHILE FALSE DO)
  PhiInsertCount: Integer;  //Number of Phis inserted at start of loop
begin
  NewBlock := True;
  NewBlockComment := 'While header';
  WHILEID := GetCurrBlockID + 1;
  WHILEIndex := ILGetCount;

  //Note: If Branch returns nil then we have a constant expression.
  //We'll use that to optimise infinite loops or never executed code. We do, however,
  //still need to parse non-executed that code!
  Result := ParseBranchExpr(Conditional, ConstExprValue);
  if Result <> qeNone then
    EXIT;

  //Test for DO - optional if there's a new line before code
  if not TestForIdent('do') then
  begin
    Result := Parser.SkipToNextLine;
    if Result = qeNewlineExpected then
      EXIT(Err(qeDOExpected));
    if Result <> qeNone then
      EXIT;
  end;

  //Jump out of loop
  if Conditional <> nil then
  begin
    Conditional.Dest.SetCondBranch;
    //If condition then jump 'into' the loop (this will get optimised away)
    Conditional.Dest.TrueBlockID := GetCurrBlockID + 1;

    NewBlock := True;
  end;

  //Generate code within the loop (not required if conditional is a literal FALSE
  PrevSkipMode := SkipModeStart((Conditional = nil) and not ConstExprValue);

  NewBlock := True;
  NewBlockComment := 'WHILE body';

  Result := ParseBlock(bsSingle, Storage);
  if Result <> qeNone then
    EXIT;

  //Loop back to the WHILE
  ILAppendBranch(WHILEID);

  //If loop var was /not/ a constant, set the jump target for the break out
  if Conditional <> nil then
    Conditional.Dest.FalseBlockID := GetCurrBlockID + 1;

  //PHI variables
  //Insert Phis at start of the loop (for any variables updated during loop)
  VarClearAdjust; //Prep for branch adjust
  VarClearTouches;
  PhiInsertCount := PhiWalkInt(ILGetCount-1, WHILEIndex-1, -1, WHILEIndex-1,
    GetCurrBlockID, WHILEID-1, False, WHILEIndex);
  //We also need to fixup references within the loop to any variables we have phi'd
  BranchFixUpRight(WHILEIndex {HeaderLastItemIndex} + PhiInsertCount {+ 2}, ILGetCount - 1);

  NewBlock := True;
  NewBlockComment := 'WHILE ended';

  SkipModeEnd(PrevSkipMode);
end;

// <repeat-statement> :=  REPEAT
//                          <statements>
//                        UNTIL <boolean-expression>
function DoREPEAT(Storage: TVarStorage): TQuicheError;
var
  REPEATID: Integer;    //Block ID of the REPEAT - so we can generate the loop code
  REPEATIndex: Integer; //Index of the first ILItem (the REPEAT)
  Conditional: PILItem; //Loop conditional. Nil if expression is a constant
  ConstExprValue: Boolean;  //If branch condition is a constant
  PhiInsertCount: Integer;  //Number of Phi nodes inserted at start of loop
begin
  //Start a new scope
  ScopeIncDepth;

  NewBlock := True;
  NewBlockComment := 'REPEAT';
  REPEATID := GetCurrBlockID + 1;
  RepeatIndex := ILGetCount;

  //Parse the code statements
  Result := ParseStatements(eaUNTIL, Storage);
  if Result <> qeNone then
    EXIT;

  //Note: If Branch returns nil then we have a constant expression.
  //We'll use that to optimise infinite loops
  Result := ParseBranchExpr(Conditional, ConstExprValue);
  if Result <> qeNone then
    EXIT;

  //Jump out of loop
  if Conditional = nil then
  begin
    if  ConstExprValue then
      //UNTIL TRUE (literal value) - doesn't loop
    else
      //UNTIL FALSE - loop unconditionally
      ILAppendBranch(REPEATID);
  end
  else
  begin //Branch expression
    Conditional.Dest.SetCondBranch;
    //If not <condition> then jump back to the REPEAT
    Conditional.Dest.FalseBlockID := REPEATID;
    //Otherwise fall through to next block
    Conditional.Dest.TrueBlockID := GetCurrBlockID + 1;
  end;

//  Result := Parser.SkipWhiteNL;
//  if Result <> qeNone then
//    EXIT;

  //PHI variables
  //Insert Phis at start of the loop (for any variables updated during loop)
  VarClearAdjust; //Prep for branch adjust
  VarClearTouches;
  PhiInsertCount := PhiWalkInt(ILGetCount-1, REPEATIndex-1, -1, REPEATIndex-1,
    GetCurrBlockID, REPEATID-1, False, REPEATIndex);
  //We also need to fixup references within the loop to any variables we have phi'd
  BranchFixUpRight(REPEATIndex {HeaderLastItemIndex} + PhiInsertCount {+ 2}, ILGetCount - 1);

  NewBlock := True;
  NewBlockComment := 'REPEAT completed';

  //End the scope
  ScopeDecDepth;
end;

//==============================CONDITIONALS

// <if-statement> := IF <boolean-expression> [THEN]
//                     <block>
//                   [ ELSE
//                     <block> ]
function DoIF(Storage: TVarStorage): TQuicheError;
var
  Branch: PILItem;      //The Branch condition. Nil if expression is a constant
  ConstExprValue: Boolean;  //If branch condition is a constant
  PrevSkipMode: Boolean;    //Cache old SkipMode
  ThenBranch: PILItem;    //Unconditional branch at end of ELSE section
  //BlockIDs
  BranchID: Integer;    //Block ID of the conditional branch
  ThenLastID: Integer;  //Last block of the THEN branch
  ElseLastID: Integer;  //Last block of the ELSE branch
  //ILItem indexes
  BranchIndex: Integer; //Item containing the conditional branch (i.e. end of previous code)
  ThenLastIndex: Integer; //Last item in the THEN path
  ElseLastIndex: Integer; //Last item in the ELSE path. -1 if no ELSE path
  Cursor: TParseCursor; //Parsing save point
begin
  ElseLastIndex := -1;
  ThenLastIndex := -1;
  BranchIndex := -1;
  ThenLastID := -1;
  ThenBranch := nil;
  ElseLastID := -1;
  BranchID := -1;

  //Note: If Branch returns nil then we have a constant expression.
  //We'll use that fact to not generating code which will never be executed
  //We do, however, still need to parse that code!
  Result := ParseBranchExpr(Branch, ConstExprValue);
  if Result <> qeNone then
    EXIT;

  //Test for THEN - optional if there's a new line before code
  if not TestForIdent('then') then
  begin
    Result := Parser.SkipToNextLine;
    if Result = qeNewlineExpected then
      EXIT(Err(qeTHENExpected));
    if Result <> qeNone then
      EXIT;
  end;

  if Branch <> nil then
  begin
    BranchID := GetCurrBlockID;
    BranchIndex := ILGetCount-1;

    //THEN Block
    NewBlock := True;
    Branch.Dest.SetCondBranch;
    Branch.Dest.TrueBlockID := GetCurrBlockID + 1;
  end;

  //Parse block
  PrevSkipMode := SkipModeStart((Branch = nil) and not ConstExprValue);

  Result := Parser.SkipWhiteNL;
  if Result <> qeNone then
    EXIT;
  //Test for empty THEN block. If so skip parsing it.
  if not TestForIdent('else') then
  begin
    Result := ParseBlock(bsSingle, Storage);
    if Result <> qeNone then
      EXIT;
  end;
  SkipModeEnd(PrevSkipMode);

  if Branch <> nil then
  begin
    //Branch to merge block
    ThenBranch := ILAppendBranch(-1); //Target to be set later
    ThenLastID := GetCurrBlockID;
    ThenLastIndex := ILGetCount-1;
  end;

  Cursor := Parser.GetCursor;
  Result := Parser.SkipWhiteNL;
  if Result <> qeNone then
    EXIT;

  //Test for ELSE
  if TestForIdent('else') then
  begin //If so, ELSE block
    if Branch <> nil then
    begin
      NewBlock := True;
      Branch.Dest.FalseBlockID := GetCurrBlockID + 1;
    end;

    //Parse block
    PrevSkipMode := SkipModeStart((Branch = nil) and ConstExprValue);
    Result := ParseBlock(bsSingle, Storage);
    if Result <> qeNone then
      EXIT;
    SkipModeEnd(PrevSkipMode);

    if Branch <> nil then
    begin
      //Branch to merge block
      ILAppendBranch(GetCurrBlockID + 1);
      ElseLastID := GetCurrBlockID;
      ElseLastIndex := ILGetCount-1;
      NewBlock := True;
    end;
  end
  else //No ELSE - if condition failed it jumps straight to the merge block
  begin
    Parser.SetCursor(Cursor);
    if Branch <> nil then
    begin
      NewBlock := True;
      ElseLastID := -1;
      ElseLastIndex := -1;

      Branch.Dest.FalseBlockID := GetCurrBlockID + 1;
    end;
  end;

  if Branch <> nil then
  begin
    //Fixup branches at end of THEN and ELSE blocks
    NewBlock := True;
    ThenBranch.SetBranchBlockID(GetCurrBlockID + 1);

    //If we have two paths then we need to fixup any variable reads
    if ElseLastIndex >= 0 then
      BranchFixup(BranchIndex + 1, ThenLastIndex, ThenLastIndex + 1, ElseLastIndex);

    //phi expression(s) - for each variable
    if ElseLastIndex >= 0 then
      PhiWalk(ThenLastIndex, ElseLastIndex, BranchIndex, ThenLastID, ElseLastID)
    else
      PhiWalk(ThenLastIndex, -1, BranchIndex, ThenLastID, BranchID);
  end;
end;  //---------------------------------------------------------- IF


//==============================================STATEMENTS

//Parses a single statement
// <statement> := <block>
//             |  <function-call>
//             |  <constant-declaration>
//             |  <variable-declaration>
//             |  <assignment>
//             |  <if-statement>
//             |  <case-statement>
//             |  <for-statement>
//             |  <while-statement>
//             |  <repeat-until-statement>
function ParseStatement(Storage: TVarStorage): TQuicheError;
var
  Ident: String;
  Keyword: TKeyword;
  Scope: PScope;
  IdentType: TIdentType;
  Item: Pointer;
begin
  Result := Parser.SkipWhiteNL;
  if Result <> qeNone then
    EXIT;

  if Parser.TestChar = ';' then
    //Empty statement
    EXIT(qeNone);

  Result := ParseIdentifier(#0, Ident);
  if Result <> qeNone then
    EXIT;

  //Do we have a keyword?
  Keyword := IdentToKeyword(Ident);
  if Keyword <> keyUNKNOWN then
  begin
    case Keyword of
      keyBEGIN: Result := ParseBlock(bsBeginRead, Storage);
      keyCONST: Result := DoCONST('');
      keyVAR: Result := DoVAR('', Storage);
      keyFOR: Result := DoFOR(Storage);
      keyIF: Result := DoIF(Storage);
      keyREPEAT: Result := DoREPEAT(Storage);
      keyWHILE: Result := DoWHILE(Storage);
    else
      EXIT(ErrSub(qeInvalidKeyword, Ident));
    end;
    EXIT;
  end;

  Item := SearchScopes(Ident, IdentType, Scope);
  if Assigned(Item) then
  begin
    case IdentType of
      itVar:
      begin
        Result := ParseAssignment(false, false, False, Ident, PVariable(Item), Storage);
        if Result <> qeNone then
          EXIT;
      end;
      itFunction:
      begin
        Result := DoParseProcedureCall(PFunction(Item));
        if Result <> qeNone then
          EXIT;
      end;
      itConst: EXIT(ErrSub(qeConstNameNotValidHere, Ident));
      itType: EXIT(ErrSub(qeTypeNameNotValidHere, Ident));
    else
      EXIT(ErrBUG('Invalid/unknown IdentType'));
    end;
  end
  else
  begin //Identifier not found
    //If followed by := raise variable not found
    Parser.Mark;
    Result := Parser.SkipWhite;
    if Result <> qeNone then
      EXIT;
    if TestAssignment then
    begin
      Parser.Undo;
      EXIT(ErrSub(qeVariableNotFound, Ident));
    end;

    //Raise identifier not found
    EXIT(ErrSub(qeUndefinedIdentifier, Ident));
  end;
end;

function ParseStatements(EndAt: TEndAt;Storage: TVarStorage): TQuicheError;
begin
  Result := Parser.NextStatement(False);
  if Result <> qeNone then
    EXIT;
  while not Parser.EOF do
  begin
    case EndAt of
      eaEND: if TestForIdent('end') then
        EXIT(qeNone);
      eaUNTIL: if TestForIdent('until') then
        EXIT(qeNone);
      eaEOF: ;  //Nothing
    else
      Assert(False);
    end;
    Result := ParseStatement(Storage);
    if Result <> qeNone then
      EXIT;
    Result := Parser.NextStatement(True);
    if Result <> qeNone then
      EXIT;
  end;

  case EndAt of
    eaEOF: Result := qeNone;
    eaEND: Result := Err(qeENDExpected);
    eaUNTIL: Result := Err(qeUNTILExpected);
  else
    Assert(False);
  end;
end;

// <block> := BEGIN [ <statement-list> ] END
//         |  <statement>
// <statement-list> := <statement> [ <statement> ]
function ParseBlock(BlockState: TBlockState;Storage: TVarStorage): TQuicheError;
begin
  ScopeIncDepth;

  if BlockState = bsBeginRead then
    Result := ParseStatements(eaEND, Storage)
  else
    Result := ParseStatement(Storage);
  if Result <> qeNone then
    EXIT;

  ScopeDecDepth;
end;

// <globals> := [ <global-defs> ] [ <global-defs> ]
//              <main-block>
// <global-defs> := <function-def>
//               | <variable-declaration>
//               | <constant-declaration>
//               | <type-declaration>
// <main-block> := <block> . <end-of-file>
//              (no space between <block> and the period).
function ParseDeclarations(IsRoot: Boolean;AllowFuncs: Boolean;Storage: TVarStorage): TQuicheError;
type TDeclState = (dsNone, dsCONST, dsVAR);
var
  Ch: Char;
  Ident: String;
  Keyword: TKeyword;
  DeclState: TDeclState;
begin
  DeclState := dsNone;
  Result := Parser.NextStatement(False);
  if Result <> qeNone then
    EXIT;

  while true do
  begin
    Ch := Parser.TestChar;
    if Ch = '[' then
    begin //Attribute
      Parser.SkipChar;
      Result := ParseAttribute;
      if Result <> qeNone then
        EXIT;
      Keyword := keyUNKNOWN;
    end
    else if CharInSet(Ch, csIdentFirst) then
    begin //Identifier
      Result := ParseIdentifier(#0, Ident);
      Keyword := IdentToKeyword(Ident);
      if Result <> qeNone then
        EXIT;
      if Keyword = keyUNKNOWN then
        case DeclState of
          dsCONST: Result := DoCONST(Ident);
//          dsTYPE: ;
          dsVAR:   Result := DoVAR(Ident, Storage);
          else
            if IsRoot then
              EXIT(Err(qeInvalidTopLevel))
            else
              EXIT(Err(qeNestedFuncsNotAllowed));
        end;
    end
    else if Ch = #0 then
      EXIT(Err(qeUnexpectedEndOfFile))
    else
      EXIT(Err(qeSyntax));

    case Keyword of
      keyUNKNOWN: ; //Ignore (already processed above)
      keyCONST:
      begin
        Result := DoCONST('');
        DeclState := dsCONST;
      end;
      keyFUNCTION:
      begin
        Result := ParseFunctionDef(False);
        DeclState := dsNone;
      end;
      keyPROCEDURE:
      begin
        Result := ParseFunctionDef(True);
        DeclState := dsNone;
      end;
{      keyTYPE:
      begin
        //...
        DeclState := dsTYPE;
      end;
}      //At this scope all vars are global
      keyVAR:
      begin
        Result := DoVAR('', Storage);
        DeclState := dsVAR;
      end;
      keyBEGIN: //BEGIN ... END block
      begin
        //Check for unsatisfied forward declared function
        Result := AreAnyForwardsUnsatisfied;
        if Result <> qeNone then
          EXIT;

        Result := ParseBlock(bsBeginRead, Storage);
        if Result <> qeNone then
          EXIT;
        if IsRoot then
          Assert(ScopeGetDepth = 0);
        if IsRoot then
          if Parser.TestChar <> '.' then
            EXIT(Err(qeEndDotExpected))
          else
          begin
            Parser.SkipChar;

            Result := Parser.SkipWhiteNL;
            if Result <> qeNone then
              EXIT;
            if not Parser.EOF then
              EXIT(Err(qeCodeAfterEndDot));
          end;

        EXIT(qeNone);
      end;
    else  //Unknown keyword
        EXIT(Err(qeInvalidTopLevel))
    end;
    if Result <> qeNone then
      EXIT;

    Result := Parser.NextStatement(True);
    if Result <> qeNone then
      EXIT;
  end;
end;

initialization
  OnScopeDone := nil;
end.

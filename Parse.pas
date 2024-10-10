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

function ErrorLineNo: Integer;
function ErrorPos: Integer;
function ErrorLine: String;

function DoVAR(Ident: String;Storage: TVarStorage): TQuicheError;

//Parses a single statement
//If the first identifier has been parsed, it should be passed in in Ident,
//otherwise Ident must be an empty string
function ParseStatement(Storage: TVarStorage): TQuicheError;

function ParseStatements(ToEnd: Boolean;Storage: TVarStorage): TQuicheError;

//Parse the declaration part, including Types, Consts, and Vars.
//If AllowFuncs is True then Functions and Procedure declarations are also allowed.
//This enables the distinction between program level decarations and function/procedure
//level declarations (which Quiche doesn't allow)
function ParseDeclarations(IsRoot: Boolean;AllowFuncs: Boolean;Storage: TVarStorage): TQuicheError;

implementation
uses SysUtils, Classes,
  Def.Functions, Def.Globals, Def.IL, Def.Operators, Def.QTypes, Def.Scopes,
  Parse.Base, Parse.Fixups, Parse.FuncCall, Parse.FuncDef, Parse.Source;

//===============================================
//Utilities


function ErrorLineNo: Integer;
begin
  if Parser.MarkLineNo >= 0 then
    Result := Parser.MarkLineNo
  else
    Result := Parser.LineNo;
end;

function ErrorPos: Integer;
begin
  if Parser.MarkLineNo >= 0 then
    Result := Parser.MarkPos
  else
    Result := Parser.Pos;
end;

function ErrorLine: String;
begin
  Result := Parser.Line;
end;


//===============================================
//Language syntax

//Creates IL to assign the slug to a variable.
//If Variable is nil it will be created.
//If VType is nil:
//  If the variable is being created it will be assigned the implicit type of the expression
//  Otherwise the variable will be checked for compatibility with the expressions result type
procedure AssignSlugToVariable(const Slug: TExprSlug;var Variable: PVariable;
  VType: TVarType);
var VarVersion: Integer;
  ILItem: PILItem;
begin
  if Variable = nil then
  begin
    if VType = vtUnknown then
      Variable := VarCreateUnknown(Slug.ImplicitType)
    else
      Variable := VarCreateUnknown(VType);
  end;

  VarVersion := Variable.IncWriteCount;

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
    else
      ILItem := ILAppend(OpMove);
    ILItem.Param1 := Slug.Operand;
    ILItem.Param2.Kind := pkNone;
    ILItem.ResultType := Slug.ResultType;
  end;

  ILItem.Dest.SetVarDestAndVersion(Variable, VarVersion);

  //Overflows for an immediate assignment must be validated by the parser
  if ILItem.Op = OpStoreImm then
    ILItem.Flags := ILItem.Flags - [cgOverflowCheck];
end;

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
function ParseAssignmentExpr(var Variable: PVariable;VType: TVarType): TQuicheError;
var
  ExprType: TVarType;
  Slug: TExprSlug;
begin
  ExprType := VType;
  Result := ParseExpressionToSlug(Slug, ExprType);
  if Result <> qeNone then
    EXIT;

  AssignSlugToVariable(Slug, Variable, VType);
end;

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

  ExprType := vtBoolean;
  Result := ParseExpressionToSlug(Slug, ExprType);
  if Result <> qeNone then
    EXIT;
  Assert(ExprType = vtBoolean);

  if Slug.ILItem <> nil then
  begin //Expression
    ILItem := Slug.ILItem;
    //Convert item to a branch
    ILItem.Dest.SetCondBranch;
    if ILItem.Op in [opUnknown, OpMove, OpStoreImm] then
      ILItem.Op := OpBranch;
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
    ILItem := ILAppend(OpCondBranch);
    ILItem.Param1 := Slug.Operand;
    ILItem.Param2.Kind := pkNone;
    ILItem.ResultType := vtBoolean; //(Not technically needed)
  end;
end;

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
function ParseAssignment(VarRead, AllowVar: Boolean;VarName: String;
  out Variable: PVariable;Storage: TVarStorage): TQuicheError;
var Ch: Char;
  VarType: TVarType;  //vtUnknown if we're using type inference
  DoAssign: Boolean;  //True if we're assigning a value
  Creating: Boolean;
  Keyword: TKeyword;
begin
  Result := Parser.SkipWhiteNL;
  if Result <> qeNone then
    EXIT;

  //VAR keyword or variable name
  if VarName = '' then
  begin
    Result := ParseIdentifier(#0,VarName);
    if Result <> qeNone then
      EXIT;
  end;

  if optAllowAutoCreation and not VarRead and AllowVar then
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
  if optAllowAutoCreation or VarRead then
  begin
    Keyword := IdentToKeyword(VarName);
    if Keyword <> keyUnknown then
      EXIT(ErrSub(qeReservedWord, VarName));

    Result := TestForTypeSymbol(VarType);
    if Result <> qeNone then
      EXIT;

    if VarType <> vtUnknown then
    //<type-symbol> form
      DoAssign := TestAssignment
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
        EXIT(ErrSyntax(synVariableDeclaration));
      Parser.NextChar(Ch);
      Result := Parser.SkipWhiteNL;
      if Result <> qeNone then
        EXIT;

      Result := ParseVarType(VarType);
      if Result <> qeNone then
        EXIT;
      if VarType = vtUnknown then
        EXIT(Err(qeUnknownType));
      if VarType in [vtReal, vtString] then
        EXIT(ErrMsg(qeTODO, 'Type not yet supported: ' + VarTypeToName(VarType)));

      Result := Parser.SkipWhite;
      if Result <> qeNone then
        EXIT;

      Ch := Parser.TestChar;
      DoAssign := Parser.TestChar = '=';
      if DoAssign then
        Parser.SkipChar;
    end;

    if VarRead then
    begin
      Variable := VarFindByNameInScope(VarName);
      if Variable <> nil then
        EXIT(ErrSub(qeVariableRedeclared, VarName));
    end
    else
      Variable := VarFindByNameAllScopes(VarName);
  end
  else  //We're only doing assignment, no creation allowed
  begin
    DoAssign := TestAssignment;
    if not DoAssign then
      EXIT(ErrSyntax(synAssignmentExpected));
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
      EXIT(ErrSub(qeVariableRedeclared, VarName));
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
function DoVAR(Ident: String;Storage: TVarStorage): TQuicheError;
var Variable: PVariable;
begin
  Result := ParseAssignment(True, False, Ident, Variable, Storage);
end;



type TBlockState = (bsSingle, bsBeginRead);

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
  EndValue: PVariable;  //Hidden variable to contain end value for the loop
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
  Result := ParseAssignment(VarRead, false, LoopVarName, LoopVar, Storage);
  if Result <> qeNone then
    EXIT;

  if TestForIdent('to') then
    Step := 1
  else if TestForIdent('downto') then
    Step := -1
  else
    EXIT(ErrSyntaxMsg(synFOR, 'TO expected'));

  //Eval TO expression
  EndValue := nil;
  //Parse and create EndValue
  Result := ParseAssignmentExpr(EndValue, LoopVar.VarType);
  if Result <> qeNone then
    EXIT;

  //TODO: Insert code here for Step value

  //Test for DO - optional if there's a new line before code
  if not TestForIdent('do') then
    if Parser.SkipToNextLine <> qeNone then
      EXIT(ErrSyntaxMsg(synFOR, 'DO expected'));

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
  LoopVarPhi.Param1.SetPhiVarSource(EntryBlockID, LoopVar.WriteCount);
  //(Fixup param2 later)

  LoopVarPhi.Dest.SetPhiVarDest(LoopVar, LoopVar.IncWriteCount);

  HeaderBlockID := GetCurrBlockID;
  PhiItemIndex := ILGetCount - 1; //Needed later so we can Phi any other variables

  //Test LoopVar and Branch to Body or Exit
  case Step of
    -1: ExitTestItem := ILAppend(OpGreaterEqual);
    1:  ExitTestItem := ILAppend(OpLessEqual);
  else
    Assert(False, 'Invalid Step value');
  end;
  ExitTestItem.ResultType := vtBoolean;

  ExitTestItem.Param1.SetVarSource(LoopVar);
  ExitTestItem.Param2.SetVarSource(EndValue);

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
  ILItem.Param2.SetImmediate(vtByte);
  ILItem.Param2.Imm.IntValue := 1;

  ILItem.Dest.SetVarDestAndVersion(LoopVar, LoopVar.IncWriteCount);

  //Insert Branch back to Header section
  ILAppendBranch(HeaderBlockID);

  //Fixup Phi for Loopvar at start of Header section
  LoopVarPhi.Param2.SetPhiVarSource(GetCurrBlockID, LoopVar.WriteCount);

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

// <if-statement> := IF <boolean-expression> THEN
//                     <block>
//                   [ ELSE
//                     <block> ]
function DoIF(Storage: TVarStorage): TQuicheError;
var
  Branch: PILItem;      //The Branch condition. Nil if expression is a constant
  ConstExprValue: Boolean;  //If branch condition is s constant
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
    if Parser.SkipToNextLine <> qeNone then
      EXIT(ErrSyntaxMsg(synIF, 'THEN expected'));

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


//===============================================================

// <statement> := <block>
//             |  <function-call>
//             |  <variable-declaration>
//             |  <assignment>
//             |  <if-statement>
//             |  <case-statement>
//             |  <for-statement>
//             |  <while-statement>
//             |  <until-statement>
function ParseStatement(Storage: TVarStorage): TQuicheError;
var
  Ch: Char;
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
    keyVAR: Result := DoVAR('', Storage);
    keyFOR: Result := DoFOR(Storage);
    keyIF: Result := DoIF(Storage);
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
        Result := ParseAssignment(false, false, Ident, PVariable(Item), Storage);
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
      EXIT(ErrMsg(qeBug, 'Invalid/unknown IdentType'));
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

//Parses a list of statements
//If AllowEnd until exits when either EOF or the keyword END if read (but not consumed)
//otherwise exits at EOF
function ParseStatements(ToEnd: Boolean;Storage: TVarStorage): TQuicheError;
begin
  Result := Parser.NextStatement(False);
  if Result <> qeNone then
    EXIT;
  while not Parser.EOF do
  begin
    if ToEnd then
      if TestForIdent('end') then
        EXIT(qeNone);
    Result := ParseStatement(Storage);
    if Result <> qeNone then
      EXIT;
    Result := Parser.NextStatement(True);
    if Result <> qeNone then
      EXIT;
  end;
  if ToEnd then
    Result := Err(qeENDExpected)
  else
    Result := qeNone;
end;

// <block> := BEGIN [ <statement-list> ] END
//         |  <statement>
// <statement-list> := <statement> [ <statement> ]
function ParseBlock(BlockState: TBlockState;Storage: TVarStorage): TQuicheError;
begin
  ScopeIncDepth;

  if BlockState = bsBeginRead then
    Result := ParseStatements(True, Storage)
  else
    Result := ParseStatement(Storage);
  if Result <> qeNone then
    EXIT;

  ScopeDecDepth;
end;

// <globals> := [ <global-defs> ] [ <global-defs> ]
//              <main-block>
// <global-defs> := <function-def>
//               | <variable-def>
//               | <type-def>
//               | <constant-def>
// <main-block> := <block> . <end-of-file>
//              (no space between <block> and the period).
function ParseDeclarations(IsRoot: Boolean;AllowFuncs: Boolean;Storage: TVarStorage): TQuicheError;
type TDeclState = (dsNone, dsVAR);
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
//      Result := ParseKeyword(Keyword);
      if Result <> qeNone then
        EXIT;
      if Keyword = keyUNKNOWN then
        case DeclState of
//          dsCONST: ;
//          dsTYPE: ;
          dsVAR:
            Result := DoVAR(Ident, Storage);
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
{      keyCONST:
      begin
        //...
        DeclState := dsCONST;
      end;
}      keyFUNCTION:
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

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

//IL will be converted to assembler by calling OnScopeDone. This is called at the
//end of a function declaration, or  the end of the program for root level code.
type TAssembleCallback = function: Boolean;
var OnScopeDone: TAssembleCallback;

type TParseMode = (
  pmRootUnknown,  //At root level. Will change to pmProgram or pmRootCode as we encouter code
  pmProgram,      //At root level, within a 'program'.
                  //PROGRAM statement has been read.
                  //Code may include multiline CONST, TYPE and VAR statements.
                  //Code must end with a BEGIN ... END. block
  pmScript,       //At root level, parsing code outside a BEGIN...END block.
                  //Multiline CONST, TYPE, VAR are not allowed.

  pmFuncDecls,     //The declarations section of a function.
  pmStatement,    //Parse a single statement (which might be a block)
  pmBlock,        //A BEGIN ... END code block.
                  //The BEGIN has already been consumed
  pmREPEAT        //In a REPEAT ... UNTIL loop. End at the UNTIL keyword
  );

const ParseModeStrings: array[low(TParseMode)..high(TParseMode)] of String = (
  'RootUnknown', 'Program', 'Script', 'FuncDecls', 'Statement', 'Block', 'REPEAT');

//Parses Quiche source code.
function ParseQuiche(ParseMode: TParseMode;AddrMode: TAddrMode): TQuicheError;

implementation
uses SysUtils, Classes,
  Def.Functions, Def.Globals, Def.IL, Def.Operators, Def.QTypes, Def.Scopes,
  Def.Consts, Def.UserTypes,
  Parse.Base, Parse.Fixups, Parse.FuncCall, Parse.FuncDef, Parse.Source, Parse.TypeDefs,
  Parse.VarDefs, Parse.Literals;

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
var ExprType: PUserType;
//  ImplicitType: TVarType; //Dummy
  Slug: TExprSlug;
begin
  Slug.Initialise;

  ExprType := nil;
  Result := ParseExprToSlug(Slug, ExprType);
  if Result <> qeNone then
    EXIT;
  if UTToVT(ExprType) <> vtBoolean then
    EXIT(Err(qeBooleanExpressionExpected));

  if Slug.ILItem <> nil then
  begin //Expression
    ILItem := Slug.ILItem;
    //Convert item to a branch
    ILItem.Dest.SetCondBranch;
    if ILItem.Op in [opUnknown, OpMove, OpStoreImm] then
      ILItem.Op := OpBranch;
    Assert(ILItem.ResultVarType = vtBoolean);
    ILItem.ResultType := GetSystemType(vtFlag);  //Required for proper primitive selection
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
    ILItem.ResultType := GetSystemType(vtBoolean); //(Not technically needed)
  end;
end;

//============================= DECLARATIONS

//With the parser positioned at the char after an identifier (variable, const etc)
//attempts to parse a type-symbol or name
//<type-specifier> := <type-symbol>
//                  | :<type-name>
//Anything other syntax will return qeNone, VT as vtUnknown, and leaves the parser
//cursor unchanged
//If the <type-name> is unknown will return an error
function ParseTypeSpecifier(out UT: PUserType): TQuicheError;
begin
  if Parser.TestChar = ':' then
  begin
    Parser.Mark;
    Parser.SkipChar;
    if Parser.TestChar = '=' then
    begin
      Parser.Undo;
      UT := nil;
      EXIT(qeNone);
    end;

    Parser.SkipChar;
    Result := Parser.SkipWhiteNL;
    if Result <> qeNone then
      EXIT;
    Result := ParseTypeDefinition(UT);
  end
  else
  begin
    Result := TestForTypeSymbol(UT);
    if not Assigned(UT) then
      Result := qeNone;
  end;
end;

// <constant-declaration> := CONST <identifier>[: <type>] = <expr>
//                           CONST <identifier><type-symbol> = <expr>
function DoCONST(const Ident: String): TQuicheError;
var ConstName: String;
  Value: TImmValue;
  UType: PUserType;
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
  Result := ParseTypeSpecifier(UType);
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
  Result := ParseConstantExpr(Value, UType);
  if Result <> qeNone then
    EXIT;

  Consts.Add(ConstName, UType, Value);
end;

type TBlockState = (bsSingle, bsBeginRead);

//=================================== LOOPS

//Parse a code block.
function ParseBlock(ParseMode: TParseMode;AddrMode: TAddrMode): TQuicheError;
begin
  Assert(ParseMode in [pmStatement, pmBlock, pmREPEAT]);
  ScopeIncDepth;

  Result := ParseQuiche(ParseMode, AddrMode);
  if Result <> qeNone then
    EXIT;

  ScopeDecDepth;
end;


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
function DoFOR(AddrMode: TAddrMode): TQuicheError;
var
  VarStatus: TVarStatus;
  LoopVarName: String;
  LoopVar: PVariable;
  Step: Integer;       //TO or DOWNTO?
  ExprType: PUserType;
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

  if CompareText(LoopVarName, 'var') = 0 then
    VarStatus := vsVarRead
  else
    VarStatus := vsVarNotAllowed;

  if VarStatus = vsVarRead then
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
  Result := ParseVarDeclaration(VarStatus, asAssignRequired, LoopVarName, LoopVar, AddrMode);
  if Result <> qeNone then
    EXIT;

  if TestForIdent('to') then
    Step := 1
  else if TestForIdent('downto') then
    Step := -1
  else
    EXIT(Err(qeTOorDOWNTOExpected));

  //TO value
  ExprType := LoopVar.UserType;
  Result := ParseExprToSlug(Slug, ExprType);
  ToVar := nil;
  if Result <> qeNone then
    EXIT;
  //If the value is a constant
  if (Slug.ILItem = nil) and (Slug.Operand.Kind = pkImmediate) then
    ToValue := Slug.Operand.Imm
  else //if the value is not constant, assign it to a temp variable
    AssignSlugToDest(Slug, ToVar, ExprType);

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
  ExitTestItem.ResultType := GetSystemType(vtBoolean);
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
  Result := ParseBlock(pmStatement, AddrMode);
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
    if IsOrdinalType(LoopVar.VarType) then
      ILItem := ILAppend(opSucc)
    else //Real??
      ILItem := ILAppend(OpAdd);
    -1:
    if IsOrdinalType(LoopVar.VarType) then
      ILItem := ILAppend(opPred)
    else //Real??
      ILItem := ILAppend(OpSubtract);
  else
    Assert(False, 'Invalid Step Value');
  end;
  ILItem.ResultType := LoopVar.UserType;
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
function DoWHILE(AddrMode: TAddrMode): TQuicheError;
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

  Result := ParseBlock(pmStatement, AddrMode);
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
function DoREPEAT(AddrMode: TAddrMode): TQuicheError;
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
  Result := ParseBlock(pmREPEAT, AddrMode);
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
function DoIF(AddrMode: TAddrMode): TQuicheError;
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
    Result := ParseBlock(pmStatement, AddrMode);
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
    Result := ParseBlock(pmStatement, AddrMode);
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

//Parse a PROGRAM statement
//<program> :== PROGRAM [identifier]
function DoPROGRAM: TQuicheError;
var Ident: String;
begin
  Result := Parser.SkipWhite;
  if Result <> qeNone then
    EXIT;
  if CharInSet(Parser.TestChar, csIdentFirst) then
    Result := ParseIdentifier(#0, Ident);
end;

//Parses keywords that can begin a statement.
//Assumes the keyword has already been consumed.
//Assumes the keyword has already been validated for use in the current source
//code context.
function DoKeyword(Keyword: TKeyword;ParseMode: TParseMode;AddrMode: TAddrMode): TQuicheError;
var Func: PFunction; //Dummy
begin
  case Keyword of
    keyUNKNOWN: Result := Err(qeBUG);

    //Declarations
    keyPROGRAM:   Result := DoPROGRAM;

    keyFUNCTION:  Result := DoFUNCTION(False, fptNormal, Func);
    keyPROCEDURE: Result := DoFUNCTION(True, fptNormal, Func);

    keyCONST:     Result := DoCONST('');
    keyTYPE:      Result := DoTYPE('');
    keyVAR:       Result := DoVAR('', AddrMode);

    //Control flow
    keyFOR: Result := DoFOR(AddrMode);
    keyIF: Result := DoIF(AddrMode);
    keyREPEAT: Result := DoREPEAT(AddrMode);
    keyWHILE: Result := DoWHILE(AddrMode);

    keyBEGIN: //BEGIN ... END block
    begin
      if ParseMode = pmProgram then
      begin
        //Check for unsatisfied forward declared functions
        Result := AreAnyForwardsUnsatisfied;
        if Result <> qeNone then
          EXIT;
      end;

      Result := ParseBlock(pmBlock, AddrMode);
      if Result <> qeNone then
        EXIT;
      if ParseMode in [pmRootUnknown, pmProgram, pmScript] then
        Assert(ScopeGetDepth = 0);
      EXIT(qeNone);
    end;
  else  //Unknown keyword
    //Uncomment to all declaration level code, but needs parsing and checking
    //of indents to disambiguate between VAR, CONST, TYPE states and code state
//    Parser.SetCursor(Cursor);
//    ParseStatement(Storage);
      EXIT(Err(qeInvalidKeyword));
  end;
end;

//Non-keyword identifier which start a statement. These will either be:
// * Variable assignments
// * Function calls
function DoNonKeyword(const Ident: String;AddrMode: TAddrMode): TQuicheError;
var
  Scope: PScope;
  IdentData: TIdentData;
begin
  IdentData := SearchScopes(Ident, Scope);
  case IdentData.IdentType of
    itUnknown:
    begin //Identifier not found
      //If followed by := raise variable not found
      Parser.Mark;
      Result := Parser.SkipWhite;
      if Result <> qeNone then
        EXIT;
      if TestAssignment then
      begin
        if optVarAutoCreate then
          //Auto-create variables
          EXIT(ErrTODO('VarAutoCreate not yet implemented'));

        Parser.Undo;
        EXIT(ErrSub(qeVariableNotFound, Ident));
      end;

      //Raise identifier not found
      EXIT(ErrSub(qeUndefinedIdentifier, Ident));
    end;
    itVar:
    begin
      Result := ParseAssignment(IdentData.V);
      if Result <> qeNone then
        EXIT;
    end;
    itFunction:
    begin
      Result := DoParseProcedureCall(IdentData.F);
      if Result <> qeNone then
        EXIT;
    end;
    itConst: EXIT(ErrSub(qeConstNameNotValidHere, Ident));
    itType: EXIT(ErrSub(qeTypeNameNotValidHere, Ident));
    itEnumItem: EXIT(ErrSub(qeEnumItemNotValidHere, Ident));
  else
    EXIT(ErrBUG('Invalid/unknown IdentType'));
  end;
end;

function ParseQuiche(ParseMode: TParseMode;AddrMode: TAddrMode): TQuicheError;
type TDeclState = (dsNone, dsCONST, dsTYPE, dsVAR);
var
  Ch: Char;
  Ident: String;
  Keyword: TKeyword;
  DeclState: TDeclState;
  Cursor: TParseCursor;
begin
  //Detect empty statements
  if ParseMode = pmStatement then
  begin
    Parser.SkipWhiteNL;
    if Parser.TestChar = ';' then
      EXIT(qeNone);
  end;

  DeclState := dsNone;
  Result := Parser.NextStatement(False);
  if Result <> qeNone then
    EXIT;

  while true do
  begin
    Cursor := Parser.GetCursor;

    Ch := Parser.TestChar;
    if Ch = '[' then
    begin //Attribute
      Parser.SkipChar;
      Result := ParseAttribute;
      if Result <> qeNone then
        EXIT;
    end
    else if CharInSet(Ch, csIdentFirst) then
    begin //Identifier
      Result := ParseIdentifier(#0, Ident);
      Keyword := IdentToKeyword(Ident);
      if Result <> qeNone then
        EXIT;

      if Keyword <> keyUNKNOWN then
      begin
        //Cancel any pre-existing state
        DeclState := dsNone;

        //Process any keywords which affect our state
        case Keyword of
          keyCONST:
            if ParseMode in [pmProgram, pmFuncDecls] then
              DeclState := dsCONST;
          keyPROGRAM:
            if ParseMode = pmRootUnknown then
              ParseMode := pmProgram
            else
              EXIT(Err(qeInvalidPROGRAM));
          keyTYPE:
            if ParseMode in [pmProgram, pmFuncDecls] then
              DeclState := dsTYPE;
          keyEND:
            if ParseMode = pmBlock then
              EXIT(qeNone);
          keyUNTIL:
            if ParseMode = pmREPEAT then
              EXIT(qeNone);
          keyVAR:
            if ParseMode in [pmProgram, pmFuncDecls] then
              DeclState := dsVAR;
          keyFUNCTION, keyPROCEDURE:
            if not (ParseMode in [pmRootUnknown, pmProgram, pmScript]) then
              EXIT(Err(qeInvalidTopLevel));
          keyBEGIN: ; //Acceptable anywhere
        else
          if ParseMode in [pmProgram, pmFuncDecls] then
            EXIT(Err(qeInvalidTopLevel));
        end;
        if ParseMode = pmRootUnknown then
          if Keyword <> keyPROGRAM then
            ParseMode := pmScript;

        Result := DoKeyword(Keyword, ParseMode, AddrMode);
        if Result <> qeNone then
          EXIT;

        if Keyword = keyBEGIN then
        begin
          if ParseMode = pmProgram then
          begin //Program mode must end at an 'END.'
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
          end;

          EXIT(qeNone);
        end;

        if ParseMode = pmStatement then
          EXIT(qeNone);
      end
      else //keyUNKNOWN
      begin
        case DeclState of
          dsNone:
            if ParseMode in [pmProgram, pmFuncDecls] then
              EXIT(Err(qeInvalidTopLevel))
            else
              //Variable assignment or function call (ie NOT an error!)
              Result := DoNonKeyword(Ident, AddrMode);

          dsCONST: Result := DoCONST(Ident);
          dsTYPE: Result := DoTYPE(Ident);
          dsVAR:   Result := DoVAR(Ident, AddrMode);
          else
            Assert(False, 'Unknown DeclState');
        end;
        if Result <> qeNone then
          EXIT;

        if ParseMode = pmstatement then
          EXIT;
      end;
    end //Identifier
    else if Ch = #0 then
    begin
      if ParseMode = pmScript then
        EXIT(qeNone)
      else
        EXIT(Err(qeUnexpectedEndOfFile));
    end
    else
      EXIT(Err(qeIdentifierExpected));

    Result := Parser.NextStatement(True);
    if Result <> qeNone then
      EXIT;
  end;
end;

initialization
  OnScopeDone := nil;
end.

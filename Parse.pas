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
uses ParseErrors, ParseExpr;

type TAssembleCallback = function: Boolean;
var OnScopeDone: TAssembleCallback;

function ErrorLineNo: Integer;
function ErrorPos: Integer;
function ErrorLine: String;

//Parses a single statement
//If the first identifier has been parsed, it should be passed in in Ident,
//otherwise Ident must be an empty string
function ParseStatement(Ident: String): TQuicheError;

type TBlockState = (bsSingle, bsBeginRead);
//Parse a code block. BlockState specifies whether the initial BEGIN keyword
//has already been parsed (If so, nothing following it should have been parsed
function ParseBlock(BlockState: TBlockState): TQuicheError;

//Parse the declaration part, including Types, Consts, and Vars.
//If AllowFuncs is True then Functions and Procedure declarations are also allowed.
//This enables the distinction between program level decarations and function/procedure
//level declarations (which Quiche doesn't allow)
function ParseGlobals(AllowFuncs: Boolean): TQuicheError;

implementation
uses SysUtils, Classes,
  SourceReader, Globals, ILData, QTypes, Variables, Functions, Scopes, Operators,
  ParserBase, ParserFixups, ParseIntrinsics;
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
function ParseAssignmentExpr(var Variable: PVariable;var VarIndex: Integer;VType: TVarType): TQuicheError;
var
  ILItem: PILItem;
  VarSub: Integer;
  ExprType: TVarType;
  Slug: TExprSlug;
begin
  ExprType := VType;
  Result := ParseExpressionToSlug(@Slug, ExprType);
  if Result <> qeNone then
    EXIT;

  if Variable = nil then
    if VType = vtUnknown then
      Variable := VarCreateHidden(Slug.ImplicitType, optDefaultVarStorage, VarIndex)
    else
      Variable := VarCreateHidden(VType, optDefaultVarStorage, VarIndex);

  VarSub := VarIndexIncWriteCount(VarIndex);

  if Slug.ILItem <> nil then
  begin
    ILItem := Slug.ILItem;
    if ILItem.OpIndex = OpIndexNone then
      ILItem.OpIndex := OpIndexAssign;
  end
  else
  begin
    ILItem := ILAppend(dtData);
    ILItem.Param1 := Slug.Operand;
    ILItem.Param2.Loc := locNone;
    ILItem.OpIndex := OpIndexAssign;
    ILItem.OpType := Slug.OpType;
    ILItem.ResultType := VarTypeToOpType(Variable.VarType);
  end;

  ILItem.Dest.Loc := locVar;
  ILItem.Dest.VarIndex := VarIndex;
  ILItem.Dest.VarSub := VarSub;

//  if VType <> vtUnknown then
    //Sets any validation needed to assign the result to the variable
    //I.e. if VType is smaller than ResultType then we need to downsize the value
//    ILItem.ResultType := GetExprResultType(VType, Slug.ResultType);


  if (ILItem.OpIndex = OpIndexAssign) and (ILItem.OpType = rtUnknown) then
    if Slug.Operand.Loc = locImmediate then
      case GetTypeSize(Slug.ResultType) of
        1: Slug.OpType := rtX8;
        2: Slug.OpType := rtX16;
      else
        raise Exception.Create('Unknown Assignment type size');
      end
    else
      Slug.OpType := VarTypeToOpType(Slug.ResultType);

  //Overflows for an immediate assignment must be validated by the parser
  if (ILItem.OpIndex = OpIndexAssign) and (ILItem.Param1.Loc = locImmediate) then
    ILItem.CodeGenFlags := ILItem.CodeGenFlags - [cgOverflowCheck];
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
  ExprType := vtBoolean;
  Result := ParseExpressionToSlug(@Slug, ExprType);
  if Result <> qeNone then
    EXIT;
  Assert(ExprType = vtBoolean);

  if Slug.ILItem <> nil then
  begin
    ILItem := Slug.ILItem;
    //Convert item to a branch
    ILItem.DestType := dtCondBranch;
    if (ILItem.OpIndex = OpIndexNone) or (ILItem.OpIndex = OpIndexAssign) then
      ILItem.OpIndex := OpIndexBranch;
  end
  //ILItem = nil
  else if Slug.Operand.Loc = locImmediate then
  begin
    Assert(Slug.Operand.ImmType = vtBoolean);
    ILItem := nil;
    ConstExprValue := (Slug.Operand.ImmValue and $ff) = (valueTrue and $ff);
  end
  else
  begin //ILItem = nil and OpIndex <> None
    ILItem := ILAppend(dtCondBranch);
    ILItem.Param1 := Slug.Operand;
    ILItem.Param2.Loc := locNone;
//    if Slug.Operand.Loc = LocImmediate then
//      ILItem.OpIndex := OpIndexConstBranch
//    else
      ILItem.OpIndex := OpIndexCondBranch;
    ILItem.OpType := rtBoolean;
    ILItem.ResultType := rtBoolean; //(Not technically needed)
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
  out Variable: PVariable;var VarIndex: Integer): TQuicheError;
var Ch: Char;
  VarType: TVarType;  //vtUnknown if we're using type inference
  DoAssign: Boolean;  //True if we're assigning a value
  Creating: Boolean;
begin
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
      Parser.SkipWhitespaceAll;
      Ch := Parser.TestChar;
      if Ch <> ':' then
        EXIT(ErrSyntax(synVariableDeclaration));
      Parser.NextChar(Ch);

      Result := ParseVarType(VarType);
      if Result <> qeNone then
        EXIT;
      if VarType = vtUnknown then
        EXIT(Err(qeUnknownType));
      if VarType in [vtReal, vtString] then
        EXIT(ErrMsg(qeTODO, 'Type not yet supported: ' + VarTypeToName(VarType)));

      Parser.SkipWhiteSpaceAll;
      Ch := Parser.TestChar;
      DoAssign := Parser.TestChar = '=';
      if DoAssign then
        Parser.SkipChar;
    end;

    if VarRead then
    begin
      Variable := VarFindByNameInScope(VarName, VarIndex);
      if Variable <> nil then
        EXIT(ErrSub(qeVariableRedeclared, VarName));
    end
    else
      Variable := VarFindByNameAllScopes(VarName, VarIndex);
{      Variable := nil;
      VarIndex := -1;
    end
    else
}  end
  else  //We're only doing assignment, no creation allowed
  begin
    DoAssign := TestAssignment;
    if not DoAssign then
      EXIT(ErrSyntax(synAssignmentExpected));
    Variable := VarFindByNameAllScopes(VarName, VarIndex);
    if Variable = nil then
      EXIT(ErrSub(qeVariableNotFound, VarName));
    VarType := Variable.VarType;
  end;

  //Are we assigning a value today?
  if DoAssign then
  begin
    Creating := Variable = nil;
    //Parse the expression
//    if Variable <> nil then
//      Result := ParseAssignmentExpr(Variable, VarIndex, Variable.VarType)
//    else
      Result := ParseAssignmentExpr(Variable, VarIndex, VarType);
    if Result <> qeNone then
      EXIT;

    if Creating and (VarType <> vtUnknown) then
      VarSetType(Variable, VarType);

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
      VarSetName(Variable, VarName);
  end
  else
  begin //Otherwise just create it. Meh. Boring
    Variable := VarCreate(VarName, VarType, optDefaultVarStorage, VarIndex);
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
function DoVAR: TQuicheError;
var Variable: PVariable;
  VarIndex: Integer;
begin
  Result := ParseAssignment(True, False, '', Variable, VarIndex);
end;

// <for-statement> := FOR <identifier> := <expr> TO <expr> DO
//                      <block>
function DoFOR: TQuicheError;
var
  VarRead: Boolean;
  LoopVarName: String;
  LoopVar: PVariable;
  LoopVarIndex: Integer;
  ToInc: Boolean;       //TO or DOWNTO?
  EndValue: PVariable;  //Hidden variable to contain end value for the loop
  EndValueIndex: Integer;
  EntryBlockID: Integer;
  EntryLastItemIndex: Integer;
//  ForLastID: Integer; //Block ID of FOR statement (prior to loop);
//  ForLastIndex: Integer; //Last Item before loop (i.e. the FOR branch into the loop)
  HeaderBlockID: Integer;
  PhiItemIndex: Integer;
  LoopVarPhi: PILItem;  //Phi node for the loop variable
//  LoopBodyBlockID: Integer;
  ExitTestItem: PILItem;
  ILItem: PILItem; //Used for various Items
  PhiInsertCount: Integer;
begin
  //ENTRY section - prepare for loop
  //-------------
  //Read loop variable name
  Result := ParseIdentifier(#0, LoopVarName);
  if Result <> qeNone then
    EXIT;

  VarRead := CompareText(LoopVarName, 'var') = 0;
  if VarRead then
  begin
    Result := ParseIdentifier(#0, LoopVarName);
    if Result <> qeNone then
      EXIT;
  end;

  //Insert test here for FOR .. IN form
//  if TestForIdent('in') then
    //

  //(Create and) assign the loop variable
  Result := ParseAssignment(VarRead, false, LoopVarName, LoopVar, LoopVarIndex);
  if Result <> qeNone then
    EXIT;

  if TestForIdent('to') then
    ToInc := True
  else if TestForIdent('downto') then
    ToInc := False
  else
    EXIT(ErrSyntaxMsg(synFOR, 'TO expected'));

  //Eval TO expression
  EndValue := nil;
  EndValueIndex := -1;
  //Parse and create EndValue
  Result := ParseAssignmentExpr(EndValue, EndValueIndex, LoopVar.VarType);//vtUnknown);
  if Result <> qeNone then
    EXIT;

  //Insert code here for Step value

  if not TestForIdent('do') then
    EXIT(ErrSyntaxMsg(synFOR, 'DO expected'));

  //Insert Branch into header
  ILItem := ILAppend(dtBranch);
  ILItem.OpIndex := OpIndexBranch;
  ILItem.BranchBlockID := GetCurrBlockID + 1;
  EntryBlockID := GetCurrBlockID;
  EntryLastItemIndex := ILGetCount - 1;

  //HEADER Block - start of the looping section, Phi nodes and test for end
  //------------
  //Insert Phi for loop variable
  NewBlock := True;
  LoopVarPhi := ILAppend(dtData);
  LoopVarPhi.OpIndex := OpIndexPhi;

  LoopVarPhi.Param1.Loc := locPhiVar;
  LoopVarPhi.Param1.PhiBlockID := EntryBlockID;
  LoopVarPhi.Param1.PhiSub := LoopVar.Sub;
  //(Fixup param2 later)
  LoopVarPhi.Param2.Loc := locPhiVar;

  LoopVarPhi.Dest.Loc := locPhiVar;
  LoopVarPhi.Dest.PhiVarIndex := LoopVarIndex;
  LoopVarPhi.Dest.PhiSub := VarIncWriteCount(LoopVar);

  HeaderBlockID := GetCurrBlockID;
  PhiItemIndex := ILGetCount - 1; //Needed later so we can Phi any other variables

  //Test LoopVar and Branch to Body or Exit
  ExitTestItem := ILAppend(dtCondBranch);
  ExitTestItem.OpType := VarTypeToOpType(LoopVar.VarType);
  ExitTestItem.ResultType := rtBoolean;
  if ToInc then
    ExitTestItem.OpIndex := OpIndexLessEqual
  else
    ExitTestItem.OpIndex := OpIndexGreaterEqual;
  ExitTestItem.Param1.Loc := locVar;
  ExitTestItem.Param1.VarIndex := LoopVarIndex;
  ExitTestItem.Param1.VarSub := LoopVar.Sub;

  ExitTestItem.Param2.Loc := locVar;
  ExitTestItem.Param2.VarIndex := EndValueIndex;
  ExitTestItem.Param2.VarSub := EndValue.Sub;

  ExitTestItem.TrueBlockID := GetCurrBlockID + 1; //Body BlockID
  //FalseBlock to be fixed up at end of loop

  //BODY section - the code that's being looped
  //------------
  NewBlock := True;
//  LoopBodyBlockID := GetCurrBlockID;

  //Parse Loop block
  Result := ParseBlock(bsSingle);
  if Result <> qeNone then
    EXIT;

  //Insert Branch into Latch section
  ILItem := ILAppend(dtBranch);
  ILItem.OpIndex := OpIndexBranch;
  ILItem.BranchBlockID := GetCurrBlockID + 1;

  //LATCH section - next LoopVar and branch back to header
  //-------------
  NewBlock := True;
  //Next loopvar
  ILItem := ILAppend(dtData);
  ILItem.OpType := VarTypeToOpType(LoopVar.VarType);
  ILItem.ResultType := ILItem.OpType;
  if ToInc then
    ILItem.OpIndex := OpIndexAdd
  else
    ILItem.OpIndex := OpIndexSubtract;
  ILItem.Param1.Loc := locVar;
  ILItem.Param1.VarIndex := LoopVarIndex;
  ILItem.Param1.VarSub := LoopVar.Sub;

  //(Uncomment to add Step value)
  ILItem.Param2.Loc := locImmediate; //locVar;
  ILItem.Param2.ImmValue := 1; //StepValueIndex;
  ILItem.Param2.ImmType := vtByte;

  ILItem.Dest.Loc := locVar;
  ILItem.OpType := VarTypeToOpType(LoopVar.VarType);
  ILItem.Dest.VarIndex := LoopVarIndex;
  ILItem.Dest.VarSub := VarIncWriteCount(LoopVar);

  //Insert Branch back to Header section
  ILItem := ILAppend(dtBranch);
  ILItem.OpIndex := OpIndexBranch;
  ILItem.BranchBlockID := HeaderBlockID;

  //Fixup Phi for Loopvar at start of Header section
  LoopVarPhi.Param2.Loc := locPhiVar;
  LoopVarPhi.Param2.PhiBlockID := GetCurrBlockID;
  LoopVarPhi.Param2.VarSub := LoopVar.Sub;

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
  ExitTestItem.FalseBlockID := GetCurrBlockID + 1;

  //Insert Phis after loop
  NewBlock := True;
end;

// <if-statement> := IF <boolean-expression> THEN
//                     <block>
//                   [ ELSE
//                     <block> ]
function DoIF: TQuicheError;
var
  Branch: PILItem;      //The Branch condition. Nil if expression is a constant
  ConstExprValue: Boolean;  //If branch condition is s constant
  PrevSkipMode: Boolean;    //Cache old SkipMode
  ThenBranch: PILItem;
  ElseBranch: PILItem;
  //BlockIDs
  BranchID: Integer;    //Block ID of the conditional branch
  ThenLastID: Integer;  //Last block of the THEN branch
  ElseLastID: Integer;  //Last block of the ELSE branch
  //ILItem indexes
  BranchIndex: Integer; //Item containing the conditional branch (i.e. end of previous code)
  ThenLastIndex: Integer; //Last item in the THEN path
  ElseLastIndex: Integer; //Last item in the ELSE path. -1 if no ELSE path
begin
  //Note: If Branch returns nil then we have a constant expression.
  //We'll use that fact to not generating code which will never be executed
  //We do, however, still need to parse that code!
  Result := ParseBranchExpr(Branch, ConstExprValue);
  if Result <> qeNone then
    EXIT;

  //Test for THEN
  if not TestForIdent('then') then
    EXIT(ErrSyntaxMsg(synIF, 'THEN expected'));

  if Branch <> nil then
  begin
    BranchID := GetCurrBlockID;
    BranchIndex := ILGetCount-1;

    //THEN Block
    NewBlock := True;
    Branch.TrueBlockID := GetCurrBlockID + 1;
  end;

  //Parse block
  PrevSkipMode := SkipModeStart((Branch = nil) and not ConstExprValue);
  Result := ParseBlock(bsSingle);
  if Result <> qeNone then
    EXIT;
  SkipModeEnd(PrevSkipMode);

  if Branch <> nil then
  begin
    //Branch to merge block
    ThenBranch := ILAppend(dtBranch);
    ThenBranch.OpIndex := OpIndexBranch;
    ThenLastID := GetCurrBlockID;
    ThenLastIndex := ILGetCount-1;
  end;

  //Test for ELSE
  if TestForIdent('else') then
  begin //If so, ELSE block
    if Branch <> nil then
    begin
      NewBlock := True;
      Branch.FalseBlockID := GetCurrBlockID + 1;
    end;

    //Parse block
    PrevSkipMode := SkipModeStart((Branch = nil) and ConstExprValue);
    Result := ParseBlock(bsSingle);
    if Result <> qeNone then
      EXIT;
    SkipModeEnd(PrevSkipMode);

    if Branch <> nil then
    begin
      //Branch to merge block
      ElseBranch := ILAppend(dtBranch);
      ElseBranch.OpIndex := OpIndexBranch;
      ElseLastID := GetCurrBlockID;
      ElseLastIndex := ILGetCount-1;

      //Branch to merge block
      NewBlock := True;
      ElseBranch.BranchBlockID := GetCurrBlockID + 1;
    end;
  end
  else //No ELSE - if condition failed it jumps straight to the merge block
  begin
    if Branch <> nil then
    begin
      NewBlock := True;
      ElseLastID := -1;
      ElseLastIndex := -1;

      Branch.FalseBlockID := GetCurrBlockID + 1;
    end;
  end;

  if Branch <> nil then
  begin
    //Fixup branches at end of THEN and ELSE blocks
    NewBlock := True;
    ThenBranch.BranchBlockID := GetCurrBlockID + 1;

    //If we have two paths then we need to fixup any variable reads
    if ElseLastIndex >= 0 then
      BranchFixup(BranchIndex + 1, ThenLastIndex, ThenLastIndex + 1, ElseLastIndex);

    //phi expression(s) - for each variable
    if ElseLastIndex >= 0 then
      PhiWalk(ThenLastIndex, ElseLastIndex, BranchIndex, ThenLastID, ElseLastID)
    else
      PhiWalk(ThenLastIndex, -1, BranchIndex, ThenLastID, BranchID);
  end;
end;

//===============================================================

function DoParseFunctionCall(Func: PFunction;AsProc: Boolean): TQuicheError;
begin
  case Func.CallingConvention of
    ccIntrinsic: Result := ErrMsg(qeBUG, 'Instrinic dispatch shouldn''t end up here');  //Special cases :)
    ccExtern: Result := ErrMsg(qeTODO, 'Extern function dispatch not yet implemented'); //Calls to assembly
    ccStackFrame: Result := ErrMsg(qeTODO, 'StackFrame funciton dispatch not yet implemented'); //IX bases
  else
    raise Exception.Create('Unknown calling convention in function despatch :(');
  end;
end;

// <statement> := <block>
//             |  <function-call>
//             |  <variable-declaration>
//             |  <assignment>
//             |  <if-statement>
//             |  <case-statement>
//             |  <for-statement>
//             |  <while-statement>
//             |  <until-statement>
function ParseStatement(Ident: String): TQuicheError;
var
  Ch: Char;
  Keyword: TKeyword;
  Variable: PVariable;
  VarIndex: Integer;
  Scope: PScope;
  IdentType: TIdentType;
  Item: Pointer;
  Index: Integer;
  Func: PFunction;
  OpIndex: Integer;
  Slug: TExprSlug;  //Dummy
  PSlug: PExprSlug;
begin
  if Ident = '' then
  begin
    Ch := Parser.TestChar;
    if Ch in csIdentFirst then
    begin
      Result := ParseIdentifier(#0, Ident);
      if Result <> qeNone then
        EXIT;
    end
    else
      EXIT(ErrSyntax(synStatementExpected));
  end;

  //Do we have a keyword?
  Keyword := IdentToKeyword(Ident);
  if Keyword <> keyUNKNOWN then
  begin
    case Keyword of
    keyVAR: Result := DoVAR;
    keyFOR: Result := DoFOR;
    keyIF: Result := DoIF;
    else
      EXIT(ErrSub(qeInvalidKeyword, Ident));
    end;
    EXIT;
  end
  else if SearchScopes(Ident, IdentType, Scope, Item, Index) then
  begin
    case IdentType of
      itVar:
      begin
        Result := ParseAssignment(false, false, Ident, PVariable(Item), Index);
        if Result <> qeNone then
          EXIT;
      end;
      itFunction:
      begin
        Result := DoParseFunctionCall(PFunction(Item), True);
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
  begin //Search for intrinsics
    OpIndex := OpProcedureToIndex(Ident);
    if OpIndex <> -1 then
    begin
      PSlug := @Slug;
      Result := ParseIntrinsic(OpIndex, False, PSlug);
    end
    else
    begin
      //TODO: Search builtin function library

      //If followed by := raise variable not found
      Parser.Mark;
      Parser.SkipWhiteSpace;
      if TestAssignment then
      begin
        Parser.Undo;
        EXIT(ErrSub(qeVariableNotFound, Ident));
      end;

      //Raise identifier not found
      EXIT(ErrSub(qeUndefinedIdentifier, Ident));
    end;
  end;
end;

// <block> := BEGIN [ <statement-list> ] END
//         |  <statement>
// <statement-list> := <statement> [ <statement> ]
function ParseBlock(BlockState: TBlockState): TQuicheError;
var
  Ch: Char;
  Ident: String;
begin
  while True do
  begin
    Parser.SkipWhitespaceAll;
    Ch := Parser.TestChar;
    if Ch in csIdentFirst then
    begin
      Result := ParseIdentifier(#0, Ident);
      if Result <> qeNone then
        EXIT;

      if (BlockState = bsBeginRead) and (CompareText(Ident, 'end') = 0) then
        EXIT(qeNone);

      if CompareText(Ident, 'begin') = 0 then
      begin
        Result := ParseBlock(bsBeginRead);
        if Result <> qeNone then
          EXIT;
        if BlockState = bsSingle then
          EXIT(qeNone);
      end
      else
      begin
        Result := ParseStatement(Ident);
        if Result <> qeNone then
          EXIT;

        repeat
          Parser.SkipWhiteSpaceAll;
          Ch := Parser.TestChar;
          if Ch = ';' then
            Parser.SkipChar;
        until Parser.EOF or not (Ch in [#0,';']);

        if BlockState = bsSingle then
          EXIT(qeNone);
        if Parser.EOF then
          EXIT(Err(qeENDExpected));
      end;
    end
    else
      EXIT(Err(qeIdentifierExpected));
  end;
end;

//<function-body> := [ <var-declaration> ]
//                |  [ <type-declaration> ]
//                |  [ <const-declaration> ]
//                <block>
//Parse the declarations and body of a function declaration
//Inputs: Func is the function being declared
//        Keyword is the first keyword after the function header
function ParseFunctionCode(Func: PFunction; Keyword: TKeyword): TQuicheError;
begin
  //Setup scope for function
  CreateCurrentScope(Func.Name);

  while True do
  begin
    case Keyword of
//      keyCONST: ;
//      keyTYPE: ;
      keyVAR: Result := DoVAR;
      keyBEGIN:
      begin
        Result := ParseBlock(bsBeginRead);
        if Result <> qeNone then
          EXIT;

        if Assigned(OnScopeDone) then
          if not OnScopeDone then
            EXIT(ErrSub(qeAssemblyError, Func.Name));

        //Result to previous scope
        EndCurrentScope;
        EXIT(qeNone);
      end;
    else
      EXIT(Err(qeFunctionBodyExpected));
    end;
    if Result <> qeNone then
      EXIT;

    Result := ParseKeyword(Keyword);
    if Result <> qeNone then
      EXIT;
  end;
end;

//======================================Functions/Procedures

//Parse a parameter name and add it to the function definition
//Also validates that the parameter name is unique within the definition, and that
//it is not a reserved word
//If Func is forward declaration (processed earlier), validates that the parameter
//name matches that declared earlier
//Inputs:
//  Ident: If an identifier (parameter name) has already been parsed, it can be
//      passed to the function here, otherwise (if Ident = '') the parameter name
//      will be read from the parser.
//      Note: If the Ident is passed in then it is *not* validated against keywords
//        This is assumed to have been done by the caller. I.e. the caller found the
//        Ident was not a parameter specifier and passed it on to us as a name
//  Func: The function definition being read
//  ParamIndex: The index of the parameter currently being parsed.
function ParseParamName(Ident: String;Func: PFunction;ParamIndex: Integer): TQuicheError;
var Keyword: TKeyword;
begin
  //No name passed in, so read one
  if Ident = '' then
  begin
    Result := ParseIdentifier(#0, Ident);
    if Result <> qeNone then
      EXIT;
    Keyword := IdentToKeyword(Ident);
    if Keyword <> keyUnknown then
      EXIT(ErrSub(qeReservedWord, Ident));
  end;

  //Have we already read a parameter with that name?
  if ParamIndex > 0 then
    if FuncFindParam(Func, Ident, ParamIndex-1) <> nil then
      EXIT(ErrMsg(qeFunctionDeclaration, 'Parameter name redeclared: ' + Ident));

  //Does current match any forward declaration?
  if ffForward in Func.Flags then
  begin
    if CompareText(Ident, Func.Params[ParamIndex].Name) <> 0 then
      EXIT(Err(qeFuncDecDoesntMatch))
  end
  else //No forward declaration, set data
    Func.Params[ParamIndex].Name := Ident;

  Result := qeNone;
end;

//Validate that a register type declaration is valid:
//  Reg params must be Value, Out or Result
//  Registers must be unique within a declaration, unless one usage is an input
//  (value) parameter, and the other is an output (out, result) parameter.
function ValidateRegParam(Func: PFunction;ParamIndex: Integer;Reg: TParamReg): Boolean;
var Specifier: TParamSpecifier;
  IsOutput: Boolean;
  P: Integer;
begin
  Specifier := Func.Params[ParamIndex].Specifier;
  if not (Specifier in [psVal, psOut, psResult]) then
    EXIT(False);
  if ParamIndex = 0 then
    EXIT(True);
  IsOutput := Specifier in [psOut, psResult];
  for P := 0 to ParamIndex-1 do
    if (Func.Params[P].Reg = Reg) and (IsOutput = (Func.Params[P].Specifier in [psOut, psResult])) then
      EXIT(False);
  Result := True;
end;

//Parse the type part of a parameter declaration and add it to the function declaration
//Assums that the ':' has already been consumed.
//Returns once a valid type definition has been consumed. No attention is given to
//the characters that follow.
//This routine is generic enough to be used both for the parameter list and the
//function return type declaration.
//Valid type definitions are:
//  <Type-def> := : <Type-name>
//             |  : <Register-name> [AS <Type-name>]
//Inputs:
//  Func: The function definition being parsed
//  FirstParam and ParamIndex:
//  Parameter definitions can declare a list of parameters to be declared
//  with the same type, for example 'A,B: Integer'. FirstParam is the index of the
//  first param in such a list ('A' in the above example) ParamIndex is the index
//  of the last ('B' in this example). If the definition is for a single parameter
//  then FirstParam and ParamIndex will be the same.
//  The type definition which is parsed will be  applied to all parameters from
//  FirstParam to ParamIndex inclusive

//The type name will be validated as a known type.
//The register name, if present, will be validated to be unique within the function
//definition. However, a register name can be used for both an input and output
//value. Ouptut value inludes the function result.
//A register type definition for a list of parameter names ('A,B:HL') is also an error.
//If any parameter is a register type definition then all must be, including the result.
//Register type definitions may only use the following parameter specifiers:
//value (default), out, and result
function ParseParamType(Func: PFunction;FirstParam, ParamIndex: Integer): TQuicheError;
var Ident: String;
  Reg: TParamReg;
  VarType: TVarType;
  P: Integer;
begin
  Parser.SkipWhiteSpaceAll;
  //Read type name or register name
  Result := ParseIdentifier(#0, Ident);
  if Result <> qeNone then
    EXIT;

  Parser.SkipWhiteSpaceAll;
  //Is a Z80 register (or flag!) specified?
  Reg := IdentToParamReg(Ident);
  if Reg <> prNone then
  begin
    //Registers have to be unique. So list parameters are an error
    if FirstParam <> ParamIndex then
      EXIT(ErrMsg(qeFunctionDeclaration, 'Register parameter redeclared: ' + Ident));
    if not ValidateRegParam(Func, ParamIndex, Reg) then
      EXIT(ErrMsg(qeFunctionDeclaration, 'Invalid register parameter name'));
    //TODO: Validate all params are reg params ?At end of definition?
    if TestForIdent('as') then
    begin //Type specified
      Result := ParseVarType(VarType);
      if Result <> qeNone then
        EXIT;
      if VarType = vtUnknown then
        EXIT(ErrSub(qeUnknownType, Ident));
    end
    else //Default types: Reg to Byte, Pair to Word, Flag to Boolean
      VarType := ParamRegToVarType[Reg]
  end
  else
  begin //Not a Reg definition
    VarType := StringToVarType(Ident);
    if VarType = vtUnknown then
      EXIT(ErrSub(qeUnknownType, Ident));
  end;

  //Set (or check) the parameter(s) data
  for P := FirstParam to ParamIndex do
  begin
    if ffForward in Func.Flags then
    begin
      if (Func.Params[P].Reg <> Reg) or
        //TODO: AreTypesCompatible()
        (Func.Params[P].VarType <> VarType) then
//        (Func.Params[P].VarTypes <> [VarType]) then
        EXIT(Err(qeFuncDecDoesntMatch));
    end
    else
    begin
      Func.Params[P].Reg := Reg;
      Func.Params[P].VarType := VarType;
//      Func.Params[P].VarTypes := [VarType];
    end;
  end;
end;

//Parse the parameter list of a function definition - I.e. the section in parentheses
//Assumes the caller has consumed the '(' before a parameter definition list.
//Returns having consumed the trailing ')' after the parameter list
// <param-def-list> := ( <Param-def> [ ; <Param-def> ] )
// <Param-def> := <Param-name> [ , <Param-name>] <Param-type>
function ParseParamDefs(Func: PFunction): TQuicheError;
var
  ParamIndex: Integer;
  ListStart: Integer; //If we have a comma separated list of parameters, this
                      //is the index of the first
  Ident: String;
  Keyword: TKeyword;
  Specifier: TParamSpecifier;
  Ch: Char;
begin
  ParamIndex := 0;

  //Loop until we hit the trailing brace or an error
  repeat
    if ParamIndex > MaxParams then
      EXIT(Err(qeDecTooManyParams));

    //Parameter specifier
    Parser.SkipWhiteSpaceAll;
    Result := ParseIdentifier(#0, Ident);
    if Result <> qeNone then
      EXIT;

    //Read parameter specifier (or name)
    Keyword := IdentToKeyword(Ident);
    case Keyword of
      keyUnknown: Specifier := psVal;
      keyVAR: SPecifier := psVar;
      keyCONST: Specifier := psConst;
//      keyIN: Specifier := psIn; //Isn't this the same as Val?
      keyOUT: Specifier := psOut;
    else  //Any other keywords are an error
      EXIT(ErrSub(qeReservedWord, Ident));
    end;
    //If we had a specifier then we haven't read the name yet
    if Keyword <> keyUnknown then
      Ident := '';

    //Loop for all the names in a comma separated list
    ListStart := ParamIndex;
    repeat
      //Hard coded maximum parameter count (eek!)
      if ParamIndex > MaxParams then
        EXIT(Err(qeDecTooManyParams));

      //Validate or set the specifier (applies to all params in the list)
      if ffForward in Func.Flags then
      begin
        if Specifier <> Func.Params[ParamIndex].Specifier then
          EXIT(Err(qeFuncDecDoesntMatch));
      end
      else
        Func.Params[ParamIndex].Specifier := Specifier;

      //Process (and parse if Ident is '') the parameter name and add it to the definition
      Parser.SkipWhiteSpaceAll;
      Result := ParseParamName(Ident, Func, ParamIndex);
      if Result <> qeNone then
        EXIT;
      Parser.SkipWhiteSpaceAll;

      //Next parameter
      inc(ParamIndex);

      //Test for more parameters in list, end of list, or error
      Ch := Parser.TestChar;
      if Ch = ',' then
        Parser.SkipChar;
      Ident := '';
    until Ch <> ',';

    dec(ParamIndex);

    //Type definition marker
    if Ch <> ':' then
      EXIT(ErrSyntaxMsg(synFunctionParameterDeclaration, ': expected after parameter name'));

    //Parse and process the type definition, and apply it to the list of parameters
    //we've just read.
    Parser.SkipChar;
    Result := ParseParamType(Func, ListStart, ParamIndex);
    if Result <> qeNone then
      EXIT;

    Parser.SkipWhiteSpaceAll;

    //We want either a semicolon to start another parameter definition, or a
    //closing brace to end
    Ch := Parser.TestChar;
    Parser.SkipChar;
    if not (Ch in [';',')']) then
      EXIT(ErrSyntaxMsg(synFunctionParameterDeclaration, ''';'' or '')'' expected'));
    inc (ParamIndex);
  until Ch = ')';

  dec(ParamIndex);
  //Store the parameter count in the function definition
  Func.ParamCount := ParamIndex+1;
  Result := qeNone;
end;

//Parse an extern directive and add it to the function definition
// <extern-def> := EXTERN <constant-expression>
//Assumes the 'extern' keyword has already been consumed
//Note: The functions calling convention value is set (and validated) by the caller
//We only parse, validate, and set the address and other data
function ParseExternDef(Func: PFunction): TQuicheError;
var IsReg: Boolean;
  P: Integer;
  Slug: TExprSlug;
  ExprType: TVarType;
begin
  //Validate an extern definition: if one param is register then they all must be
  if Func.ParamCount + Func.ResultCount > 1 then
  begin
    IsReg := Func.Params[0].Reg <> prNone;
    for P := 1 to Func.ParamCount + Func.ResultCount-1 do
      if IsReg <> (Func.Params[P].Reg <> prNone) then
        EXIT(ErrMsg(qeFunctionDeclaration, 'Either all parameters must be register parameters, or none'));
  end;

  ExprType := vtPointer;
  Result := ParseExpressionToSlug(@Slug, ExprType);
  if Result <> qeNone then
    EXIT;
  if (Slug.ILItem <> nil) or (Slug.Operand.Loc <> locImmediate) then
    EXIT(Err(qeConstantExpressionExpected));
  Func.CodeAddress := Slug.Operand.immValue;
  Result := qeNone;
end;

//Parses the directives section of a function declaration.
//This function loops through any valid directives, processing them as needed until
//the first keyword which is not a directive, then either:
//1) Parses the function declarations and body and returns NextKeyword as keyUnknown.
//2) If the function has no body (external, forward) stops returns with NextKeyword
//   containing the first keyword after the function declaration.
//Note: Any identifiers which are not keywords will return an error.
function ParseDirectives(Func: PFunction;out NoCode: Boolean;out NextKeyword: TKeyword): TQuicheError;
var IsForward: Boolean; //This declaration is a Forward
//  NoCode: Boolean;  //True if the function has no code section after it (extern, forward)
  Convention: TCallingConvention; //If current directive is a calling convention
begin
  Isforward := False;
  NoCode := False;

  while True do
  begin
    Convention := ccUnknown;

    //Possible separator for directives
    Parser.SkipWhiteSpaceAll;
    while Parser.TestChar = ';' do
    begin
      Parser.SkipChar;
      Parser.SkipWhiteSpaceAll;
    end;

    //Do we have a any directives? E.g. calling convention
    Result := ParseKeyword(NextKeyword);
    if Result <> qeNone then
      EXIT;
    if NextKeyword = keyUnknown then
      EXIT(ErrSyntaxMsg(synFunctionDeclaration, 'Directive or function body expected'));
    case NextKeyword of
      keyFORWARD:
      begin
        if IsForward or (Func.CallingConvention = ccExtern) then
          //Extern and forward are incompatible
          EXIT(ErrMsg(qeFunctionDeclaration, 'Extern functions can''t be forward declared'));
        NoCode := True;
        IsForward := True;
        if ffForward in Func.Flags then
          EXIT(ErrMsg(qeFunctionDeclaration, 'Function has already been declared as FORWARD'));
      end;
      keyEXTERN:
      begin
        if IsForward then //Extern and forward are incompatible
          EXIT(ErrMsg(qeFunctionDeclaration, 'Extern functions can''t be forward declared'));
        Convention := ccExtern;
        NoCode := True;
        Result := ParseExternDef(Func);
        if Result <> qeNone then
          EXIT;
      end;
//      keySTACK: Convention := ccStackFrame;
//      keyREGISTER: Convention := ccRegister;
      //To add extra calling conventions:
      //* Set the Convention variable
      //* Perform additional parsing and validation as required
      //The code after this case statement validates that we don't already have a
      //calling convention, that it matches a forward etc.
    else
      //Update forward status of the definition as needed. If this is forward add it,
      //if previous was forward remove it (which shows we don't have any 'hanging' forwards
      //and avoid multiple definitions)
      if IsForward then
        Func.Flags := Func.Flags + [ffForward]
      else
        Func.Flags := Func.Flags - [ffForward];

      if Func.CallingConvention = ccUnknown then
        Func.CallingConvention := DefaultCallingConvention;

//      if not NoCode then
        EXIT(qeNone);
{      begin
      //If it's a forward declaraion then anything else will be more global stuff.
      //If not forward then it's the start of the routine's code.
        Result := ParseFunctionCode(Func, NextKeyword);
        if Result <> errNone then
          EXIT;
        NextKeyword := keyUnknown;
      end;
      EXIT(errNone);
}    end;

    //We have a calling convention
    if Convention <> ccUnknown then
      if ffForward in Func.Flags then
      begin //Forward must match previous
        if Func.CallingConvention <> Convention then
          EXIT(Err(qeFuncDecDoesntMatch))
      end
      else if Func.CallingConvention <> ccUnknown then
        //Can't have muliple calling conventions
        EXIT(ErrMsg(qeFunctionDeclaration, 'Only one calling conventioned allowed'))
      else
        Func.CallingConvention := Convention;
  end;
end;

// <function-def> := FUNCTION [ <param-def-list> ] <type-def> ; [<directive>] [,<directive>]
//                |  PROCEDURE [ <param-def-list> ] ; [ <directive> ] [, <directive> ]
// <directive> := <extern-def> | FORWARD
//Parse a function delaration and add it to the list of functions.
//If the function has previously been 'forward' defined validates the definition
//correctly matches it.
//Assumes that the 'function' or 'procedure' keyword has already been consumed
//Inputs:
//  Proc is True if we're parsing a Procedure definition, false for a Function
function ParseFunctionDef(Proc: Boolean;out NextKeyword: TKeyword): TQuicheError;
var Func: PFunction;
  Ident: String;
  NoCode: Boolean;  //Returned by ParseDirectives. If True the declaration has no body
                    //(forward, extern etc).
begin
  //Function name...
  Parser.SkipWhiteSpace;
  Result := ParseIdentifier(#0, Ident);
  if Result <> qeNone then
    EXIT;
  //...error if it's a keyword or already defined (unless a forward)
  if IdentToKeyword(Ident) <> keyUnknown then
    EXIT(ErrSub(qeReservedWord, Ident));
  Func := FuncFindInScope(Ident);
  if Func = nil then
    Func := FuncCreate(NameSpace, Ident)
  else
  begin
    if not (ffForward in Func.Flags) then
      EXIT(ErrSub(qeFunctionRedeclared, Ident))
    else if Proc <> (Func.ResultCount = 0) then
      EXIT(Err(qeFuncDecDoesntMatch));
  end;
  //Corrupts attribute - TODO
  //Probably better to have a preserves list instead?
  Func.Corrupts := AttrCorrupts;
  AttrCorrupts := [];

  //Note: If function is Forward then we need to varify this declaration is an
  //exact copy of the original

  //Does it have a parameter list? If so parse it
  Parser.SkipWhiteSpace;
  if Parser.TestChar = '(' then
  begin
    Parser.SkipChar;
    Result := ParseParamDefs(Func);
    if Result <> qeNone then
      EXIT;
  end;

  //If it's a function then it needs a return type,
  //It it's a procedure then it mustn't have one
  Parser.SkipWhiteSpaceAll;
  if not Proc then
    if Parser.TestChar = ':' then
    begin
      Parser.SkipChar;
      if ffForward in Func.Flags then
      begin
        if Func.Params[Func.ParamCount].Specifier <> psResult then
          EXIT(Err(qeFuncDecDoesntMatch))
      end
      else
        Func.Params[Func.ParamCount].Specifier := psResult;
      Result := ParseParamType(Func, Func.ParamCount, Func.ParamCount);
      if Result <> qeNone then
        EXIT;
      Func.ResultCount := 1;
    end
    else
      EXIT(ErrMsg(qeFunctionDeclaration, 'Function must have a result'));

  Result := ParseDirectives(Func, NoCode, NextKeyword);
  if Result <> qeNone then
    EXIT;

  if not NoCode then
  begin
    //If it's a forward declaraion then anything else will be more global stuff.
    //If not forward then it's the start of the routine's code.
    Result := ParseFunctionCode(Func, NextKeyword);
    if Result <> qeNone then
      EXIT;
    NextKeyword := keyUnknown;
  end;

{  if (ffForward in Func.Flags) or (Func.CallingConvention = ccExtern) then
  else
    Result := ParseFunctionCode(Func, NextKeyword);
}end;

//---------------------------
//The main BEGIN ... END. block
function ParseMainBlock: TQuicheError;
begin
  Result := ParseBlock(bsBeginRead);
  if Result <> qeNone then
    EXIT;
  if Parser.TestChar <> '.' then
    EXIT(Err(qeEndDotExpected));
  Parser.SkipChar;

  Parser.SkipWhiteSpaceAll;
  if not Parser.EOF then
    EXIT(Err(qeCodeAfterEndDot));
  Result := qeNone;
end;

// <globals> := [ <global-defs> ] [ <global-defs> ]
//              <main-block>
// <global-defs> := <function-def>
//               | <variable-def>
//               | <type-def>
//               | <constant-def>
// <main-block> := <block> . <end-of-file>
//              (no space between <block> and the period).
function ParseGlobals(AllowFuncs: Boolean): TQuicheError;
var
  Ch: Char;
  Keyword: TKeyword;
  NextKeyword: TKeyword;
begin
  NextKeyword := keyUnknown;

  while true do
  begin
    if NextKeyword <> keyUnknown then
      Keyword := NextKeyword
    else
    begin
      Parser.SkipWhiteSpaceAll;
      Ch := Parser.TestChar;
      //Attribute
      if Ch = '[' then
      begin
        Parser.SkipChar;
        Result := ParseAttribute;
        if Result <> qeNone then
          EXIT;
        Keyword := keyUnknown;
      end
      else if Ch in csIdentFirst then
      begin //Identifier
        Result := ParseKeyword(Keyword);
        if Result <> qeNone then
          EXIT;
        if Keyword = keyUnknown then
          EXIT(Err(qeInvalidTopLevel));
      end
      else
        EXIT(Err(qeInvalidTopLevel));
    end;

    NextKeyword := keyUnknown;
    if Keyword <> keyUnknown then
    begin
      case Keyword of
//        keyCONST: ;
        keyFUNCTION: Result := ParseFunctionDef(False, NextKeyword);
        keyPROCEDURE: Result := ParseFunctionDef(True, NextKeyword);
//        keyTYPE: ;
        keyVAR: Result := DoVAR;
        keyBEGIN: //the program's BEGIN ... END. block
        begin
            Result := ParseMainBlock;
            EXIT;
        end;

      else
        EXIT(Err(qeInvalidTopLevel))
      end;
//TESTING ONLY - TO BE REMOVED
//If Result = errIdentifierExpected then
//  EXIT(errNone);
//--END
      if Result <> qeNone then
        EXIT;

    end;
  end;
end;

initialization
  OnScopeDone := nil;
end.

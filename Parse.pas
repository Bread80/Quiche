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
uses ParseErrors, ParseExpr, Variables;

type TAssembleCallback = function: Boolean;
var OnScopeDone: TAssembleCallback;

function ErrorLineNo: Integer;
function ErrorPos: Integer;
function ErrorLine: String;

function DoVAR(Storage: TVarStorage): TQuicheError;

//Parses a single statement
//If the first identifier has been parsed, it should be passed in in Ident,
//otherwise Ident must be an empty string
function ParseStatement(Ident: String;Storage: TVarStorage): TQuicheError;

type TBlockState = (bsSingle, bsBeginRead);
//Parse a code block. BlockState specifies whether the initial BEGIN keyword
//has already been parsed (If so, nothing following it should have been parsed
function ParseBlock(BlockState: TBlockState;Storage: TVarStorage): TQuicheError;

//Parse the declaration part, including Types, Consts, and Vars.
//If AllowFuncs is True then Functions and Procedure declarations are also allowed.
//This enables the distinction between program level decarations and function/procedure
//level declarations (which Quiche doesn't allow)
function ParseGlobals(AllowFuncs: Boolean): TQuicheError;

implementation
uses SysUtils, Classes,
  SourceReader, Globals, ILData, QTypes, Functions, Scopes, Operators,
  ParserBase, ParserFixups, ParseIntrinsics, ParseFuncDef;
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
function ParseAssignmentExpr(var Variable: PVariable;Storage: TVarStorage;
  var VarIndex: Integer;VType: TVarType): TQuicheError;
var
  ILItem: PILItem;
  VarSub: Integer;
  ExprType: TVarType;
  Slug: TExprSlug;
begin
  ExprType := VType;
  Result := ParseExpressionToSlug(Slug, ExprType);
  if Result <> qeNone then
    EXIT;

  if Variable = nil then
    if VType = vtUnknown then
      Variable := VarCreate('', Slug.ImplicitType, Storage, VarIndex)
    else
      Variable := VarCreate('', VType, Storage, VarIndex);

  VarSub := VarIndexIncWriteCount(VarIndex);

  if Slug.ILItem <> nil then
  begin
    ILItem := Slug.ILItem;
    if ILItem.OpIndex = OpIndexNone then
      ILItem.OpIndex := OpIndexAssign;
  end
  else
  begin
    ILItem := ILAppend(dtData, OpIndexAssign);
    ILItem.Param1 := Slug.Operand;
    ILItem.Param2.Loc := locNone;
    ILItem.OpType := Slug.OpType;
    ILItem.ResultType := VarTypeToOpType(Variable.VarType);
  end;

  ILItem.Dest.SetVar(VarIndex, VarSub);

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
  Result := ParseExpressionToSlug(Slug, ExprType);
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
    ILItem := ILAppend(dtCondBranch, OpIndexCondBranch);
    ILItem.Param1 := Slug.Operand;
    ILItem.Param2.Loc := locNone;
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
  out Variable: PVariable;Storage: TVarStorage;var VarIndex: Integer): TQuicheError;
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
      Result := ParseAssignmentExpr(Variable, Storage, VarIndex, VarType);
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
    Variable := VarCreate(VarName, VarType, Storage, VarIndex);
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
function DoVAR(Storage: TVarStorage): TQuicheError;
var Variable: PVariable;
  VarIndex: Integer;
begin
  Result := ParseAssignment(True, False, '', Variable, Storage, VarIndex);
end;

// <for-statement> := FOR <identifier> := <expr> TO <expr> DO
//                      <block>
function DoFOR(Storage: TVarStorage): TQuicheError;
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
  ScopeIncDepth;  //If loop counter is decalred here, ensure it goes out of scope
                  //after the loop

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
  Result := ParseAssignment(VarRead, false, LoopVarName, LoopVar, Storage, LoopVarIndex);
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
  Result := ParseAssignmentExpr(EndValue, Storage, EndValueIndex, LoopVar.VarType);
  if Result <> qeNone then
    EXIT;

  //Insert code here for Step value

  if not TestForIdent('do') then
    EXIT(ErrSyntaxMsg(synFOR, 'DO expected'));

  //Insert Branch into header
  ILItem := ILAppend(dtBranch, OpIndexBranch);
  ILItem.BranchBlockID := GetCurrBlockID + 1;
  EntryBlockID := GetCurrBlockID;
  EntryLastItemIndex := ILGetCount - 1;

  //HEADER Block - start of the looping section, Phi nodes and test for end
  //------------
  //Insert Phi for loop variable
  NewBlock := True;
  LoopVarPhi := ILAppend(dtData, OpIndexPhi);
  LoopVarPhi.Param1.Loc := locPhiVar;
  LoopVarPhi.Param1.PhiBlockID := EntryBlockID;
  LoopVarPhi.Param1.PhiSub := LoopVar.WriteCount;
  //(Fixup param2 later)
  LoopVarPhi.Param2.Loc := locPhiVar;

  LoopVarPhi.Dest.Loc := locPhiVar;
  LoopVarPhi.Dest.PhiVarIndex := LoopVarIndex;
  LoopVarPhi.Dest.PhiSub := LoopVar.IncWriteCount;

  HeaderBlockID := GetCurrBlockID;
  PhiItemIndex := ILGetCount - 1; //Needed later so we can Phi any other variables

  //Test LoopVar and Branch to Body or Exit
  if ToInc then
    ExitTestItem := ILAppend(dtCondBranch, OpIndexLessEqual)
  else
    ExitTestItem := ILAppend(dtCondBranch, OpIndexGreaterEqual);
  ExitTestItem.OpType := VarTypeToOpType(LoopVar.VarType);
  ExitTestItem.ResultType := rtBoolean;

  ExitTestItem.Param1.SetVar(LoopVarIndex, LoopVar.WriteCount);
  ExitTestItem.Param2.SetVar(EndValueIndex, EndValue.WriteCount);

  ExitTestItem.TrueBlockID := GetCurrBlockID + 1; //Body BlockID
  //FalseBlock to be fixed up at end of loop

  //BODY section - the code that's being looped
  //------------
  NewBlock := True;
//  LoopBodyBlockID := GetCurrBlockID;

  //Parse Loop block
  Result := ParseBlock(bsSingle, Storage);
  if Result <> qeNone then
    EXIT;

  //Insert Branch into Latch section
  ILItem := ILAppend(dtBranch, OpIndexBranch);
  ILItem.BranchBlockID := GetCurrBlockID + 1;

  //LATCH section - next LoopVar and branch back to header
  //-------------
  NewBlock := True;
  //Next loopvar
  if ToInc then
    ILItem := ILAppend(dtData, OpIndexAdd)
  else
  ILItem := ILAppend(dtData, OpIndexSubtract);
  ILItem.OpType := VarTypeToOpType(LoopVar.VarType);
  ILItem.ResultType := ILItem.OpType;
  ILItem.Param1.SetVar(LoopVarIndex, LoopVar.WriteCount);
  //(Uncomment to add Step value)
  ILItem.Param2.SetImmediate(1, vtByte);

  ILItem.Dest.SetVar(LoopVarIndex, LoopVar.IncWriteCount);

  //Insert Branch back to Header section
  ILItem := ILAppend(dtBranch, OpIndexBranch);
  ILItem.BranchBlockID := HeaderBlockID;

  //Fixup Phi for Loopvar at start of Header section
  LoopVarPhi.Param2.Loc := locPhiVar;
  LoopVarPhi.Param2.PhiBlockID := GetCurrBlockID;
  LoopVarPhi.Param2.VarSub := LoopVar.WriteCount;

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

  ScopeDecDepth;  //If loop counter was declared take it out of scope
end;

// <if-statement> := IF <boolean-expression> THEN
//                     <block>
//                   [ ELSE
//                     <block> ]
function DoIF(Storage: TVarStorage): TQuicheError;
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
  Result := ParseBlock(bsSingle, Storage);
  if Result <> qeNone then
    EXIT;
  SkipModeEnd(PrevSkipMode);

  if Branch <> nil then
  begin
    //Branch to merge block
    ThenBranch := ILAppend(dtBranch, OpIndexBranch);
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
    Result := ParseBlock(bsSingle, Storage);
    if Result <> qeNone then
      EXIT;
    SkipModeEnd(PrevSkipMode);

    if Branch <> nil then
    begin
      //Branch to merge block
      ElseBranch := ILAppend(dtBranch, OpIndexBranch);
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
function ParseStatement(Ident: String;Storage: TVarStorage): TQuicheError;
var
  Ch: Char;
  Keyword: TKeyword;
  Scope: PScope;
  IdentType: TIdentType;
  Item: Pointer;
  Index: Integer;
  OpIndex: Integer;
  Slug: TExprSlug;  //Dummy, value assigned will be ignored
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
    keyVAR: Result := DoVAR(Storage);
    keyFOR: Result := DoFOR(Storage);
    keyIF: Result := DoIF(Storage);
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
        Result := ParseAssignment(false, false, Ident, PVariable(Item), Storage, Index);
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
      Result := ParseIntrinsic(OpIndex, False, Slug)
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
function ParseBlock(BlockState: TBlockState;Storage: TVarStorage): TQuicheError;
var
  Ch: Char;
  Ident: String;
begin
  //Variable depth
  if BlockState = bsBeginRead then
    ScopeIncDepth;

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
      begin
        ScopeDecDepth;
        EXIT(qeNone);
      end;

      if CompareText(Ident, 'begin') = 0 then
      begin
        Result := ParseBlock(bsBeginRead, Storage);
        if Result <> qeNone then
          EXIT;
        if BlockState = bsSingle then
          EXIT(qeNone);
      end
      else
      begin
        Result := ParseStatement(Ident, Storage);
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


//---------------------------
//The main BEGIN ... END. block
function ParseMainBlock: TQuicheError;
begin
  Result := ParseBlock(bsBeginRead, vsAbsolute);
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
        //At this scope all vars are global
        keyVAR: Result := DoVAR(vsAbsolute);
        keyBEGIN: //the program's BEGIN ... END. block
        begin
            Result := ParseMainBlock;
            if Result <> qeNone then
              EXIT;
            Assert(ScopeGetDepth = 0);
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

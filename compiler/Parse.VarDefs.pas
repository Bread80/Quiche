(*
Parsing variable declararions and assignments to variables
*)
unit Parse.VarDefs;

interface
uses Def.Variables, Def.UserTypes,
  Parse.Errors, Parse.Base;

type
  TVarStatus = (
    vsVarRead,  //VAR keyword has been parsed (we're in an declaration)
    vsVarAllowed, //VAR keyword needs to be tested for (we're in assignment or declaration);
    vsVarNotAllowed);  //We're in an assignment expression

  TAssignStatus = (
    asAssignAllowed,
    asAssignRequired,
    asAssignNotAllowed);

// <assignment> := <variable-name> := <expression>
//Parses assignment and variable declarations
//VarStatus specifies whether a VAR keyword has been read, or is allowed to be read
//If the variable name has already been parsed it must be passed in in VarName,
//if not VarName must be empty
//Variable returns the Variable which was either created or assigned to
//VarIndex returns the Index in the variable list of Variable
//Also inspects the optAllowAutoCreation option to determine if a declaration
//requires an explicit 'var' or can be implied by the first assignment to a variable
function ParseVarDeclaration(VarStatus: TVarStatus; AssignStatus: TAssignStatus;const Ident: String;
  out Variable: PVariable;AddrMode: TAddrMode): TQuicheError;

// <variable-declararion> := VAR <identifier>[: <type>] [= <expr>]
//                           (Either <type> or <expr> (or both) must be given
//                        |  VAR <identifier><type-symbol> [:= <expr>]
//                           (no space allowed between <identifier> and <type-symbol>)
//                           VAR <identifier> := <expr>
function DoVAR(const Ident: String;AddrMode: TAddrMode): TQuicheError;

implementation
uses SysUtils,
  Def.Globals, Def.VarTypes, Def.Scopes,
  Parse.TypeDefs, Parse.Expr;


function ParseVarDeclaration(VarStatus: TVarStatus; AssignStatus: TAssignStatus;const Ident: String;
  out Variable: PVariable;AddrMode: TAddrMode): TQuicheError;
var VarName: String;
  UserType: PUserType;  //nil if we're using type inference
  HaveAssign: Boolean;  //True if we're assigning a value
  Creating: Boolean;
  Keyword: TKeyword;
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

  if optVarAutoCreate and (VarStatus = vsVarAllowed) then
  begin
    if CompareText(VarName, 'var') = 0 then
      VarStatus := vsVarRead;

    if VarStatus = vsVarRead then
    begin
      Result := ParseIdentifier(#0,VarName);
      if Result <> qeNone then
        EXIT;
    end;
  end;

  UserType := nil;
  //Is a there any form of type specifier?
  if optVarAutoCreate or (VarStatus = vsVarRead) then
  begin
    Keyword := IdentToKeyword(VarName);
    if Keyword <> keyUnknown then
      EXIT(ErrSub(qeReservedWord, VarName));

    Result := TestForTypeSymbol(UserType);
    if Result <> qeNone then
      EXIT;

    if Assigned(UserType) then
      //<type-symbol> form
      HaveAssign := TestAssignment
    else if TestAssignment then
      //Type inference form - nothing to do here
      HaveAssign := True
    else
    begin //Type name form
      Result := Parser.SkipWhite;
      if Result <> qeNone then
        EXIT;

      if Parser.TestChar <> ':' then
        EXIT(Err(qeColonExpectedInVAR));
      Parser.SkipChar;
      Result := Parser.SkipWhiteNL;
      if Result <> qeNone then
        EXIT;

      Result := ParseTypeDefinition(UserType);
      if Result <> qeNone then
        EXIT;
      if not Assigned(UserType) then //(This is probably no longer needed)
        EXIT(Err(qeUnknownType));
      case UTToVT(UserType) of
        vtReal, vtString:
          EXIT(ErrTODO('Type not yet supported: ' + UserType.Name));
        vtUnboundArray:
          EXIT(ErrSub2(qeInstantiateUnboundedArray, UserType.Name, UserType.Name));
        vtVector:
          if UserType.VecLength = iUnboundedArray then
            EXIT(ErrSub2(qeInstantiateUnboundedArray, UserType.Name, UserType.Name));
        vtList:
          if UserType.VecLength = iUnboundedArray then
            EXIT(ErrSub2(qeInstantiateUnboundedArray, UserType.Name, UserType.Name));
      else
        //No probs
      end;


      Result := Parser.SkipWhite;
      if Result <> qeNone then
        EXIT;

      HaveAssign := Parser.TestChar = '=';

      if (AssignStatus = asAssignRequired) and not HaveAssign then
        EXIT(Err(qeEqualExpectedInAssignment));
      if HaveAssign then
        Parser.SkipChar;
    end;

    if VarStatus = vsVarRead then
    begin
      if GetCurrentScope.Search(VarName).IdentType <> itUnknown then
        EXIT(ErrSub(qeIdentifierRedeclared, VarName));
      Variable := nil;
    end
    else
      Variable := Vars.FindByNameAllScopes(VarName);
  end
  else  //We're only doing assignment, no creation allowed
  begin
    HaveAssign := TestAssignment;
    if not HaveAssign then
      EXIT(Err(qeAssignmentExpected));
    Variable := Vars.FindByNameAllScopes(VarName);
    if Variable = nil then
      EXIT(ErrSub(qeVariableNotFound, VarName));
    UserType := Variable.UserType;
  end;
  if HaveAssign and (AssignStatus = asAssignNotAllowed) then
    EXIT(Err(qeAssignmentNotAllowed));
  if (AssignStatus = asAssignRequired) and not HaveAssign then
    EXIT(Err(qeAssignmentExpected));

  //Are we assigning a value today?
  if HaveAssign then
  begin
    Creating := Variable = nil;
    Result := ParseAssignmentExpr(Variable, UserType);
    if Result <> qeNone then
      EXIT;

    if Creating and Assigned(UserType) then
      Variable.SetType(UserType);

    if Creating then
      Variable.SetName(VarName);
  end
  else
  begin //Otherwise just create it. Meh. Boring
    Variable := Vars.Add(VarName, UserType);
    if Variable = nil then
      EXIT(ErrSub(qeIdentifierRedeclared, VarName));
    Result := qeNone;
  end;
end;

function DoVAR(const Ident: String;AddrMode: TAddrMode): TQuicheError;
var Variable: PVariable;
begin
  Result := ParseVarDeclaration(vsVarRead, asAssignAllowed, Ident, Variable, AddrMode);
end;

end.

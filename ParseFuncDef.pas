unit ParseFuncDef;

interface
uses ParserBase, ParseErrors;

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

implementation
uses Functions, SysUtils, QTypes, ParseExpr, ILData, Variables, Scopes, Parse, Globals;


//<function-body> := [ <var-declaration> ]
//                |  [ <type-declaration> ]
//                |  [ <const-declaration> ]
//                <block>
//Parse the declarations and body of a function declaration
//Inputs: Func is the function being declared
//        Keyword is the first keyword after the function header
function ParseFunctionBody(Func: PFunction; Keyword: TKeyword; Storage: TVarStorage): TQuicheError;
begin
  //Setup scope for function
  CreateCurrentScope(Func.Name);

  while True do
  begin
    case Keyword of
//      keyCONST: ;
//      keyTYPE: ;
      keyVAR: Result := DoVAR(Storage);
      keyBEGIN:
      begin
        Result := ParseBlock(bsBeginRead, Storage);
        if Result <> qeNone then
          EXIT;

        if Assigned(OnScopeDone) then
          if not OnScopeDone then
            EXIT(ErrSub(qeAssemblyError, Func.Name));

        Assert(ScopeGetDepth = 0);
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
  Result := ParseExpressionToSlug(Slug, ExprType);
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
  Ident: String;
  Convention: TCallingConvention; //If current directive is a calling convention
//  HaveDirective: Boolean;
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

//    HAveDirective := False;
    //Do we have a any directives? E.g. calling convention
    Parser.Mark;
    Result := ParseIdentifier(#0, Ident);
    if Ident <> '' then
      NextKeyword := IdentToKeyword(Ident)
    else
      NextKeyword := keyUNKNOWN;
//    if NextKeyword = keyUnknown then
//      EXIT(ErrSyntaxMsg(synFunctionDeclaration, 'Directive or function body expected'));
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
      if NextKeyword = keyUNKNOWN then
        Parser.Undo;
      //Update forward status of the definition as needed. If this is forward add it,
      //if previous was forward remove it (which shows we don't have any 'hanging' forwards
      //and avoid multiple definitions)
      if IsForward then
        Func.Flags := Func.Flags + [ffForward]
      else
        Func.Flags := Func.Flags - [ffForward];

      if Func.CallingConvention = ccUnknown then
        Func.CallingConvention := DefaultCallingConvention;

        EXIT(qeNone);
    end;

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
    Result := ParseFunctionBody(Func, NextKeyword, optDefaultVarStorage); //TODO: USe storage type for calling convention!!
    if Result <> qeNone then
      EXIT;
    NextKeyword := keyUnknown;
  end;

{  if (ffForward in Func.Flags) or (Func.CallingConvention = ccExtern) then
  else
    Result := ParseFunctionCode(Func, NextKeyword);
}end;


end.

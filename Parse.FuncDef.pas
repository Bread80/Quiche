unit Parse.FuncDef;
{Calling Conventions
===

StackLocal
---
Arguments passed on the stack. Local variables alloocated on the stack.

Stack:
  <Start of previous stack frame>   <- Original IX
  <End of local vars, pushes, etc>  <- Original SP
  <Arguments pushed onto stack>
  Return-addr         <- From call to function
  Prev-IX             <- From previous stack frame
  <Local variables>   <- New IX points here
  <End of local vars> <- New SP
}

interface
uses Parse.Base, Parse.Errors,
  Z80.CPU;

// <function-def> := FUNCTION [ <param-def-list> ] <type-def> ; [<directive>] [,<directive>]
//                |  PROCEDURE [ <param-def-list> ] ; [ <directive> ] [, <directive> ]
// <directive> := <calling-convention> | <extern-def> | FORWARD
//Parse a function delaration and add it to the list of functions.
//If the function has previously been 'forward' defined validates the definition
//correctly matches it.
//Assumes that the 'function' or 'procedure' keyword has already been consumed
//Inputs:
//  Proc is True if we're parsing a Procedure definition, false for a Function
function ParseFunctionDef(Proc: Boolean): TQuicheError;

//Verifies all forward decalarations are satisfied (ie. which have no implementation)
function AreAnyForwardsUnsatisfied: TQuicheError;

implementation
uses SysUtils,
  Def.Functions, Def.Globals, Def.IL, Def.Operators, Def.QTypes, Def.Scopes, Def.Variables,
  Parse, Parse.Expr;

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
  end;

  Keyword := IdentToKeyword(Ident);
  if Keyword <> keyUnknown then
    EXIT(ErrSub(qeReservedWord, Ident));

  //Have we already read a parameter with that name?
  if ParamIndex > 0 then
    if Func.FindParam(Ident, ParamIndex-1) <> nil then
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
function ValidateRegParam(Func: PFunction;ParamIndex: Integer;Reg: TCPUReg): Boolean;
var Access: TVarAccess;
  IsOutput: Boolean;
  P: Integer;
begin
  Access := Func.Params[ParamIndex].Access;
  if not (Access in [vaVal, vaOut, vaResult]) then
    EXIT(False);
  if ParamIndex = 0 then
    EXIT(True);
  IsOutput := Access in [vaOut, vaResult];
  for P := 0 to ParamIndex-1 do
    if (Func.Params[P].Reg = Reg) and (IsOutput = (Func.Params[P].Access in [vaOut, vaResult])) then
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
  Reg: TCPUReg;
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
  Reg := IdentToCPUReg(Ident);
  if Reg <> rNone then
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
      VarType := CPURegToVarType[Reg]
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
  Access: TVarAccess;
  Ch: Char;
begin
  ParamIndex := 0;

  //Loop until we hit the trailing brace or an error
  repeat
    if ParamIndex > MaxFunctionParams then
      EXIT(Err(qeDecTooManyParams));

    //Parameter access specifier or parameter name
    Parser.SkipWhiteSpaceAll;
    Result := ParseIdentifier(#0, Ident);
    if Result <> qeNone then
      EXIT;

    //Is this an access specifier
    if IdentToAccessSpecifier(Ident, Access) then
    //If we had a specifier then we haven't read the name yet
      Ident := '';

    //Loop for all the names in a comma separated list
    ListStart := ParamIndex;
    repeat
      //Hard coded maximum parameter count (eek!)
      if ParamIndex > MaxFunctionParams then
        EXIT(Err(qeDecTooManyParams));

      //Validate or set the specifier (applies to all params in the list)
      if ffForward in Func.Flags then
      begin
        if Access <> Func.Params[ParamIndex].Access then
          EXIT(Err(qeFuncDecDoesntMatch));
      end
      else
        Func.Params[ParamIndex].Access := Access;

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
    if not CharInSet(Ch, [';',')']) then
      EXIT(ErrSyntaxMsg(synFunctionParameterDeclaration, ''';'' or '')'' expected'));
    inc (ParamIndex);
  until Ch = ')';

  dec(ParamIndex);
  //Store the parameter count in the function definition
  Func.ParamCount := ParamIndex+1;
  Result := qeNone;
end;

//Parse an extern directive and add it to the function definition
// <extern-def> :- CALL <integer-constant-expression>
// <extern-def> :- RST <integer-constant-expression>
//Assumes the CALL or RST keyword has already been consumed
//Parses and validates the constant expression.
//Note: The function's calling convention value is set (and validated) by the caller
//We only parse, validate, and set the address and other data
function ParseExternDef(Func: PFunction): TQuicheError;
var IsReg: Boolean;
  P: Integer;
  Slug: TExprSlug;
  ExprType: TVarType;
begin
  Slug.Initialise;

  //Validate an extern definition: if one param is register then they all must be
  if Func.ParamCount + Func.ResultCount > 1 then
  begin
    IsReg := Func.Params[0].Reg <> rNone;
    for P := 1 to Func.ParamCount + Func.ResultCount-1 do
      if IsReg <> (Func.Params[P].Reg <> rNone) then
        EXIT(ErrMsg(qeFunctionDeclaration, 'Either all parameters must be register parameters, or none'));
  end;

  ExprType := vtPointer;
  Result := ParseExpressionToSlug(Slug, ExprType);
  if Result <> qeNone then
    EXIT;
  if (Slug.ILItem <> nil) or (Slug.Operand.Kind <> pkImmediate) then
    EXIT(Err(qeConstantExpressionExpected));
  if not IsIntegerType(Slug.Operand.Imm.VarType) then
    EXIT(errSyntaxMsg(synFunctionDeclaration, 'Value for Code and RST directives must be an integer type'));
  case Func.CallingConvention of
    ccCall:
      if (Slug.Operand.Imm.IntValue < 0) or (Slug.Operand.Imm.IntValue > $ffff) then
        EXIT(errSyntaxMsg(synFunctionDeclaration, 'Invalid value for Code directive'));
    ccRST:
      if not Slug.Operand.Imm.IntValue in [0..8,$10,$18,$20,$28,$30,$38] then
        EXIT(errSyntaxMsg(synFunctionDeclaration, 'Invalid value for RST directive'));
  else
    Assert(False);
  end;

  Func.CodeAddress := Slug.Operand.Imm.IntValue;
  Result := qeNone;
end;

// <directive-list> = <directive> [; <directive-list>]
// <directive>      = | FORWARD
//                    | CALL <code-address>
//                    | RST <restart-index>
//Parses the directives section of a function declaration.
//This function loops through any valid directives, processing them as needed until
//the first keyword which is not a directive.
//Returns NoCode True for directives such as CALL, RST and FORWARD where the body is
//declared elsewhere
function ParseDirectives(Func: PFunction;out NoCode: Boolean): TQuicheError;
var IsForward: Boolean; //This declaration is a Forward
  NextDirective: TFuncDirective;
  Ident: String;
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
    NextDirective := dirUNKNOWN;
    if TestIdentFirst then
    begin
      Parser.Mark;
      Result := ParseIdentifier(#0, Ident);
      if Result <> qeNone then
        EXIT;

      if Ident <> '' then
        NextDirective := IdentToFuncDirective(Ident);
      if NextDirective = dirUNKNOWN then
        Parser.Undo;
    end;

    case NextDirective of
      dirFORWARD:
      begin
        if IsForward or Func.IsExtern then
          //Extern and forward are incompatible
          EXIT(ErrMsg(qeFunctionDeclaration, 'Call and RST functions can''t be forward declared'));
        NoCode := True;
        IsForward := True;
        if ffForward in Func.Flags then
          EXIT(ErrMsg(qeFunctionDeclaration, 'Function has already been declared as FORWARD'));
      end;
      dirCALL, dirRST:
      begin
        if IsForward then //Extern and forward are incompatible
          EXIT(ErrMsg(qeFunctionDeclaration, 'Call and RST functions can''t be forward declared'));
        if Func.IsExtern then //Extern and forward are incompatible
          EXIT(ErrMsg(qeFunctionDeclaration, 'Multiple Call or RST directives not allowed'));
        NoCode := True;
        Func.IsExtern := True;
        case NextDirective of
          dirCALL: Func.CallingConvention := ccCall;
          dirRST: Func.CallingConvention := ccRST;
        else
          Assert(False);
        end;
        Result := ParseExternDef(Func);
        if Result <> qeNone then
          EXIT;
      end;
      dirSTACK:
        if Func.CallingConvention <> ccUnknown then
          EXIT(ErrMsg(qeFunctionDeclaration, 'Multiple calling convention directives given'))
        else
          Convention := ccStack;
      dirREGISTER:
        if Func.CallingConvention <> ccUnknown then
          EXIT(ErrMsg(qeFunctionDeclaration, 'Multiple calling convention directives given'))
        else
          Convention := ccRegister;
      //To add extra calling conventions:
      //* Set the Convention variable
      //* Perform additional parsing and validation as required
      //The code after this case statement validates that we don't already have a
      //calling convention, that it matches a forward etc.
      dirUNKNOWN:
      begin
        //Update forward status of the definition as needed. If this is forward add it,
        //if previous was forward remove it (which shows we don't have any 'hanging' forwards
        //and avoid multiple definitions)
        if IsForward then
          Func.Flags := Func.Flags + [ffForward]
        else
          Func.Flags := Func.Flags - [ffForward];

        if Func.CallingConvention = ccUnknown then
          Func.CallingConvention := optDefaultCallingConvention;

        EXIT(qeNone);
      end;
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


//<function-body> := [ <var-declaration> ]
//                |  [ <type-declaration> ]
//                |  [ <const-declaration> ]
//                <block>
//Parse the declarations and body of a function declaration
//Inputs: Func is the function being declared
function ParseFunctionBody(Func: PFunction): TQuicheError;
var I: Integer;
  ParamStorage: TVarStorage;
  LocalStorage: TVarStorage;
  ParamName: String;
  ILItem: PILItem;
  Param: PILParam;
  Variable: PVariable;
begin
  //Setup scope for function
  CreateCurrentScope(Func, Func.Name);

  ParamStorage := Func.GetParamStorage;
  LocalStorage := Func.GetLocalStorage;

  ILItem := nil;
  //Add Parameters to variables list for the function
  for I := Func.ParamCount + Func.ResultCount - 1 downto 0 do
  begin
    //'Result' is accessed via the function name. (Result is syntactic sugar handled
    //elsewhere
    if Func.Params[I].Access = vaResult then
      ParamName := Func.Name
    else
      ParamName := Func.Params[I].Name;
    Variable := VarCreateParameter(ParamName, Func.Params[I].VarType, ParamStorage,
      Func.Params[I].Access);
    Variable.FuncParamIndex := I;

(*
  ***Removed - this needs to be done in the code generator, not in the IL Code generator (us).
    //For Register calling convention,
    //generate ILCode to store each register into a temporary variable in memory.
    //This will (hopefully) be eliminated if the variable can be held in a register.
    if Func.CallingConvention = ccRegister then
      if Func.Params[I].Access in [vaVar, vaVal, vaConst] then
      begin
        Assert(Func.Params[I].Reg <> rNone, 'No register assigned (for Register calling convention)');

        Param := ILAppendRegStore(ILItem);
        Param.Kind := pkVarDest;
        Param.Variable := Variable;
        Param.VarVersion := Variable.WriteCount;
        Param.Reg := Func.Params[I].Reg;
      end;
*)
  end;

  //The function code
  Result := ParseDeclarations(False, False, LocalStorage);
  if Result <> qeNone then
    EXIT;

  //TODO: Move this block to the code generator. Loading of the Result
  //value becomes an implicit part of the function postamble
  //Process Result/values to be returned
  //If we're a function, load Result into A if 8-bit or DE if 16-bit
  case Func.CallingConvention of
    ccStack:
      if Func.ResultCount > 0 then
      begin
        ILItem := nil;
        Param := ILAppendRegLoad(ILItem);
        Param.Kind := pkVarSource;
        Variable := VarFindResult;
        Assert(Variable <> nil);
        Param.Variable := Variable;
        Param.VarVersion := Variable.WriteCount;
        case GetTypeSize(Variable.VarType) of
          1: Param.Reg := rA;
          2: Param.Reg := rDE;
        else
          Assert(False, 'Unable to process result type');
        end;
      end;
    ccRegister: ; //End of function loads are handled by the code generator
  else
    Assert(False);
  end;

  //This call generates the output code for the function
  if Assigned(OnScopeDone) then
    if not OnScopeDone then
      EXIT(ErrSub(qeAssemblyError, Func.Name));

  Assert(ScopeGetDepth = 0);
  //Return to previous scope
  EndCurrentScope;
  Result := qeNone;
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
function ParseFunctionDef(Proc: Boolean): TQuicheError;
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
        if Func.Params[Func.ParamCount].Access <> vaResult then
          EXIT(Err(qeFuncDecDoesntMatch))
      end
      else
        Func.Params[Func.ParamCount].Access := vaResult;
      Result := ParseParamType(Func, Func.ParamCount, Func.ParamCount);
      if Result <> qeNone then
        EXIT;
      Func.ResultCount := 1;
    end
    else
      EXIT(ErrMsg(qeFunctionDeclaration, 'Function must have a result'));

  Result := ParseDirectives(Func, NoCode);
  if Result <> qeNone then
    EXIT;

  if not NoCode then
  begin
    //If it's a forward declaraion then anything else will be more global stuff.
    //If not forward then it's the start of the routine's code.
    Result := ParseFunctionBody(Func); //TODO: Use storage type for calling convention!!
    if Result <> qeNone then
      EXIT;
  end;
end;


function AreAnyForwardsUnsatisfied: TQuicheError;
var Scope: PScope;
  I: Integer;
begin
  Scope := GetCurrentScope;
  for I := 0 to Scope.FuncList.Count-1 do
    if ffForward in Scope.FuncList[I].Flags then
      EXIT(ErrSub(qeUnsatisfiedForward, Scope.FuncList[I].Name));

  Result := qeNone;
end;

end.

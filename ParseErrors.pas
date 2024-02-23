unit ParseErrors;

interface
uses Functions, Operators;

//This enum list general error classes. This is intended to be used either for
//basic, generic, errors or for the bootstrap compiler.
type TQuicheError = (
  qeNone,               //No error!

  //General syntax
  qeSyntax,             //Generic syntax error
  qeFunctionCall,       //Syntax error in function call
  qeInvalidKeyword,
  qeENDExpected,
  qeENDdotExpected,
  qeCodeAfterENDdot,
  qeInvalidTopLevel,

  //Function declarations
  qeFunctionRedeclared,
  qeFunctionBodyExpected,
  qeFunctionDeclaration,  //General error in function declaration
  qeFuncDecDoesntMatch,   //Forward declaraed functions
  qeDecTooManyParams,     //Beyond system maximum :(

  //Identifiers
  qeIdentifierExpected,
  qeUndefinedIdentifier,
  qeReservedWord,
  qeVariableNotFound,
  qeVariableRedeclared,
  qeUnknownType,
  qeConstNameNotValidHere,

  qeTypeNameNotValidHere,
  qeTypeMismatch,

  //Expressions
  qeExpression,         //Generic expression error
  qeOperandExpected,
  qeOperatorExpected,
  qeUnknownOperator,
  qeConstantExpressionExpected,
  qeConstantExpressionOverflow,
  qeConstantOutOfRange,
  qeDivByZero,
  qeInvalidDecimalNumber,
  qeInvalidHexNumber,
  qeInvalidBinaryNumber,
  qeUnterminatedString,
  qeInvalidCharLiteral,
  qeUnmatchedBrackets,

  //Techie stuff
  qeAttributeError,     //Error in an attribute
  qeAssemblyError,      //Error in code generation

  qeBUG,                //Should never happen. Oops!
  qeTODO);              //A compiler feature we have yet to implement

const Errors : array[TQuicheError] of String = (
  'No error',

  'Syntax error',
  'Error in function call',
  'Invalid keyword',
  'END expected',
  'END. expected',
  'Code after END.',
  'Incorrect code at top-level. Expecting FUNCTION, PROCEDURE, TYPE, CONST, VAR or BEGIN',

  'Function redeclared',
  'Function body expected',
  'Error in function declaration',
  'Function declaration doesn''t match forward declaration',
  'Too many parameters - there is a hard coded maximum. Sorry',

  'Identifier expected',
  'Undefined identifier',
  'Reserved word',
  'Variable not found',
  'Variable already declared',
  'Invalid or undeclared type identifier',
  'Constant name not valid here',

  'Type name not valid here',
  'Incompatible types',

  'Expression error',
  'Operand or unary operator expected',
  'Operator expected',
  'Unknown operator',
  'Constant expression expected',
  'Constant expression overflow',
  'Constant expression out of range',
  'Division by zero',
  'Incorrect decimal number',
  'Incorrect hex number',
  'Incorrect binary number',
  'Unterminated string',
  'Incorrect character literal',
  'Unmatched brackets',

  'Invalid attribute',
  'Error in code generation',

  'Bug. Oops',
  'Feature not yet implemented');

const SubErrors : array[TQuicheError] of String = (
  '',

  '',
  '',
  '',
  '',
  '',
  '',
  '',

  'Function redeclared: ''%s''',
  '',
  '',
  '',
  '',
  'Invalid keyword in this context: ''%s''',
  'Undeclared identifier: ''%s''',
  'Reserved word: ''%s''',
  'Variable not found: ''%s''',
  'Variable already declared: ''%s''',
  'Invalid or undeclared type identifier: ''%s''',
  'Constant name not valid here: ''%s''',

  'Type name not valid here: ''%s''',
  '',
  '',

  '',
  '',
  '',

  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  'Code generation error in: ''%s''',

  '',
  '');

//Raise a generic error
function Err(ErrClass: TQuicheError): TQuicheError;

//Raise a generic error with some meta data to substitute into the error message.
function ErrSub(ErrClass: TQuicheError;Sub: String): TQuicheError;

//Raise an error where the entire message is submitted in the Msg parameter
function ErrMsg(ErrClass: TQuicheError;Msg: String): TQuicheError;


type TSyntaxError = (
  synFOR,
  synIF,

  synStatementExpected,
  synParameterList,
  synVariableDeclaration,
  synAssignmentExpected,

  synFunctionDeclaration,
  synFunctionParameterDeclaration,

  synLAST);


const SyntaxErrors: array[TSyntaxError] of String = (
  'FOR loop error',
  'IF error',

  'Statement expected',
  'Error in parameter list',
  'Invalid variable declaration',
  'Assignment expected',

  'Error in function declaration',
  'Error in function parameter',

  'EndOfList');

const SyntaxHelp: array[TSyntaxError] of String = (
  'FOR <var-name> := <from-expr> TO|DOWNTO <to-expr> DO'#13#10 +  //FOR loops
    'FOR VAR <var-name>: <type-name> := <from-expr> TO|DOWNTO <to-expr> DO'#13#10 +
    'FOR VAR <var-name><type-symbol> := <from-expr> TO|DOWNTO <to-expr> DO'#13#10,

  '',
  'IF <boolean-expr> THEN <block> [ELSE <block]', //If statements

  '(<item>[,<item>])',    //Parameter list

  'VAR <var-name>: <type-name>'#13#10 +     //Variable declaration
    'VAR <var-name><type-symbol>'#13#10 +
    'VAR <var-name>: <type-name> = <expr>'#13#10 +
    'VAR <var-name><type-symbol> = <expr>'#13#10 +
    'VAR <var-name> := <expr>',

  '<var-name> := <expr>',

  'FUNCTION <func-name> param-def-list : <result-type> ; [<directives> ;] <function-body>'#13#10 +
  'PROCEDURE <func-name> param-def-list ; [<directives> ;] <functin-body>',

  '<param-def-list> := ( <Param-def> [ ; <Param-def> ] )'#13#10 +
    '<param-def> := <param-name> [ , <param-name>]: <Param-type>'#13#10,

  'EndOfList');

//Raises a syntax error
function ErrSyntax(SyntaxError: TSyntaxError): TQuicheError;

function ErrSyntaxMsg(SyntaxError: TSyntaxError;Msg: String): TQuicheError;

function ErrOpUsage(Msg: String;Op: TOperator): TQuicheError;

function ErrFuncCall(Msg: String;Func: PFunction): TQuicheError;

const
  ermCommaOrCloseParensExpected = ''','' or '')'' expected in parameter list';
  ermParameterListExpected = 'Parameter list expected';
  ermIncorrectParameterCount = 'Incorrect parameter count';
  ermCantAssignProcedure = 'Procedure has no return value. Invalid in assignments';


var
  LastError: TQuicheError;
  LastErrorMessage: String;
  LastErrorHelp: String;

implementation
uses SysUtils;

function Err(ErrClass: TQuicheError): TQuicheError;
begin
  LastErrorMessage := Errors[ErrClass];
  LastErrorHelp := '';
  Result := ErrClass;
end;


function ErrSub(ErrClass: TQuicheError;Sub: String): TQuicheError;
begin
  LastErrorMessage := Format(SubErrors[ErrClass], [Sub]);
  if LastErrorMessage = '' then
    raise Exception.Create('Nothing to sub! (in error message)');
  LastErrorHelp := '';
  Result := ErrClass;
end;

function ErrMsg(ErrClass: TQuicheError;Msg: String): TQuicheError;
begin
  LastErrorMessage := Msg;
  LastErrorHelp := '';
  Result := ErrClass;
end;

function ErrSyntax(SyntaxError: TSyntaxError): TQuicheError;
begin
  LastErrorMessage := SyntaxErrors[SyntaxError];
  LastErrorHelp := SyntaxHelp[SyntaxError];
  Result := qeSyntax;
end;

function ErrSyntaxMsg(SyntaxError: TSyntaxError;Msg: String): TQuicheError;
begin
  LastErrorMessage := Msg;
  LastErrorHelp := SyntaxHelp[SyntaxError];
  Result := qeSyntax;
end;

function ErrOpUsage(Msg: String;Op: TOperator): TQuicheError;
begin
  LastErrorMessage := Msg;
  LastErrorHelp := OpToUsage(Op);
  Result := qeSyntax;
end;

function ErrFuncCall(Msg: String;Func: PFunction): TQuicheError;
begin
  LastErrorMessage := Msg;
  LastErrorHelp := Func.ToString;
  if Func.Comments <> '' then
    LastErrorHelp := LastErrorHelp + #13 + Func.Comments;
  Result := qeFunctionCall;
end;

end.

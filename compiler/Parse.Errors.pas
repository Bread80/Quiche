unit Parse.Errors;

interface
uses Def.Functions, Def.Operators;

function ErrorLineNo: Integer;
function ErrorPos: Integer;
function ErrorLine: String;

//This enum list general error classes. This is intended to be used either for
//basic, generic, errors or for the bootstrap compiler.
type TQuicheError = (
  qeNone,               //No error!

  //General syntax
  qeSyntax,             //Generic syntax error
  qeFunctionCall,       //Syntax error in function call
  qeInvalidKeyword,
  qeInvalidPROGRAM,     //PROGRAM statement not allowed here
  qeENDExpected,
  qeENDdotExpected,
  qeCodeAfterENDdot,
  qeInvalidTopLevel,
  qeUnexpectedEndOfFile,
  qeTextAfterContinuationChar,  //After \ line ending
  qeUnterminatedComment,
  qeInvalidEndOfLine,     //End of line not allowed at this point
  qeEndOfStatementExpected,
  qeNewlineExpected,      //This should probably be overridden by the caller
                          //to give a more specific error
  qeCommaOrCloseParensExpected,
  qeCloseSquareBraceExpected,

  //FOR loops
  qeTOorDOWNTOExpected,
  qeDOExpected,
  //REPEAT..UNTIL
  qeUNTILExpected,
  //IF conditionals
  qeTHENExpected,

  //VAR declarations
  qeColonExpectedInVAR,
  //CONST declarations
  qeEqualExpectedInCONST,

  //TYPE declarations
  qeEqualExpectedInTYPE,
//  qeTypeNameOrRangeExpected,
  qeInvalidTypeDefinition,
  qeUndeclaredTypeOrInvalidTypeDef,
  qePointedTypeNameExpected,  //Type definition ^SomeType but identifier is invalid

  qeRangeOperatorExpected,    //'..' operator
  qeRangeBoundsTypeMismatch,  //x..y

  qeOrdinalTypeExpected,
  qeOrdinalConstExprExpected,  //Expression must evaluate to a value which is a
                              //member of an ordinal type
  qeRangeExprValuesTooWide,   //Numeric range bounds values won't fit into any available type
  qeRangeValuesMisordered,    //The left value >= the right value
  qeOFExpected,
  qeArrayBoundsDefinition,
  qeListCapacityError,
  qeConcreteOrPointerTypeRequired,  //(In RECORD definition. Possibly elsewhere too)

  //Assignment
  qeAssignmentExpected, // := operator
  qeEqualExpectedInAssignment,
  qeAssignmentNotAllowed,
  qeAssignToCONSTVar,     //Attempting to assign a value to a constant variable (argument)

  //Function declarations
  qeFunctionRedeclared,
  qeFunctionBodyExpected,
  qeFunctionDeclaration,  //General error in function declaration
  qeFuncDecDoesntMatch,   //Forward declaraed functions
  qeDecTooManyParams,     //Beyond system maximum :(
  qeParamNameRedeclared,
  qeRegisterParamRedeclared,
  qeInvalidRegisterName,
  qeRegisterParamMismatch,  //Either all parameters must be register parameters, or none
  qeRegisterParamInvalidAccessType,
  qeDefaultValueMulti,    //Can't assign a default value to multiple parameters (param list)
  qeDefaultValueNotLast,  //A parameter without a default value follows one with
  qeFunctionResultExpected,

  qeNestedFuncsNotAllowed,
  qeUnsatisfiedForward, //Forward declared function not defined
  qeIntegerExpectedForCALLOrRST,  //Integer value expected for code or RST directives
  qeCallDirectiveOutOfRange,  //Parameter for Call directive is out of range
  qeRSTDirectiveOutOfRange, //Parameter for RST directive
  qeCALLOrRSTForwardDeclared,
  qeMultipleCALLOrRST,
  qeMultipleCallingConventions,
  qeFORWARDRedeclared,
  qeFORWARDInTypeOrRecord,
  qeCALLOrRSTInTypeOrRecord,
  qeFuncTypeDefUnspecifiedReg,  //No register specified for a parameter in a function
                                //type definition (when the calling convention requires it)
  qeColonExpectedFuncDecl,  //Colon expected in function parameter
  qeSemicolonOrCloseBraceExpectedFuncDecl,  //After parameter

  //Function calls
  qeArgMustBeVariable,        //For VAR or OUT parameters
  qeReturnedArgTypeMismatch,  //For VAR and OUT parameters
  qeCantPassCONSTasVARorOUT,  //For VAR and OUT parameters
  qeTooManyArgs,              //Number of arguments being passed his above hard coded limit
                              //No function can have this many arguments anyway, so you have a problem :)
  qeIntegerConstantArgExpected, //For some intrinsics such as INC and DEC
  qeBytePassedToHiLoSwap,
  qeNotEnoughParameters,
  qeFuncPrimitiveNotFound,    //Not found with matching arg types
  qeCantAssignProcedure,      //Try to assign procedure result to a variable or pass as argument

  //Identifiers
  qeIdentifierExpected,
  qeUndefinedIdentifier,
  qeReservedWord,
  qeVariableNotFound,
  qeIdentifierRedeclared,
  qeUnknownType,
  qeConstNameNotValidHere,

  //Types
  qeTypeNameNotValidHere,
  qeTypeMismatch,
  qeTypeMismatchImplicitReal,
  qeTypeMismatchNoOverlap,
  qeEnumItemNotValidHere,
  qeInstantiateUnboundedArray,  //Attempting to instantiate an unbounded array variable

  //Expressions
  qeExpression,         //Generic expression error
  qeOperandExpected,
  qeOperatorExpected,
  qeUnknownOperator,
  qeConstantExpressionExpected,
  qeConstantExpressionOverflow,
  qeConstantOutOfRange,
  qeConstantAssignmentOutOfRange,
  qeDivByZero,
  qeInvalidDecimalNumber,
  qeInvalidHexNumber,
  qeInvalidBinaryNumber,
  qeUnterminatedString,
  qeInvalidCharLiteral,
  qeUnmatchedBrackets,
  qeAt,                 //@ operator syntax
  qeBooleanExpressionExpected,  //For conditionals etc.

  //Pointers
  qePointerDerefError,  //Data is not a pointer so can't deref

  //Ops
  qeOpIncompatibleTypes,  //Binary operator
  qeOpIncompatibleType,   //Unary operators

  //Directives
  qeUnknownDirective,
  qeDirectiveSyntax,    //General error in directive syntax
  qeInvalidDirectiveValue,  //The value given is not applicable to this directive

  //Attributes
  qeInvalidAttrName,
  qeCloseSquareExpectedAttr,
  qeXOrYExpected,       //In Corrupts/Preserves list
  qeInvalidCorruptsAttr,

  //Techie stuff
  qeAttributeError,     //Error in an attribute
  qeAssemblyError,      //Error in code generation

  qeBUG,                //Should never happen. Oops!
  qeTODO);              //A compiler feature we have yet to implement

//Raise a generic error
function Err(ErrClass: TQuicheError): TQuicheError;

//Raise a generic error with some meta data to substitute into the error message.
function ErrSub(ErrClass: TQuicheError;const Sub: String): TQuicheError;
function ErrSub2(ErrClass: TQuicheError;const Sub1, Sub2: String): TQuicheError;

//Operator errors. Help text will be autogenerated from available Ops
function ErrOpUsage(ErrClass: TQuicheError;Op: TOperator): TQuicheError;
function ErrOpUsageSub(ErrClass: TQuicheError;const Sub: String;Op: TOperator): TQuicheError;
function ErrOpUsageSub2(ErrClass: TQuicheError;const Sub1, Sub2: String;Op: TOperator): TQuicheError;

function ErrFuncCall(ErrClass: TQuicheError;Func: PFunction): TQuicheError;
function ErrFuncCallSub(ErrClass: TQuicheError;const Sub: String; Func: PFunction): TQuicheError;
function ErrFuncCallSub2(ErrClass: TQuicheError;const Sub1, Sub2: String;
  Func: PFunction): TQuicheError;
function ErrFuncCallSub3(ErrClass: TQuicheError;const Sub1, Sub2, Sub3: String;
  Func: PFunction): TQuicheError;

//Raise a qeTODO or qeBUG error.
//qeTODO is for stubs waiting to be built out
//qeBUG is, basically, for assertion failures
function ErrTODO(Msg: String): TQuicheError;
function ErrBUG(Msg: String): TQuicheError;

function IsValidCompileError(const AName: String): Boolean;
function ErrorToName(Err: TQuicheError): String;

var
  LastError: TQuicheError;
  LastErrorMessage: String;
  LastErrorHelp: String;

//Init the error messages/error data
procedure LoadErrorData(const Filename: String);

implementation
uses SysUtils, Classes,
  Parse.Base;

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

type TErrorData = record
    Name: String;
    Text: String;
    Help: String;
  end;

var ErrorData: array[TQuicheError] of TErrorData;

function IsValidCompileError(const AName: String): Boolean;
var Data: TErrorData;
begin
  for Data in ErrorData do
    if CompareText(AName, Data.Name) = 0 then
      EXIT(True);
  Result := False;
end;

function ErrorToName(Err: TQuicheError): String;
begin
  Result := ErrorData[Err].Name;
end;

//HelpEx (if any) will be appended to the Help text in the error data
function DoErr(ErrClass: TQuicheError;const HelpEx: String): TQuicheError;
begin
  LastErrorMessage := ErrorData[ErrClass].Text;
  LastErrorHelp := ErrorData[ErrClass].Help;
  if HelpEx <> '' then
    if LastErrorHelp <> '' then
      LastErrorHelp := LastErrorHelp + #13 + HelpEx
    else
      LastErrorHelp := HelpEx;
  Result := ErrClass;
end;

function DoErrSub(ErrClass: TQuicheError;const HelpEx, Sub: String): TQuicheError;
begin
  Result := DoErr(ErrClass, HelpEx);
  LastErrorMessage := Format(LastErrorMessage, [Sub]);
  LastErrorHelp := Format(LastErrorHelp, [Sub]);
  if LastErrorMessage = '' then
    raise Exception.Create('ERROR WHILE REPORTING AN ERROR: Nothing to substitute! (in error message)');
end;

function DoErrSub2(ErrClass: TQuicheError;const HelpEx, Sub1, Sub2: String): TQuicheError;
begin
  Result := DoErr(ErrClass, HelpEx);
  LastErrorMessage := Format(LastErrorMessage, [Sub1, Sub2]);
  LastErrorHelp := Format(LastErrorHelp, [Sub1, Sub2]);
  if LastErrorMessage = '' then
    raise Exception.Create('ERROR WHILE REPORTING AN ERROR: Nothing to substitute! (in error message)');
end;

function DoErrSub3(ErrClass: TQuicheError;const HelpEx, Sub1, Sub2, Sub3: String): TQuicheError;
begin
  Result := DoErr(ErrClass, HelpEx);
  LastErrorMessage := Format(LastErrorMessage, [Sub1, Sub2, Sub3]);
  LastErrorHelp := Format(LastErrorHelp, [Sub1, Sub2, Sub3]);
  if LastErrorMessage = '' then
    raise Exception.Create('ERROR WHILE REPORTING AN ERROR: Nothing to substitute! (in error message)');
end;

function Err(ErrClass: TQuicheError): TQuicheError;
begin
  Result := DoErr(ErrClass, '');
end;

function ErrSub(ErrClass: TQuicheError;const Sub: String): TQuicheError;
begin
  Result := DoErrSub(ErrClass, '', Sub);
end;

function ErrSub2(ErrClass: TQuicheError;const Sub1, Sub2: String): TQuicheError;
begin
  Result := DoErrSub2(ErrClass, '', Sub1, Sub2);
end;

function ErrOpUsage(ErrClass: TQuicheError;Op: TOperator): TQuicheError;
begin
  Result := DoErr(ErrClass, OpToUsage(Op));
end;

function ErrOpUsageSub(ErrClass: TQuicheError;const Sub: String;Op: TOperator): TQuicheError;
begin
  Result := DoErrSub(ErrClass, OpToUsage(Op), Sub);
end;

function ErrOpUsageSub2(ErrClass: TQuicheError;const Sub1, Sub2: String;Op: TOperator): TQuicheError;
begin
  Result := DoErrSub2(ErrClass, OpToUsage(Op), Sub1, Sub2);
end;

function FuncToHelpEx(Func: PFunction): String;
begin
  Result := Func.ToString;
  if Func.Comments <> '' then
    Result := Result + #13 + Func.Comments;
end;

function ErrFuncCall(ErrClass: TQuicheError;Func: PFunction): TQuicheError;
begin
  Result := DoErr(ErrClass, FuncToHelpEx(Func));
end;

function ErrFuncCallSub(ErrClass: TQuicheError;const Sub: String; Func: PFunction): TQuicheError;
begin
  Result := DoErrSub(ErrClass, FuncToHelpEx(Func), Sub);
end;

function ErrFuncCallSub2(ErrClass: TQuicheError;const Sub1, Sub2: String;
  Func: PFunction): TQuicheError;
begin
  Result := DoErrSub2(ErrClass, FuncToHelpEx(Func), Sub1, Sub2);
end;

function ErrFuncCallSub3(ErrClass: TQuicheError;const Sub1, Sub2, Sub3: String;
  Func: PFunction): TQuicheError;
begin
  Result := DoErrSub3(ErrClass, FuncToHelpEx(Func), Sub1, Sub2, Sub3);
end;

function ErrTODO(Msg: String): TQuicheError;
begin
  LastErrorMessage := ErrorData[qeTODO].Text;
  LastErrorHelp := Msg;
  Result := qeTODO;
end;

function ErrBUG(Msg: String): TQuicheError;
begin
  LastErrorMessage := ErrorData[qeBUG].Text;
  LastErrorHelp := Msg;
  Result := qeBUG;
end;

procedure LoadErrorData(const Filename: String);
type TLoadState = (lsInit, lsName, lsText, lsItemDone);
var SL: TStringList;
  Err: TQuicheError;
  Line: String;
  State: TLoadState;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(Filename);
    Err := low(TQuicheError);
    State := lsInit;
    for Line in SL do
      if (Line <> '') and (Line.Chars[0] <> ';') then
      begin
        if Line.Chars[0] = '=' then
        begin
          if State = lsInit then
            Err := low(TQuicheError)
          else if Err = high(TQuicheError) then
            raise Exception.Create('Too many items in Errors listing file (' + Filename + ')')
          else
            Err := succ(Err);
          State := lsName;
          ErrorData[Err].Name := Line.Substring(1);
          ErrorData[Err].Text := '';
          ErrorData[Err].Help := '';
        end
        else
          case State of
            lsName:
            begin
              ErrorData[Err].Text := Line.Trim;
              State := lsText;
            end;
            lsText:
            begin
              if ErrorData[Err].Help <> '' then
                ErrorData[Err].Help := ErrorData[Err].Help + #13;
              ErrorData[Err].Help := ErrorData[Err].Help + Line.Trim;
            end;
          else
            raise Exception.Create('Error in message file at line:'#13 + Line);
          end;
      end;

    if Err <> high(TQuicheError) then
      raise Exception.Create('Not enough items in Errors listing file (' + Filename + ')');
  finally
    SL.Free;
  end;
end;

end.

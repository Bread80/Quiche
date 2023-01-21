unit MErrors;

interface

const
  csWhiteSpace = [#0..' '];
  csIdentFirst = ['A'..'Z','a'..'z','.','_'];
  csIdentOther = ['0'..'9','A'..'Z','a'..'z','.','_'];
  csDecimalFirst = ['0'..'9'];
  csHexChars = ['0'..'9','a'..'f','A'..'F'];

type TAssembleError = (
  errNone,      //No error

  //Reader errors
  errEOLN,    //End of line (but data expected) (may not be an error depending on context)
  errEOF,
  errIdentifierExpected,  //Found something which isn't an identifier (label, opcode, directive etc)
  errUnterminatedString,

  //Expression errors
  errUnexpectedChar,
  errParameterExpected,
  errInvalidExpression,
  errHexDigitsExpected, //$ or & or 0x not followed by a value hex digit
  errInvalidHexNumber, //Failed to convert a hex humber. Too large?
  errInvalidBinaryNumber,
  errNumberExpected,
  errInvalidDecimalNumber,
  errUndeclaredIdentifier,
  errOperatorExpected,
  errUnknownOperator,
  errUnmatchedBrackets,
  errUnterminatedCharString,  //Character without closing quote

  //Conditionals
  errUnterminatedConditional, //If etc without matching endif
  errEndIfWithoutIf,
  errElseWithoutIf,

  //Labels and Identifiers
  errLabelRedeclared,  //Label has already been used
  errTooManyParameters,
  errFilenameExpected,    //For include directive
  errUnknownIdentifier,

  //Opcode parsing errors
  errOpcodeNotFound,  //Internal error (data not in table!)
  errBug,   //bug.
  errNoParam,  //No parameters expected
  errTooManyParams,  //Expected 1 param, got 2
  errNotEnoughParams, //Expecting 2 params
  errInvalidParams, //
  errParameterOutOfRange, //
  errByteParameterOutOfRange, //Expecting a value -255 .. 255
  errIMParameterOutOfRange, //Expecting a value of 0..2 (for IM)
  errExpectingDirectAddress,  //Expecting (nn)
  errExpectingIOPortAddress,  //Expecting (n) - IO Port address
  errAddressOutOfRange, //Expecing value $0000 to $ffff
  errIOPortOutOfRange,  //Expecting a value 0 to 255
  errWordOutOfRange, //Expecting a value $0000 to $ffff
  errInvalidRSTParameter, //
  errRelativeJumpOutOfRange,  //Relative jump must be -128..127
  errIndexOffsetOutOfRange, //Offset for IX/IY+/-d
  errInvalidIXIY, //Instruction can't have IX, IY


  //Quiche errors
  errAssignmentExpected,
  errSyntaxError,
  errEndExpected,
  errInvalidKeyword,
  errTHENExpected,

  err1  //Debugging


  );

const Errors: array[TAssembleError] of String = (
  'No error',

  //Reader errors
  'Premature end of line',    //End of line (but data expected) (may not be an error depending on context)
  'Premature end of file',
  'Identifier expected (ie label, opcode, directive etc)',  //Found something which isn't an identifier (label, opcode, directive etc)
  'Untermonated string',

  //Expression errors
  'Unexpected character in expression',
  'Parameter expected',
  'Invalid expression',
  'Hex digits expected', //$ or & or 0x not followed by a value hex digit
  'Invalid hex number (possibly too large)', //Failed to convert a hex humber. Too large?
  'Invalid binay number (possibly too large)',
  'Number expected',
  'Invalid decimal number',
  'Undeclared identifier (label symbol)',
  'Operator expected',
  'Unknown operator',
  'Unmatched brackets',
  'Unterminated character or string',  //Character without closing quote

  'Unterminated conditional',
  'EndIf without corresponding Ifxx',
  'Else or ElsIfxxx without corresponding Ifxxx',

  'Label already declared',  //Label has already been used
  'Too many parameters',
  'Filename expected',    //For include directive
  'Unknown identifier',

  //Opcode parsing errors
  'Opcode not found (Internal error)',  //Internal error (data not in table!)
  'Bug in assembler :(',   //bug.
  'No parameters expected',  //No parameters expected
  'Too many parameters. Expected 1, found 2',  //Expected 1 param, got 2
  'Not enough parameters. Expected 2', //Expecting 2 params
  'Invalid paramaters (for this opcode)', //
  'Parameter out of range', //
  'Parameter out of range. Expecting a value between -255 and 255', //Expecting a value -255 .. 255
  'Parameter out of range. Expecting a value between 0 and 2, //Expecting a value of 0..2 (for IM)',
  'Expecting a direct address ''(nn)''',  //Expecting (nn)
  'Expecting I/O port address in range 0..255',  //Expecting (n) - IO Port address
  'Address out of range. Expecting $0000 to $ffff', //Expecing value $0000 to $ffff
  'I/O port address out of range. Expecting a value between 0 and 255',  //Expecting a value 0 to 255
  'Word value out of range. Expecting a value between $0000 and $ffff', //Expecting a value $0000 to $ffff
  'RST parameter out of range', //
  'Relative jump out of range. Distance must be in range -128 to +127',  //Relative jump must be -128..127
  'Index offset out of range. Expecting a value between -128 and +127', //Offset for IX/IY+/-d
  'Invalid IX or IY parameter. This instruction doesn''t have and IX/IY variant', //Instruction can't have IX, IY

  //Quiche errors
  'Assignment expected',
  'Syntax error',
  'End expected at end of block',
  'The keyword is invalid at this location',
  'THEN expected after IF expression',



  'Debugging error'  //Debugging
  );

implementation

end.

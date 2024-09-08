//Globals data and current settings for the compiler.
unit Globals;

interface
uses Generics.Collections, Classes, QTypes, Variables;

const
  //Lanuage constants
  valueFalse = 0;
  valueTrue = -1;
  BooleanToBinary: array[False..True] of Integer = (valueFalse, valueTrue);

  //Runtime errors
  rerrNone      = 0;
  rerrOverflow  = 1;  //Result of an operation doesn't fit the target
  rerrDivByZero = 2;  //Divide by zero
  rerrRange     = 3;  //Array index out of bounds, or assignment out of range

  //CompilerOptions
var
  //If true, variables can be auto-created by any assignment to an un-declared
  //variable. Explicit type declarations are allowed when this option is used.
  //If not type inference will be used. Note that type declarations are ONLY
  //allowed the first time the variable is assigned.
  optAllowAutoCreation: Boolean;

  //If enabled the compiler will generate extra code after certain maths operations
  //the verify the result is within the range for the result type.
  //Using this options makes the output code larger and slower and should only be
  //used during debugging
  //Use the {$Q} compiler directive
  optOverflowChecks: Boolean;

  //If enabled the compiler will generate extra code to validate string and array bounds.
  //Code will also be generated to validate values passed as parameters and operands
  //for maths etc operators, where the type of the value being passed differs from
  //that of the parameter or operand in a way which could cause data loss.
  //Use th {$R} compiler directive
  optRangeChecks: Boolean;

  optDefaultVarStorage: TVarStorage;


type TBlockType = (btDefault, btStatic, btStack);

var RootBlockType: TBlockType;

implementation

end.

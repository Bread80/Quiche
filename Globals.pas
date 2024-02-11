unit Globals;

interface
uses Generics.Collections, Classes, QTypes, Variables;

const
  //Lanuage constants
  valueFalse = 0;
  valueTrue = -1;
  BooleanToBinary: array[False..True] of Integer = (valueFalse, valueTrue);

  //Runtime errors
  rerrNone = 0;
  rerrOverflow = 1;
  rerrDivByZero = 2;

  //CompilerOptions
var
  //If true, variables can be auto-created by any assignment to an un-declared
  //variable. Explicit type declarations are allowed when this option is used.
  //If not type inference will be used. Note that type declarations are ONLY
  //allowed the first time the variable is assigned.
  optAllowAutoCreation: Boolean;

  //If enable certain maths operations will be checked for overflow
  //Use the {$Q} compiler directive
  optOverflowChecks: Boolean;

  optDefaultVarStorage: TVarStorage;


type TBlockType = (btDefault, btStatic, btStack);

var RootBlockType: TBlockType;

implementation

end.

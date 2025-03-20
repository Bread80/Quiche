//Globals data and current settings for the compiler.

//For IDE settings see the MConfig unit
unit Def.Globals;

interface
uses Classes,
  Def.Functions, Def.Variables;

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
  optVarAutoCreate: Boolean;

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

  //The default implicit type to assign to decimal integers with no sign (+ or -)
  //specfied. Value must either be vtInteger or vtWord.
  //Use the {$IMPLICITINTEGERTYPE} directive
//  optImplicitIntegerType: TVarType;

  //Should decimal integer literals be parsed as signed or unsigned?
  //This options affects the 'implicit types' functionality of the parser.
  //If an integer literal has a sign prefix (+123 or -123) the value will always
  //be parsed as a signed integer (vtInt8 or vtInteger).
  //This setting affects when the literal does /not/ have a sign prefix.
  //If this setting is on such literals will be parsed as signed,
  //if this setting is off such literals will be parsed as unsigned.
  //The generated code is usually more efficient when using unsigned values and,
  //therefore, unsigned values are recommended unless signing is explicitly needed.
  //This setting does not affect hexadecimal or binary literals
  optDefaultSignedInteger: Boolean;

  //If this setting is on decimal integers will be parsed into the smallest type
  //which can contain the value the value. Thus, a literal value '10' will be
  //parsed as a vtInt or vtByte (depending on optDefaultSignedInteger). If this
  //setting is off the value will be parsed as a vtInteger or vtWord.
  //The generated code is usually more efficient when using 8-bit data,
  //therefore, 8-bit values should be used where possible.
  //This setting does not affect hexadecimal or binary literals
  optDefaultSmallestInteger: Boolean;

  optDefaultVarStorage: TVarStorage;

  optDefaultCallingConvention: TCallingConvention;

  optCleverPuppy: Boolean;
  //Enable or disable the Clever Puppy code generator
  //This feature is currently experimantal and likely to fail


type TBlockType = (btDefault, btStatic, btStack);

//Specifies how root level local variables are stored. Normally this will be
//btStatic.
//btStack is useful for testing small code fragments.
//btDefault is no valid as a RootBlockType
var RootBlockType: TBlockType;

const BlockTypeStrings: array[low(TBlockType)..high(TBlockType)] of String = (
  'Default', 'Static', 'Stack');

implementation

end.

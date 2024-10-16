//Types used by the Quiche compiler. Not to be confused with types used by the
//Delphi/Pascal compiler itself!
unit Def.QTypes;

interface

//========================CPU Metadata
const
  iCPUWordSize  = 2;  //For OG Z80
  iCPUWordMask  = $ffff;  //Mask for 'bitness' of an integer
  iIntegerMin   = $8000;  //Lowest value for an integer. Used when massing type values
  iRealSize     = 5;  //Byte size of a float on the current target. 5 is CPC size :)

//========================Super types

//Used in data file for Intrinsics
type TSuperType = (
  stParameterized,  //Actual type is given by a TypeDef parameter
  stAny, stNumeric, stAnyInteger, stOrdinal);

function StringToSuperType(const S: String;out Super: TSuperType): Boolean;
function SuperTypeToString(Super: TSuperType): String;

//========================Language types

type TVarType = (
  //Numeric types
  vtInt8,
  vtInteger,
  vtByte,
  vtWord,
  vtPointer,
  vtReal,

  //Boolean types
  vtBoolean,
  vtFlag,

  //Other simple types
  vtChar,
  vtTypeDef,
//  vtEnumeration,
//  vtRange,
//  vtSet,

  //Array types
  vtString,
//  vtArray,

  //Other complex types
//  vtRecord,

  //Error/undefined
  vtUnknown
  );

{
  //TODO:
  TUserType = record
    Name: String;
    case VarType: TVarType of
      vtEnumeration: NameList (Pointer), ItemCount;
      vtRange: Lower, Upper;  //Constant or run-time?
      vtSet: Enumartion: PUserType<Enumeration>
      vtArray: BoundRange: PUserType<Range>;
      vtRecord: FieldList (Pointer)
  end;
}


function VarTypeToName(VarType: TVarType): String;

function StringToVarType(VarTypeName: String): TVarType;

//Returns the number of bytes used to store the type.
//NOT the same as sizeof(): For types which use pointers (real, string etc)
//returns the size of the pointer data, not the stored data.
function GetTypeSize(VarType: TVarType): Integer;

//Any numeric type. Not typed pointers - these can't be used in expressions
function IsNumericType(VarType: TVarType): Boolean;

//Any integer type. Not typed pointers - these can't be used in expressions
function IsIntegerType(VarType: TVarType): Boolean;

//Returns True if the type is a signed numeric type. Returns False for *all* other
//types
function IsSignedType(VarType: TVarType): Boolean;

//Any type which only occupies a single byte
function IsByteType(VarType: TVarType): Boolean;

//Any numeric type which only occupies two bytes
function IsWordType(VarType: TVarType): Boolean;

//Any type which can be used in logical/boolean operations
function IsLogicalType(VarType: TVarType): Boolean;

//An enumerable type is one with a fixed range of named values
function IsEnumerable(VarType: TVarType): Boolean;


function TryIntegerToVarType(Value: Integer;out VarType: TVarType): Boolean;

//===================Type fitness

//Range types are used within the parser to assess how to store, expand and find
//primitives for numeric types
//NOTE: The compiler could be adapted for targets with different bitness by modifying
//this table and/or the constants which go with it (NumberRangeBounds)
//However, adaptations will almost certainly be required elsewhere.
type TNumberRange = (
    rgReal,   //..-32769:     Real                      n/a                      Real
    rgS16,    //-32768..-129: Unsigned 16               Real                     S16
    rgS8,     //-128..-1:     Signed 8 or Signed 16     Signed 16                S8
    rgAny,    //0..127:       Signed 8 or unsigned 8    Signed 16 or Unsigned 16 Any
    rgS16U8,  //128..255:     Signed 16 or unsigned 8   Signed 16 or Unsigned 16 S16U8
    rgS16U16, //256..32767:   Signed 16 or Unsigned 16  Real or Unsigned 16      S16U16
    rgU16     //32768..65535: Unsigned 16               Real                     U16
//  (Real)      65536..:      Real                      n/a                      Real
    );
const SignedRanges = [rgReal, rgS16, rgS8];

const NumberRangeBounds: array[low(TNumberRange)..high(TNumberRange)] of Integer = (
      -32769, -129,      -1,     127,    255,       32767,     65535);
    NumberRangeToSignedType: array[low(TNumberRange)..high(TNumberRange)] of TVarType = (
      vtReal, vtInteger, vtInt8, vtInt8, vtInteger, vtInteger, vtReal);
    NumberRangeToUnSignedType: array[low(TNumberRange)..high(TNumberRange)] of TVarType = (
      vtReal, vtReal,    vtReal, vtByte, vtByte,    vtWord,    vtWord);

//Takes an integer value and returns which of the above ranges if best fits
function IntToNumberRange(Value: Integer): TNumberRange;

//Assesses the compatibility level between the code (variable) type and primitive's
//parameter type.
//  <0: totally incompatible
//   0: exact match
//1..9: data will need to be expanded. The lower the number the less costly the
//      expansion
//>=10: data will need to be shrunk - note all values in the original type can
//      be represented in the target type. Values will need to be validated
//      (if validation is enabled). Result may fail (run-time error) or be incorrect
//      The higher the value the greater the incompatibility

//CodeType is the Type of the value (from a variable)
//PrimType is the type of the primitive's argument
//Signed should be true if SignCombine is set for the operator AND the other parameter
//is a signed type[1].
//Signed should be false in all other cases.
//[1] If the other parameter is a constant it's type should be established (via
//GetFitnessTypeRange) before calling this function
function GetFitnessTypeType(CodeType, PrimType: TVarType;Signed: Boolean): Integer;

//As GetFitnessTypeType but where the parameter is a constant AND a numerical type
function GetFitnessTypeRange(CodeRange: TNumberRange; PrimType: TVarType;Signed: Boolean): Integer;

function GetImmSignCombineType(Value: Integer;LType, RType: TVarType): TVarType;

//===============TImmValue

//Record to store a typed constant value. Used within ILParams and as default
//parameters within function definitions
type TImmValue = record
  constructor CreateInteger(AValue: Integer);
  constructor CreateChar(AValue: Char);
  constructor CreateBoolean(AVAlue: Boolean);

  //For (mostly) code generation
  //Only applicable to enumerated types
  function ToInteger: Integer;

  //Returns a string suitable for passing to the assembler.
  //Value returned must be a single byte. Value will be masked (with $ff) if necessary
  //This routine can return chars as string literals and makae code easier to read
  //than using numeric literals
  function ToStringByte: String;
  //Returns a 16-bit value masked with $ffff
  function ToStringWord: String;

  //For debugging. Sometimes for code generation
  function ToString: String;

  case VarType: TVarType of
    vtInt8, vtInteger, vtByte, vtWord, vtPointer:
      (IntValue: Integer);
    vtReal: (); //TODO
    vtBoolean, vtFlag: (BoolValue: Boolean);
    vtChar: (CharValue: Char);
    vtTypeDef: (TypeValue: TVarType);
    vtString: (); //TODO
  end;

//Get maximum/minimum value for a types range. Result only has meaning for enumarable types
//For non-numeric types the result is the integer representation of the value
function GetMaxValue(VarType: TVarType): Integer;
function GetMinValue(VarType: TVarType): Integer;
procedure SetMaxValue(var Value: TImmValue);
procedure SetMinValue(var Value: TImmValue);

//Validates whether the ExprType can be assigned to the variable (etc)
//with type of AssignType
function ValidateAssignmentType(AssignType, ExprType: TVarType): Boolean;


implementation
uses SysUtils,
  Def.Globals,
  CodeGen;

const SuperTypeNames: array[low(TSuperType)..high(TSuperType)] of String = (
  'Parameterized', 'Any', 'Numeric', 'AnyInteger', 'Ordinal');

function StringToSuperType(const S: String;out Super: TSuperType): Boolean;
var LSuper: TSuperType;
begin
  for LSuper := low(TSuperType) to high(TSuperType) do
    if CompareText(S, SuperTypeNames[LSuper]) = 0 then
    begin
      Super := LSuper;
      EXIT(True);
    end;
  Result := False;
end;

function SuperTypeToString(Super: TSuperType): String;
begin
  Result := SuperTypeNames[Super];
end;

const VarTypeNames: array[low(TVarType)..high(TVarType)] of String = (
  'Int8', 'Integer', 'Byte', 'Word', 'Pointer', 'Real',
  'Boolean', '<Flag>',
  'Char', 'TypeDef',
  {'Enumeration', 'Range', 'Set',}
  'String', {'Array',}
  {'Record',}
  '<Unknown>');
function VarTypeToName(VarType: TVarType): String;
begin
  Result := VarTypeNames[VarType];
end;

function StringToVarType(VarTypeName: String): TVarType;
begin
  for Result := low(VarTypeNames) to high(VarTypeNames) do
    if CompareText(VarTypeNames[Result], VarTypeName) = 0 then
      EXIT;

  //TODO: User defined types

  //TODO: FFBoolean should be a user defined type alias
  if CompareText(VarTypeName, 'FFBoolean') = 0 then
    EXIT(vtBoolean);

  Result := vtUnknown;
end;


const VarTypeSizes: array[low(TVarType)..high(TVarType)] of Integer = (
  1,2,1,2,2,iRealSize,
  1,1,
  1,1,
  {-1, -1, -1,}
  -1, {-1,}
  {-1,}
  0);
function GetTypeSize(VarType: TVarType): Integer;
begin
  Result := VarTypeSizes[VarType];

  //Add code for complex types here
  Assert(Result <> -1);
end;

function IsNumericType(VarType: TVarType): Boolean;
begin
  Result := VarType in [vtInt8, vtByte, vtInteger, vtWord, vtPointer, vtReal];
end;

function IsIntegerType(VarType: TVarType): Boolean;
begin
  Result := VarType in [vtInt8, vtByte, vtInteger, vtWord, vtPointer];
end;

function IsSignedType(VarType: TVarType): Boolean;
begin
  Result := VarType in [vtInt8, vtInteger, vtReal];
end;

function IsByteType(VarType: TVarType): Boolean;
begin
  Result := GetTypeSize(VarType) = 1;
end;

function IsWordType(VarType: TVarType): Boolean;
begin
  Result := GetTypeSize(VarType) = 2;
end;

function IsLogicalType(VarType: TVarType): Boolean;
begin
  Result := VarType in [vtBoolean, vtFlag];
end;

function IsEnumerable(VarType: TVarType): Boolean;
begin
  Result := VarType in [vtWord, vtByte, vtPointer, vtInt8, vtInteger, vtChar, vtBoolean];
end;

function GetMaxValue(VarType: TVarType): Integer;
begin
  case VarType of
    vtInt8: Result := 127;
    vtByte, vtChar: Result := 255;
    vtInteger: Result := 32767;
    vtWord, vtPointer: Result := 65535;
  else
    Assert(False);
    Result := 0;
  end;
end;

procedure SetMaxValue(var Value: TImmValue);
begin
  case Value.VarType of
    vtInt8: Value.IntValue := 127;
    vtByte: Value.IntValue := 255;
    vtInteger: Value.IntValue := 32767;
    vtWord, vtPointer: Value.IntValue := 65535;
    vtBoolean: Value.BoolValue := True;
    vtChar: Value.CharValue := #255;
    vtTypeDef: Value.TypeValue := high(TVarType);
  else
    Assert(False);
  end;
end;

function GetMinValue(VarType: TVarType): Integer;
begin
  case VarType of
    vtInt8: Result := -128;
    vtByte, vtChar, vtWord, vtPointer: Result := 0;
    vtInteger: Result := -32768;
  else
    Assert(False);
    Result := 0;
  end;
end;

procedure SetMinValue(var Value: TImmValue);
begin
  case Value.VarType of
    vtInt8: Value.IntValue := -128;
    vtByte, vtWord, vtPointer: Value.IntValue := 0;
    vtInteger: Value.IntValue := -32768;
    vtBoolean: Value.BoolValue := False;
    vtChar: Value.CharValue := #0;
    vtTypeDef: Value.TypeValue := low(TVarType);
  else
    Assert(False);
  end;
end;

function ValidateAssignmentType(AssignType, ExprType: TVarType): Boolean;
begin
  if AssignType = ExprType then
    EXIT(True);

  //Numeric types
  if IsNumericType(AssignType) and IsNumericType(ExprType) then
    EXIT(ExprType <> vtReal);
  case AssignType of
    vtString: EXIT(ExprType in [vtChar, vtString]);
    vtChar: //TODO: if AssignType = vtChar we can assign a string of length one to it
      if ExprType = vtString then
        raise Exception.Create('TODO: Add code to allow assigning string of length one to a Char');
  end;

  Result := False;
end;

function TryIntegerToVarType(Value: Integer;out VarType: TVarType): Boolean;
begin
  if Value < -32768 then
    EXIT(False)
  else if Value < -128 then
    VarType := vtInteger
  else if Value < 0 then
    VarType := vtInt8
  else if Value < 256 then
    VarType := vtByte
  else if Value <= 65535 then
    VarType := vtWord
  else
    EXIT(False);

  Result := True;
end;

//----------------------Fitness

function IntToNumberRange(Value: Integer): TNumberRange;
begin
  for Result := low(TNumberRange) to high(TNumberRange) do
    if Value <= NumberRangeBounds[Result] then
      EXIT;
  Result := high(TNumberRange);
end;

//Fitness values for (<primitive-type>,<code-type>)
const FitnessVarTypeVarType: array[vtInt8..vtReal,vtInt8..vtReal] of Integer =
//From. This axis is the parameter type. Other is primitive argument type
//To	    Int8	Int	Byt	Wrd	Ptr	Real
{Int8}	  ((0,	10,	10,	30,	30,	30),		//Otherwise:
{Integer}	(1,	  0,	3,	10,	10,	10),		//	If both parameters are unsigned set Byte to Integer to 3
{Byte}	  (20,	30,	0,	20,	20,	40),		//If SignCombine is set for an operator:
{Word}	  (10,	20,	2,	0,	1,	20),		//If SignCombine is set for an operator:
{Pointer}	(10,	20,	3,	1,	0,	20),		//	If either parameter is signed, set Byte to Integer to 1
{Real}	  (4,	  4,	4,	4,	4,	0));    //**This is handled in code in GetFitnessTypeType


//NOTE: FINAL VALUES TODO
const FitnessNumberRangeVarTypeUnsigned:
  array[vtInt8..vtReal,low(TNumberRange)..high(TNumberRange)] of Integer =
//	From. This axis is the parameter type. Other is primitive argument type
//To	  NR_Real	S16	S8	Any	S16U8	S16U16	U16
{Int8}	  ((40,	30,	0,	0,	10,	  20,	  30),
{Integer}	(20,  0,	1,	2,	2,	  1,	  10),
{Byte}	  (30,  20,	10,	0,	0,	  10,	  20),
{Word}	  (10,  10,	20,	1,	1,	  0,	  0),
{Pointer}	(10,  10,	20,	1,	1,	  0,	  0),
{Real}	  (0,	  4,	4,	4,	4,	  4,	  4));

//NOTE: FINAL VALUES TODO
const FitnessNumberRangeVarTypeSigned:
  array[vtInt8..vtReal,low(TNumberRange)..high(TNumberRange)] of Integer =
//	From. This axis is the parameter type. Other is primitive argument type
//To	  NR_Real	S16	S8	Any	S16U8	S16U16	U16
{Int8}	  ((40,	30,	0,	0,	10,	  20,	  30),
{Integer}	(20,  0,	1,	1,	1,	  0,	  10), //-1 from Any S16U8 S16u16
{Byte}	  (30,  20,	10,	0,	0,	  10,	  20),
{Word}	  (10,  10,	20,	2,	2,	  1,	  0), //+1 to Any, S16U8, S16u16
{Pointer}	(10,  10,	20,	2,	2,	  1,	  0), //+1 to Any, S16U8, S16u16
{Real}	  (0,	  4,	4,	4,	4,	  4,	  4));

//See the PrimSelection.xlsx spreadsheet for calculations, validations, and notes
//on the above fitness values.

//Assesses the compatibility level between the code (variable) type and primitive's
//parameter type.
//  <0: totally incompatible
//   0: exact match
//1..9: data will need to be expanded. The lower the number the less costly the
//      expansion
//>=10: data will need to be shrunk - note all values in the original type can
//      be represented in the target type. Values will need to be validated
//      (if validation is enabled). Result may fail (run-time error) or be incorrect
//      The higher the value the greater the incompatibility

//CodeType is the Type of the value (from a variable)
//PrimType is the type of the primitive's argument
//Signed should be true if SignCombine is set for the operator AND the other parameter
//is a signed type[1].
//Signed should be false in all other cases.
//[1] If the other parameter is a constant it's type should be established (via
//GetFitnessTypeRange) before calling this function
function GetFitnessTypeType(CodeType, PrimType: TVarType;Signed: Boolean): Integer;
begin
  if IsNumericType(CodeType) then
  begin
    if not IsNumericType(PrimType) then
      Result := -1
    else
      if Signed and (CodeType = vtByte) and (PrimType = vtInteger) then
        Result := 1
      else
        Result := FitnessVarTypeVarType[PrimType, CodeType];
  end
  else
    if CodeType = PrimType then
      Result := 0
    else
      Result := -1;
end;

//As GetFitnessTypeType but where the parameter is a constant AND a numerical type
function GetFitnessTypeRange(CodeRange: TNumberRange; PrimType: TVarType;Signed: Boolean): Integer;
begin
  //Ranges only apply to numeric types
  if not IsNumericType(PrimType) then
    EXIT(-1);

  if Signed then
    Result := FitnessNumberRangeVarTypeSigned[PrimType, CodeRange]
  else
    Result := FitnessNumberRangeVarTypeUnsigned[PrimType, CodeRange];
end;

function GetImmSignCombineType(Value: Integer;LType, RType: TVarType): TVarType;
begin
  Assert(IsNumericType(LType) and IsNumericType(RType));

  if LType = RType  then
    Result := LType
  else if IsSignedType(LType) and IsSignedType(RType) then
    Result := vtInteger
  else if (LType = vtPointer) or (RType = vtPointer) then
    Result := vtPointer
  else if (GetTypeSize(LType) = 1) and (GetTypeSize(RType) = 1) then
    if Value >= 0 then
      Result := vtByte
    else
      Result := vtInt8
  else if (LType = vtInteger) or (RType = vtInteger) then
    Result := vtInteger
  else
    Result := vtWord;

  if (Value >= GetMinValue(Result)) and (Value <= GetMaxValue(Result)) then
    EXIT;
  if Result in [vtInteger, vtWord, vtPointer] then
    EXIT;
  if Result = vtByte then
    EXIT(vtWord);

  Result := vtInteger;
end;

//----------------------TImmValue

constructor TImmValue.CreateBoolean(AValue: Boolean);
begin
  VarType := vtBoolean;
  BoolValue := AValue;
end;

constructor TImmValue.CreateChar(AValue: Char);
begin
  VarType := vtChar;
  CharValue := AValue;
end;

constructor TImmValue.CreateInteger(AValue: Integer);
begin
  VarType := vtInteger;
  IntValue := AValue;
end;

function TImmValue.ToInteger: Integer;
begin
  case VarType of
    vtInt8, vtInteger, vtByte, vtWord, vtPointer : Result := IntValue;
    vtBoolean:
      if BoolValue then
        Result := valueTrue
      else
        Result := valueFalse;
    vtChar: Result := ord(CharValue);
    vtTypeDef: Result := ord(TypeValue);
  else
    Assert(False);
    Result := 0;
  end;
end;

function TImmValue.ToString: String;
begin
  case VarType of
    vtByte: Result := '$' + IntToHex(IntValue, 2);
    vtWord, vtPointer: Result := '$' + IntToHex(IntValue, 4);
    vtInt8, vtInteger: Result := IntValue.ToString;
    vtBoolean:
      if BoolValue then
        Result := 'True'
      else
        Result := 'False';
    vtChar:
      if CharInSet(CharValue, [#32..#126]) then
        Result := ''''+CharValue+''''
      else
        Result := '#' + ord(CharValue).ToString;
    vtTypeDef:
      Result := VarTypeToName(TypeValue);
  else
    Assert(False);
  end;
end;

function TImmValue.ToStringByte: String;
begin
  case VarType of
    vtBoolean: Result := ByteToStr(ToInteger);
    vtChar:
      if CharInSet(CharValue, [#32..#127]) then
        Result := '''' + CharValue + ''''
      else
        Result := ByteToStr(ToInteger);
  else
    Result := ByteToStr(ToInteger);
  end;
end;

function TImmValue.ToStringWord: String;
begin
  Result := WordToStr(ToInteger);
end;

end.

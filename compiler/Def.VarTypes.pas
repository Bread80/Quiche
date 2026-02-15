//Types used by the Quiche compiler. Not to be confused with types used by the
//Delphi/Pascal compiler itself!
unit Def.VarTypes;

interface
uses SysUtils;

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
  //========== Types which can be instantiated 'as-is'
  //Numeric types
  vtInt8,
  vtInteger,
  vtByte,
  vtWord,
  vtPointer,
  vtTypedPointer, //Pointer to a type
  vtReal,

  //Boolean types
  vtBoolean,
  vtFlag,         //System only (at present)

  //Other simple types
  vtChar,
  vtTypeDef,      //System only (at present) - a pointer to the type data

  //========== Types which require a full declaration to instantiate
  //User types
  vtEnumeration,
(*
  vtSparseSet,  //Parse time only  - contains values and ranges
  vtRange,        //Run time
*)
  vtSetByte,    //A set which fits into a byte (1..8 elements)
  vtSetWord,    //A set which fits into a word (9..16 elements)
  vtSetMem,     //A set stored in memory

  //Array types
  vtArrayType,

  //Other complex types
  vtRecord,       //Has multiple fields
(*
  vtStream,       //Readble or writeable sequence of bytes or chars
*)
  vtFunction,     //Code as data.

  //Error/undefined
  vtUnknown
  );

  //How is array data stored?
  TArrayType = (
    atUnknown,    //Used by library routines to specify any array type
    atArray,      //An array with no run-time meta data. All data about bounds,
                  //length and capacity is stored in the type data and accessible
                  //only at compile time (unless RTTI is available).
    atVector,     //Array data is fixed length and stored with the data. For bounded
                  //arrays length is defined at compile time. For unbounded arrays
                  //the length field can be queried at run-time. This is used by
                  //pointers to vectors and function parameters.
    atList);      //The arrays /capacity/ is defined by the type and stored with
                  //the data. The current length is also stored with the data.

  //Max length of an array type (and size of meta data fields)
  TArraySize = (
    asUnknown,    //Not yet assigned (ie an error aonce the type has been defined),
                  //or not available (ie atArray)
    asShort,      //Max size is 255 elements (ie meta data is stored in bytes)
    asLong);      //Max size is 65535 elements (ie. meta data is stored in words)

  //Extra type data for array definitions
  PArrayDef = ^TArrayDef;
  TArrayDef = record
    ArrayType: TArrayType;  //How data is stored
    ArraySize: TArraySize;  //How bit is our meta data
    IsUnbounded: Boolean;   //Is this an unbounded array?
    ElementSize: Integer;   //Could be used for element-typeless parameters

    //Returns the byte size of meta data for this type
    function MetaSize: Integer;
    //Returns the VarType required to store the mate data for this type (vtUnknown for atArray)
    function MetaType: TVarType;
  end;

function VarTypeToName(VarType: TVarType): String;

//Returns vtUnknown if the name is not found.
function StringToVarType(TypeString: String): TVarType;

//Returns the number of bytes used to store the type.
//NOT the same as sizeof(): For types which use pointers (real, string etc)
//returns the size of the pointer data, not the stored data.
function GetVarTypeSize(VarType: TVarType): Integer;

//A Register type is one where the value of the variable is stored in registers.
//A Pointered type is one where the register stores a pointer to the actual data.
//Register types are usually those whose value fits into a register and Pointered
//types are those whose values (data) are often too large to fit into a register.
//Every type is either a Pointered Type or a Register Type. nothing is both.
function IsPointeredVarType(VarType: TVarType): Boolean;
function IsRegisterVarType(VarType: TVarType): Boolean;

//Any numeric type. Not typed pointers - these can't be used in expressions
function IsNumericVarType(VarType: TVarType): Boolean;

//Any integer type. Not typed pointers - these can't be used in expressions
function IsIntegerVarType(VarType: TVarType): Boolean;

//Returns True if the type is a signed numeric type. Returns False for *all* other
//types
function IsSignedVarType(VarType: TVarType): Boolean;

//Any type which only occupies a single byte
function IsByteVarType(VarType: TVarType): Boolean;

//Any numeric type which only occupies two bytes
function IsWordVarType(VarType: TVarType): Boolean;

//Any type which can be used in logical/boolean operations
function IsBooleanVarType(VarType: TVarType): Boolean;

//An ordinal type is one with an ordered set of values with a defined first and
//last value. These are the integer numeric types, chars, booleans, enumerations
//and subranges.
function IsOrdinalVarType(VarType: TVarType): Boolean;

//An enumerable type is one with an ordered set of values which can be 'enumerated'
//over is sequence. This includes Ordinal types as well as array types.
function IsEnumerableVarType(VarType: TVarType): Boolean;

//Any arrayed type. Array, vector, list, string etc.
function IsArrayVarType(VarType: TVarType): Boolean;

//For Integer and system ordinal types only: Returns True if Value is in range for type VarType
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

function GetImmSignCombineType(Value: Integer;LType, RType: TVarType): TVarType;

//Get maximum/minimum value for a types range. Result only has meaning for enumarable types
//For non-numeric types the result is the integer representation of the value
function GetMaxValue(VarType: TVarType): Integer;
function GetMinValue(VarType: TVarType): Integer;

type EVarType = class(Exception)
    constructor Create;
  end;

implementation
uses Def.Globals;

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
  'Int8', 'Integer', 'Byte', 'Word',
  'Pointer', 'TypedPointer',
  'Real',
  'Boolean', '<Flag>',
  'Char', 'TypeDef',
  'Enumeration', (*'<SparseRange>', 'Range',*)
  'Set','Set','Set',  //SetByte, SetWord, SetMem
  '<Array>',  //Use type specific name
  'Record',
(*'Stream',*)'Function',
  '<Unknown>');

function VarTypeToName(VarType: TVarType): String;
begin
  Result := VarTypeNames[VarType];
end;

function StringToVarType(TypeString: String): TVarType;
begin
  for Result := low(VarTypeNames) to high(VarTypeNames) do
    if CompareText(VarTypeNames[Result], TypeString) = 0 then
      EXIT;

  //TODO: FFBoolean should be a user defined type alias
  if CompareText(TypeString, 'FFBoolean') = 0 then
    EXIT(vtBoolean);

  Result := vtUnknown;
end;

//TODO: Clarify if we want the pointer size or the data size!
const VarTypeSizes: array[low(TVarType)..high(TVarType)] of Integer = (
  1,2,1,2,2,2,  //Integers and pointers
  iRealSize,    //Reals
  1,1,      //Boolean,<Flag>
  1,0,      //Char, TypeDef
  1,        //Enumeration
(*-1, 2,*)  //SparseRange, Range
  1, 2, 0,  //SetByte, SetWord, SetMem
  0,        //ArrayType
  0,        //Record
  (*2,*)    //Stream
  2,        //Function
  0);      //<Unknown>);

function GetVarTypeSize(VarType: TVarType): Integer;
begin
  Result := VarTypeSizes[VarType];

  //Add code for complex types here
  Assert(Result <> 0);
end;

const VarTypeIsPointered: array[low(TVarType)..high(TVarType)] of Boolean = (
  False, False, False, False, //Integers
  False, False, //Pointers
  False,        //Real
  False, False, //Booleans
  False, False, //Char, TypeDef (?)
  False, (* True, True, *) //Misc
  False, False, True, //SetByte, SetWord, SetMem
  True,         //Arrays
  True,         //Records
(*  True, *)    //Streams
  True,         //Functions
  False);       //Unknown

function IsPointeredVarType(VarType: TVarType): Boolean;
begin
  Result := VarTypeIsPointered[VarType];
end;

function IsRegisterVarType(VarType: TVarType): Boolean;
begin
  Result := not IsPointeredVarType(VarType);
end;

function IsNumericVarType(VarType: TVarType): Boolean;
begin
  Result := VarType in [vtInt8, vtByte, vtInteger, vtWord, vtPointer, vtTypedPointer, vtReal];
end;

function IsIntegerVarType(VarType: TVarType): Boolean;
begin
  Result := VarType in
    [vtInt8, vtByte, vtInteger, vtWord, vtPointer, vtTypedPointer];
end;

function IsSignedVarType(VarType: TVarType): Boolean;
begin
  Result := VarType in [vtInt8, vtInteger, vtReal];
end;

function IsByteVarType(VarType: TVarType): Boolean;
begin
  Result := GetVarTypeSize(VarType) = 1;
end;

function IsWordVarType(VarType: TVarType): Boolean;
begin
  Result := GetVarTypeSize(VarType) = 2;
end;

function IsBooleanVarType(VarType: TVarType): Boolean;
begin
  Result := VarType in [vtBoolean, vtFlag];
end;

function IsOrdinalVarType(VarType: TVarType): Boolean;
begin
  Result := VarType in [vtWord, vtByte, vtPointer, vtInt8, vtInteger,
    vtChar, vtBoolean, vtEnumeration];
end;

function IsArrayVarType(VarType: TVarType): Boolean;
begin
  Result := VarType = vtArrayType;
end;

function IsEnumerableVarType(VarType: TVarType): Boolean;
begin
  Result := IsOrdinalVarType(VarType) or IsArrayVarType(VarType);
end;

function GetMaxValue(VarType: TVarType): Integer;
begin
  case VarType of
    vtInt8: Result := 127;
    vtByte, vtChar: Result := 255;
    vtInteger: Result := 32767;
    vtWord, vtPointer: Result := 65535;
    vtBoolean: Result := valueTrue;
(*    vtTypeDef: Result := Integer(high(TVarType));
*)  else
    Assert(False);
    Result := 0;
  end;
end;

function GetMinValue(VarType: TVarType): Integer;
begin
  case VarType of
    vtInt8: Result := -128;
    vtByte, vtChar, vtWord, vtPointer(*, vtTypeDef*): Result := 0;
    vtInteger: Result := -32768;
    vtBoolean: Result := valueFalse;
  else
    Assert(False);
    Result := 0;
  end;
end;

function TryIntegerToVarType(Value: Integer;out VarType: TVarType): Boolean;
begin
  case VarType of
    vtInteger: EXIT((Value >= -32768) and (Value <= 32767));
    vtInt8: EXIT((Value >= -128) and (Value <= 127));
    vtByte, vtChar: EXIT((Value >= 0) and (Value <= 255));
    vtWord, vtPointer, vtTypedPointer: EXIT((Value >= 0) and (Value <= 65535));
    vtBoolean: EXIT((Value = -1) or (Value = 0));
  else
    Assert(False);
  end;
end;

function GetImmSignCombineType(Value: Integer;LType, RType: TVarType): TVarType;
begin
  Assert(IsNumericVarType(LType) and IsNumericVarType(RType));

  if LType = RType  then
    Result := LType
  else if IsSignedVarType(LType) and IsSignedVarType(RType) then
    Result := vtInteger
  else if (LType = vtPointer) or (RType = vtPointer) then
    Result := vtPointer
  else if (GetVarTypeSize(LType) = 1) and (GetVarTypeSize(RType) = 1) then
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

{ EVarType }

constructor EVarType.Create;
begin
  inherited Create('Invalid VarType');
end;

{ TArrayDef }

function TArrayDef.MetaSize: Integer;
const MetaUnitSize: array[low(TArraySize)..high(TArraySize)] of Integer =
  (0,1,2);
const MetaUnitCount: array[low(TArrayType)..high(TArrayType)] of Integer =
  (0,0,1,2);
begin
  Result := MetaUnitSize[ArraySize] * MetaUnitCount[ArrayType];
end;

function TArrayDef.MetaType: TVarType;
const MetaTypes: array[low(TArraySize)..high(TArraySize)] of TVarType =
  ( vtUnknown, vtByte, vtWord);
begin
  Result := MetaTypes[ArraySize];
end;

end.

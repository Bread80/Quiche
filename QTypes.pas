//Types used by the Quiche compiler. Not to be confused with types used by the
//Delphi/Pascal compiler itself!
unit QTypes;

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


//Record to store a typed constant value. Used within ILParams and as default
//parameters within function definitions
type TImmValue = record
  //For (mostly) code generation
  //Only applicable to enumerated types
  function ToInteger: Integer;

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
uses SysUtils, Globals;

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
      if CharValue in [#32..#126] then
        Result := ''''+CharValue+''''
      else
        Result := '#' + ord(CharValue).ToString;
    vtTypeDef:
      Result := VarTypeToName(TypeValue);
  else
    Assert(False);
  end;
end;
end.

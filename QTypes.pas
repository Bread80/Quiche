//Types used by the Quiche compiler. Not to be confused with types used by the
//Delphi/Pascal compiler itself!
unit QTypes;

interface

const
  iCPUWordSize  = 2;  //For OG Z80
  iRealSize     = 5;  //Byte size of a float on the current target. 5 is CPC size :)

//This is the type system as seen by the langauge parser
type TQType = Byte;

const
  //Simple types
  //ORDERING IS SIGNIFICANT - allows code optimisation
  vtUnknown = $ff;  //Unspecified (bug) or don't care/any type

  vtWord    = 0;
  vtByte    = 1;
  vtPointer = 2;  //Untyped pointer. Can be used in expressions
  vtInt8    = 3;
  vtInteger = 4;
  vtUntyped = 5;  //A special case for expr parser: Signals that an immediate value is the result
                  //of a typecast and should be accepted 'as is' without further type checking of
                  //an assignment.
                  //NOTE: ONLY to be used for immediate values
  vtReal    = 6;  //For future use.
  vtChar    = 7;
  //Unused  = 8
  vtBoolean = 9;
  vtString  = 10; //For future use. Pointer to actual data.
//  vtEnumeration
//  vtSet

  //Complex types
//  vtArray
//  vtRecord
  vtPointerTo = $80;  //Bitwise value. Pointer to the type specified in the other bits


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

//Any signed type (real or integer)
function IsSignedType(VarType: TVarType): Boolean;

//Any type which only occupies a single byte
function IsByteType(VarType: TVarType): Boolean;

//Any numeric type which only occupies two bytes
function IsWordType(VarType: TVarType): Boolean;

//Any type which can be used in logical/boolean operations
function IsLogicalType(VarType: TVarType): Boolean;

//An enumerable type is one with a fixed range of named values
function IsEnumerable(VarType: TVarType): Boolean;


//Get maximum/minium value for a types range. Result only has meaning for enumarable types
//For non-numeric types the result is the integer representation of tha value
function GetMaxValue(VarType: TVarType): Integer;
function GetMinValue(VarType: TVarType): Integer;


//--------------Internal types
//These are the types used for internal operations. Thus, for example, an Word
//and a Pointer all act as unsigned 16-bit. Bytes and Chars all act as unsigned 8-bits
//Used to specify both data storage and operations. Some values are only valid
//for one or the other
type TOpType = (
    rtUnknown,  //Should never happen!
    rtU8,       //Unsigned 8 bit: Byte, Char
    rtU16,      //Unsigned 16 bit: Word, Pointer
    rtS8,       //Unsigned 8-bit: only avaliable for basic operations (addition,  subtraction)
    rtS16,      //Signed 16 bit: Integer, Int16
    rtM16S16,   //Mixed S16 and U16 operands with a result of S16
    rtM16U16,   //Mixed S16 and U16 operands with a result of U16
    rtX8,       //8 bit operator. Exact type is immaterial
    rtX16,      //16 bit operator. Exact type is immaterial
    rtReal,     //Not yet valid
    rtBoolean   //Boolean (duh!)
    );
type TOpTypeSet= set of TOpType;

const OpTypeSize: array[low(TOpType) .. high(TOpType)] of Integer =
  (0,
  1, 2, 1, 2, 2, 2, 2, 1,
  2, 1);

const OpTypeNames: array[low(TOpType) .. high(TOpType)] of String =
  ('Unknown', 'U8','U16','S8','S16','M16S16','M16U16','X8','X16','Real','Boolean');

function VarTypeToOpType(VarType: TVarType): TOpType;

function StringToOpType(S: String): TOpType;

function StringToOpTypeSet(S: String): TOpTypeSet;


//-------------Type enumerations. Used within Operator definitions
type TTypeEnum =
    (teUnknown, teNone,//Special
    teBoolean, teByte, teWord, teInt8, teInteger, tePointer, teChar, //Simple types
    teReal, teString  //Stored as pointers
//    teRecord, teArray, teEnumeration, teSet
    //Complex types (also stored as pointer)
    );
  TTypeEnumSet = set of TTypeEnum;

const
  tesNumeric = [teByte, teWord, teInt8, teInteger, tePointer, teReal];
  tesInteger = [teByte, teWord, teInt8, teInteger, tePointer];
  tesWord = [teWord, teInteger, tePointer];
  //Types which can be used by logical operators
  tesLogical = [teBoolean, teByte, teWord, teInt8, teInteger, tePointer];
  tesEnumerated = [teByte, teWord, teInt8, teInteger, tePointer, teChar];

const TypeEnumNames: array[low(TTypeEnum)..high(TTypeEnum)] of String =
    ('Unknown', 'None',
    'Boolean','Byte','Word','Int8','Integer','Pointer','Char',
    'Real','String'
//    'Record','Array','Enumeration','Set'
    );

const TypeEnumToVarType: array[low(TTypeEnum)..high(TTypeEnum)] of TVarType =
    (vtUnknown, vtUnknown,
    vtBoolean,vtByte, vtWord, vtInt8, vtInteger, vtPointer, vtChar,
    vtReal, vtString
//    'Record','Array','Enumeration','Set'
    );
const TypeEnumToOpType: array[low(TTypeEnum)..high(TTypeEnum)] of TOpType =
    (rtUnknown, rtUnknown,
    rtBoolean,rtU8,rtU16,rtS8,rtS16,rtU16,rtU8,
    rtReal,rtU16{String}
//    'Record','Array','Enumeration','Set'
    );

//Validates whether the ExprType can be assigned to the variable (etc)
//with type of AssignType
function ValidateAssignmentType(AssignType, ExprType: TVarType): Boolean;

//As above but validates where the target can accept a number of types specified
//with a TTypeEnumSet. Useful for function etc parameters.
//If a match is found returns the minimum common available type
function FindAssignmentTypes(AssignTypes: TTypeEnumSet; ExprType: TVarType): TVarType;

function StringToTypeEnum(S: String): TTypeEnum;

function StringToTypeEnumSet(S: String): TTypeEnumSet;

function TypeEnumSetToString(Enum: TTypeEnumSet): String;

implementation
uses SysUtils, Globals;

const VarTypeNames : array[vtWord..vtString] of String = (
  'Word','Byte','Pointer','Int8','Integer','<INVALID>','Real','Char','<INVALID>','Boolean','String');

function VarTypeToName(VarType: TVarType): String;
begin
  if VarType = $ff then
    EXIT('<Unknown>');
  if VarType >= $80 then
    EXIT('^' + VarTypeToName(VarType and $7f));
  if VarType <= vtString then
    EXIT(VarTypeNames[VarType]);
  EXIT('<INVALID>');
end;

function StringToVarType(VarTypeName: String): TVarType;
begin
  for Result := low(VarTypeNames) to high(VarTypeNames) do
    if CompareText(VarTypeNames[Result], VarTypeName) = 0 then
      EXIT;

  //TODO: User defined types

  Result := vtUnknown;
end;

function GetTypeSize(VarType: TVarType): Integer;
const TypeSizes: array[vtWord..vtString] of Byte =
  (2,1,2,1,2,0,iRealSize,1,0,1,2);
begin
  if VarType = vtUnknown then
    //Unknown/invalid
    EXIT(0);
  if VarType >= $80 then
    //Pointer to type
    EXIT(iCPUWordSize);
  if VarType <= vtString then
    EXIT(TypeSizes[VarType]);

  //Add code for complex types here
  raise Exception.Create('Unknown type');
end;

function IsNumericType(VarType: TVarType): Boolean;
begin
  Result := VarType <= vtReal;
end;

function IsIntegerType(VarType: TVarType): Boolean;
begin
  Result := VarType < vtReal;
end;

function IsSignedType(VarType: TVarType): Boolean;
begin
  Result := (VarType >= vtInt8) and (VarType <= vtReal);
end;

function IsByteType(VarType: TVarType): Boolean;
begin
  Result := (VarType <= vtBoolean) and ((VarType and $01) = $01);
end;

function IsWordType(VarType: TVarType): Boolean;
begin
  Result := (VarType <= vtBoolean) and ((VarType and $01) = $00);
end;

function IsLogicalType(VarType: TVarType): Boolean;
begin
  Result := (VarType <= vtInteger) or (VarType = vtBoolean);
end;

function IsEnumerable(VarType: TVarType): Boolean;
begin
  Result := VarType in [vtWord, vtByte, vtPointer, vtInt8, vtInteger, vtChar, vtBoolean];
end;

function GetMaxValue(VarType: TVarType): Integer;
const MaxInbuilt : array[0..9] of Integer =
  (65535, 255, 65535, 127, 32767, -1, -1, 255, -1, valueTrue);
begin
  if VarType <= vtBoolean then
    Result := MaxInBuilt[VarType]
  else
    Result := -1;
end;

function GetMinValue(VarType: TVarType): Integer;
const MinInbuilt : array[0..9] of Integer =
  (0, 0, 0, -128, -32768, -1, -1, 0, -1, valueFalse);
begin
  if VarType <= vtBoolean then
    Result := MinInBuilt[VarType]
  else
    Result := -1;
end;

function VarTypeToOpType(VarType: TVarType): TOpType;
const lutVarTypeToOpType : array[vtWord..vtString] of TOpType =
  (rtU16, rtU8, rtU16, rtS8, rtS16, rtUnknown, rtReal, rtU8, rtUnknown, rtBoolean, rtU16);
begin
  if VarType <= vtString then
    EXIT(lutVarTypeToOpType[VarType]);
  if VarType >= $80 then
    EXIT(rtU16);
  raise Exception.Create('Unknown type');
end;

function StringToOpType(S: String): TOpType;
begin
  S := S.ToLower;
  if (S = 'same') or (S = 'left op') or (S = 'n/a') then
    EXIT(rtUnknown);
  for Result := low(TOpType) to High(TOpType) do
    if CompareText(S, OpTypeNames[Result]) = 0 then
      EXIT;
  raise Exception.Create('Unknown raw type name: ' + S);
end;

function StringToOpTypeSet(S: String): TOpTypeSet;
var
  RT: TOpType;
  Items: TArray<String>;
  Item: String;
begin
  S := S.ToLower;
  Items := S.Split([' ']);
  Result := [];

  for Item in Items do
  begin
    if Item = 'none' then
      //Nothing
    else if Item = 'any' then
    begin
      for RT := low(TOpType) to high(TOpType) do
        Result := Result + [RT];
    end
    else if Item = 'numeric' then
      Result := Result + [rtU8, rtU16, {rtS8,} rtS16, rtReal]
    else if Item = 'integer' then
      Result := Result + [rtU8, rtU16, {rtS8,} rtS16]
    else
      Result := Result + [StringToOpType(Item)];
  end;
end;

{function StringToVarType(S: String): TVarType;
const VarTypeNames: array[vtWord..vtString] of String =
    ('Word','Byte','Pointer','Int8','Integer',#0,'Real','Char',#0,'Boolean','String');
begin
  S := S.ToLower;
//  if (S = 'same') or (S = 'left op') or (S = 'n/a') then
//    EXIT(rtUnknown);
  for Result := low(TVarType) to High(TVarType) do
    if CompareText(S, VarTypeNames[Result]) = 0 then
      EXIT;
  raise Exception.Create('Unknown var type name');
end;
}

function ValidateAssignmentType(AssignType, ExprType: TVarType): Boolean;
begin
  //Numeric types
  if IsNumericType(AssignType) and IsNumericType(ExprType) then
    EXIT(ExprType <> vtReal);
  if AssignType = vtBoolean then
    EXIT(ExprType = vtBoolean);

  if AssignType = vtString then
    EXIT(ExprType in [vtChar, vtString]);
  if AssignType = vtChar then
    EXIT(ExprType = vtChar);

  Result := False;
end;

function FindAssignmentTypes(AssignTypes: TTypeEnumSet; ExprType: TVarType): TVarType;
begin
  //TODO: PointerTo types
  if ExprType = vtInt8 then
    if teInt8 in AssignTypes then
      EXIT(vtInt8);
  if ExprType in [vtInt8, vtInteger] then
    if teInteger in AssignTypes then
      EXIT(vtInteger)
    else if teReal in AssignTypes then
      EXIT(vtReal);

  if ExprType = vtByte then
    if teByte in AssignTypes then
      EXIT(vtByte);
  if ExprType in [vtByte, vtWord] then
    if teWord in AssignTypes then
      EXIT(vtWord)
    else if teReal in AssignTypes then
      EXIT(vtReal);
  if ExprType in [vtByte, vtPointer] then
    if tePointer in AssignTypes then
      EXIT(vtPointer);

  if (ExprType = vtReal) and (teReal in AssignTypes) then
    EXIT(vtReal);
  if (ExprType = vtBoolean) and (teBoolean in AssignTYpes) then
    EXIT(vtBoolean);
  if (ExprType = vtChar) and (teChar in AssignTypes) then
    EXIT(vtChar);

  if (ExprType in [vtChar, vtString]) and (teString in AssignTypes) then
    EXIT(vtString);

  Result := vtUnknown;
end;


function StringToTypeEnum(S: String): TTypeEnum;
begin
  S := S.ToLower;
//  if (S = 'same') or (S = 'left op') or (S = 'n/a') then
//    EXIT(rtUnknown);
  for Result := low(TTypeEnum) to High(TTypeEnum) do
    if CompareText(S, TypeEnumNames[Result]) = 0 then
      EXIT;
  raise Exception.Create('Unknown type enum name: ' + S);
end;


function StringToTypeEnumSet(S: String): TTypeEnumSet;
var
  RT: TTypeEnum;
begin
  S := S.ToLower;
  Result := [];
  if S = 'n/a' then
    EXIT;
  if S = 'any' then
  begin
    for RT := low(TTypeEnum) to high(TTypeEnum) do
      Result := Result + [RT];
    EXIT;
  end;
  if S = 'numeric' then
    EXIT(tesNumeric);
  if S = 'integer' then
    EXIT(tesInteger);
  if S = 'logical' then
    EXIT(tesLogical);
  if S = 'enumerated' then
    EXIT(tesEnumerated);
  if S = 'word' then
    EXIT(tesWord);
  Result := [StringToTypeEnum(S)];
end;

function TypeEnumSetToString(Enum: TTypeEnumSet): String;
var TE: TTypeEnum;
begin
  Result := '';
  for TE in Enum do
  begin
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + TypeEnumNames[TE];
  end;
end;

end.

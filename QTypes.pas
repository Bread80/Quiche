//Types used by the Quiche compiler. Not to be confused with types used by the
//Delphi/Pascal compiler itself!
unit QTypes;

interface

const iCPUWordSize = 2;  //For OG Z80

//This is the type system as seen by the langauge parser
type TQType = Byte;

const
  //Simple types
  //ORDERING IS SIGNIFICANT - allows code optimisation
  vtUnknown = $ff;  //Unspecified (bug) or don't care/any type

  vtWord    = 0;
  vtByte    = 1;
  vtPointer = 2;
  vtInt8    = 3;
  vtInteger = 4;
  //Unused    5
  vtReal    = 6;  //For future use. Pointer to actual data
  vtChar    = 7;
  //Unused  = 8
  vtBoolean = 9;
  vtString  = 10;  //For future use. Pointer to actual data. Length?
//  vtEnumeration //Index to declaration
//  vtSet         //Index to declaration

  //Complex types
//  vtArray       //Bounds plus Element type or index to custom type
//  vtRecord      //Index to declaration
  vtPointerTo = $80;  //Bitwise value. Pointer to the type specified in the other bits


function VarTypeToName(VarType: TVarType): String;

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
  (0, 1, 2, 1, 2, 2, 2, 2, 1, 2, 1);

const OpTypeNames: array[low(TOpType) .. high(TOpType)] of String =
  ('Unknown', 'U8','U16','S8','S16','M16S16','M16U16','X8','X16','Real','Boolean');

function VarTypeToOpType(VarType: TVarType): TOpType;

function StringToOpType(S: String): TOpType;

function StringToOpTypeSet(S: String): TOpTypeSet;


//-------------Type enumerations. Used within Operator definitions
type TTypeEnum =
    (teUnknown, //Special
    teBoolean, teByte, teWord, teInt8, teInteger, tePointer, teChar, //Simple types
    teReal, teString  //Stored as pointers
//    teRecord, teArray, teEnumeration, teSet
    //Complex types (also stored as pointer)
    );
  TTypeEnumSet = set of TTypeEnum;

const TypeEnumNames: array[low(TTypeEnum)..high(TTypeEnum)] of String =
    ('Unknown',
    'Boolean','Byte','Word','Int8','Integer','Pointer','Char',
    'Real','String'
//    'Record','Array','Enumeration','Set'
    );

const TypeEnumToVarType: array[low(TTypeEnum)..high(TTypeEnum)] of TVarType =
    (vtUnknown,
    vtBoolean,vtByte, vtWord, vtInt8, vtInteger, vtPointer, vtChar,
    vtReal, vtString
//    'Record','Array','Enumeration','Set'
    );
{const TypeEnumToOpType: array[low(TTypeEnum)..high(TTypeEnum)] of TOpType =
    (rtUnknown,
    rtBoolean,rtU8,rtU16,rtS8,rtS16,rtU16,rtU8,
    rtReal,rtU16{String}
//    'Record','Array','Enumeration','Set'
{    );
}
function StringToTypeEnum(S: String): TTypeEnum;

function StringToTypeEnumSet(S: String): TTypeEnumSet;

implementation
uses SysUtils;

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

function GetTypeSize(VarType: TVarType): Integer;
begin
  if VarType = vtUnknown then
    //Unknown/invalid
    EXIT(0);
  if VarType >= $80 then
    //Pointer to type
    EXIT(iCPUWordSize);
  if VarType <= vtString then
    //Bit zero specifies byte size
    EXIT(iCPUWordSize - (VarType and $01));

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
    EXIT([teByte, teWord, teInt8, teInteger, tePointer, teReal]);
  if S = 'integer' then
    EXIT([teByte, teWord, teInt8, teInteger, tePointer]);
  if S = 'logical' then
    EXIT([teBoolean, teByte, teWord, teInt8, teInteger, tePointer]);
  Result := [StringToTypeEnum(S)];
end;

end.

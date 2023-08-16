unit QTypes;

interface


{
  //Simple types
  //For values < 8, bit 0 is set for 1 byte types, clear for two byte (system word) types
  vtUnknown = $00;  //Unspecified (bug) or don't care/any type
  vtByte    = $01;
  vtWord    = $02;
  vtInt8    = $03;
  vtInteger = $04;
  vtChar    = $05;
  vtPointer = $06;
  vtBoolean = $07;
//  vtString      //Length?
//  vtFloat
//  vtEnumeration //Index to declaration
//  vtSet         //Index to declaration

  //Complex types
//  vtArray       //Bounds plus Element type or index to custom type
//  vtRecord      //Index to declaration
  vtPointerTo = $80;  //Bitwise value. Pointer to the type specified in the other bits
}

//-------------Types
type TVarType =
    (vtUnknown, //Special
    vtBoolean, vtByte, vtWord, vtInt8, vtInt16, vtInteger, vtPointer, vtChar, //Simple types
    vtReal, vtString  //Stored as pointers
    //Complex types (also stored as pointer)
    );
  TVarTypeSet = set of TVarType;
const TypeSize: array[low(TVarType) .. high(TVarType)] of Integer =
  (0,
  1,1,2,1,2,2,2,1,
  2,2 //Values stored as pointer = sizeof(Pointer)
  );
  VarTypeNames: array[low(TVarType) .. high(TVarType)] of String =
    ('<Unknown>', //Special
    'Boolean','Byte','Word','Int8','Int16','Integer','Pointer','Char',  //Simple
    'Real','String' //Stored as pointers
    );
const
  NumericTypes: TVarTypeSet = [vtByte, vtWord, vtInt8, vtInt16, vtInteger, vtPointer, vtReal];
  IntegerTypes = [vtByte, vtWord, vtInt8, vtInt16, vtInteger, vtPointer];
  SignedTypes = [vtInt8, vtInt16, vtInteger];
  ByteTypes = [vtBoolean, vtByte, vtInt8, vtChar];

//--------------Internal types
//Data type to be used for an operation.
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
  (0, 1, 2, 1, 2, 2, 2, 2, 1,2, 1);
//const lutRawTypeToVarType: array[low(TRawType) .. high(TRawType)] of TVarType =
//    (vtUnknown, vtByte, vtWord, vtInt8, vtInteger, vtReal, vtBoolean);
const lutVarTypeToOpType: array[low(TVarType)..high(TVarType)] of TOpType =
  (rtUnknown,
  rtBoolean, rtU8, rtU16, rtS8, rtS16,rtS16, rtU16, rtU8,
  rtUnknown, rtUnknown);
const RawTypeNames: array[low(TOpType) .. high(TOpType)] of String =
  ('Unknown', 'U8','U16','S8','S16','M16S16','M16U16','X8','X16','Real','Boolean');

function StringToOpType(S: String): TOpType;

function StringToOpTypeSet(S: String): TOpTypeSet;

function StringToVarType(S: String): TVarType;

function StringToVarTypeSet(S: String): TVarTypeSet;

implementation
uses SysUtils;

function StringToVarType(S: String): TVarType;
begin
  S := S.ToLower;
//  if (S = 'same') or (S = 'left op') or (S = 'n/a') then
//    EXIT(rtUnknown);
  for Result := low(TVarType) to High(TVarType) do
    if CompareText(S, VarTypeNames[Result]) = 0 then
      EXIT;
  raise Exception.Create('Unknown var type name');
end;

function StringToVarTypeSet(S: String): TVarTypeSet;
var
  RT: TVarType;
begin
  S := S.ToLower;
  Result := [];
  if S = 'n/a' then
    EXIT;
  if S = 'any' then
  begin
    for RT := low(TVarType) to high(TVarType) do
      Result := Result + [RT];
    EXIT;
  end;
  if S = 'numeric' then
    EXIT(NumericTypes);//[rtU8, rtU16, rtS8, rtS16, rtReal]);
  if S = 'integer' then
    EXIT(IntegerTypes);//[rtU8, rtU16, rtS8, rtS16]);
  if S = 'logical' then
    EXIT(IntegerTypes + [vtBoolean]);
  Result := [StringToVarType(S)];
end;

function StringToOpType(S: String): TOpType;
begin
  S := S.ToLower;
  if (S = 'same') or (S = 'left op') or (S = 'n/a') then
    EXIT(rtUnknown);
  for Result := low(TOpType) to High(TOpType) do
    if CompareText(S, RawTypeNames[Result]) = 0 then
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

end.

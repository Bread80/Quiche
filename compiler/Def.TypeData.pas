(*
Meta data for UserTypes
(Separate unit required to avoid circular references to Def.Consts)
*)
unit Def.TypeData;

interface
uses Def.Consts, Def.UserTypes,
  Parse.Errors;

function GetTypeLowValue(UserType: PUserType;var Value: TImmValue): TQuicheError;
function GetTypeHighValue(UserType: PUserType;var Value: TImmValue): TQuicheError;

implementation
uses Def.QTypes;

function GetTypeLowValue(UserType: PUserType;var Value: TImmValue): TQuicheError;
begin
  if not Assigned(UserType) then
    EXIT(ErrSub(qeOrdinalTypeExpected, 'nil'));

  if not IsOrdinalType(UserType.VarType) then
    EXIT(ErrSub(qeOrdinalTypeExpected, UserType.Name));

  Value.CreateTyped(UserType, UserType.Low);
  Result := qeNone;
end;

function GetTypeHighValue(UserType: PUserType;var Value: TImmValue): TQuicheError;
begin
  if not Assigned(UserType) then
    EXIT(ErrSub(qeOrdinalTypeExpected, 'nil'));

  if not IsOrdinalType(UserType.VarType) then
    EXIT(ErrSub(qeOrdinalTypeExpected, UserType.Name));

  Value.CreateTyped(UserType, UserType.High);
  Result := qeNone;
end;

end.

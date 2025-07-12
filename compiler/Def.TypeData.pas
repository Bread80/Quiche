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
    EXIT(ErrSub(qeSimpleTypeExpected, 'nil'));

  if IsOrdinalType(UserType.VarType) then
  begin
    case UserType.VarType of
      vtEnumeration: Value.CreateTyped(UserType, 0);
      vtSubRange: Value.CreateTyped(UserType, UserType.Low);
    else
      Value.CreateTyped(UserType, GetMinValue(UserType.VarType));
    end;
    Result := qeNone;
  end
  else
    EXIT(ErrSub(qeSimpleTypeExpected, UserType.Name));
end;

function GetTypeHighValue(UserType: PUserType;var Value: TImmValue): TQuicheError;
begin
  if not Assigned(UserType) then
    EXIT(ErrSub(qeSimpleTypeExpected, 'nil'));

  if IsOrdinalType(UserType.VarType) then
  begin
    case UserType.VarType of
      vtEnumeration: Value.CreateTyped(UserType, Length(UserType.EnumItems));
      vtSubRange: Value.CreateTyped(UserType, UserType.High);
    else
      Value.CreateTyped(UserType, GetMaxValue(UserType.VarType));
    end;
    Result := qeNone;
  end
  else
    EXIT(ErrSub(qeSimpleTypeExpected, UserType.Name));
end;

end.

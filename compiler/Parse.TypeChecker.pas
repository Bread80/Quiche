unit Parse.TypeChecker;

interface
uses Def.UserTypes,
  Parse.Errors, Parse.Literals;


//Tests whether a value of type FromType can be assigned to the variable, argument (etc)
//of type ToType
function TypeCheck(ToType, FromType: PUserType): TQuicheError;

//Tests whether the type of the value returned by Slug is compatible with the ToType.
//If it is returns errNone, otherwise returns a suitable error code
function TypeCheckFromSlug(ToType: PUserType;const Slug: TExprSlug): TQuicheError;


implementation
uses SysUtils,
  Def.VarTypes, Def.Consts, Def.Operators, Def.IL;

//Assignments to an array. Both types are array types, and both are different types
function TypeCheckArrays(ToType, FromType: PUserType): TQuicheError;
var ToAT: TArrayType;
  FromAT: TArrayType;
begin
  //Both must match longshort/unknown and both must have the same element size
  if (ToType.ArrayDef.ArraySize <> FromType.ArrayDef.ArraySize) or
    (ToType.ArrayDef.ElementSize <> FromType.ArrayDef.ElementSize) then
    EXIT(ErrSub2(qeTypeMismatch, FromType.Description, ToType.Description));

  //Element types must be compatible
  Result := TypeCheck(ToType.OfType, FromType.OfType);
  if Result <> qeNone then
    EXIT;

  ToAT := ToType.ArrayDef.ArrayType;
  FromAT := FromType.ArrayDef.ArrayType;
  if ToType.ArrayDef.IsUnbounded then
    case ToType.ArrayDef.ArrayType of
      atArray: EXIT(qeNone);
      atVector:
        if FromAT in [atVector, atList] then
          EXIT(qeNone);
      atList:
        if FromAT = atList then
          EXIT(qeNone);
    else
      Assert(False);
    end
  else  //ToType is bounded - types must match exactly
    if ToAT = FromAT then
      EXIT(qeNone);

  Result := ErrSub2(qeTypeMismatch, FromType.Description, ToType.Description);
end;

function TypeCheck(ToType, FromType: PUserType): TQuicheError;
var ToVT: TVarType;
  FromVT: TVarType;
begin
  Assert(Assigned(ToType));
  Assert(Assigned(FromType));

  if ToType = FromType then
    EXIT(qeNone);

  ToVT := ToType.VarType;
  FromVT := FromType.VarType;

  if IsOrdinalType(ToType) then
  begin
    //Numeric types - we can convert between them...
    if IsNumericType(ToType) then
    begin
      //..except from Reals to Integer types (need to use Trunc)
      if FromVT = vtReal then
        EXIT(Err(qeTypeMismatchImplicitReal));

      if not IsNumericType(FromType) then
          EXIT(ErrSub2(qeTypeMismatch, FromType.Description, ToType.Description));
    end
    else
      //For other ordinals the base type must be the same
      if IsOrdinalType(ToType) and IsOrdinalType(FromType) then
        if RemoveSubRange(ToType) <> RemoveSubRange(FromType) then
          EXIT(ErrSub2(qeTypeMismatch, FromType.Description, ToType.Description));

    //Ordinal ranges must intersect in some way
    if IsOrdinalType(ToType) and IsOrdinalType(FromType) then
      if (ToType.High >= FromType.Low) and (FromType.High >= ToType.Low) then
        EXIT(qeNone)
      else
        EXIT(ErrSub2(qeTypeMismatchNoOverlap, FromType.Description, ToType.Description));
  end;


  if IsArrayType(ToType) and IsArrayType(FromType) then
    EXIT(TypeCheckArrays(ToType, ToType));

  //Other types. Probably plenty to add here
  case ToVT of
    vtReal:
      if IsNumericType(FromType) then
        EXIT(qeNone);
    vtArrayType:
      Assert(False, 'TODO');
(*      if FromVT in [vtChar, vtString] then
        EXIT(qeNone);
*)    vtChar: //TODO: if ToType = vtChar we can assign a string of length one to it
      if FromVT = vtArrayType then
        Assert(False, 'TODO');
//        raise Exception.Create('TODO: Add code to allow assigning string of length one to a Char');
    vtPointer:  //Typed pointers can be assigned to untyped pointers
      if FromVT = vtTypedPointer then
        EXIT(qeNone);
  end;

  Result := ErrSub2(qeTypeMismatch, FromType.Description, ToType.Description);
end;

//Validate whether an immediate value can be assigned to a variable (etc) of type ToType.
function TypeCheckImm(ToType: PUserType;const Imm: TImmValue): TQuicheError;
begin
  if IsIntegerType(ToType) and IsIntegerVarType(Imm.VarType) then
  begin //Integer value in range?
    if not ((Imm.IntValue >= ToType.Low) and (Imm.IntValue <= ToType.High)) then
      EXIT(ErrSub2(qeConstantAssignmentOutOfRange, Imm.ToString, ToType.Description));
    EXIT(qeNone);
  end
  else if IsOrdinalType(ToType) and IsOrdinalType(Imm.UserType) then
  begin //Other ordinal types
    //Are the types compatible?
    Result := TypeCheck(ToType, Imm.UserType);
    if Result <> qeNone then
      EXIT;

    //Is the value in range?
    if ToType.VarType <> vtBoolean then //(Boolean ranges are different)
      if not ((Imm.ToInteger >= ToType.Low) and (Imm.ToInteger <= ToType.High)) then
        EXIT(ErrSub2(qeConstantAssignmentOutOfRange, Imm.ToString, ToType.Description));
    EXIT(qeNone);
  end
  else
    Result := TypeCheck(ToType, Imm.UserType);
end;

function TypeCheckFromSlug(ToType: PUserType;const Slug: TExprSlug): TQuicheError;
begin
  Assert(Assigned(ToType));

  if (Slug.Op = OpUnknown) and (Slug.ILItem = nil) and (Slug.Operand.Kind = pkImmediate) then
    //An immediate with no expression - can we assign the Imm to the type?
    Result := TypeCheckImm(ToType, Slug.Operand.Imm)
  else //We either have an operation or a variable - can we assign the type to the type?
    Result := TypeCheck(ToType, Slug.ResultType);
end;

end.

unit Parse.TypeChecker;

interface
uses Def.UserTypes,
  Parse.Errors, Parse.Literals;


//Validates whether the ExprType can be assigned to the variable (etc)
//with type of ToType
function ValidateAssignmentType(ToType, ExprType: PUserType): TQuicheError;

//Tests whether the expression returned by Slug is compatible with the type
//given in ToType. If it is returns errNone, otherwise returns a suitable error code
function ValidateAssignment(ToType: PUserType;const Slug: TExprSlug): TQuicheError;


implementation
uses SysUtils,
  Def.VarTypes, Def.Consts, Def.Operators, Def.IL;

function ValidateAssignmentType(ToType, ExprType: PUserType): TQuicheError;
var ToVT: TVarType;
  ExprVT: TVarType;
begin
  Assert(Assigned(ToType));
  Assert(Assigned(ExprType));

  if ToType = ExprType then
    EXIT(qeNone);

  ToVT := ToType.VarType;
  ExprVT := ExprType.VarType;

  if IsOrdinalType(ToVT) then
  begin
    //Numeric types - we can convert between them...
    if IsNumericType(ToVT) then
    begin
      //..except from Reals to Integer types (need to use Trunc)
      if ExprVT = vtReal then
        EXIT(Err(qeTypeMismatchImplicitReal));

      if not IsNumericType(ExprVT) then
          EXIT(ErrSub2(qeTypeMismatch, ExprType.Description, ToType.Description));
    end
    else
      //For other ordinals the base type must be the same
      if IsOrdinalType(ToVT) and IsOrdinalType(ExprVT) then
        if RemoveSubRange(ToType) <> RemoveSubRange(ExprType) then
          EXIT(ErrSub2(qeTypeMismatch, ExprType.Description, ToType.Description));

    //Ordinal ranges must intersect in some way
    if IsOrdinalType(ToVT) and IsOrdinalType(ExprVT) then
      if (ToType.High >= ExprType.Low) and (ExprType.High >= ToType.Low) then
        EXIT(qeNone)
      else
        EXIT(ErrSub2(qeTypeMismatchNoOverlap, ExprType.Description, ToType.Description));
  end;


  //Other types. Probably plenty to add here
  case ToVT of
    vtReal:
      if IsNumericType(ExprVT) then
        EXIT(qeNone);
    vtString:
      if ExprVT in [vtChar, vtString] then
        EXIT(qeNone);
    vtChar: //TODO: if ToType = vtChar we can assign a string of length one to it
      if ExprVT = vtString then
        raise Exception.Create('TODO: Add code to allow assigning string of length one to a Char');
    vtPointer:  //Typed pointers can be assigned to untyped pointers
      if ExprVT = vtTypedPointer then
        EXIT(qeNone);
  end;

  Result := ErrSub2(qeTypeMismatch, ExprType.Description, ToType.Description);
end;

//Validate whether an immediate value can be assigned to a variable (etc) of type ToType.
function ValidateAssignImm(ToType: PUserType;const Imm: TImmValue): TQuicheError;
begin
  if IsIntegerType(ToType) and IsIntegerVarType(Imm.VarType) then
  begin //Integer value in range?
    if not ((Imm.IntValue >= ToType.Low) and (Imm.IntValue <= ToType.High)) then
      EXIT(ErrSub2(qeConstantAssignmentOutOfRange, Imm.ToString, ToType.Description));
    EXIT(qeNone);
  end
  else if IsOrdinalType(UTToVT(ToType)) and IsOrdinalType(Imm.VarType) then
  begin //Other ordinal types
    //Are the types compatible?
    Result := ValidateAssignmentType(ToType, Imm.UserType);//ExprType);
    if Result <> qeNone then
      EXIT;

    //Is the value in range?
    if ToType.VarType <> vtBoolean then //(Boolean ranges are different)
      if not ((Imm.ToInteger >= ToType.Low) and (Imm.ToInteger <= ToType.High)) then
        EXIT(ErrSub2(qeConstantAssignmentOutOfRange, Imm.ToString, ToType.Description));
    EXIT(qeNone);
  end
  else
    Result := ValidateAssignmentType(ToType, Imm.UserType);//ExprType);
end;

function ValidateAssignment(ToType: PUserType;const Slug: TExprSlug): TQuicheError;
begin
  Assert(Assigned(ToType));

  if (Slug.Op = OpUnknown) and (Slug.ILItem = nil) and (Slug.Operand.Kind = pkImmediate) then
    //An immediate with no expression - can we assign the Imm to the type?
    Result := ValidateAssignImm(ToType, Slug.Operand.Imm)
  else //We either have an operation or a variable - can we assign the type to the type?
    Result := ValidateAssignmentType(ToType, Slug.ResultType);
end;

end.

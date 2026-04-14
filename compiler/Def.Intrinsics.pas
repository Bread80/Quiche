unit Def.Intrinsics;
(*
Intrinsics are Operations which use function syntax in the Quiche program source
code.
*)

interface
uses Def.Compiler, Def.Functions, Def.Scopes;

procedure InitialiseIntrinsics;

//!!Currently adds to GetCurrentScope!!
procedure LoadIntrinsicsFile(const Filename: String;SysUnit: TUnit);


implementation
uses SysUtils, Classes,
  Parse.Base,
  Def.Operators, Def.VarTypes, Def.Variables, Def.UserTypes;

procedure InitialiseIntrinsics;
begin
  //Nothing
end;

function ParseIntrinsicFlags(const Field: String): TIntrinsicFlagSet;
begin
  Result := [];
  if CompareText(Field, 'totype') = 0 then
    Result := Result + [ifToType]
  else if Field <> '' then
    raise Exception.Create('Invalid flag or flags for Intrinsic: ' + Field);
end;

const //Column indexes
  fName           = 1;
  fP1Access       = 2;
  fP1Name         = 3;
  fP1VarType      = 4;
  fP1Flags        = 5;
  fP2Access       = 6;
  fP2Name         = 7;
  fP2VarType      = 8;
  fP2DefaultValue = 9;
  fP2Flags        = 10;
  fResultType     = 11;
  fComments       = 12;

procedure LoadIntrinsicsFile(const Filename: String;SysUnit: TUnit);

  procedure ProcessParam(Param: TParameter;const Access, Name, VarType,
    DefaultValue, Flags: String);
  var ArrayDef: TArrayDef;
    UserType: TUserType;
    IntValue: Integer;
  begin
    if CompareText(Access, 'val') = 0 then
      Param.Access := paVal
    else
      if not IdentToAccessSpecifierEX(Access, Param.Access, Param.IsByRef) then
        raise Exception.Create('Unknown access specifier: ' + Access);

    Param.Name := Name;

    //TODO: Should only include builtin types. Should allow 'base' types
    Param.UserType := GetSystemType(StringToType(VarType, ArrayDef), @ArrayDef);
    if Param.UserType = nil then
      if not StringToSuperType(VarType, Param.SuperType) then
        raise Exception.Create('Invalid VarType for Intrinsic: ''' + VarType + '''');

    if DefaultValue <> '' then
      begin //TODO: A proper parse value to TImmValue!
        if Param.VarType = vtTypeDef then
        begin
          UserType := IdentToType(DefaultValue);
          if UserType = nil then
            raise Exception.Create('Unable to parse TypeDef default value: ''' + DefaultValue + '''')
          else
            Param.DefaultValue.CreateTypeDef(UserType);
        end
        else
        begin
          //TODO: Set type based on value
          if not TryStrToInt(DefaultValue, IntValue) then
            //Test for other valid value types
            raise Exception.Create('Unable to parse default value: ''' + DefaultValue + '''');

          Param.DefaultValue.CreateTyped(vtInteger, IntValue);
        end;

        Param.HasDefaultValue := True;
      end;

    Param.IntrinsicFlags := ParseIntrinsicFlags(Flags);
  end;

var Data: TStringList;
  Line: String;
  Fields: TArray<String>;
  I: Integer;
  Intrinsic: TIntrinsic;
  ArrayDef: TArrayDef;
  Arg1, Arg2, Res: TParameter;
begin
  ParseData.OpenILScope(SysUnit);

  Data := TStringlist.Create;
  try
    Data.LoadFromFile(Filename);

    for Line in Data do
      if (Length(Line) > 0) and (Line.Chars[0] <> ';') then
      begin
        if Line.StartsWith('END') then
          EXIT;

        Fields := Line.Split([',']);
        if Fields[fName] <> '' then
        begin
          if Length(Fields) < 12 then
            raise Exception.Create('Operators line too short: ' + Line);
          for I:=0 to Length(Fields)-1 do
            Fields[I] := Fields[I].Trim;

          Intrinsic := TIntrinsic.Create(Fields[fName], SysUnit);
          SysUnit.Add(Intrinsic);
//          Intrinsic.CallingConvention := ccIntrinsic;

          Intrinsic.Op := IdentToIntrinsicOperator(Fields[fName]);
          if Intrinsic.Op = opUnknown then
            raise Exception.Create('Operator not found: ' + Fields[fName]);

          //================FIRST PARAMETER

          if Fields[fP1Access] <> '' then
          begin
            Arg1 := Intrinsic.AddArg;

            ProcessParam(Arg1, Fields[fP1Access], Fields[fP1Name],
              Fields[fP1VarType], '', Fields[fP1Flags]);

            //=================SECOND PARAMETER

            if Fields[fP2Access] <> '' then
            begin
              Arg2 := Intrinsic.AddArg;

              ProcessParam(Arg2, Fields[fP2Access], Fields[fP2Name],
                Fields[fP2VarType], Fields[fP2DefaultValue], Fields[fP2Flags]);
            end;
          end;

          //=======================RESULT

          if CompareText(Fields[fResultType], 'None') <> 0 then
          begin
            Res := Intrinsic.AddResult;

            Res.Access := paResult;
            if CompareText(Fields[fResultType], 'Param1') = 0 then
            begin
              Res.UserType := Arg1.UserType;
              Res.SuperType := Arg1.SuperType;
            end
            else
            begin
              Res.UserType :=
                GetSystemType(StringToType(Fields[fResultType], ArrayDef), @ArrayDef);
              if Res.UserType = nil then
                if not StringToSuperType(Fields[fResultType], Res.SuperType) then
                  raise Exception.Create('Invalid ResultType for Intrinsic: ' + Fields[fResultType]);
            end;
          end;

          for I := fComments to Length(Fields)-1 do
            Intrinsic.Comments := Intrinsic.Comments + Fields[I] + ', ';
          //Remove trailing comma-space
          Intrinsic.Comments := Intrinsic.Comments.SubString(0, Length(Intrinsic.Comments)-2);
          //There will be quotes if the data contains a comma
          if Intrinsic.Comments.StartsWith('"') then
            Intrinsic.Comments := Intrinsic.Comments.SubString(1, Length(Intrinsic.Comments)-2);
        end;
      end;

  finally
    ParseData.CloseILScope(SysUnit);
    Data.Free;
  end;
end;

end.

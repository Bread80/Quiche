unit Def.Intrinsics;
(*
Intrinsics are Operations which use function syntax in the Quiche program source
code.
*)

interface
uses Def.Functions;

procedure InitialiseIntrinsics;

procedure LoadIntrinsicsFile(const Filename: String);


implementation
uses SysUtils, Classes,
  Def.Operators, Def.QTypes, Def.Variables, Def.UserTypes, Def.Scopes;

procedure InitialiseIntrinsics;
begin
  //Nothing
end;

function ParseIntrinsicFlags(const Field: String): TIntrinsicFlagSet;
begin
  Result := [];
  if CompareText(Field, 'arrayasbounds') = 0 then
    Result := Result + [ifArrayAsBounds]
  else if CompareText(Field, 'totype') = 0 then
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

procedure LoadIntrinsicsFile(const Filename: String);
var Data: TStringList;
  Line: String;
  Fields: TArray<String>;
  I: Integer;
  Intrinsic: PFunction;
  UT: PUserType;
  IntValue: Integer;
  PrevScope: PScope;
begin
  PrevScope := GetCurrentScope;
  SetCurrentScope(@SystemScope);

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

          Intrinsic := FuncCreate('System', Fields[fName]);
          Intrinsic.CallingConvention := ccIntrinsic;

          Intrinsic.Op := IdentToIntrinsicOperator(Fields[fName]);
          if Intrinsic.Op = opUnknown then
            raise Exception.Create('Operator not found: ' + Fields[fName]);

          //================FIRST PARAMETER

          if Fields[fP1Access] = '' then
            Intrinsic.ParamCount := 0
          else
          begin
            if CompareText(Fields[fP1Access], 'val') = 0 then
              Intrinsic.Params[0].Access := vaVal
            else
              Assert(IdentToAccessSpecifier(Fields[fP1Access], Intrinsic.Params[0].Access));
            Intrinsic.Params[0].Name := Fields[fP1Name];

            //TODO: Should only include builtin types. Should allow 'base' types
            Intrinsic.Params[0].UserType := GetSystemType(StringToVarType(Fields[fP1VarType]));
            if Intrinsic.Params[0].UserType = nil then
              if not StringToSuperType(Fields[fP1VarType], Intrinsic.Params[0].SuperType) then
                raise Exception.Create('Invalid P1VarType for Intrinsic: ' + Fields[fP1VarType]);

            Intrinsic.Params[0].IntrinsicFlags := ParseIntrinsicFlags(Fields[fP1Flags]);

            //=================SECOND PARAMETER

            if Fields[fP2Access] = '' then
              Intrinsic.ParamCount := 1
            else
            begin
              Intrinsic.ParamCount := 2;
              if CompareText(Fields[fP2Access], 'val') = 0 then
                Intrinsic.Params[1].Access := vaVal
              else
                Assert(IdentToAccessSpecifier(Fields[fP2Access], Intrinsic.Params[1].Access));
              Intrinsic.Params[1].Name := Fields[fP2Name];

              Intrinsic.Params[1].UserType := GetSystemType(StringToVarType(Fields[fP2VarType]));
              if Intrinsic.Params[1].UserType = nil then
                if not StringToSuperType(Fields[fP2VarType], Intrinsic.Params[1].SuperType) then
                  raise Exception.Create('Invalid P2VarType for Intrinsic: ' + Fields[fP2VarType]);
              if Fields[fP2DefaultValue] <> '' then
              begin //TODO: A proper parse value to TImmValue!
                if UTToVT(Intrinsic.Params[1].UserType) = vtTypeDef then
                begin
                  UT := IdentToType(Fields[fP2DefaultValue]);
                  if UT = nil then
                    raise Exception.Create('Unable to parse TypeDef default value: ' + Fields[fP2DefaultValue])
                  else
                  begin
                    Intrinsic.Params[1].DefaultValue.CreateTypeDef(UT);
                  end
                end
                else
                begin
                  //TODO: Set type based on value
                  if not TryStrToInt(Fields[fP2DefaultValue], IntValue) then
                    //Test for other valid value types
                    raise Exception.Create('Unable to parse default value: ' + Fields[fP2DefaultValue]);

                  Intrinsic.Params[1].DefaultValue.CreateTyped(vtInteger, IntValue);
                end;
                Intrinsic.Params[1].HasDefaultValue := True;
              end;

              Intrinsic.Params[1].IntrinsicFlags := ParseIntrinsicFlags(Fields[fP2Flags]);
            end;
          end;

          //=======================RESULT

          if CompareText(Fields[fResultType], 'None') <> 0 then
          begin
            Intrinsic.Params[Intrinsic.ParamCount].Access := vaResult;
            if CompareText(Fields[fResultType], 'Param1') = 0 then
            begin
              Intrinsic.Params[Intrinsic.ParamCount].UserType := Intrinsic.Params[0].UserType;
              Intrinsic.Params[Intrinsic.ParamCount].SuperType := Intrinsic.Params[0].SuperType;
            end
            else
            begin
              Intrinsic.Params[Intrinsic.ParamCount].UserType := GetSystemType(StringToVarType(Fields[fResultType]));
              if Intrinsic.Params[Intrinsic.ParamCount].UserType = nil then
                if not StringToSuperType(Fields[fResultType], Intrinsic.Params[Intrinsic.ParamCount].SuperType) then
                  raise Exception.Create('Invalid ResultType for Intrinsic: ' + Fields[fResultType]);
            end;
            Intrinsic.ResultCount := 1;
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
    Data.Free;
    SetCurrentScope(PrevScope);
  end;

end;

end.

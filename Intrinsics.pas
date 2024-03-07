unit Intrinsics;
(*
Intrinsics are Operations which use function syntax in the Quiche program source
code.
*)

interface
uses Functions;

procedure InitialiseIntrinsics;

procedure LoadIntrinsicsFile(const Filename: String);


implementation
uses SysUtils, Classes, Generics.Collections,
  QTypes, Operators, Variables;

procedure InitialiseIntrinsics;
begin
  //Nothing
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
  Op: POpData;
  VT: TVarType;
begin
  Data := TStringlist.Create;
  try
    Data.LoadFromFile(Filename);

  Data := TStringList.Create;
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

        Intrinsic.Op := IdentToOperator(Fields[fName]);
        if Op = nil then
          raise Exception.Create('Operator not found: ' + Fields[fName]);

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
          Intrinsic.Params[0].VarType := StringToVarType(Fields[fP1VarType]);
          if Intrinsic.Params[0].VarType = vtUnknown then
            if not StringToSuperType(Fields[fP1VarType], Intrinsic.Params[0].SuperType) then
              raise Exception.Create('Invalid P1VarType for Intrinsic: ' + Fields[fP1VarType]);

          Assert(Fields[fP1Flags] = '');
          //  fP1Flags    = 4;
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

            Intrinsic.Params[1].VarType := StringToVarType(Fields[fP2VarType]);
            if Intrinsic.Params[1].VarType = vtUnknown then
              if not StringToSuperType(Fields[fP2VarType], Intrinsic.Params[1].SuperType) then
                raise Exception.Create('Invalid P2VarType for Intrinsic: ' + Fields[fP2VarType]);
            if Fields[fP2DefaultValue] <> '' then
            begin
              if Intrinsic.Params[1].VarType = vtTypeDef then
              begin
                VT := StringToVarType(Fields[fP2DefaultValue]);
                if VT = vtUnknown then
                  raise Exception.Create('Unable to parse TypeDef default value: ' + Fields[fP2DefaultValue])
                else
                  Intrinsic.Params[1].DefaultValueInt := Integer(VT);
              end
              else
              begin
                if not TryStrToInt(Fields[fP2DefaultValue], Intrinsic.Params[1].DefaultValueInt) then
                  //Test for other valid value types
                  raise Exception.Create('Unable to parse default value: ' + Fields[fP2DefaultValue]);
              end;
              Intrinsic.Params[1].HasDefaultValue := True;
            end;
            Assert(Fields[fP2Flags] = '');
          //fP2Flags    = 7;
          end;
        end;
        if CompareText(Fields[fResultType], 'None') <> 0 then
        begin
          Intrinsic.Params[Intrinsic.ParamCount].Access := vaResult;
          if CompareText(Fields[fResultType], 'Param1') = 0 then
          begin
            Intrinsic.Params[Intrinsic.ParamCount].VarType := Intrinsic.Params[0].VarType;
            Intrinsic.Params[Intrinsic.ParamCount].SuperType := Intrinsic.Params[0].SuperType;
          end
          else
          begin
            Intrinsic.Params[Intrinsic.ParamCount].VarType := StringToVarType(Fields[fResultType]);
            if Intrinsic.Params[Intrinsic.ParamCount].VarType = vtUnknown then
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
  end;
end;

end.

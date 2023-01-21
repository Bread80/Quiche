unit Variables;

interface
uses Classes;

type TVarType = (vtUnknown, vtInteger);
type
  PVariable = ^TVariable;
  TVariable = record
    Name: String;
    WriteCount: Integer;
    VarType: TVarType;
    ValueInt: Integer;
  end;

procedure ClearVars;
function FindOrAddVar(AName: String;out Index: Integer;out VarSub: Integer): PVariable;

function VarIncWritecount(VarIndex: Integer): Integer;
function VarIndexToName(Index: Integer): String;
function VarToString(V: PVariable): String;
procedure VarsToStrings(S: TStrings);

implementation
uses Generics.Collections, SysUtils;

var Vars: TList<PVariable>;

procedure ClearVars;
var V: PVariable;
begin
  for V in Vars do
    Dispose(V);
  Vars.Clear;
end;

function FindOrAddVar(AName: String;out Index: Integer;out VarSub: Integer): PVariable;
var I: Integer;
begin
  for I := 0 to Vars.Count-1 do
    if CompareText(Vars[I].Name, AName) = 0 then
    begin
      Index := I;
      VarSub := Vars[I].WriteCount;
      EXIT(Vars[I]);
    end;

  New(Result);
  Index := Vars.Add(Result);
  VarSub := 0;
  Result.Name := AName;
  Result.VarType := vtUnknown;
  Result.WriteCount := 0;
end;

function VarIncWritecount(VarIndex: Integer): Integer;
begin
  Result := Vars[VarIndex].WriteCount + 1;
  Vars[VarIndex].WriteCount := Result;
end;

function VarIndexToName(Index: Integer): String;
begin
  Result := Vars[Index].Name;
end;

function VarToString(V: PVariable): String;
begin
  Result := V.Name + ': ';
  case V.VarType of
    vtUnknown: Result := Result + '<unknown>';
    vtInteger: Result := Result + 'Integer = ' + IntToStr(V.ValueInt);
  else
    Result := Result + '*** UNCODED TYPE ***';
  end;
end;

procedure VarsToStrings(S: TStrings);
var I: Integer;
begin
  S.Clear;
  for I := 0 to Vars.Count-1 do
    S.Add(IntToStr(I) + '- ' + VarToString(Vars[I]));
end;

initialization
  Vars := TList<PVariable>.Create;
finalization
  ClearVars;
  Vars.Free;
end.

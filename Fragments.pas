unit Fragments;

interface
uses ILData, Scopes;

type
  PFragment = ^TFragment;
  TFragment = record
    Name: String;
    Code: String;
  end;

procedure InitialiseFragments;

procedure LoadFragmentsFile(Filename: String);

function FindFragmentByName(const AName: String): PFragment;

function FragmentSub(AName: String;ILItem: PILItem;Scope: PScope): String;

function FragmentParamSub(AName: String;Param: TILParam;const Prefix: String): String;


implementation
uses Classes, Generics.Collections, SysUtils, Variables, QTypes, Z80.CPU;

var  FragList: TList<PFragment>;

procedure ClearFragments;
var Entry: PFragment;
begin
  for Entry in FragList do
    Dispose(Entry);
  FragList.Clear;
end;

procedure InitialiseFragments;
begin
  ClearFragments;
end;

function FindFragmentByName(const AName: String): PFragment;
begin
  for Result in FragList do
    if CompareText(AName, Result.Name) = 0 then
      EXIT;
  Result := nil;
end;

procedure LoadFragmentsFile(Filename: String);
var SL: TStringList;
  Line: String;
  Entry: PFragment;
begin
  Entry := nil;
  SL := nil;
  try
    SL := TStringList.Create;
    SL.LoadFromFile(Filename);

    for Line in SL do
    begin
      if (Line.Trim = '') or (Line.Chars[0] = ';') then
        //Skip
      else if Line.Chars[0] = '=' then
      begin
        Entry := New(PFragment);
        Entry.Name := Line.Trim.Substring(1,MaxInt);
        if Assigned(FindFragmentByName(Entry.Name)) then
          raise Exception.Create('LoadFragments: Entry redeclared: ' + Entry.Name);
        Entry.Code := '';
        FragList.Add(Entry);
      end
      else
        if Entry = nil then
          raise Exception.Create('Code library data found but not in library Entry: "' + Line + '"')
        else
        begin
          if Entry.Code <> '' then
            Entry.Code := Entry.Code + #13#10;
          Entry.Code := Entry.Code + Line;
        end;
    end;

  finally
    SL.Free;
  end;
end;

function ByteToStr(Value: Integer): String;
begin
  Result := '$' + IntToHex(lo(Value), 2).ToLower
end;

function WordLoToStr(Value: Integer): String;
begin
  Result := '$' + IntToHex(lo(Value), 4).ToLower
end;

function ImmByte(const Param: TILParam): String;
begin
  Result := ByteToStr(lo(Param.ImmValueInt));
end;

function ImmHighByte(const Param: TILParam): String;
begin
  Result := ByteToStr(hi(Param.ImmValueInt));
end;

function ImmWord(const Param: TILParam): String;
begin
  Result := '$' + IntToHex(Param.ImmValueInt and iCPUWordMask, 4).ToLower;
end;

function OffsetToStr(Offset: Integer): String;
begin
  if Offset < 0 then
    Result := '-' + ByteToStr(-Offset)
  else
    Result := '+' + ByteToStr(Offset);
end;

function CodeOffset(const Param: TILParam;out Comment: String): String;
var Variable: PVariable;
begin
  Variable := Param.ToVariable;
  Assert(Variable.Storage = vsStack);
  Result := OffsetToStr(Variable.Offset);
  Comment := Variable.Name;
end;

function CodeOffsetHigh(const Param: TILParam;out Comment: String): String;
var Variable: PVariable;
begin
  Variable := Param.ToVariable;
  Assert(Variable.Storage = vsStack);
  Result := OffsetToStr(Variable.Offset+1);
  Comment := Variable.Name;
end;

function CodeVarName(const Param: TILParam;out Comment: String): String;
var Variable: PVariable;
begin
  Variable := Param.ToVariable;
  Assert(Variable.Storage = vsStatic);
  Result := Variable.GetAsmName;
  Comment := Variable.Name;
end;

function DoParamSubs(S: String;const Param: TILParam;const Prefix: String;ThrowErrors: Boolean): String;
var
  St: Integer;  //Start of param
  En: Integer;  //End of param
  Pfx: Integer;
  PrefixMatch: String;
  PName: String;  //Parameter name (to be substituted)
  Sub: String;  //Substitution string
  Comment: String;
begin
  Result := S;
  St := 0;
  while St < Length(Result) do
  begin
    Comment := '';
    while (St < Length(Result)) and (Result.Chars[St] <> '<') do
      inc(St);
    if St >= Length(Result) then
      EXIT;
    inc(St);
    En := St;
    while (En < Length(Result)) and (Result.Chars[En] <> '>') do
      inc(En);
    if En < Length(Result) then
    begin
      Pfx:= St;
      while (Pfx < Length(Result)) and (Result.Chars[Pfx] <> '.') do
        inc(Pfx);
      if Pfx >= Length(Result) then
        raise Exception.Create('Invalid substitution in ''' + S + '''');
      PrefixMatch := Result.SubString(St, Pfx-St);
      if PrefixMatch.StartsWith(Prefix) then
      begin
        Sub := '';
        PName := Result.Substring(Pfx+1,En-Pfx-1);
        //Immediate data
        if CompareText(PName, 'immbyte') = 0 then
          Sub := ImmByte(Param)
        else if CompareText(PName, 'immword') = 0 then
          Sub := ImmWord(Param)
        else if CompareText(PName, 'immwordlow') = 0 then
          Sub := ImmByte(Param)
        else if CompareText(PName, 'immwordhigh') = 0 then
          Sub := ImmHighByte(Param)
        //8 bit registers
        else if CompareText(PName, 'r8') = 0 then
          Sub := CPUReg8ToChar[Param.Reg]
        //16 bit registers
        else if CompareText(PName, 'r16') = 0 then
          Sub := CPURegPairToString[Param.Reg]
        else if CompareText(PName, 'r16low') = 0 then
          Sub := CPURegLowToChar[Param.Reg]
        else if CompareText(PName, 'r16high') = 0 then
          Sub := CPURegHighToChar[Param.Reg]
        //Absolutes (fixed/global variables)
        else if CompareText(PName, 'varname') = 0 then
          Sub := CodeVarName(Param, Comment)
        //Offsets (stack variables)
        else if CompareText(PName, 'offset') = 0 then
          Sub := CodeOffset(Param, Comment)
        else if CompareText(PName, 'offsetlow') = 0 then
          Sub := CodeOffset(Param, Comment)
        else if CompareText(PName, 'offsethigh') = 0 then
          Sub := CodeOffsetHigh(Param, Comment)
        ;

        if Sub = '' then
        begin
          if ThrowErrors then
            raise Exception.Create('Library substitution not found: ' + PName);
        end
        else
          Result := Result.Substring(0,St-1) + Sub + Result.Substring(En+1);

        if Comment <> '' then
          Result := Result.Replace('$$',Comment);
      end;
    end
    else
      raise Exception.Create('Unmatched braces in code snippet: ' + S);
  end;
end;

function DoScopeSubs(S: String;ILItem: PILItem;Scope: PScope): String;
var
  St: Integer;  //Start of param
  En: Integer;  //End of param
  PName: String;  //Parameter name (to be substituted)
  Sub: String;  //Substitution string
  Comment: String;
begin
  Result := S;
  while True do
  begin
    Comment := '';
    St := Result.IndexOf('<');
    En := Result.IndexOf('>');
    Sub := '';
    if (St = -1) and (En = -1) then
      EXIT;

    if (St <> -1) and (En <> -1) then
    begin
      PName := Result.Substring(St+1,En-St-1);

      if CompareText(PName, 'vars.localsbytesize') = 0 then
        Sub := WordLoToStr(VarGetLocalsByteSize)
      else if CompareText(PName, 'vars.paramsbytesize') = 0 then
        Sub := WordLoToStr(VarGetParamsByteSize)
        ;


      if Sub = '' then
        raise Exception.Create('Library substitution not found: ' + PName);

      Result := Result.Substring(0,St) + Sub + Result.Substring(En+1);
      if Comment <> '' then
        Result := Result.Replace('$$',Comment);
    end
    else
      raise Exception.Create('Unmatched braces in code snippet: ' + S);
  end;
end;

function FragmentSub(AName: String;ILItem: PILItem;Scope: PScope): String;
var Frag: PFragment;
begin
  Frag := FindFragmentByName(AName);
  if not Assigned(Frag) then
    raise Exception.Create('Library fragment not found for: ' + AName);

  Result := DoParamSubs(Frag.Code, ILItem.Param1, 'p1', False);
  Result := DoParamSubs(Result, ILItem.Param2, 'p2', False);
  Result := DoParamSubs(Result, ILItem.Dest, 'd', False);
  Result := DoScopeSubs(Result, ILItem, Scope);
end;

function FragmentParamSub(AName: String;Param: TILParam;const Prefix: String): String;
var Frag: PFragment;
begin
  Frag := FindFragmentByName(AName);
  if not Assigned(Frag) then
    raise Exception.Create('Library fragment not found for: ' + AName);

  Result := DoParamSubs(Frag.Code, Param, Prefix, True);
end;

initialization
  FragList := TList<PFragment>.Create;
end.

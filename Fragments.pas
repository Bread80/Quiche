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

function ImmByte(Param: PILParam): String;
begin
  Result := ByteToStr(lo(Param.ImmValueInt));
end;

function ImmHighByte(Param: PILParam): String;
begin
  Result := ByteToStr(hi(Param.ImmValueInt));
end;

function ImmWord(Param: PILParam): String;
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

function CodeOffset(Param: PILParam;out Comment: String): String;
var Variable: PVariable;
begin
  Variable := Param.ToVariable;
  Assert(Variable.Storage = vsStack);
  Result := OffsetToStr(Variable.Offset);
  Comment := Variable.Name;
end;

function CodeOffsetHigh(Param: PILParam;out Comment: String): String;
var Variable: PVariable;
begin
  Variable := Param.ToVariable;
  Assert(Variable.Storage = vsStack);
  Result := OffsetToStr(Variable.Offset+1);
  Comment := Variable.Name;
end;

function CodeVarName(Param: PILParam;out Comment: String): String;
var Variable: PVariable;
begin
  Variable := Param.ToVariable;
  Assert(Variable.Storage = vsStatic);
  Result := Variable.GetAsmName;
  Comment := Variable.Name;
end;

function DoSubs(S: String;ILItem: PILItem;Scope: PScope): String;
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
      //Immediate data
      if CompareText(PName, 'p1.immbyte') = 0 then
        Sub := ImmByte(@ILItem.Param1)
      else if CompareText(PName, 'p2.immbyte') = 0 then
        Sub := ImmByte(@ILItem.Param2)

      else if CompareText(PName, 'p1.immword') = 0 then
        Sub := ImmWord(@ILItem.Param1)
      else if CompareText(PName, 'p1.immwordlow') = 0 then
        Sub := ImmByte(@ILItem.Param1)
      else if CompareText(PName, 'p1.immwordhigh') = 0 then
        Sub := ImmHighByte(@ILItem.Param1)

      else if CompareText(PName, 'p2.immword') = 0 then
        Sub := ImmWord(@ILItem.Param2)
      else if CompareText(PName, 'p2.immwordlow') = 0 then
        Sub := ImmByte(@ILItem.Param2)
      else if CompareText(PName, 'p2.immwordhigh') = 0 then
        Sub := ImmHighByte(@ILItem.Param2)

      //8 bit registers
      else if CompareText(PName, 'd.r8') = 0 then
        Sub := CPUReg8ToChar[ILItem.Param3.Reg]
      else if CompareText(PName, 'p1.r8') = 0 then
        Sub := CPUReg8ToChar[ILItem.Param1.Reg]
      else if CompareText(PName, 'p2.r8') = 0 then
        Sub := CPUReg8ToChar[ILItem.Param2.Reg]

      //16 bit registers
      else if CompareText(PName, 'd.r16') = 0 then
        Sub := CPURegPairToString[ILItem.Param3.Reg]
      else if CompareText(PName, 'd.r16low') = 0 then
        Sub := CPURegLowToChar[ILItem.Param3.Reg]
      else if CompareText(PName, 'd.r16high') = 0 then
        Sub := CPURegHighToChar[ILItem.Param3.Reg]

      else if CompareText(PName, 'p1.r16') = 0 then
        Sub := CPURegPairToString[ILItem.Param1.Reg]
      else if CompareText(PName, 'p1.r16low') = 0 then
        Sub := CPURegLowToChar[ILItem.Param1.Reg]
      else if CompareText(PName, 'p1.r16high') = 0 then
        Sub := CPURegHighToChar[ILItem.Param1.Reg]

      else if CompareText(PName, 'p2.r16') = 0 then
        Sub := CPURegPairToString[ILItem.Param2.Reg]
      else if CompareText(PName, 'p2.r16low') = 0 then
        Sub := CPURegLowToChar[ILItem.Param2.Reg]
      else if CompareText(PName, 'p2.r16high') = 0 then
        Sub := CPURegHighToChar[ILItem.Param2.Reg]

      //Absolutes (fixed/global variables)
      else if CompareText(PName, 'd.varname') = 0 then
        Sub := CodeVarName(@ILItem.Param3, Comment)
      else if CompareText(PName, 'p1.varname') = 0 then
        Sub := CodeVarName(@ILItem.Param1, Comment)
      else if CompareText(PName, 'p2.varname') = 0 then
        Sub := CodeVarName(@ILItem.Param2, Comment)

      //Offsets (stack variables)
      else if CompareText(PName, 'd.offset') = 0 then
        Sub := CodeOffset(@ILItem.Param3, Comment)
      else if CompareText(PName, 'd.offsetlow') = 0 then
        Sub := CodeOffset(@ILItem.Param3, Comment)
      else if CompareText(PName, 'd.offsethigh') = 0 then
        Sub := CodeOffsetHigh(@ILItem.Param3, Comment)

      else if CompareText(PName, 'p1.offset') = 0 then
        Sub := CodeOffset(@ILItem.Param1, Comment)
      else if CompareText(PName, 'p1.offsetlow') = 0 then
        Sub := CodeOffset(@ILItem.Param1, Comment)
      else if CompareText(PName, 'p1.offsethigh') = 0 then
        Sub := CodeOffsetHigh(@ILItem.Param1, Comment)

      else if CompareText(PName, 'p2.offset') = 0 then
        Sub := CodeOffset(@ILItem.Param2, Comment)
      else if CompareText(PName, 'p2.offsetlow') = 0 then
        Sub := CodeOffset(@ILItem.Param2, Comment)
      else if CompareText(PName, 'p2.offsethigh') = 0 then
        Sub := CodeOffsetHigh(@ILItem.Param2, Comment)

      else if CompareText(PName, 'vars.localsbytesize') = 0 then
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

  Result := DoSubs(Frag.Code, ILItem, Scope);
end;

initialization
  FragList := TList<PFragment>.Create;
end.

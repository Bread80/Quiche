unit Fragments;

interface
uses ILData;

type
  PFragment = ^TFragment;
  TFragment = record
    Name: String;
    Code: String;
  end;

procedure LoadFragments(Filename: String);

function FindFragmentByName(const AName: String): PFragment;

function FragmentSub(AName: String;ILItem: PILItem): String;


implementation
uses Classes, Generics.Collections, SysUtils, Variables;

var  Lib: TList<PFragment>;

procedure ClearFragments;
var Entry: PFragment;
begin
  for Entry in Lib do
    Dispose(Entry);
  Lib.Clear;
end;

function FindFragmentByName(const AName: String): PFragment;
begin
  for Result in Lib do
    if CompareText(AName, Result.Name) = 0 then
      EXIT;
  Result := nil;
end;

function FindEntry(AName: String): String;
var Frag: PFragment;
begin
  Frag := FindFragmentByName(AName);

  if Frag = nil then
    raise Exception.Create('Library segment not found for: ' + AName);

  Result := Frag.Code;
end;

procedure LoadFragments(Filename: String);
var SL: TStringList;
  Line: String;
  Entry: PFragment;
begin
  ClearFragments;
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
        Lib.Add(Entry);
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

function ImmByte(Param: PILParam): String;
begin
  Result := ByteToStr(lo(Param.ImmValue));
end;

function ImmHighByte(Param: PILParam): String;
begin
  Result := ByteToStr(hi(Param.ImmValue));
end;

function ImmWord(Param: PILParam): String;
begin
  Result := '$' + IntToHex(Param.ImmValue, 4).ToLower;
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
  Variable := ILParamToVariable(Param);
  Assert(Variable.Storage = vsRelative);
  Result := OffsetToStr(Variable.Offset);
  Comment := Variable.Name;
end;

function CodeOffsetHigh(Param: PILParam;out Comment: String): String;
var Variable: PVariable;
begin
  Variable := ILParamToVariable(Param);
  Assert(Variable.Storage = vsRelative);
  Result := OffsetToStr(Variable.Offset+1);
  Comment := Variable.Name;
end;

function CodeVarName(Param: PILParam;out Comment: String): String;
var Variable: PVariable;
begin
  Variable := ILParamToVariable(Param);
  Assert(Variable.Storage = vsAbsolute);
  Result := VarGetAsmName(Variable);
  Comment := Variable.Name;
end;

function DoSubs(S: String;ILItem: PILItem): String;
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
        Sub := AllocLocToReg8[ILItem.ResultAlloc]
      else if CompareText(PName, 'p1.r8') = 0 then
        Sub := AllocLocToReg8[ILItem.Param1Alloc]
      else if CompareText(PName, 'p2.r8') = 0 then
        Sub := AllocLocToReg8[ILItem.Param2Alloc]

      //16 bit registers
      else if CompareText(PName, 'd.r16') = 0 then
        Sub := AllocLocToHighReg[ILItem.ResultAlloc] + AllocLocToLowReg[ILItem.ResultAlloc]
      else if CompareText(PName, 'd.r16low') = 0 then
        Sub := AllocLocToLowReg[ILItem.ResultAlloc]
      else if CompareText(PName, 'd.r16high') = 0 then
        Sub := AllocLocToHighReg[ILItem.ResultAlloc]

      else if CompareText(PName, 'p1.r16') = 0 then
        Sub := AllocLocToRegPair[ILItem.Param1Alloc]
      else if CompareText(PName, 'p1.r16low') = 0 then
        Sub := AllocLocToLowReg[ILItem.Param1Alloc]
      else if CompareText(PName, 'p1.r16high') = 0 then
        Sub := AllocLocToHighReg[ILItem.Param1Alloc]

      else if CompareText(PName, 'p2.r16') = 0 then
        Sub := AllocLocToRegPair[ILItem.Param2Alloc]
      else if CompareText(PName, 'p2.r16low') = 0 then
        Sub := AllocLocToLowReg[ILItem.Param2Alloc]
      else if CompareText(PName, 'p2.r16high') = 0 then
        Sub := AllocLocToHighReg[ILItem.Param2Alloc]

      //Absolutes (fixed/global variables)
      else if CompareText(PName, 'd.varname') = 0 then
        Sub := CodeVarName(@ILItem.Dest, Comment)
      else if CompareText(PName, 'p1.varname') = 0 then
        Sub := CodeVarName(@ILItem.Param1, Comment)
      else if CompareText(PName, 'p2.varname') = 0 then
        Sub := CodeVarName(@ILItem.Param2, Comment)

      //Offsets (stack variables)
      else if CompareText(PName, 'd.offset') = 0 then
        Sub := CodeOffset(@ILItem.Dest, Comment)
      else if CompareText(PName, 'd.offsetlow') = 0 then
        Sub := CodeOffset(@ILItem.Dest, Comment)
      else if CompareText(PName, 'd.offsethigh') = 0 then
        Sub := CodeOffsetHigh(@ILItem.Dest, Comment)

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
        Sub := CodeOffsetHigh(@ILItem.Param2, Comment);


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

function FragmentSub(AName: String;ILItem: PILItem): String;
var Code: String;
begin
  Code := FindEntry(AName);
  Result := DoSubs(Code, ILItem);
end;


initialization
  Lib := TList<PFragment>.Create;
end.

(*
Generate code from library fragments
*)
unit Lib.GenFragments;

interface
uses Def.IL,
  Lib.Data;

//Generate a fragment with no substitutions
procedure GenFragment(Fragment: PFragment);
procedure GenFragmentName(const AName: String);
//Generate a fragment with substitiions from an ILItem
procedure GenFragmentItem(Fragment: PFragment;ILItem: PILItem);
procedure GenFragmentItemName(const AName: String;ILItem: PILItem);
//Generate a fragment with substitutions from an ILParam
procedure GenFragmentParam(Fragment: PFragment;const Param: TILParam;const Prefix: String);
procedure GenFragmentParamName(const AName: String;Param: TILParam;const Prefix: String);

//=====================OTHER

//Insert code from a fragment or a library call
procedure GenLibraryProc(const ProcName: String;ILItem: PILItem);

implementation
uses Classes, Generics.Collections, SysUtils,
  IDE.Compiler, //For meta commentary
  Def.QTypes, Def.Consts, Def.Variables, Def.UserTypes,
  Lib.CPUState,
  Parse.Source,
  CG.Data,
  Z80.CPUState, Z80.Hardware, Z80.Assembler, Z80.GenProcs, Z80.Load;

//==============================SUBSTITUTIONS

function ImmByte(const Param: TILParam): String;
begin
  Result := Param.Imm.ToStringByte;
end;

function ImmHighByte(const Param: TILParam): String;
begin
  Assert(IsIntegerVarType(Param.Imm.VarType));
  Result := ByteToStr(hi(Param.Imm.IntValue));
end;

function ImmWord(const Param: TILParam): String;
begin
  Result := WordToStr(Param.Imm.ToInteger);
end;

function CodeOffset(const Param: TILParam;out Comment: String): String;
var Variable: PVariable;
begin
  Variable := Param.ToVariable;
  Assert(Variable.AddrMode = amStack);
  Result := OffsetToStr(rIX, Variable);
  Comment := Variable.Name;
end;

function CodeOffsetHigh(const Param: TILParam;out Comment: String): String;
var Variable: PVariable;
begin
  Variable := Param.ToVariable;
  Assert(Variable.AddrMode = amStack);
  Result := OffsetToStr(rIX, Variable, 1);
  Comment := Variable.Name;
end;

function CodeRawOffset(const Param: TILParam;out Comment: String): String;
var Variable: PVariable;
begin
  Variable := Param.ToVariable;
  Assert(Variable.AddrMode = amStack);
  Result := '';
  if Variable.Offset < 0 then
    Result := Result + '0-';
  Result := Result + Variable.GetAsmName;
  Comment := Variable.Name + ' offset';
end;

function CodeTypeHigh8(const Param: TILParam;out Comment: String): String;
var UT: PUserType;
begin
  UT := Param.GetUserType;
  Assert(UT <> nil);
  Assert(UT.High < 255);  //Code will fail if enum has 256 elements (!) TODO
                          //(will need to use a different fragment)
  Result := ByteToStr(UT.High);
  Comment := UT.Description + ' (high)';
end;

function CodeVarName(const Param: TILParam;out Comment: String): String;
var Variable: PVariable;
begin
  Variable := Param.ToVariable;
  Assert(Variable.AddrMode = amStatic);
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
        else if CompareText(PName, 'rawoffset') = 0 then
          Sub := CodeRawOffset(Param, Comment)
        else if CompareText(PName, 'typehigh_u8') = 0 then
          Sub := CodeTypeHigh8(Param, Comment)

        ; // - Leave this here
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

function DoScopeSubs(S: String;ILItem: PILItem): String;
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

//Do before fragment substitutions
procedure PreSub(Fragment: PFragment);
begin
  if IDE.Compiler.GetConfig.CodeGen.FragmentNames then
    AsmLine('                     ;Fragment: ' + Fragment.Name);

  if Fragment.HaveMeta then
    LoadEntryLiterals(Fragment.ProcMeta);
end;

procedure PostSub(Fragment: PFragment);
begin
  if Fragment.HaveMeta then
  begin
    if Fragment.ProcMeta.HaveCorrupts then
      RegStateSetUnknowns(Fragment.ProcMeta.Corrupts);
    LibStateUpdateExitState(Fragment.ProcMeta);
  end;
end;

procedure GenFragment(Fragment: PFragment);
begin
  case Fragment.FragmentType of
    ftCode:
    begin
      PreSub(Fragment);
      AsmLines(Fragment.Code);
      PostSub(Fragment);
    end;
    ftSubroutine: Assert(False, 'TODO');
    ftProc: Assert(False, 'TODO');
  else
    Assert(False);
  end;
end;

procedure GenFragmentName(const AName: String);
var Frag: PFragment;
begin
  Frag := FindFragmentByName(AName);
  if not Assigned(Frag) then
    raise Exception.Create('Library fragment not found for: ' + AName);

  GenFragment(Frag);
end;

procedure GenFragmentItem(Fragment: PFragment;ILItem: PILItem);
var Code: String;
  Proc: TCodeGenProc;
begin
  case Fragment.FragmentType of
    ftCode:
    begin
      PreSub(Fragment);
      Code := DoParamSubs(Fragment.Code, ILItem.Param1, 'p1', False);
      Code := DoParamSubs(Code, ILItem.Param2, 'p2', False);
      Code := DoParamSubs(Code, ILItem.Dest, 'd', False);
      Code := DoScopeSubs(Code, ILItem);
      AsmLines(Code);
      PostSub(Fragment);
    end;
    ftSubroutine: Assert(False, 'TODO');
    ftProc:
    begin
      Proc := FindCodeGenProc(Fragment.Code);
      Assert(Assigned(Proc));
      Proc(ILItem);
    end;
  else
    Assert(False);
  end;
end;

procedure GenFragmentItemName(const AName: String;ILItem: PILItem);
var Frag: PFragment;
begin
  Frag := FindFragmentByName(AName);
  if not Assigned(Frag) then
    raise Exception.Create('Library fragment not found for: ' + AName);

  GenFragmentItem(Frag, ILItem);
end;

procedure GenFragmentParam(Fragment: PFragment;const Param: TILParam;const Prefix: String);
var Code: String;
begin
  case Fragment.FragmentType of
    ftCode:
    begin
      PreSub(Fragment);
      Code := DoParamSubs(Fragment.Code, Param, Prefix, True);
      AsmLines(Code);
      PostSub(Fragment);
    end;
    ftSubroutine: Assert(False, 'TODO');
    ftProc: Assert(False, 'TODO');
  else
    Assert(False);
  end;
end;

procedure GenFragmentParamName(const AName: String;Param: TILParam;const Prefix: String);
var Frag: PFragment;
begin
  Frag := FindFragmentByName(AName);
  if not Assigned(Frag) then
    raise Exception.Create('Library fragment not found for: ' + AName);

  GenFragmentParam(Frag, Param, Prefix);
end;

//=====================OTHER

//Insert code from a fragment or a library call
procedure GenLibraryProc(const ProcName: String;ILItem: PILItem);
begin
  if ProcName.Chars[0] = ':' then
    AsmInstr('call ' + ProcName.SubString(1))
  else
    GenFragmentItemName(ProcName, ILItem);
end;

end.

unit CG.Fragments;

interface
uses Def.IL, Def.Scopes,
  Z80.CPU;

  const CodeMetaLiteralMax = 2;

//Used to specify literals required on entry or set on exit
type TCodeProcLiteral = record
    Reg: TCPUReg;
    //FromReg: TCPUReg; //Exit only
    Value: Integer;
  end;
  TCodeProcLiteralArray = array[0..CodeMetaLiteralMax] of TCodeProcLiteral;

  //Where is the parameter value stored (Location)
  TCodeProcLoc = (
    plNone,       //No such paramater (or no return value)
    plRegister, //Value will be consumed from/be output in a register
    //The remaining types are used where a primitive can directly consume/write to
    //a location other than a CPU register
    plImmediate,  //The primitive can directly consume an immediate/literal, eg. LD r,n. Not valid as a result.
    plStaticVar,  //The primitive can read from/write to a static variable, eg. LD a,(nn)
    plStackVar);  //The primitive can read from/write to a stack variable , eg. INC (IX+d)

//Will be copied to the Primitive.
//Some items may be overwritten as required by the primitive, especially Result info
//(becuase some operations require different results, especially for comparisons)
type TCodeProcMeta = record
    //Register literal states which must be satisfied on entry to the procedure.
    //Eg, CF=0 specifies that the carry flag must be clear, eg before an SBC instrcution
    OnEntry: TCodeProcLiteralArray;
    //Register literal values which will be set on exit,
    //Eg, CF=0 specifies that the carry flag will be clear on exit, eg after an AND instruction
    OnExit: TCodeProcLiteralArray;

    LLoc: TCodeProcLoc;     //Where is the left parameter stored.
    LRegs: TCPURegSet;      //If LLoc is plRegister, what registers can the left parameter accept?
    RLoc: TCodeProcLoc;     //Again for right parameter
    RRegs: TCPURegSet;
    ResultLoc: TCodeProcLoc;//And for result
    ResultRegs: TCPURegSet; //What register is the result returned in? (if ResultInLReg is False)
    ResultInLReg: Boolean;  //If True the result is returned in the same register
                            //as used for LReg. If False, see DRegs

    HaveCorrupts: Boolean;  //Does Corrupts contain meaningful data?
    Corrupts: TCPURegSet;   //Registers/flags which get corrupted

    procedure Init;         //Initialise to safe defaults
  end;

type
  PFragment = ^TFragment;
  TFragment = record
    Name: String;
    Code: String;

    HaveMeta: Boolean;
    ProcMeta: TCodeProcMeta;
  end;

procedure InitialiseFragments;

function FindFragmentByName(const AName: String): PFragment;

//Generate a fragment with no substitutions
procedure GenFragment(Fragment: PFragment);
procedure GenFragmentName(const AName: String);
//Generate a fragment with substitiions from an ILItem
procedure GenFragmentItem(Fragment: PFragment;ILItem: PILItem;Scope: PScope);
procedure GenFragmentItemName(const AName: String;ILItem: PILItem;Scope: PScope);
//Generate a fragment with substitutions from an ILParam
procedure GenFragmentParam(Fragment: PFragment;const Param: TILParam;const Prefix: String);
procedure GenFragmentParamName(const AName: String;Param: TILParam;const Prefix: String);

procedure LoadFragmentsFile(Filename: String);


implementation
uses Classes, Generics.Collections, SysUtils,
  IDE.Compiler, //For meta commentary
  Def.QTypes, Def.Variables,
  Parse.Source,
  CodeGen,
  Z80.CPUState;

//=================FRAGMENTS LIST

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

//============================CODEPROCMETA


{ TCodeProcMeta }

procedure TCodeProcMeta.Init;
var I: Integer;
begin
  for I := 0 to CodeMetaLiteralMax do
  begin
    OnEntry[I].Reg := rNone;
    OnExit[I].Reg := rNone;
//    OnExit[I].FromReg := rNone;
  end;

  LLoc := plNone;
  LRegs := [];
  RLoc := plNone;
  RRegs := [];
  ResultLoc := plNone;
  ResultRegs := [];
  ResultInLReg := False;

  HaveCorrupts := False;
end;

//Load any literals required for the fragment
procedure LoadEntryLiterals(const Meta: TCodeProcMeta);
var I: Integer;
  Options: TMoveOptionSet;
begin
  //Load any required literals
  //Establish what we can corrupt - based on what the primitive will corrupt
  Options := [moPreserveA];
  if not (rCF in Meta.Corrupts) then
    Options := Options + [moPreserveCF];
  if [rZF, rFlags] * Meta.Corrupts <> [rZF, rFlags] then
    Options := Options + [moPreserveOtherFlags];

  if Meta.LLoc = plRegister then
    if rCF in Meta.LRegs then
      Options := Options + [moPreserveCF]
    else if rZF in Meta.LRegs then
      Options := Options + [moPreserveOtherFlags];

  for I := 0 to high(Meta.OnEntry) do
    if Meta.OnEntry[I].Reg = rNone then
      EXIT
    else
      GenLoadRegLiteral(Meta.OnEntry[I].Reg, TImmValue.CreateInteger(Meta.OnEntry[I].Value),
        Options);
end;

//Update CPU state with state from the ProcMeta
procedure UpdateExitState(const Meta: TCodeProcMeta);
var I: Integer;
begin
  for I := 0 to high(Meta.OnExit) do
    if Meta.OnExit[I].Reg = rNone then
      EXIT
    else
      //Literal
      RegStateSetLiteral(Meta.OnExit[I].Reg, Meta.OnExit[I].Value);
end;

//==============================SUBSTITUTIONS

function ImmByte(const Param: TILParam): String;
begin
  Result := Param.Imm.ToStringByte;
end;

function ImmHighByte(const Param: TILParam): String;
begin
  Assert(IsIntegerType(Param.Imm.VarType));
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
  Assert(Variable.Storage = vsStack);
  Result := OffsetToStr(rIX, Variable);
  Comment := Variable.Name;
end;

function CodeOffsetHigh(const Param: TILParam;out Comment: String): String;
var Variable: PVariable;
begin
  Variable := Param.ToVariable;
  Assert(Variable.Storage = vsStack);
  Result := OffsetToStr(rIX, Variable, 1);
  Comment := Variable.Name;
end;

function CodeRawOffset(const Param: TILParam;out Comment: String): String;
var Variable: PVariable;
begin
  Variable := Param.ToVariable;
  Assert(Variable.Storage = vsStack);
  Result := '';
  if Variable.Offset < 0 then
    Result := Result + '0-';
  Result := Result + Variable.GetAsmName;
  Comment := Variable.Name + ' offset';
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
        else if CompareText(PName, 'rawoffset') = 0 then
          Sub := CodeRawOffset(Param, Comment)        ;

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

//Do before fragment substitutions
procedure PreSub(Fragment: PFragment);
begin
  if IDE.Compiler.Config.CodeGen.FragmentNames then
    Line('                     ;Fragment: ' + Fragment.Name);

  if Fragment.HaveMeta then
    LoadEntryLiterals(Fragment.ProcMeta);
end;

procedure PostSub(Fragment: PFragment);
begin
  if Fragment.HaveMeta then
  begin
    if Fragment.ProcMeta.HaveCorrupts then
      RegStateSetUnknowns(Fragment.ProcMeta.Corrupts);
    UpdateExitState(Fragment.ProcMeta);
  end;
end;

procedure GenFragment(Fragment: PFragment);
begin
  PreSub(Fragment);
  Lines(Fragment.Code);
  PostSub(Fragment);
end;

procedure GenFragmentName(const AName: String);
var Frag: PFragment;
begin
  Frag := FindFragmentByName(AName);
  if not Assigned(Frag) then
    raise Exception.Create('Library fragment not found for: ' + AName);

  GenFragment(Frag);
end;

procedure GenFragmentItem(Fragment: PFragment;ILItem: PILItem;Scope: PScope);
var Code: String;
begin
  PreSub(Fragment);
  Code := DoParamSubs(Fragment.Code, ILItem.Param1, 'p1', False);
  Code := DoParamSubs(Code, ILItem.Param2, 'p2', False);
  Code := DoParamSubs(Code, ILItem.Dest, 'd', False);
  Code := DoScopeSubs(Code, ILItem, Scope);
  Lines(Code);
  PostSub(Fragment);
end;

procedure GenFragmentItemName(const AName: String;ILItem: PILItem;Scope: PScope);
var Frag: PFragment;
begin
  Frag := FindFragmentByName(AName);
  if not Assigned(Frag) then
    raise Exception.Create('Library fragment not found for: ' + AName);

  GenFragmentItem(Frag, ILItem, Scope);
end;

procedure GenFragmentParam(Fragment: PFragment;const Param: TILParam;const Prefix: String);
var Code: String;
begin
  PreSub(Fragment);
  Code := DoParamSubs(Fragment.Code, Param, Prefix, True);
  Lines(Code);
  PostSub(Fragment);
end;

procedure GenFragmentParamName(const AName: String;Param: TILParam;const Prefix: String);
var Frag: PFragment;
begin
  Frag := FindFragmentByName(AName);
  if not Assigned(Frag) then
    raise Exception.Create('Library fragment not found for: ' + AName);

  GenFragmentParam(Frag, Param, Prefix);
end;

//===========================LOAD FRAGMENTS FILE

function AddLiteral(var Data: TCodeProcLiteralArray;Reg: TCPUReg;Value: Integer): String;
var I: Integer;
begin
  for I := 0 to CodeMetaLiteralMax do
    if Data[I].Reg = Reg then
      EXIT('Register or flag redeclared')
    else if Data[I].Reg = rNone then
    begin
      Data[I].Reg := Reg;
      Data[I].Value := Value;
      EXIT('');
    end;

  Result := 'Too many Register literals declared';
end;

function ParseLiteral(Parser: TGenericReader;var Data: TCodeProcLiteralArray): String;
var RegStr: String;
  Reg: TCPUReg;
  Value: String;
  ValueInt: Integer;
  Error: String;
begin
  RegStr := Parser.ReadIdentifier;
  Reg := IdentToCPUReg(RegStr);
  if Reg = rNone then
    EXIT('Invalid register or flag');
  Parser.SkipWhitespace;
  if not CharInSet(Parser.TestChar, ['=',':']) then
    EXIT('''='' or '':'' expected');
  Parser.SkipChar;
  Value := Parser.ReadNumber;
  if not TryStrToInt(Value, ValueInt) then
    EXIT('Invalid decimal or hex literal');
  Result := AddLiteral(Data, Reg, ValueInt);
end;


function ParseLiterals(Parser: TGenericReader;var Data: TCodeProcLiteralArray): String;
begin
  while True do
  begin
    if not CharInSet(Parser.TestChar, ['a'..'z','A'..'Z']) then
      EXIT('');
    Result := ParseLiteral(Parser, Data);
    if Result <> '' then
      EXIT;
    Parser.SkipWhiteSpace;
    if Parser.TestChar = ',' then
    begin
      Parser.SkipChar;
      Parser.SkipWhiteSpace
    end;
  end;
end;

//Returns empty set if there is an error
function ParseRegList(Parser: TGenericReader;const Ident: String): TCPURegSet;
var S: String;
  Reg: TCPUReg;
begin
  if Ident = '' then
  begin
    S := Parser.ReadIdentifier;
    Parser.SkipWhiteSpace;
  end
  else
    S := Ident;

  Result := [];
  if CompareText(Ident, 'none') = 0 then
    EXIT;

  repeat
    Reg := StrToCPURegAll(S, True);
    if Reg = rNone then
      EXIT([]);

    Result := Result + [StrToCPURegAll(S, True)];
    if Parser.TestChar = ',' then
    begin
      Parser.SkipChar;
      Parser.SkipwhiteSpace;
    end;
    S := Parser.ReadIdentifier;
    Parser.SkipWhiteSpace;
  until S = '';
end;

function ParseParam(Parser: TGenericReader;out Loc: TCodeProcLoc;out Regs: TCPURegSet): Boolean;
var S: String;
begin
  Regs := [];
  S := Parser.ReadIdentifier;
  if S = '' then
    EXIT(False);
  Parser.SkipWhiteSpace;

  if CompareText(S, 'none') = 0 then
  begin
    Loc := plNone;
    Regs := [];
  end
  else if CompareText(S, 'imm') = 0 then
    Loc := plImmediate
  else if CompareText(S, 'static') = 0 then
    Loc := plStaticVar
  else if CompareText(S, 'stack') = 0 then
    Loc := plStackVar
  else
  begin
    Loc := plRegister;
    Regs := ParseRegList(Parser, S);
    if Regs = [] then
      EXIT(False);
  end;

  Result := True;
end;

function ParseDest(Parser: TGenericReader;var Meta: TCodeProcMeta): Boolean;
var S: String;
  Regs: TCPURegSet;
begin
  Regs := [];
  S := Parser.ReadIdentifier;
  if S = '' then
    EXIT(False);
  Parser.SkipWhiteSpace;

  if CompareText(S, 'p1') = 0 then
  begin
    Meta.ResultInLReg := True;
    Meta.ResultLoc := plRegister;
    Meta.ResultRegs := [];
  end
  else if CompareText(S, 'static') = 0 then
    Meta.ResultLoc := plStaticVar
  else if CompareText(S, 'stack') = 0 then
    Meta.ResultLoc := plStackVar
  else
  begin
    Meta.ResultLoc := plRegister;
    Meta.ResultRegs := ParseRegList(Parser, S);
    if Meta.ResultRegs = [] then
      EXIT(False);
  end;

  Result := True;
end;

function ParseCorrupts(Parser: TGenericReader;out Regs: TCPURegSet): Boolean;
var S: String;
begin
  Regs := ParseRegList(Parser, '');
  if Regs = [] then
    EXIT(False);
  Result := True;
end;

procedure ParseMetaCommand(Parser: TGenericReader;var Meta: TCodeProcMeta);
var At: Integer;
  Command: String;
  Error: String;
begin
  Assert(Parser.TestChar = '-');
  Parser.SkipChar;
  Command := Parser.ReadIdentifier;
  Parser.SkipWhiteSpace;

  if CompareText(Command, 'p1') = 0 then
  begin
    if not ParseParam(Parser, Meta.LLoc, Meta.LRegs) then
      raise Exception.Create('Syntax error in -p1 settings at ' +
        Parser.CursorToString + ' in'#13 + Parser.Line);
  end
  else if CompareText(Command, 'p2') = 0 then
  begin
    if not ParseParam(Parser, Meta.RLoc, Meta.RRegs) then
      raise Exception.Create('Syntax error in -p2 settings at ' +
        Parser.CursorToString + ' in'#13 + Parser.Line);
  end
  else if CompareText(Command, 'd') = 0 then
  begin
    if not ParseDest(Parser, Meta) then
      raise Exception.Create('Syntax error in -d settings at ' +
        Parser.CursorToString + ' in'#13 + Parser.Line);
  end
  //TODO: DestInLLoc
  else if CompareText(Command, 'corrupts') = 0 then
  begin
    if not ParseCorrupts(Parser, Meta.Corrupts) then
      raise Exception.Create('Syntax error in -corrupts settings. ' +
        'Please check list of corrupable registers. ' +
        Parser.CursorToString + ' in'#13 + Parser.Line);
    Meta.HaveCorrupts := True;
  end
  else if CompareText(Command, 'entry') = 0 then
  begin
    Error := ParseLiterals(Parser, Meta.OnEntry);
    if Error <> '' then
      raise Exception.Create(Error + ' at ' +
        Parser.CursorToString + ' in'#13 + Parser.Line);
  end
     //....
  else if CompareText(Command, 'exit') = 0 then
  begin
    Error := ParseLiterals(Parser, Meta.OnExit);
    if Error <> '' then
      raise Exception.Create(Error + ' at ' +
        Parser.CursorToString + ' in'#13 + Parser.Line);
  end
  else if CompareText(Command, 'preservesall') = 0 then
  begin
    Meta.Corrupts := [];
    Meta.HaveCorrupts := True;
  end

  else
    raise Exception.Create('Unknown Fragment command: "' + Command + '" at ' +
      Parser.CursorToString + ' in'#13 + Parser.Line);
end;

procedure LoadFragmentsFile(Filename: String);
var Parser: TGenericReader;
  Entry: PFragment;
begin
  try
    Entry := nil;
    Parser := TGenericReader.Create;
    Parser.OpenFile(Filename);

    while not Parser.EOF do
    begin
      if not Parser.SkipWhiteSpace then
      begin
        case Parser.TestChar of
          ';',#0: Parser.NextLine; //Comment or empty line - ignore line
          '=':  //Start of a fragment
          begin
            Parser.SkipChar;
            Entry := New(PFragment);
            Entry.Name := Parser.ReadIdentifier;
            if Assigned(FindFragmentByName(Entry.Name)) then
              raise Exception.Create('LoadFragments: Entry redeclared: ' + Entry.Name +
                ' at ' + Parser.CursorToString);
            Entry.Code := '';
            Entry.HaveMeta := False;
            Entry.ProcMeta.Init;
            FragList.Add(Entry);
            Parser.SkipWhiteSpace;
          end;
          '-':  //Command/meta data
          begin
            if (Entry = nil) or (Entry.Code <> '') then
            raise Exception.Create('Command or meta data found outside of a library Entry at: ' +
              Parser.CursorToString + ' in'#13 +
              Parser.Line);
            ParseMetaCommand(Parser, Entry.ProcMeta);
            Entry.HaveMeta := True;
          end
        else  //Otherwise treat the line as code
          if Entry = nil then
            raise Exception.Create('Code library data found but not in library Entry at: ' +
              Parser.CursorToString + ' in'#13 +
              Parser.Line)
          else
          begin
            if Entry.Code <> '' then
              Entry.Code := Entry.Code + #13#10;
            Entry.Code := Entry.Code + '  ' + Parser.ReadLine;
          end;
        end;
      end;
      if Parser.EOLN then
        Parser.NextLine;
    end;
  finally
    Parser.Free;
  end;
end;

initialization
  FragList := TList<PFragment>.Create;
end.

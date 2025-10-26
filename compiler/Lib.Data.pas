unit Lib.Data;

interface
uses Def.Operators, Def.VarTypes,
  Z80.Hardware;


//========================CODEPROCMETA

const CodeMetaLiteralMax = 2;

//Used to specify literals required on entry or set on exit
type TCodeProcLiteral = record
    Reg: TCPUReg;
    //FromReg: TCPUReg; //Exit only
    Value: Integer;
  end;
  TCodeProcLiteralArray = array[0..CodeMetaLiteralMax] of TCodeProcLiteral;

  TCosts = record
    Bytes: Integer;         //Bytes of generated code
    Cycles: Integer;        //Number of execution cycles

    //Set all values to -1
    procedure Init;
    //Set all values to 0
    procedure Zero;
    //Add data from Costs to self
    procedure Add(const Costs: TCosts);

    function ToString: String;
  end;

//Will be copied to the Primitive.
//Some items may be overwritten as required by the primitive, especially Result info
//(because some operations require different results, especially for comparisons)
type TCodeProcMeta = record
  //Param data
    LLoc: TCodeProcLoc;     //Where is the left parameter stored.
    LRegs: TCPURegSet;      //If LLoc is plRegister, what registers can the left parameter accept?
    RLoc: TCodeProcLoc;     //Again for right parameter
    RRegs: TCPURegSet;
    ResultLoc: TCodeProcLoc;//And for result
    ResultRegs: TCPURegSet; //What register is the result returned in? (if ResultInLReg is False)
    ResultInLReg: Boolean;  //If True the result is returned in the same register
                            //as used for LReg. If False, see DRegs

  //CPU State data
    //Register literal states which must be satisfied on entry to the procedure.
    //Eg, CF=0 specifies that the carry flag must be clear, eg before an SBC instrcution
    OnEntry: TCodeProcLiteralArray;
    //Register literal values which will be set on exit,
    //Eg, CF=0 specifies that the carry flag will be clear on exit, eg after an AND instruction
    OnExit: TCodeProcLiteralArray;

    HaveCorrupts: Boolean;  //Does Corrupts contain meaningful data?
    Corrupts: TCPURegSet;   //Registers/flags which get corrupted
    StateProc: String;      //An optional (compiler) procedure to call to update the CPU State
                            //This is used where the routine affects more than just corrupting
                            //registers or setting them to fixed values

  //Meta data
    Costs: TCosts;

    procedure Init;         //Initialise to safe defaults
  end;

//=================FRAGMENTS

type
  TFragmentType = (
    ftCode,       //Source code to be inserted 'inline'
    ftSubroutine, //A library subroutine to be called
    ftProc        //A compiler function to be called
    );

type
  PFragment = ^TFragment;
  TFragment = record
    Name: String;

    FragmentType: TFragmentType;
    Code: String;   //Code template (ftCode), subroutine name or proc name

    HaveMeta: Boolean;
    ProcMeta: TCodeProcMeta;
  end;

procedure InitialiseFragments;

function FindFragmentByName(const AName: String): PFragment;

procedure LoadFragmentsFile(Filename: String);

//Add a fragment for a GenProc. The Fragment must have the same name as the GenProc
procedure AddGenProcFragment(const AName, Meta: String);

//==========================FRAGMENTS

type
  TPrimValidation = (pvYes, pvNo, pvEither);
  TValidationProc = procedure(RH, RL: Char);

  TLoadParamType = (
    lptNormal,
    lptHigh,  //Load high byte of param, not range checked
    lptLow);  //Load low byte of param, not range checked

type
  PPrimitive = ^TPrimitive;
  TPrimitive = record
    //Fields to use for primitive selection
    Op: TOperator;
    Fragment: PFragment;  //If the item is a fragment
    ProcName: String;     //Name of the Proc (code), Fragment, or Subroutine used
                          //during code generation
    LType: TVarType;      //Left operand type (base type)
    RType: TVarType;      //Right operand type (base type)
    Commutative: Boolean; //If True the left and right operators can be swapped
    ResultType: TVarType; //Destination (Result) type, unless...
    ResultTypeIsLType: Boolean;
    ResultTypeIsRType: Boolean;
    IfOverflowChecking: TPrimValidation;  //Can this routine be used if overflow checking is on? off? either?

    //Fields for primitive use
    LLoadType: TLoadParamType;  //Special load type for param1 only

    ProcMeta: TCodeProcMeta;  //Data about the code generation proc

    Preserves: TCPURegSet; //What registers are preserved by this routine
                          //(Should this include Result register?)

    OverflowCheckProcName: String; //Used when we need to overflow check the result value
                          //If empty a generic overflow checking routine will be used
                          //Set this value if there is an alternate routine which
                          //needs to be used.
                          //Examples:
                          //A generic routine for signed values might check the
                          //overflow flag. But if, for example, a subroutine returns
                          //the overflow condition in the carry flag a routine to
                          //test the carry flag will need to be specified here.
    RangeCheckToS8: String; //As with OverflowCheckProcName, generic routines are
                          //used to range check type conversions. If a more specific
                          //or optimised routine is available it can be specified
                          //here. For example a generic routine to range check a
                          //signed to unsigned type conversion will need to do an
                          //operation to test the sign flag. If the primitive already
                          //sets that flag an optimised routine can skip the test
                          //operation and just do the branch if error.
    RangeCheckToU8: String;
    RangeCheckToS16: String;
    RangeCheckToU16: String;

    //Possibly add data regarding code bytesize and execution cycles.
    ValidateProc: TValidationProc;  //Proc to generate validation code (if generated in code)
  end;

implementation
uses Generics.Collections, Classes, SysUtils,
  Parse.Source,
  Z80.GenProcs;

//=================CODEPROCMETA

{ TCosts }

procedure TCosts.Add(const Costs: TCosts);
begin
  if (Bytes >= 0) and (Cycles >= 0) and (Costs.Bytes >= 0) and (Costs.Cycles >= 0) then
  begin
    Bytes := Bytes + Costs.Bytes;
    Cycles := Cycles + Costs.Cycles;
  end
  else
    Init;
end;

procedure TCosts.Init;
begin
  Bytes := -1;
  Cycles := -1;
end;

function TCosts.ToString: String;
begin
  Result := 'B-'+Bytes.ToString + ' C-'+Cycles.ToString;
end;

//============================CODEPROCMETA

procedure TCosts.Zero;
begin
  Bytes := 0;
  Cycles := 0;
end;

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
  StateProc := '';

  Costs.Init;
end;


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
begin
  RegStr := Parser.ReadIdentifier;
  Reg := IdentToCPUReg(RegStr);
  if Reg = rNone then
    EXIT('Invalid register or flag');
  Parser.SkipWhiteChars;
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
    Parser.SkipWhiteChars;
    if Parser.TestChar = ',' then
    begin
      Parser.SkipChar;
      Parser.SkipWhiteChars
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
    Parser.SkipWhiteChars;
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

    Result := Result + [Reg];
    if Parser.TestChar = ',' then
    begin
      Parser.SkipChar;
      Parser.SkipwhiteChars;
    end;
    S := Parser.ReadIdentifier;
    Parser.SkipWhiteChars;
  until S = '';
end;

function ParseParam(Parser: TGenericReader;out Loc: TCodeProcLoc;out Regs: TCPURegSet): Boolean;
var S: String;
begin
  Regs := [];
  S := Parser.ReadIdentifier;
  if S = '' then
    EXIT(False);
  Parser.SkipWhiteChars;

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
  Parser.SkipWhiteChars;

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
begin
  Regs := ParseRegList(Parser, '');
  if Regs = [] then
    EXIT(False);
  Result := True;
end;

procedure ParseMetaCommand(Parser: TGenericReader;var Frag: PFragment);
var Command: String;
  Error: String;
  StateProc: TStateProc;
begin
  Assert(Parser.TestChar = '-');
  Parser.SkipChar;
  Command := Parser.ReadIdentifier;
  Parser.SkipWhiteChars;

  if CompareText(Command, 'p1') = 0 then
  begin
    if not ParseParam(Parser, Frag.ProcMeta.LLoc, Frag.ProcMeta.LRegs) then
      raise Exception.Create('Syntax error in -p1 settings at ' +
        Parser.CursorToString + ' in'#13 + Parser.Line);
  end
  else if CompareText(Command, 'p2') = 0 then
  begin
    if not ParseParam(Parser, Frag.ProcMeta.RLoc, Frag.ProcMeta.RRegs) then
      raise Exception.Create('Syntax error in -p2 settings at ' +
        Parser.CursorToString + ' in'#13 + Parser.Line);
  end
  else if CompareText(Command, 'd') = 0 then
  begin
    if not ParseDest(Parser, Frag.ProcMeta) then
      raise Exception.Create('Syntax error in -d settings at ' +
        Parser.CursorToString + ' in'#13 + Parser.Line);
  end
  //TODO: DestInLLoc
  else if CompareText(Command, 'corrupts') = 0 then
  begin
    if not ParseCorrupts(Parser, Frag.ProcMeta.Corrupts) then
      raise Exception.Create('Syntax error in -corrupts settings. ' +
        'Please check list of corrupable registers. ' +
        Parser.CursorToString + ' in'#13 + Parser.Line);
    Frag.ProcMeta.HaveCorrupts := True;
  end
  else if CompareText(Command, 'entry') = 0 then
  begin
    Error := ParseLiterals(Parser, Frag.ProcMeta.OnEntry);
    if Error <> '' then
      raise Exception.Create(Error + ' at ' +
        Parser.CursorToString + ' in'#13 + Parser.Line);
  end
     //....
  else if CompareText(Command, 'exit') = 0 then
  begin
    Error := ParseLiterals(Parser, Frag.ProcMeta.OnExit);
    if Error <> '' then
      raise Exception.Create(Error + ' at ' +
        Parser.CursorToString + ' in'#13 + Parser.Line);
  end
  else if CompareText(Command, 'preservesall') = 0 then
  begin
    Frag.ProcMeta.Corrupts := [];
    Frag.ProcMeta.HaveCorrupts := True;
  end
  else if CompareText(Command, 'stateproc') = 0 then
    //If we're procedurally updating CPU state
    try
      StateProc := nil;
      Frag.ProcMeta.StateProc := Frag.Name;
      StateProc := FindStateproc(Frag.Name);
      if not Frag.ProcMeta.HaveCorrupts then
        Frag.ProcMeta.Corrupts := [];
      Frag.procMeta.HaveCorrupts := True;
    except
      on E:Exception do
      //FindStateProc will raise it's own exception
      if not Assigned(StateProc) then
        raise Exception.Create(E.Message + ' at ' +
          Parser.CursorToString + ' in'#13 + Parser.Line);
    end

  else if CompareText(Command, 'bytes') = 0 then
  begin
    if not TryStrToInt(Parser.ReadNumber, Frag.ProcMeta.Costs.Bytes) then
      raise Exception.Create('Invalid number for -bytes at ' +
        Parser.CursorToString + ' in'#13 + Parser.Line);
  end
  else if CompareText(Command, 'cycles') = 0 then
  begin
    if not TryStrToInt(Parser.ReadNumber, Frag.ProcMeta.Costs.Cycles) then
      raise Exception.Create('Invalid number for -cycles at ' +
        Parser.CursorToString + ' in'#13 + Parser.Line);
  end

  else
    raise Exception.Create('Unknown Fragment command: "' + Command + '" at ' +
      Parser.CursorToString + ' in'#13 + Parser.Line);
end;

procedure LoadFragmentsFile(Filename: String);
var Parser: TGenericReader;
  Entry: PFragment;
begin
  Parser := TGenericReader.Create;
  try
    Entry := nil;
    Parser.OpenFile(Filename);

    while not Parser.EOF do
    begin
      if not Parser.SkipWhiteChars then
      begin
        case Parser.TestChar of
          ';',#0: Parser.NextLine; //Comment or empty line - ignore line
          '=':  //Start of a fragment
          begin
            Parser.SkipChar;
            Entry := New(PFragment);
            Entry.FragmentType := ftCode;

            Entry.Name := Parser.ReadIdentifier;
            if Assigned(FindFragmentByName(Entry.Name)) then
              raise Exception.Create('LoadFragments: Entry redeclared: ' + Entry.Name +
                ' at ' + Parser.CursorToString);
            FragList.Add(Entry);

            Entry.Code := '';
            Entry.HaveMeta := False;
            Entry.ProcMeta.Init;
            Parser.SkipWhiteChars;
          end;
          '-':  //Command/meta data
          begin
            if (Entry = nil) or (Entry.Code <> '') then
            raise Exception.Create('Command or meta data found outside of a library Entry at: ' +
              Parser.CursorToString + ' in'#13 +
              Parser.Line);
            ParseMetaCommand(Parser, Entry);
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

procedure AddGenProcFragment(const AName, Meta: String);
var Entry: PFragment;
  Parser: TGenericReader;
begin
  Assert(not Assigned(FindFragmentByName(AName)), 'AddGenProcFragment: Entry redeclared: ' + AName);
  Entry := New(PFragment);
  FragList.Add(Entry);
  Entry.Name := AName;
  Entry.Code := AName;
  Entry.FragmentType := ftProc;
  Entry.HaveMeta := False;
  Entry.ProcMeta.Init;

  //Parse meta data here
  Parser := TGenericReader.Create;
  try
    Parser.LoadFromString(Meta);
    Parser.SkipWhiteChars;
    while not Parser.EOF do
      if Parser.TestChar = '-' then
      begin
        ParseMetaCommand(Parser, Entry);
        Entry.HaveMeta := True;
        Parser.SkipWhiteNL;
      end
      else
        Assert(False, 'Invalid GenProc Meta string: ''' + Meta + '''');
  finally
    Parser.Free;
  end;
end;

initialization
  FragList := TList<PFragment>.Create;
end.

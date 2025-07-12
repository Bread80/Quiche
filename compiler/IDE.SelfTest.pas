unit IDE.SelfTest;

interface
uses SysUtils, Generics.Collections,
  Parse,
  Def.Globals, Def.QTypes, Def.Variables,
  CodeGen,
  IDE.Compiler;

type
  PTestOptions = ^TTestOptions;
  TTestOptions = record
    //blocktype static|stack      - Select Stack to generate a stack frame and use local vars
    BlockType: TBlockType;
    //parsetype declarations|code
    ParseMode: TParseMode;
    //$overflow on|off            - enable or disable overflow checking
    OverFlowCheck: Boolean;
    //$range on|off               - enable or disable range checking
    RangeCheck: Boolean;
    //logprimitives on|off
    LogPrimitives: Boolean;

    function ToString: String;
  end;

type
  TTestStatus = (tsNotRun, tsPassed, tsFailed, tsError);
type
  TTestAction = (taVarType, taCompileError, taUsesPrimitive, taVarValue, taRuntimeError);

  PTestStep = ^TTestStep;
  TTestStep = record
    Name: String; //Data held here depends on the Action
    Value: String;
    Report: String;
    Status: TTestStatus;

    //Extra data specific to each action type
    case Action: TTestAction of
      //-----Compilation
      //vartype <varname> <vartype> - variable type
      taVarType: (
        //compile noerror|error [<errorname>] - compile error
        //Name = Variable name
        //Type = Variable type
        );
      taCompileError: (
          //Name field contains error type
          //TODO: Add error identifier field
        );

      //-----Code generation
      //usesprimitive <primitive-name>
      taUsesPrimitive: (
        //Name field is Primitive name
        );

      //-----Execution
      //varvalue <varname> <value>  - variable value
      taVarValue: (
        //Name is variable name
        //Value is value
        //{VarValue: TImmValue;}
      );
      //runtime noerror|overflow|dividebyzero|range   - runtime error
      taRuntimeError: (
        //Name field contains required error
//        RunTimeError: Integer
        );
  end;



type TTestable = class
  private
    function GetName: String;virtual;abstract;
    function GetIsEnabled: Boolean;virtual;abstract;
    procedure SetIsEnabled(Value: Boolean);virtual;abstract;
    function GetStatus: TTestStatus;virtual;abstract;
    function GetDetails: String;virtual;
    function GetReport: String;virtual;
  public
    procedure Run;virtual;abstract;
    property Name: String read GetName;
    property IsEnabled: Boolean read GetIsEnabled write SetIsEnabled;
    property Status: TTestStatus read GetStatus;

    //Returns the test details in text form
    property Details: String read GetDetails;
    //Returns the result of running the test(s) (if they have been run!)
    property Report: String read GetReport;
  end;

  TTest = class(TTestable)
  private
    FName: String;

    //The tests to run
    FOptions: TTestOptions;
    FCode: String;
    //If True a compile error won't be treated as a fail.
    FWantCompileError: Boolean;
    //Ditto for runtime errors
    FWantRuntimeError: Boolean;
    FSteps: TArray<TTestStep>;

    //Meta-data
    FIsEnabled: Boolean; //If true this test is to be run.

    FCompileOkay: Boolean;

    FParseErrorNo: Integer;
    FParseErrorHelp: String;
    FParseErrorPos: Integer;
    FParseErrorLine: Integer;
    FParseErrorString: String;

    FAssembleError: Boolean;
    FAssemblerLog: String;

    FEmulateError: Boolean;

    FRunTimeErrorAddress: Word;
    FWriteBuffer: String;
    FRunTimeError: Byte;
    FTestLog: String;
    //If we had an unwanted compile or runtime error
    FErrorReport: String;
    FStatus: TTestStatus;
  protected
    //Inherited methods
    function GetName: String;override;
    function GetIsEnabled: Boolean;override;
    procedure SetIsEnabled(Value: Boolean);override;
    function GetStatus: TTestStatus;override;
    function GetDetails: String;override;
    function GetReport: String;override;

    //Adding test steps
    function AddStep: PTestStep;

    //Compile and run code
    procedure ClearTestStatuses;
    procedure WarmInitCompiler;
    function Compile: Boolean;
    function Emulate: Boolean;

    //Test for errors
    //Set error status with given message
    procedure Error(Step: PTestStep;const Msg: String);
    procedure Fail(Step: PTestStep;const Msg: String);
    procedure Pass(Step: PTestStep);
    procedure Check(Passed: Boolean;Step: PTestStep;const Msg: String);
    procedure TestVarType(Step: PTestStep);
    //Verify that we get a compile error when we expect to
    procedure TestCompileError(Step: PTestStep);
    procedure TestRunTimeError(Step: PTestStep);
    procedure TestUsesPrimitive(Step: PTestStep);
    procedure TestVarValue(Step: PTestStep);
    procedure RunSteps;

  public
    //Returns True if the test wants a compile error, False if the Compile parameter is NoError
    function AddCompileError(Fields: TArray<String>): String;
    function AddVarType(Fields: TArray<String>): String;
    function AddUsesPrimitive(Fields: TArray<String>): String;
    function AddVarValue(Fields: TArray<String>): String;
    function AddRuntimeError(Fields: TArray<String>): String;

    procedure Run;override;
    property Name: String read FName write FName;
    property Code: String read FCode write FCode;
    property CompileOkay: Boolean read FCompileOkay;

    //Parser
    property ParseErrorNo: Integer read FParseErrorNo;
    property ParseErrorLine: Integer read FParseErrorLine;
    property ParseErrorPos: Integer read FParseErrorPos;
    property ParseErrorString: String read FParseErrorString;
    property ParseErrorHelp: String read FParseErrorHelp;

    //Output log of the assembler
    property AssemblerLog: String read FAssemblerLog;
    property AssembleError: Boolean read FAssembleError;

    //Emulator
    property WriteBuffer: String read FWriteBuffer;    //Text written to output
    property RunTimeError: Byte read FRunTimeError;
    property RunTimeErrorAddress: Word read FRunTimeErrorAddress;

    property TestLog: String read FTestLog;
  end;


type TTestGroup = class(TTestable)
  private
    FTests: TList<TTest>;
    FName: String;
    function GetCount: Integer;
    function GetTests(Index: Integer): TTest;
  protected
    //Inherited methods
    function GetName: String;override;
    function GetIsEnabled: Boolean;override;
    procedure SetIsEnabled(Value: Boolean);override;
    //True is one or more of  tests has Enabled = True
    //False only if all tests have Enabled = False
    function GetStatus: TTestStatus;override;
    function GetReport: String;override;

    procedure ColdInitCompiler;
    procedure InitOptions(var Options: TTestOptions);
  public
    constructor Create;
    destructor Destroy;override;

    procedure Clear;
    //Create a new, empty test with the given options and adds it to the list of tests
    function CreateTest(const Options: TTestOptions): TTest;

    //Run any enabled tests
    procedure Run;override;

    property Count: Integer read GetCount;
    property Tests[Index: Integer]: TTest read GetTests;default;
  end;

type TTestFile = class(TTestGroup)
  private
    FLastError: String;
  protected
    //Test options
    function ParseBlockType(Fields: TArray<String>): TBlockType;
    function ParseParseMode(Fields: TArray<String>): TParseMode;
    function ParseBoolean(Fields: TArray<String>): Boolean;
  public
    procedure LoadTestFile(const Filename: String);
  end;

//Abstract type for generated tests
type TGeneratedTests = class(TTestGroup)
  protected
    function GetName: String;override;

    function TypecastInteger(Value: Integer;ToType: TVarType): Integer;
  public
    procedure Generate;virtual;abstract;
  end;

//Test the register mode parameter loading (ie. for register mode function call)
type TTestLoadParams = class(TGeneratedTests)
  private
  public
    procedure Generate;override;
  end;

//Test the register mode parameter loading (ie. for register mode function call)
type TTestStoreResults = class(TGeneratedTests)
  private
  public
    procedure Generate;override;
  end;

type
  TTestGroups = TList<TTestGroup>;

var TestGroups: TTestGroups;

procedure LoadSelfTests;

implementation
uses Classes, IOUtils,
  Def.Consts, Def.UserTypes,
  Parse.Errors, Parse.Expr;

const TestActionStrings: array[low(TTestAction)..high(TTestAction)] of String = (
  'VarType', 'Compile', 'UsesPrimitive', 'VarValue', 'Runtime');

const StatusStrings: array[low(TTestStatus)..high(TTestStatus)] of String = (
  'Test not run',
  'Passed :-)',
  'Failed :-(',
  'ERROR');

//Parse errors
const
  errExpectedTwoFields = 'Expected two fields';
  errExpectedThreeFields = 'Expected three fields';
//  errExpectedTwoOrThreeFields = 'Expected two or three fields';
  errInvalidField = 'Invalid field: ';
  errUnknownCompileError = 'Unknown compile error: ';

  { TTestOptions }

function TTestOptions.ToString: String;
const BoolToStr: array[False..True] of String = ('Off','On');
begin
  Result := 'Block Type: ' + BlockTypeStrings[BlockType] + #13 +
    'Parse Mode: ' + ParseModeStrings[ParseMode] + #13 +
    'OverFlow Checks: ' + BoolToStr[OverflowCheck] + #13 +
    'Range Checks: ' + BoolToStr[RangeCheck] + #13 +
    'Log Primitives: ' + BoolToStr[LogPrimitives];
end;

{ TTestable }

function TTestable.GetDetails: String;
begin
  Result := '';
end;

function TTestable.GetReport: String;
begin
  Result := 'Status: ' + StatusStrings[Status] + #13;
end;

{ TTest }

function TTest.AddCompileError(Fields: TArray<String>): String;
var Step: PTestStep;
begin
  if Length(Fields) <> 2 then
    EXIT(errExpectedTwoFields);

  Result := '';
  Step := AddStep;
  Step.Action := taCompileError;
  Step.Name := Fields[1];
  FWantCompileError := CompareText(Step.Name, 'noerror') <> 0;

(*  if FWantCompileError then
    if not IsValidCompileError(Step.Name) then
      EXIT(errUnknownCompileError + Step.Name);
*)end;

function TTest.AddRuntimeError(Fields: TArray<String>): String;
var Step: PTestStep;
begin
  if Length(Fields) <> 2 then
    EXIT(errExpectedTwoFields);

  Result := '';
  Step := AddStep;
  Step.Action := taRuntimeError;
  Step.Name := Fields[1];

  FWantRuntimeError := CompareText(Step.Name, 'noerror') <> 0;

  //TODO: Validate error identifier
  //TODO: Decode error identifier
end;

function TTest.AddStep: PTestStep;
begin
  SetLength(FSteps, Length(FSteps)+1);
  Result := @FSteps[Length(FSteps)-1];
  Result.Status := tsNotRun;
end;

function TTest.AddUsesPrimitive(Fields: TArray<String>): String;
var Step: PTestStep;
begin
  if Length(Fields) <> 2 then
    EXIT(errExpectedTwoFields);

  Result := '';
  Step := AddStep;
  Step.Action := taUsesPrimitive;
  Step.Name := Fields[1];
end;

function TTest.AddVarType(Fields: TArray<String>): String;
var Step: PTestStep;
begin
  if Length(Fields) <> 3 then
    EXIT(errExpectedThreeFields);

  Result := '';
  Step := AddStep;
  Step.Action := taVarType;
  Step.Name := Fields[1];
  Step.Value := Fields[2];
end;

function TTest.AddVarValue(Fields: TArray<String>): String;
var Step: PTestStep;
begin
  if Length(Fields) <> 3 then
    EXIT(errExpectedThreeFields);

  Result := '';
  Step := AddStep;
  Step.Action := taVarValue;
  Step.Name := Fields[1];
  Step.Value := Fields[2];
  //TODO: Decode the value field
end;

procedure TTest.Check(Passed: Boolean; Step: PTestStep;const Msg: String);
begin
  if Passed then
    Pass(Step)
  else
    Fail(Step, Msg);
end;

procedure TTest.ClearTestStatuses;
var  I: Integer;
begin
  for I := 0 to Length(FSteps)-1 do
  begin
    FSteps[I].Status := tsNotRun;
    FSTeps[I].Report := '';
  end;
end;

function TTest.Compile: Boolean;
var SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := FCode;
    FCompileOkay := IDE.Compiler.CompileStrings(SL, FOptions.BlockType, FOptions.ParseMode, False, True);
  finally
    SL.Free;
  end;

  Result := FCompileOkay;

  FParseErrorNo := IDE.Compiler.ParseErrorNo;
  FParseErrorHelp := IDE.Compiler.ParseErrorHelp;
  FParseErrorPos := IDE.Compiler.ParseErrorPos;
  FParseErrorLine := IDE.Compiler.ParseErrorLine;
  FParseErrorString := IDE.Compiler.ParseErrorString;
  FAssemblerLog := IDE.Compiler.AssemblerLog;
end;

function TTest.Emulate: Boolean;
begin
  FWRiteBuffer := '';
  FRuntimeError := 0;

  Result := IDE.Compiler.Deploy(IDE.Compiler.GetBinaryFileName);
  FEmulateError := not Result;
  if FEmulateError then
    FErrorReport := 'ERROR: Unexpected runtime error '#13
  else
  begin
    FWriteBuffer := IDE.Compiler.WriteBuffer;
    FRunTimeError := IDE.Compiler.RunTimeError;
  end;
end;

procedure TTest.Error(Step: PTestStep;const Msg: String);
begin
  Step.Status := tsError;
  Step.Report := Msg;
  FTestLog := FTestLog + 'ERROR: ' + Msg + #13;
  FStatus := tsError;
end;

procedure TTest.Fail(Step: PTestStep;const Msg: String);
begin
  Step.Status := tsFailed;
  Step.Report := Msg;
  if FStatus <> tsError then
  begin
    FStatus := tsFailed;
    FTestLog := FTestLog + 'FAIL: ' + Msg + #13;
  end;
end;

function TTest.GetDetails: String;
var I: Integer;
  Step: PTestStep;
begin
  Result := inherited +
    FOptions.ToString + #13#13 +
    'Code'#13 +
      FCode +
    'EndCode'#13#13;

  for I := 0 to Length(FSteps)-1 do
  begin
    Step := @FSteps[I];
    Result := Result + TestActionStrings[Step.Action];
    case Step.Action of
      taVarType, taVarValue:
        Result := Result + ' ' + Step.Name + ' ' + Step.Value + #13;
      taCompileError, taUsesPrimitive, taRuntimeError:
        Result := Result + ' ' + Step.Name + #13;
    else
      raise Exception.Create('Unknown Action');
    end;
  end;
end;

function TTest.GetIsEnabled: Boolean;
begin
  Result := FIsEnabled;
end;

function TTest.GetName: String;
begin
  Result := FName;
end;

function TTest.GetReport: String;
var Step: PTestStep;
  I: Integer;
begin
  Result := inherited + #13 + FErrorReport;

  for I := 0 to Length(FSteps)-1 do
  begin
    Step := @FSteps[I];
    if Step.Report <> '' then
      Result := Result + Step.Report + #13;
  end;
end;

function TTest.GetStatus: TTestStatus;
begin
  Result := FStatus;
end;

procedure TTest.Pass(Step: PTestStep);
begin
  Step.Status := tsPassed;
  Step.Report := '';
end;

procedure TTest.Run;
begin
  FStatus := tsNotRun;
  FTestLog := '';
  FEmulateError := False;
  FErrorReport := '';

  ClearTestStatuses;
  WarmInitCompiler;

  if Compile and (FParseErrorNo = 0) and not AssembleError then
    if not Emulate then
      FStatus := tsError;

  RunSteps;
  if FStatus = tsNotRun then
    FStatus := tsPassed;
end;

procedure TTest.RunSteps;
var Step: PTestStep;
  I: Integer;
begin
  FErrorReport := '';
  //If we got a code gen asertion failure we'll get an error string but no parse error.
  //TODO: Make this better
  if (ParseErrorNo = 0) and (ParseErrorString <> '') then
  begin
    FErrorReport := ParseErrorString;
    FStatus := tsError;
  end
  else if (ParseErrorNo <> 0) and not FWantCompileError then
  begin
    FErrorReport := 'Unexpected Compile (Parse) Error: '#13 + ParseErrorString + #13;
    FStatus := tsFailed;
  end
  else if FAssembleError then
  begin
    FErrorReport := 'Assembly error:'#13 + FAssemblerLog + #13;
    FStatus := tsFailed;
  end
  else if (RuntimeError <> 0) and not FWantRuntimeError then
  begin
    FErrorReport := 'Unexpected Runtime Error: ' + RuntimeError.ToString + #13;
    FStatus := tsFailed;
  end;

  if Length(FSteps) = 0 then
  begin
    FErrorReport := 'Empty test'#13;
    FStatus := tsError;
    EXIT;
  end;

  for I := 0 to Length(FSteps)-1 do
  begin
    Step := @FSteps[I];
    try
      case Step.Action of
        taVarType: TestVarType(Step);
        taCompileError: TestCompileError(Step);
        taUsesPrimitive: TestUsesPrimitive(Step);
        taVarValue:
          if (ParseErrorNo = 0) and not FAssembleError and not FEmulateError then
            TestVarValue(Step);
        taRuntimeError:
          if (ParseErrorNo = 0) and not FAssembleError and not FEmulateError then
            TestRuntimeError(Step);
        else
        raise Exception.Create('Unknown Step Action');
      end;
    except
      on E:Exception do
      begin
        FStatus := tsError;
        Step.Status := tsError;
        Step.Report := 'EXCEPTION: ' + E.Message;
      end;
    end;
  end;
end;

procedure TTest.SetIsEnabled(Value: Boolean);
begin
  FIsEnabled := Value;
end;

procedure TTest.TestCompileError(Step: PTestStep);
var Err: String;
begin
  if CompareText(Step.Name, 'noerror') = 0 then
    EXIT;

  Check(Step.Name <> '', Step, 'COMPILE ERROR EXPECTED BUT NO ERROR SPECIFIED');
  Check(Step.Name <> 'noerror', Step, 'COMPILE ERROR EXPECTED BUT NO ERROR SPECIFIED');

  Err := ErrorToName(LastError);
  if LastError = qeNone then
    Check(False, Step, 'Parse error NOT generated. Expected ''' + Step.Name + ''''#13)
  else
    Check(CompareText(Err, Step.Name) = 0, Step, 'Incorrect parse error generated. Expected: ' +
      Step.Name + ' got ' + Err + #13);
end;

procedure TTest.TestRunTimeError(Step: PTestStep);
begin
  if CompareText(Step.Name, 'noerror') = 0 then
    EXIT;

  //TODO: check for the exact error. (For now we can just test for the presence of
  //an error
  Check(RuntimeError <> 0, Step, 'Runtime error NOT generated. Expected ''' + Step.Name + ''''#13);
end;

procedure TTest.TestUsesPrimitive(Step: PTestStep);
begin
  Check(CodeGen.UsesPrimitive(Step.Name), Step, 'Compiler failed to use primitive: ''' + Step.Name + ''''#13);
end;

procedure TTest.TestVarType(Step: PTestStep);
var V: PVariable;
begin
  V := VarFindByNameAllScopes(Step.Name);
  if not Assigned(V) then
    Error(Step, 'ERROR: No such variable: ''' + Step.Name + ''''#13)
  else
    Check(CompareText(VarTypeToName(V.VarType), Step.Value) = 0, Step, 'VarType mismatch on ' + V.Name +
      ', wanted ' + Step.Value + ' got ' + VarTypeToName(V.VarType) + #13);
end;

procedure TTest.TestVarValue(Step: PTestStep);

  function CharToStr(I: Integer): String;
  begin
    if (I > 32) and (I < 128) then
      Result := '''' + Chr(I) + ''' '
    else
      Result := '';
    Result := Result + '(#' + I.ToString + ')';
  end;

var V: PVariable;
  I: Integer;
  C: Char;
  ExprType: PUserType;
  Value: TImmValue;
begin
  V := VarFindByNameAllScopes(Step.Name);
  if not Assigned(V) then
    Error(Step, 'ERROR: No such variable: ''' + Step.Name + ''''#13)
  else
  begin
    IDE.Compiler.LoadSourceString(Step.Value);
    ExprType := V.UserType;
    if ParseConstantExpr(Value, ExprType) <> qeNone then
      raise Exception.Create('Invalid test expression: ' + Step.Value + #13 +
        IDE.Compiler.ParseErrorString);

    if IsIntegerType(V.UserType) and IsIntegerType(ExprType) then
      Check(V.Value.ToInteger = Value.ToInteger, Step, 'VarValue mismatch on ' + V.Name +
          ', wanted ' + Value.ToString + ' got ' + V.Value.ToString + #13)
    else
      Check(V.Value.ToString = Value.ToString, Step, 'VarValue mismatch on ' + V.Name +
          ', wanted ' + Value.ToString + ' got ' + V.Value.ToString + #13);
  end;
end;

procedure TTest.WarmInitCompiler;
begin
  //Compiler options
//  Def.Globals.optAllowAutoCreation := ??
  Def.Globals.optOverflowChecks := FOptions.OverFlowCheck;
  Def.Globals.optRangeChecks := FOptions.RangeCheck;
//  Def.Globals.optDefaultVarStorage := ??
//  Def.Globals.optDefaultCallingConvention := ??

  //Code generator options
  CodeGen.LogPrimitives := FOptions.LogPrimitives;

  FCompileOkay := False;

  FParseErrorNo := -1;
  FParseErrorHelp := '';
  FParseErrorPos := -1;
  FParseErrorLine := -1;
  FParseErrorString := '';

  FAssembleError := False;
  FAssemblerLog := '';

  FWriteBuffer := '';
  FRunTimeError := 0;
  FRunTimeErrorAddress := 0;
end;

{ TTestGroup }

procedure TTestGroup.Clear;
var Test: TTest;
begin
  for Test in FTests do
    Test.Free;
  FTests.Clear;
end;

procedure TTestGroup.ColdInitCompiler;
begin
  //Initialise directive to known state. Cold start the compiler (load data files)
  IDE.Compiler.Initialise(True, False);    //Initialise directives to known state

  //Platform
  IDE.Compiler.SetPlatformToDefault;
  IDE.Compiler.SetDeploy('quiche');
end;

constructor TTestGroup.Create;
begin
  inherited;

  FTests := TList<TTest>.Create;
end;

function TTestGroup.CreateTest(const Options: TTestOptions): TTest;
begin
  Result := TTest.Create;
  FTests.Add(Result);
  Result.FName := '';
  Result.FOptions := Options;
  Result.FCode := '';
  Result.FWantCompileError := False;
  Result.FWantRuntimeError := False;

  Result.FIsEnabled := True; //For now!
end;

destructor TTestGroup.Destroy;
begin
  Clear;
  FTests.Free;
  inherited;
end;

function TTestGroup.GetCount: Integer;
begin
  Result := FTests.Count;
end;

function TTestGroup.GetIsEnabled: Boolean;
var Test: TTest;
begin
  for Test in FTests do
    if Test.FIsEnabled then
      EXIT(True);

  Result := False;
end;

function TTestGroup.GetName: String;
begin
  Result := FName;
end;

function TTestGroup.GetReport: String;
var Counts: array[low(TTestStatus)..high(TTestStatus)] of Integer;
  Stat: TTestStatus;
  Test: TTest;
begin
  for Stat := low(TTestStatus) to high(TTestStatus) do
    Counts[Stat] := 0;

  for Test in FTests do
    inc(Counts[Test.Status]);

  Result := Inherited + #13;
  for Stat := low(TTestStatus) to high(TTestStatus) do
    Result := Result + Counts[Stat].ToString + ' ' + StatusStrings[Stat] + #13;

  Result := Result + '-----'#13 +
    Count.ToString + ' Total'#13;
end;

function TTestGroup.GetStatus: TTestStatus;
var Test: TTest;
begin
  Result := tsNotRun;
  for Test in FTests do
    if Test.Status > Result then
      Result := Test.Status;
end;

function TTestGroup.GetTests(Index: Integer): TTest;
begin
  Result := FTests[Index];
end;

procedure TTestGroup.InitOptions(var Options: TTestOptions);
begin
  Options.BlockType := btStatic;
  Options.ParseMode := pmRootUnknown;
  Options.OverflowCheck := True;
  Options.RangeCheck := True;
  Options.LogPrimitives := False;
end;

procedure TTestGroup.Run;
var Test: TTest;
begin
  ColdInitCompiler;
  for Test in FTests do
    if Test.IsEnabled then
      Test.Run;
end;

procedure TTestGroup.SetIsEnabled(Value: Boolean);
var Test: TTest;
begin
  for Test in FTests do
    Test.FIsEnabled := Value;
end;

{ TTestFile }

procedure TTestFile.LoadTestFile(const Filename: String);
var Options: TTestOptions;
  SL: TStringList;
  Test: TTest;
  Line: String;
  LineNo: Integer;
  Fields: TArray<String>;
  InCode: Boolean;
  Error: String;
begin
  FName := TPath.GetFilenameWithoutExtension(Filename);

  InitOptions(Options);
  Test := nil;
  FLastError := '';
  Error := '';

  SL := TStringList.Create;
  SL.LoadFromFile(Filename);

  InCode := False;
  LineNo := 0;
  for Line in SL do
  begin
    inc(LineNo);

    if InCode then
    begin
      if Line.StartsWith('endcode') then
        InCode := False
      else
        Test.FCode := Test.FCode + Line + #13#10;
    end
    else  //Not InCode
      if (Line.Trim.Length > 1) and (Line.Trim.Chars[0] <> ';') then
      begin
        Fields := Line.Trim.Split([' ']);
        if CompareText(Fields[0], 'code') = 0 then
        begin
          InCode := True;
          Test := CreateTest(Options);
          Test.FName := Line.Substring(5).Trim;
        end

        //Option specifications are allowed outside and before any tests
        else if CompareText(Fields[0], 'blocktype') = 0 then
          Options.BlockType := ParseBlockType(Fields)
        else if CompareText(Fields[0], 'parsetype') = 0 then
          Options.ParseMode := ParseParseMode(Fields)
        else if CompareText(Fields[0], '$overflow') = 0 then
          Options.OverflowCheck := ParseBoolean(Fields)
        else if CompareText(Fields[0], '$range') = 0 then
          Options.RangeCheck := ParseBoolean(Fields)
        else if CompareText(Fields[0], 'logprimitives') = 0 then
          Options.LogPrimitives := ParseBoolean(Fields)

        else
          if not Assigned(Test) then
            raise Exception.Create('Invalid line before a test: ' + Line + #13' in file ' + Filename)

        else if CompareText(Fields[0], 'compile') = 0 then
          Error {:= Test.FWantCompileError }:= Test.AddCompileError(Fields)
        else if CompareText(Fields[0], 'vartype') = 0 then
          Error := Test.AddVarType(Fields)
        else if CompareText(Fields[0], 'usesprimitive') = 0 then
          Error := Test.AddUsesPrimitive(Fields)

        else if CompareText(Fields[0], 'varvalue') = 0 then
          Error := Test.AddVarValue(Fields)
        else if CompareText(Fields[0], 'runtime') = 0 then
          Error {:= Test.FWantRuntimeError} := Test.AddRuntimeError(Fields)
        else
          raise Exception.Create('Invalid line: ' + Line + #13' in file ' + Filename);
      end;
    if Error <> '' then
      raise Exception.Create('Error loading test file: ' + Filename + #13 +
        Error + ' in line ' + LineNo.ToString + #13 + Line);
    if FLastError <> '' then
      raise Exception.Create('Error loading test file: ' + Filename + #13 +
        FLastError + ' in line ' + LineNo.ToString + #13 + Line);
  end;
end;

function TTestFile.ParseBlockType(Fields: TArray<String>): TBlockType;
begin
  Result := btStatic;
  if Length(Fields) <> 2 then
  begin
    FLAstError := errExpectedTwoFields;
    EXIT;
  end;

  if CompareText(Fields[1], 'stack') = 0 then
    Result := btStack
  else if CompareText(Fields[1], 'static') = 0 then
    Result := btStatic
  else
    FLastError := errInvalidField + Fields[1];
end;

function TTestFile.ParseBoolean(Fields: TArray<String>): Boolean;
begin
  Result := False;
  if Length(Fields) <> 2 then
  begin
    FLastError := errExpectedTwoFields;
    EXIT;
  end;

  if CompareText(Fields[1], 'on') = 0 then
    Result := True
  else if CompareText(Fields[1], 'off') = 0 then
    Result := False
  else if CharInSet(Fields[1].Chars[0], ['y','Y','T','t','1','+']) then
    Result := True
  else if CharInSet(Fields[1].Chars[0], ['n','N','f','F','0','-']) then
    Result := False
  else
    FLastError := errInvalidField + Fields[1];
end;

function TTestFile.ParseParseMode(Fields: TArray<String>): TParseMode;
var Mode: TParseMode;
begin
  Result := pmRootUnknown;
  if Length(Fields) <> 2 then
  begin
    FLastError := errExpectedTwoFields;
    EXIT;
  end;

  if CompareText(Fields[1], 'declarations') = 0 then
    Result := pmProgram
  else if CompareText(Fields[1], 'code') = 0 then
    Result := pmRootUnknown
  else
  begin
    for Mode := low(TParseMode) to high(TParseMode) do
      if CompareText(ParseModeStrings[Mode], Fields[1]) = 0 then
        EXIT(Mode);
    FLastError := errInvalidField + Fields[1];
  end;
end;

//;;================

procedure ClearTestGroups;
var F: TTestGroup;
begin
  for F in TestGroups do
  begin
    F.Free;
  end;
  TestGroups.Clear;
end;

procedure LoadTestFolder(const Folder: String);
var Files: TArray<String>;
  Filename: String;
  TestFile: TTestFile;
  Gen: TGeneratedTests;
begin
  Files := TDirectory.GetFiles(Folder, '*.tst');

  for Filename in Files do
  begin
    TestFile := TTestFile.Create;
    TestGroups.Add(TestFile);
    TestFile.LoadTestFile(Filename);
  end;

  Gen := TTestLoadParams.Create;
  Gen.Generate;
  TestGroups.Add(Gen);
  Gen := TTestStoreResults.Create;
  Gen.Generate;
  TestGroups.Add(Gen);
end;

procedure LoadSelfTests;
begin
  ClearTestGroups;
  LoadTestFolder(GetTestsFolder);
end;

{ TGeneratedTests }

function TGeneratedTests.GetName: String;
begin
  if ClassName.StartsWith('TTest') then
    Result := ClassName.SubString(5) + ' (Generated)'
  else
    Result := ClassName;
end;

function TGeneratedTests.TypecastInteger(Value: Integer;
  ToType: TVarType): Integer;
begin
  case ToType of
    vtInteger:
    begin
      Result := Value and iCPUWordMask;
      if Result and iIntegerMin <> 0 then
        Result := Result or (-1 xor iCPUWordMask);
    end;
    vtInt8:
    begin
      Result := Value and $ff;
      if Result and $80 <> 0 then
        Result := Result or (-1 xor $ff);
    end;
    vtByte: Result := Value and $ff;
    vtWord, vtPointer: Result := Value and iCPUWordMask;
  else
    raise Exception.Create('Unhandled integer type');
  end;
end;

const IntegerValues: TArray<Integer> = [-32768, -32767, -129, -128, -127, -1, 0, 1, 127, 128, 255, 256, 32767];const Int8Values: TArray<Integer> = [-128, -127, -1, 0, 1, 127];const WordValues: TArray<Integer> = [0, 1, 127, 128, 255, 256, 32767, 32768, 65535];const ByteValues: TArray<Integer> = [0, 1, 127, 128, 255];
{ TTestLoadParams }

procedure TTestLoadParams.Generate;
const NameTemplate = '%%From->%%To (%%Value) Range %%Range %%Access';
const CodeTemplate =
'var x: %%To;'#13+''#13+'procedure f(a: %%To);%%Access;'#13+'begin'#13+'  x:= a;'#13+'end;'#13+''#13+'begin'#13+'  var i: %%From = %%Value;'#13+'  f(i);'#13+'end.'#13;var Options: TTestOptions;
  FromType: TVarType;
  ToType: TVarType;
  Values: TArray<Integer>;
  Value: Integer;
  RangeCheck: Boolean;
  Access: Boolean;
  ToValues: TArray<Integer>;
  Test: TTest;
const BoolStr: array[False..True] of String = ('Off', 'On');
const AccessStr: array[False..True] of String = ('Stack', 'Register');
begin
  InitOptions(Options);
  Options.ParseMode := pmRootUnknown;

  for FromType in [vtInteger, vtInt8, vtByte, vtWord, vtPointer] do
  begin
    case FromType of
      vtByte: Values := ByteValues;
      vtInt8: Values := Int8Values;
      vtInteger: Values := IntegerValues;
      vtWord, vtPointer: Values := WordValues;
    else
      raise Exception.Create('Unhandled type');
    end;
    for ToType in [vtInteger, vtInt8, vtByte, vtWord, vtPointer] do
    begin
      case ToType of
        vtByte: ToValues := ByteValues;
        vtInt8: ToValues := Int8Values;
        vtInteger: ToValues := IntegerValues;
        vtWord, vtPointer: ToValues := WordValues;
      else
        raise Exception.Create('Unhandled type');
      end;
      for Value in Values do
        for RangeCheck in [False..True] do
          for Access in [False..True] do
        begin
          Options.RangeCheck := RangeCheck;
          Test := CreateTest(Options);
          Test.Name := NameTemplate;
          Test.Name := Test.Name.Replace('%%From', VarTypeToName(FromType), [rfReplaceAll]);
          Test.Name := Test.Name.Replace('%%To', VarTypeToName(ToType), [rfReplaceAll]);
          Test.Name := Test.Name.Replace('%%Value', Value.ToString, [rfReplaceAll]);
          Test.Name := Test.Name.Replace('%%Range', BoolStr[RangeCheck], [rfReplaceAll]);
          Test.Name := Test.Name.Replace('%%Access', AccessStr[Access], [rfReplaceAll]);
          Test.Code := CodeTemplate;
          Test.Code := Test.Code.Replace('%%From', VarTypeToName(FromType), [rfReplaceAll]);
          Test.Code := Test.Code.Replace('%%To', VarTypeToName(ToType), [rfReplaceAll]);
          Test.Code := Test.Code.Replace('%%Value', Value.ToString, [rfReplaceAll]);
          Test.Code := Test.Code.Replace('%%Access', AccessStr[Access], [rfReplaceAll]);


          if (Value < ToValues[0]) or (Value > ToValues[Length(ToValues)-1]) then
            if RangeCheck then
              Test.AddRuntimeError(['', 'rangecheck'])
            else
              Test.AddVarValue(['','x', TypecastInteger(Value, ToType).ToString])
          else
            Test.AddVarValue(['', 'x', Value.ToString]);
        end;
      end;
    end;
end;

{ TTestStoreResults }

procedure TTestStoreResults.Generate;

const NameTemplate = '%%From->%%To (%%Value) Range %%Range %%Access';
const CodeTemplate =
'function f: %%From;%%Access;'#13+'begin'#13+'  var a:%%From = %%Value'#13+'  Result := a;'#13+'end;'#13+''#13+'var x: %%To;'#13+'begin'#13+'  x := f;'#13+'end.'#13;var Options: TTestOptions;  FromType: TVarType;
  ToType: TVarType;
  Values: TArray<Integer>;
  Value: Integer;
  RangeCheck: Boolean;
  ToValues: TArray<Integer>;
  Access: Boolean;
  Test: TTest;
const BoolStr: array[False..True] of String = ('Off', 'On');
const AccessStr: array[False..True] of String = ('Stack', 'Register');
begin
  InitOptions(Options);
  Options.ParseMode := pmRootUnknown;

  for FromType in [vtInteger, vtInt8, vtByte, vtWord, vtPointer] do
  begin
    case FromType of
      vtByte: Values := ByteValues;
      vtInt8: Values := Int8Values;
      vtInteger: Values := IntegerValues;
      vtWord, vtPointer: Values := WordValues;
    else
      raise Exception.Create('Unhandled type');
    end;
    for ToType in [vtInteger, vtInt8, vtByte, vtWord, vtPointer] do
    begin
      case ToType of
        vtByte: ToValues := ByteValues;
        vtInt8: ToValues := Int8Values;
        vtInteger: ToValues := IntegerValues;
        vtWord, vtPointer: ToValues := WordValues;
      else
        raise Exception.Create('Unhandled type');
      end;

      for Value in Values do
        for RangeCheck in [False..True] do
          for Access in [False..True] do
          begin
            Options.RangeCheck := RangeCheck;
            Test := CreateTest(Options);
            Test.Name := NameTemplate;
            Test.Name := Test.Name.Replace('%%From', VarTypeToName(FromType), [rfReplaceAll]);
            Test.Name := Test.Name.Replace('%%To', VarTypeToName(ToType), [rfReplaceAll]);
            Test.Name := Test.Name.Replace('%%Value', Value.ToString, [rfReplaceAll]);
            Test.Name := Test.Name.Replace('%%Range', BoolStr[RangeCheck], [rfReplaceAll]);
            Test.Name := Test.Name.Replace('%%Access', AccessStr[Access], [rfReplaceAll]);

            Test.Code := CodeTemplate;
            Test.Code := Test.Code.Replace('%%From', VarTypeToName(FromType), [rfReplaceAll]);
            Test.Code := Test.Code.Replace('%%To', VarTypeToName(ToType), [rfReplaceAll]);
            Test.Code := Test.Code.Replace('%%Value', Value.ToString, [rfReplaceAll]);
            Test.Code := Test.Code.Replace('%%Access', AccessStr[Access], [rfReplaceAll]);


            if (Value < ToValues[0]) or (Value > ToValues[Length(ToValues)-1]) then
              if RangeCheck then
                Test.AddRuntimeError(['', 'rangecheck'])
              else
                Test.AddVarValue(['','x', TypecastInteger(Value, ToType).ToString])
            else
              Test.AddVarValue(['', 'x', Value.ToString]);
          end;
        end;
      end;
end;

initialization
  TestGroups := TTestGroups.Create;
finalization
  ClearTestGroups;
  TestGroups.Free;
end.

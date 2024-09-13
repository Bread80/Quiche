unit IDE.SelfTest;

interface
uses SysUtils, Generics.Collections,
  Def.Globals, Def.QTypes, Def.Variables,
  CodeGen,
  IDE.Compiler;

type
  PTestOptions = ^TTestOptions;
  TTestOptions = record
    //blocktype static|stack      - Select Stack to generate a stack frame and use local vars
    BlockType: TBlockType;
    //parsetype declarations|code
    ParseType: TParseType;
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
  protected
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
    //Virtual methods
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
    procedure InitialiseCompiler;
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


type TTestFile = class(TTestable)
  private
    FTests: TList<TTest>;
    FLastError: String;
    FName: String;
    function GetCount: Integer;
    function GetTests(Index: Integer): TTest;
  protected
    //Virtual methods
    function GetName: String;override;
    function GetIsEnabled: Boolean;override;
    procedure SetIsEnabled(Value: Boolean);override;
    function GetStatus: TTestStatus;override;

    procedure InitOptions(var Options: TTestOptions);

    //Test options
    function ParseBlockType(Fields: TArray<String>): TBlockType;
    function ParseParseType(Fields: TArray<String>): TParseType;
    function ParseBoolean(Fields: TArray<String>): Boolean;
  public
    constructor Create;
    destructor Destroy;override;

    procedure Clear;
    //Create a new, empty test with the given optins and adds it to the list of tests
    function CreateTest(const Options: TTestOptions): TTest;
    procedure LoadTestFile(const Filename: String);

    //Run any enabled tests
    procedure Run;override;

    property Count: Integer read GetCount;
    property Tests[Index: Integer]: TTest read GetTests;default;

    //True is one or more of  tests has Enabled = True
    //False only if all tests have Enabled = False
  end;

  TTestFiles = TList<TTestFile>;

var TestFiles: TTestFiles;

procedure ClearTestFiles;
procedure LoadTestFolder(const Folder: String);

implementation
uses Classes, IOUtils;

const TestActionStrings: array[low(TTestAction)..high(TTestAction)] of String = (
  'VarType', 'CompileError', 'UsesPrimitive', 'VarValue','RuntimeError');

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

  { TTestOptions }

function TTestOptions.ToString: String;
const BoolToStr: array[False..True] of String = ('Off','On');
begin
  Result := 'Block Type: ' + BlockTypeStrings[BlockType] + #13 +
    'Parse Type: ' + ParseTypeStrings[ParseType] + #13 +
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
  Result := 'Status: ' + StatusStrings[Status];
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

  //TODO: Decode error identifier
end;

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
var Step: PTestStep;
  I: Integer;
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
    FCompileOkay := IDE.Compiler.CompileStrings(SL, FOptions.BlockType, FOptions.ParseType, False, True);
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

  FEmulateError := IDE.Compiler.Emulate(IDE.Compiler.BinaryFileName);
  Result := FEmulateError;
  if FEmulateError then
    FErrorReport := 'ERROR: Unexpected runtime error '
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
  Result := inherited + #13#13;

  if (ParseErrorNo <> 0) and not FWantCompileError then
    Result := Result + FErrorReport
  else if AssembleError then
    Result := Result + FErrorReport
  else if (FRuntimeError <> 0) and not FWantRuntimeError then
    Result := Result + FErrorReport;




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

procedure TTest.InitialiseCompiler;
begin
  //Initialise directive to known state. Cold start the compiler (load data files)
  IDE.Compiler.Initialise(True, False);    //Initialise directives to known state

  //Platform
  IDE.Compiler.Config.PlatformName := 'TestCase';
  IDE.Compiler.Deploy.Clear;
  //TODO: TEMPORARY
    IDE.Compiler.Deploy.LoadFromFile('C:\Dropbox\Delphi\Quiche\Platforms\TestCase\Deploy\EmulateRun.deploy');
{TPath.Combine(
      TPath.Combine(IDE.Compiler.GetPlatformFolder, DeployFolderName),
      cbDeploy.Items[cbDeploy.ItemIndex] + DeployExt));
}
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

  ClearTestStatuses;
  InitialiseCompiler;

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
  if (ParseErrorNo <> 0) and not FWantCompileError then
  begin
    FErrorReport := 'Unexpected Compile (Parse) Error: ' + ParseErrorString;
    FStatus := tsFailed;
  end
  else if FAssembleError then
  begin
    FErrorReport := 'Assembly error:'#13 + FAssemblerLog;
    FStatus := tsFailed;
  end
  else if (RuntimeError <> 0) and not FWantRuntimeError then
  begin
    FErrorReport := 'Unexpected Runtime Error: ' + RuntimeError.ToString;
    FStatus := tsFailed;
  end;

  for I := 0 to Length(FSteps)-1 do
  begin
    Step := @FSteps[I];
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
  end;
end;

procedure TTest.SetIsEnabled(Value: Boolean);
begin
  FIsEnabled := Value;
end;

procedure TTest.TestCompileError(Step: PTestStep);
begin
  if CompareText(Step.Name, 'noerror') = 0 then
    EXIT;

  //TODO: check for the exact error. (For now we can just test for the presence of
  //an error
  Check(ParseErrorNo <> 0, Step, 'Parse error NOT generated. Expected ''' + Step.Name);
end;

procedure TTest.TestRunTimeError(Step: PTestStep);
begin
  if CompareText(Step.Name, 'noerror') = 0 then
    EXIT;

  //TODO: check for the exact error. (For now we can just test for the presence of
  //an error
  Check(RuntimeError <> 0, Step, 'Runtime error NOT generated. Expected ''' + Step.Name);
end;

procedure TTest.TestUsesPrimitive(Step: PTestStep);
var PrimName: String;
begin
  Check(CodeGen.UsesPrimitive(Step.Name), Step, 'Compiler failed to use primitive: ''' + Step.Name + '''');
end;

procedure TTest.TestVarType(Step: PTestStep);
var V: PVariable;
begin
  V := VarFindByNameAllScopes(Step.Name);
  if not Assigned(V) then
    Error(Step, 'ERROR: No such variable: ' + Step.Name)
  else
    Check(CompareText(VarTypeToName(V.VarType), Step.Value) = 0, Step, 'VarType mismatch on ' + V.Name +
      ', wanted ' + Step.Value + ' got ' + VarTypeToName(V.VarType));
end;

procedure TTest.TestVarValue(Step: PTestStep);
var V: PVariable;
  I: Integer;
  C: Char;
begin
  V := VarFindByNameAllScopes(Step.Name);
  if not Assigned(V) then
    Error(Step, 'ERROR: No such variable: ' + Step.Name)
  else
  case V.VarType of
    vtInteger, vtInt8, vtWord, vtByte, vtPointer:
    begin
      if not TryStrToInt(Step.Value, I) then
        Error(Step, 'Invalid integer value: ''' + Step.Value + '''')
      else
        Check(V.ValueInt = I, Step, 'VarValue mismatch on ' + V.Name +
          ', wanted ' + Step.Value + ' got ' + V.ValueInt.ToString);
    end;
    vtBoolean:
    begin
      if CompareText(Step.Value, 'false') = 0 then
      Check(V.ValueInt = valueFalse and $ff, Step, 'VarValue mismatch on ' + V.Name +
        ', wanted ' + Step.Value + ' got ' + V.ValueInt.ToString)
      else if CompareText(Step.Value, 'true') = 0 then
        Check(V.ValueInt = valueTrue and $ff, Step, 'VarValue mismatch on ' + V.Name +
          ', wanted ' + Step.Value + ' got ' + V.ValueInt.ToString)
      else
        Error(Step, 'Invalid boolean value: ''' + Step.Value + '''');
    end;
    vtChar:
    begin
      if (Length(Step.Value) = 3) and (Step.Value.Chars[0] = '''') and (Step.Value.Chars[2] = '''') then
        Check(V.ValueInt = ord(C), Step, 'VarValue mismatch on ' + V.Name +
          ', wanted ''' + C + ''' got ''' + chr(V.ValueInt) + '''')
      else if Step.Value.Chars[0] = '#' then
        //Numeric char literal
        if TryStrToInt(Step.Value.Substring(1), I) then
          if I <= 255 then
            Check(V.ValueInt = ord(C), Step, 'VarValue mismatch on ' + V.Name +
              ', wanted ''' + C + ''' got ''' + chr(V.ValueInt) + '''')
          else
            Error(Step, 'Invalid string value: ''' + Step.Value + '''')
        else
          Error(Step, 'Invalid string value: ''' + Step.Value + '''');
    end;
  else
    Error(Step, 'Unknown variable type for ' + Step.Name + '''');
  end;
end;

{ TTestList }


procedure TTestFile.Clear;
var Test: TTest;
begin
  for Test in FTests do
    Test.Free;
  FTests.Clear;
end;

constructor TTestFile.Create;
begin
  inherited;

  FTests := TList<TTest>.Create;
end;

function TTestFile.CreateTest(const Options: TTestOptions): TTest;
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

destructor TTestFile.Destroy;
begin
  Clear;
  FTests.Free;
  inherited;
end;

function TTestFile.GetCount: Integer;
begin
  Result := FTests.Count;
end;

function TTestFile.GetIsEnabled: Boolean;
var Test: TTest;
begin
  for Test in FTests do
    if Test.FIsEnabled then
      EXIT(True);

  Result := False;
end;

function TTestFile.GetName: String;
begin
  Result := FName;
end;

function TTestFile.GetStatus: TTestStatus;
var Test: TTest;
begin
  Result := tsNotRun;
  for Test in FTests do
    if Test.Status > Result then
      Result := Test.Status;
end;

function TTestFile.GetTests(Index: Integer): TTest;
begin
  Result := FTests[Index];
end;

procedure TTestFile.InitOptions(var Options: TTestOptions);
begin
  Options.BlockType := btStatic;
  Options.ParseType := ptCode;
  Options.OverflowCheck := True;
  Options.RangeCheck := True;
  Options.LogPrimitives := False;
end;

procedure TTestFile.LoadTestFile(const Filename: String);
var Options: TTestOptions;
  SL: TStringList;
  Test: TTest;
  Line: String;
  Fields: TArray<String>;
  InCode: Boolean;
  Okay: Boolean;
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
  for Line in SL do
  begin
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
        Okay := True;
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
          Options.ParseType := ParseParseType(Fields)
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
      raise Exception.Create(Error);
    if FLastError <> '' then
      raise Exception.Create(FLastError);
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

function TTestFile.ParseParseType(Fields: TArray<String>): TParseType;
begin
  Result := ptCode;
  if Length(Fields) <> 2 then
  begin
    FLastError := errExpectedTwoFields;
    EXIT;
  end;

  if CompareText(Fields[1], 'declarations') = 0 then
    Result := ptDeclarations
  else if CompareText(Fields[1], 'code') = 0 then
    Result := ptCode
  else
    FLastError := errInvalidField + Fields[1];
end;

procedure TTestFile.Run;
var Test: TTest;
begin
  for Test in FTests do
    if Test.IsEnabled then
      Test.Run;
end;

procedure TTestFile.SetIsEnabled(Value: Boolean);
var Test: TTest;
begin
  for Test in FTests do
    Test.FIsEnabled := Value;
end;

//;;================



procedure ClearTestFiles;
var F: TTestFile;
begin
  for F in TestFiles do
  begin
    F.Free;
  end;
  TestFiles.Clear;
end;

procedure LoadTestFolder(const Folder: String);
var Files: TArray<String>;
  Filename: String;
  TestFile: TTestFile;
begin
  Files := TDirectory.GetFiles(Folder, '*.tst');

  for Filename in Files do
  begin
    TestFile := TTestFile.Create;
    TestFiles.Add(TestFile);
    TestFile.LoadTestFile(Filename);
  end;
end;

initialization
  TestFiles := TTestFiles.Create;
finalization
  ClearTestFiles;
  TestFiles.Free;
end.

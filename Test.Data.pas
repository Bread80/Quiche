unit Test.Data;

interface
uses SysUtils, IOUtils, Compiler, Variables, QTypes, Globals, CodeGen;

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
  end;

type
  TTestAction = (taVarType, taCompileError, taUsesPrimitive, taVarValue, taRuntimeError);

  PTestStep = ^TTestStep;
  TTestStep = record
    Name: String; //Data held here depends on the Action
    Value: String;

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
        WantCompile: Boolean;
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
        WantRuntime: Boolean;
        //Name field contains required error
//        RunTimeError: Integer
        );
  end;




type PTest = ^TTest;
  TTest = record
    Name: String;
    Options: TTestOptions;
    Code: String;
    Steps: TArray<TTestStep>;
  end;

procedure LoadTestFile(const FileName: String);

implementation
uses Classes, Generics.Collections;

var TestList: TList<PTest>;

procedure InitOptions(var Options: TTestOptions);
begin
  Options.BlockType := btStatic;
  Options.ParseType := ptCode;
  Options.OverflowCheck := True;
  Options.RangeCheck := True;
  Options.LogPrimitives := False;
end;

procedure ClearTestList;
var Test: PTest;
begin
  for Test in TestList do
    Dispose(Test);
end;

procedure InitTestList;
begin
  if not Assigned(TestList) then
    TestList := TList<PTest>.Create
  else
    ClearTestList;
end;

function CreateTest(const Options: TTestOptions): PTest;
begin
  New(Result);
  Result.Name := '';
  Result.Options := Options;
  Result.Code := '';
end;

const
  errExpectedTwoFields = 'Expected two fields';
  errExpectedThreeFields = 'Expected three fields';
  errExpectedTwoOrThreeFields = 'Expected two or three fields';
  errInvalidField = 'Invalid field: ';

function ParseBlockType(Fields: TArray<String>;var Error: String): TBlockType;
begin
  Result := btStatic;
  if Length(Fields) <> 2 then
  begin
    Error := errExpectedTwoFields;
    EXIT;
  end;

  if CompareText(Fields[1], 'stack') = 0 then
    Result := btStack
  else if CompareText(Fields[1], 'static') = 0 then
    Result := btStatic
  else
    Error := errInvalidField + Fields[1];
end;

function ParseParseType(Fields: TArray<String>;var Error: String): TParseType;
begin
  Result := ptCode;
  if Length(Fields) <> 2 then
  begin
    Error := errExpectedTwoFields;
    EXIT;
  end;

  if CompareText(Fields[1], 'declarations') = 0 then
    Result := ptDeclarations
  else if CompareText(Fields[1], 'code') = 0 then
    Result := ptCode
  else
    Error := errInvalidField + Fields[1];
end;

function ParseBoolean(Fields: TArray<String>;var Error: String): Boolean;
begin
  Result := False;
  if Length(Fields) <> 2 then
  begin
    Error := errExpectedTwoFields;
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
    Error := errInvalidField + Fields[1];
end;

function AddStep(var Steps: TArray<TTestStep>): PTestStep;
begin
  SetLength(Steps, Length(Steps)+1);
  Result := @Steps[Length(Steps)-1];
end;

procedure ParseCompileError(var Steps: TArray<TTestStep>;Fields: TArray<String>;var Error: String);
var Step: PTestStep;
begin
  if Length(Fields) < 2 then
  begin
    Error := errExpectedTwoOrThreeFields;
    EXIT;
  end;

  Step := AddStep(Steps);
  Step.Action := taCompileError;

  if CompareText(Fields[1], 'noerror') = 0 then
  begin
    if Length(Fields) <> 2 then
    begin
      Error := errExpectedTwoFields;
      EXIT;
    end;

    Step.WantCompile := False;
  end
  else if CompareText(Fields[1], 'error') = 0 then
  begin
    Step.WantCompile := True;
    Step.Name := Fields[2];

    //TODO: Decode error identifier
  end
  else
    Error := errInvalidField + Fields[1];
end;

procedure ParseVarType(var Steps: TArray<TTestStep>;Fields: TArray<String>;var Error: String);
var Step: PTestStep;
begin
  if Length(Fields) <> 3 then
  begin
    Error := errExpectedThreeFields;
    EXIT;
  end;

  Step := AddStep(Steps);
  Step.Action := taVarType;
  Step.Name := Fields[1];
  Step.Value := Fields[2];
end;

procedure ParseUsesPrimitive(var Steps: TArray<TTestStep>;Fields: TArray<String>;var Error: String);
var Step: PTestStep;
begin
  if Length(Fields) <> 2 then
  begin
    Error := errExpectedTwoFields;
    EXIT;
  end;

  Step := AddStep(Steps);
  Step.Action := taUsesPrimitive;
  Step.Name := Fields[1];
end;

procedure ParseVarValue(var Steps: TArray<TTestStep>;Fields: TArray<String>;var Error: String);
var Step: PTestStep;
begin
  if Length(Fields) <> 3 then
  begin
    Error := errExpectedThreeFields;
    EXIT;
  end;

  Step := AddStep(Steps);
  Step.Action := taVarValue;
  Step.Name := Fields[1];
  Step.Value := Fields[2];
  //TODO: Decode the value field
end;

procedure ParseRuntimeError(var Steps: TArray<TTestStep>;Fields: TArray<String>;var Error: String);
var Step: PTestStep;
begin
  if Length(Fields) < 2 then
  begin
    Error := errExpectedTwoOrThreeFields;
    EXIT;
  end;

  Step := AddStep(Steps);
  Step.Action := taRuntimeError;

  if CompareText(Fields[1], 'noerror') = 0 then
  begin
    if Length(Fields) <> 2 then
    begin
      Error := errExpectedTwoFields;
      EXIT;
    end;

    Step.WantRuntime := False;
  end
  else if CompareText(Fields[1], 'error') = 0 then
  begin
    Step.WantRuntime := True;
    Step.Name := Fields[2];

    //TODO: Decode error identifier
  end
  else
    Error := errInvalidField + Fields[1];
end;

procedure LoadTestFile(const FileName: String);
var Options: TTestOptions;
  SL: TStringList;
  Test: PTest;
  Line: String;
  Fields: TArray<String>;
  InCode: Boolean;
  Okay: Boolean;
  Error: String;
begin
  InitOptions(Options);
  InitTestList;
  Test := nil;
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
        Test.Code := Test.Code + Line + #13#10;
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
          Test.Name := Line.Substring(5).Trim;
        end

        //Option specifications are allowed outside and before any tests
        else if CompareText(Fields[0], 'blocktype') = 0 then
          Options.BlockType := ParseBlockType(Fields, Error)
        else if CompareText(Fields[0], 'parsetype') = 0 then
          Options.ParseType := ParseParseType(Fields, Error)
        else if CompareText(Fields[0], '$overflow') = 0 then
          Options.OverflowCheck := ParseBoolean(Fields, Error)
        else if CompareText(Fields[0], '$range') = 0 then
          Options.RangeCheck := ParseBoolean(Fields, Error)
        else if CompareText(Fields[0], 'logprimitives') = 0 then
          Options.LogPrimitives := ParseBoolean(Fields, Error)

        else
          if not Assigned(Test) then
            raise Exception.Create('Invalid line before a test: ' + Line)

        else if CompareText(Fields[0], 'compile') = 0 then
          ParseCompileError(Test.Steps, Fields, Error)
        else if CompareText(Fields[0], 'vartype') = 0 then
          ParseVarType(Test.Steps, Fields, Error)
        else if CompareText(Fields[0], 'usesprimitive') = 0 then
          ParseUsesPrimitive(Test.Steps, Fields, Error)

        else if CompareText(Fields[0], 'varvalue') = 0 then
          ParseVarValue(Test.Steps, Fields, Error)
        else if CompareText(Fields[0], 'runtime') = 0 then
          ParseRuntimeError(Test.Steps, Fields, Error)
        else
          raise Exception.Create('Invalid line: ' + Line);
      end;
  end;
end;

initialization
  TestList := nil;
finalization
  if Assigned(TestList) then
  begin
    ClearTestList;
    TestList.Free;
  end;
end.

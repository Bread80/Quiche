unit Testing;
{
Test file:
;Source code
code [<testname>]
...
endcode
;Compilation
compile error [<errorname>]
;Parsing
vartype <varname> <vartype>
;Execution
varvalue <varname> <value>
}

interface
uses Classes, ILData;

procedure Initialise;

procedure RunTestFile(Filename: String;StopOnError: Boolean);
procedure RunAllTests(Folder: String;StopOnError: Boolean);

procedure TestLogToStrings(SL: TStrings);

implementation
uses SysUtils, IOUtils, Compiler, Variables, QTypes, Globals;

var
  TestName: String;
  TestCode: TStringList;
  CompileScope: TCompileScope;
  IsCompiled: Boolean;
  CompileOkay: Boolean;
  HasRun: Boolean;
  RunOkay: Boolean;

  Log: TStringList;

  TotalCount: Integer;
  TotalFails: Integer;
  TotalPasses: Integer;

  FileCount: Integer;
  FileFails: Integer;
  FilePasses: Integer;

  TestCount: Integer;
  TestFails: Integer;
  TestPasses: Integer;

procedure ResetTotalStats;
begin
  TotalCount := 0;
  TotalFails := 0;
  TotalPasses := 0;
end;

procedure ResetFileStats;
begin
  Inc(TotalCount, FileCount);
  Inc(TotalFails, FileFails);
  Inc(TotalPasses, FilePasses);
  FileCount := 0;
  FileFails := 0;
  FilePasses := 0;
end;

procedure ResetTestStats;
begin
  Inc(FileCount, TestCount);
  Inc(FileFails, TestFails);
  Inc(FilePasses, TestPasses);
  TestCount := 0;
  TestFails := 0;
  TestPasses := 0;
end;

procedure Initialise;
begin
  if Assigned(Log) then
    Log.Clear;
  ResetTestStats;
  ResetFileStats;
  ResetTotalStats;
  CompileScope := csBlock;
end;

procedure TestLogToStrings(SL: TStrings);
begin
  SL.Assign(Log);
end;

procedure TestLog(Msg: String);
begin
  Log.Add(Msg);
end;

procedure ShowStats(Text: String;Count, Fails, Passes: Integer);
begin
  TestLog(Text + ' totals: Count: ' + Count.ToString + ' Fails: ' + Fails.ToString +
    ' Passes: ' + Passes.ToString);
end;

procedure ShowTotalStats;
begin
  ShowStats('Grand', TotalCount, TotalFails, TotalPasses);
  ResetTotalStats;
end;

procedure ShowFileStats;
begin
  ShowStats('File', FileCount, FileFails, FilePasses);
  ResetFileStats;
end;

procedure ShowTestStats;
begin
  if TestCount > 0 then
    ShowStats('Test', TestCount, TestFails, TestPasses);
  ResetTestStats;
end;



procedure DoTest(B: Boolean;Msg: String);
begin
  inc(TestCount);
  if not B then
  begin
    inc(TestFails);
    TestLog('FAIL: ' + Msg);
  end
  else
  begin
    inc(TestPasses);
//    TestLog('Pass');
  end;
end;

//Compiles the code if it is not already compiled
//Returns False if there was a compilation error
//If IgnoreErrors is False, errors will be raised, otherwise no error will be
//raised. Use this option to test for compilation errors.
function CompileNeeded(IgnoreErrors: Boolean): Boolean;
begin
  if not IsCompiled then
  begin
    CompileOkay := Compiler.CompileStrings(TestCode, CompileScope, False);
    Result := CompileOkay;
    if not CompileOkay and not IgnoreErrors then
    begin
      if Compiler.LastErrorNo <> 0 then
        TestLog('FAIL: Compiler error: ' + Compiler.LastErrorString)
      else
        TestLog('FAIL: Assembler error? ' + AssemblerLog);
//        TestLog(Compiler.LastErrorLine);
    end;
    IsCompiled := True;
  end
  else
    Result := CompileOkay;
end;

function RunNeeded(IgnoreErrors: Boolean): Boolean;
begin
  if not CompileNeeded(IgnoreErrors) then
    EXIT(False);

  if not HasRun then
  begin
    RunOkay := Compiler.Emulate(Compiler.BinaryFileName);
    if not RunOkay and not IgnoreErrors then
      TestLog('ERROR: Unexpected runtime error ');
    Result := RunOkay;
    HasRun := True;
  end
  else
    Result := RunOkay;
end;

function TestVarType(Fields: TArray<String>): Boolean;
var V: PVariable;
  Index: Integer;
begin
  try
    if not CompileNeeded(False) then
      EXIT(False);
  except
    on E:Exception do
    begin
      TestLog('EXCEPTION: ' + E.Message);
      EXIT(False);
    end;
  end;

  if Length(Fields) <> 3 then
    EXIT(False);
  V := VarFindByNameAllScopes(Fields[1], Index);
  if not Assigned(V) then
    TestLog('ERROR: No such variable: ' + Fields[1])
  else
    DoTest(CompareText(VarTypeToName(V.VarType), Fields[2]) = 0, 'VarType mismatch on ' + V.Name +
      ', wanted ' + Fields[2] + ' got ' + VarTypeToName(V.VarType));
  Result := True;
end;

function TestVarValue(Fields: TArray<String>): Boolean;
var V: PVariable;
  Index: Integer;
  I: Integer;
  B: Integer;
  C: Char;
begin
  if not RunNeeded(False) then
    EXIT(False);
  if Compiler.LastErrorNo <> 0 then
    EXIT(False);

  if Length(Fields) <> 3 then
    EXIT(False);
  V := VarFindByNameAllScopes(Fields[1], Index);
  Result := True;
  if not Assigned(V) then
    TestLog('ERROR: No such variable: ' + Fields[1])
  else
  case V.VarType of
    vtInteger, vtInt8, vtWord, vtByte, vtPointer:
    begin
      if not TryStrToInt(Fields[2], I) then
        EXIT(False);
      DoTest(V.ValueInt = I, 'VarValue mismatch on ' + V.Name +
        ', wanted ' + I.ToString + ' got ' + V.ValueInt.ToString);
    end;
    vtBoolean:
    begin
      if CompareText(Fields[2], 'false') = 0 then
        B := valueFalse and $ff
      else if CompareText(Fields[2], 'true') = 0 then
        B := valueTrue and $ff
      else
        EXIT(False);
      DoTest(V.ValueInt = B, 'VarValue mismatch on ' + V.Name +
        ', wanted ' + B.ToString + ' got ' + V.ValueInt.ToString);
    end;
    vtChar:
    begin
      if (Length(Fields[2]) = 3) and (Fields[2].Chars[0] = '''') and (Fields[2].Chars[2] = '''') then
        C := Fields[2].Chars[1]
      else
        EXIT(False);
      DoTest(V.ValueInt = ord(C), 'VarValue mismatch on ' + V.Name +
        ', wanted ''' + C + ''' got ''' + chr(V.ValueInt) + '''');
    end;
  else
    EXIT(False);
  end;
end;

function TestCompile(Fields: TArray<String>): Boolean;
var Err: Boolean;
begin
  if Length(Fields) < 2 then
    EXIT(False);

  if CompareText(Fields[1], 'error') = 0 then
    Err := True
  else if CompareText(Fields[1], 'noerror') = 0 then
    Err := False
  else
    EXIT(False);

  CompileNeeded(True);
  Result := CompileOkay <> Err;
  if CompileOkay then
    DoTest(Result, 'Compiler failed to raise an error')
  else
    DoTest(Result, 'Compiler raised an error: ' + Compiler.LastErrorString)
end;

function TestRuntime(Fields: TArray<String>): Boolean;
var Err: Integer;
begin
  if Length(Fields) <> 2 then
    EXIT(False);

  if CompareText(Fields[1], 'overflow') = 0 then
    Err := rerrOverflow
  else if CompareText(Fields[1], 'dividebyzero') = 0 then
    Err := rerrDivByZero
  else if CompareText(Fields[1], 'noerror') = 0 then
    Err := 0
  else
    EXIT(False);

  RunNeeded(True);
  //Abort if compile failed - that's not what we're testing here
  if not CompileOkay then
    EXIT(False);

  DoTest(Compiler.RunTimeError = Err, 'Runtime ' + Err.ToString +
    ' error expected. Got ' + Compiler.RuntimeError.ToString);
  Result := Compiler.RunTimeError = Err;
end;

function SetCompileScope(Fields: TArray<String>): Boolean;
begin
  if Length(Fields) <> 2 then
    EXIT(False);

  if CompareText(Fields[1], 'block') = 0 then
    CompileScope := csBlock
  else if CompareText(Fields[1], 'global') = 0 then
    CompileScope := csGlobal
  else
    EXIT(False);

  Result := True;
end;

function OptionOverflow(Fields: TArray<String>): Boolean;
begin
  if Length(Fields) <> 2 then
    EXIT(False);

  if CompareText(Fields[1], 'on') = 0 then
    optOverflowChecks := True
  else if CompareText(Fields[1], 'off') = 0 then
    optOverflowChecks := False
  else
    EXIT(False);

  Result := True;
end;

procedure RunTestFile(Filename: String;StopOnError: Boolean);
var SL: TStringList;
  Line: String;
  Fields: TArray<String>;
  InCode: Boolean;
  Okay: Boolean;
begin
  if not Assigned(Log) then
    Log := TStringList.Create;

  Compiler.Initialise(True);    //Initialise directives to known state

  TestLog('Processing file: ' + Filename);
  TestLog('------------------------------');
  SL := TStringList.Create;
  SL.LoadFromFile(Filename);

  InCode := False;
  TestName := '';
  IsCompiled := False;
  HasRun := False;
  TestCode := TStringList.Create;
  for Line in SL do
  begin
    try
    if InCode then
    begin
      if Line.StartsWith('endcode') then
        InCode := False
      else
        TestCode.Add(Line);
    end
    else  //Not InCode
      if (Line.Trim.Length > 1) and (Line.Trim.Chars[0] <> ';') then
      begin
        Okay := True;
        Fields := Line.Trim.Split([' ']);
        if CompareText(Fields[0], 'code') = 0 then
        begin
          ShowTestStats;

          InCode := True;
          IsCompiled := False;
          HasRun := False;
          TestCode.Clear;
          TestName := Line.Substring(5).Trim;

          TestLog('');
          TestLog('Test: ' + TestName);
        end
        else if CompareText(Fields[0], '$overflow') = 0 then
          Okay := OptionOverflow(Fields)
        else if CompareText(Fields[0], 'vartype') = 0 then
          Okay := TestVarType(Fields)
        else if CompareText(Fields[0], 'varvalue') = 0 then
          Okay := TestVarValue(Fields)
        else if CompareText(Fields[0], 'compile') = 0 then
          Okay := TestCompile(Fields)
        else if CompareText(Fields[0], 'runtime') = 0 then
          Okay := TestRuntime(Fields)
        else if CompareText(Fields[0], 'compilescope') = 0 then
          Okay := SetCompileScope(Fields)
        else
        begin
          TestLog('Invalid command: ' + Line);
          Okay := False;
        end;

        if not Okay then
        begin
          TestLog('ERROR in line: ' + Line);
          if StopOnError then
          begin
            TestLog('HALTED ON FILE ERROR');
            EXIT;
          end;
        end;
      end;
    if StopOnError and (FileFails <> 0) then
    begin
      TestLog('HALTED ON TEST FAILURE ERROR');
      EXIT;
    end;
    except
      on E:Exception do
      begin
        TestLog('***EXCEPTION: ' + E.Message);
        EXIT;
      end;
    end;
  end;

  TestCode.Free;
  SL.Free;

  ShowTestStats;
  TestLog('');
  ShowFileStats;
  TestLog('');
  TestLog('');
  TestLog('');
end;

procedure RunAllTests(Folder: String; StopOnError: Boolean);
var Files: TArray<String>;
  FileName: String;
  FullFolder: String;
begin
  Initialise;

  FullFolder := TPath.GetFullPath(Folder);

  Files := TDirectory.GetFiles(FullFolder, '*.tst');

  for FileName in Files do
    RunTestFile(TPath.Combine(FullFolder, FileName), StopOnError);

  TestLog('');
  TestLog('');
  TestLog('===========================');
  ShowTotalStats;
  TestLog('');
end;

end.

//IDE configuration

//This file won't be present when compiling for running on device. It should never
//be 'used' by compiler source. Settings for the compiler (e.g. compiler flags)
//will be found in the Globals unit

//Settings for the compiler itself are in the Globals unit.
unit IDE.Config;

interface
uses Classes,
  Def.Functions, Def.Variables;

type
  //Settings relating to the behaviour and/or state of the IDE/editor itself
  TIDESettings = record
    //The project (or file) currently open in the IDE
    //Can be a project file (if such a thing exists, or just a single Quiche file)
    ProjectFile: String;

    function SetValue(const Option, Value: String): Boolean;
    procedure AppendToStringlist(SL: TStringList);
  end;

  TCodeGenSettings = record
    //Show block information (for control flow)
    BlockInfo: Boolean;
    //Show original source code lines
    SourceCode: Boolean;
    //Show IL code lines
    ILCode: Boolean;
    //Show Fragments being used
    FragmentNames: Boolean;
    //Show primitives being selected
    PrimitiveNames: Boolean;
    //Show values currently allocated to registers
    CPUState: Boolean;
    //Show variable map (for each function)
    VarMap: Boolean;

    function SetValue(const Option, Value: String): Boolean;
    procedure AppendToStringlist(SL: TStringList);
  end;

  TCompilerConfig = record
    PlatformName: String;

    //CompilerOptions

    //If true, variables can be auto-created by any assignment to an un-declared
    //variable. Explicit type declarations are allowed when this option is used.
    //If not type inference will be used. Note that type declarations are ONLY
    //allowed the first time the variable is assigned.
    AllowAutoCreation: Boolean;

    //If enabled certain maths operations will be checked for overflow
    //Use the {$Q} compiler directive
    OverflowChecks: Boolean;
    //If enabled string and array expressions will be verified to be within the
    //defined bounds, and assignments to numeric and enumerated types will be
    //checked to be within range.
    //Use the ($R} compiler directive
    RangeChecks: Boolean;

    DefaultVarStorage: TVarStorage;
    DefaultCallingConvention: TCallingConvention;

    IDESettings: TIDESettings;
    CodeGen: TCodeGenSettings;

    procedure SaveToFile(const Filename: String);
    procedure LoadFromFile(const Filename: String);
  end;

  TDeploy = record
    //Shell command to run the output
    Run: String;
    //ConfigFile for emulator
    ConfigFile: String;

    procedure Clear;
    procedure LoadFromFile(const Filename: String);
  end;

implementation
uses SysUtils;

const
  scPlatform = 'Platform';
  scAllowautoCreation = 'AllowAutoCreation';
  scOverflowChecks = 'OverflowChecks';
  scRangeChecks = 'RangeChecks';
  scDefaultVarStorage = 'DefaultVarStorage';
    scStorageAbsolute = 'Absolute';
    scStorageRelative = 'Relative';


function BoolToStr(Value: Boolean): String;
begin
  if Value then
    Result := 'True'
  else
    Result := 'False';
end;

procedure WriteString(SL: TStringList;Option, Value: String);
begin
  SL.Add(Option + ': ' + Value);
end;

procedure WriteBool(SL: TStringList;Option: String; Value: Boolean);
begin
  WriteString(SL, Option, BoolToStr(Value));
end;

procedure TCompilerConfig.SaveToFile(const Filename: String);
var SL: TStringList;
begin
  SL := TStringList.Create;
  try
    WriteString(SL, scPlatform, PlatformName);
    WriteBool(SL, scAllowAutoCreation, AllowAutoCreation);
    WriteBool(SL, scOverflowChecks, OverFlowChecks);
    WriteBool(SL, scRangeChecks, RangeChecks);
    case DefaultVarStorage of
      vsStatic: WriteString(SL, scDefaultVarStorage, scStorageAbsolute);
      vsStack: WriteString(SL, scDefaultVarStorage, scStorageRelative);
    else
      Assert(True);
    end;

    IDESettings.AppendToStringList(SL);
    CodeGen.AppendToStringList(SL);

    SL.SaveToFile(Filename);
  finally
    SL.Free;
  end;
end;

function SplitLine(const Line: String;out Option, Value: String): Boolean;
var At: Integer;
begin
  At := Line.IndexOf(':');
  if At < 0 then
    EXIT(False);
  Option := Line.Substring(0, At);
  Value := Line.Substring(At+1).Trim;
  Result := True;
end;

function ValueToBoolean(Value: String): Boolean;
begin
  if Value <> '' then
    case Value.Chars[0] of
      'T','t': EXIT(True);
      'F','f': EXIT(False);
    end;
  raise Exception.Create('Invalid boolean value in compiler config file');
end;

procedure TCompilerConfig.LoadFromFile(const Filename: String);
var SL: TStringList;
  Line: String;
  Option: String;
  Value: String;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(Filename);

    for Line in SL do
    begin
      if not SplitLine(Line, Option, Value) then
        raise Exception.Create('Error in compiler config file in line: ' + Line);

      if Option = scPlatform then
        PlatformName := Value
      else if Option = scAllowAutoCreation then
        AllowAutoCreation := ValueToBoolean(Value)
      else if Option = scOverflowChecks then
        OverflowChecks := ValueToBoolean(Value)
      else if Option = scRangeChecks then
        RangeChecks := ValueToBoolean(Value)
      else if Option = scDefaultVarStorage then
      begin
        if Value = scStorageAbsolute then
          DefaultVarStorage := vsStatic
        else if Value = scStorageRelative then
          DefaultVarStorage := vsStack
        else
          Exception.Create('Unknown value in compiler config line: ' + Line)
      end
      else
        if not CodeGen.SetValue(Option, Value) then
        if not IDESettings.SetValue(Option, Value) then
          raise Exception.Create('Unknown compiler config option in line: ' + Line);
    end;
  finally
    SL.Free;
  end;
end;


{ TDeploy }

const
  scRun = 'Run';
  scConfigFile = 'ConfigFile';

procedure TDeploy.Clear;
begin
  ConfigFile := '';
  Run := '';
end;

procedure TDeploy.LoadFromFile(const Filename: String);
var SL: TStringList;
  Line: String;
  Option: String;
  Value: String;
begin
  Clear;
  SL := TStringList.Create;
  try
    SL.LoadFromFile(Filename);

    for Line in SL do
    begin
      if not SplitLine(Line, Option, Value) then
        raise Exception.Create('Error in Deploy file in line: ' + Line);

      if Option = scRun then
        Run := Value
      else if Option = scConfigFile then
        ConfigFile := value
      else
        raise Exception.Create('Unknown Deploy option in line: ' + Line);
    end;
  finally
    SL.Free;
  end;
end;

{ TCodeGenSettings }

const
  scBlockInfo = 'BlockInfo';
  scSourceCode = 'OutputSourceCode';
  scILCode = 'OutputILCode';
  scFragmentNames = 'OutputFragmentNames';
  scPrimitiveNames = 'OutputPrimitiveNames';
  scCPUState = 'OutputCPUState';
  scVarMap = 'VariableMap';

procedure TCodeGenSettings.AppendToStringlist(SL: TStringList);
begin
  WriteBool(SL, scBlockInfo, BlockInfo);
  WriteBool(SL, scSourceCode, SourceCode);
  WriteBool(SL, scILCode, ILCode);
  WriteBool(SL, scFragmentNames, FragmentNames);
  WriteBool(SL, scPrimitiveNames, PrimitiveNames);
  WriteBool(SL, scCPUState, CPUState);
  WriteBool(SL, scVarMap, VarMap);
end;

function TCodeGenSettings.SetValue(const Option, Value: String): Boolean;
begin
  Result := True;
  if Option = scBlockInfo then
    BlockInfo := ValueToBoolean(Value)
  else if Option = scSourceCode then
    SourceCode := ValueToBoolean(Value)
  else if Option = scILCode then
    ILCode := ValueToBoolean(Value)
  else if Option = scFragmentNames then
    FragmentNames := ValueToBoolean(Value)
  else if Option = scPrimitiveNames then
    PrimitiveNames := ValueToBoolean(Value)
  else if Option = scCPUState then
    CPUState := ValueToBoolean(Value)
  else if Option = scVarMap then
    VarMap := ValueToBoolean(Value)

  else
    Result := False;
end;

{ TIDESettings }

const
  scProjectFile = 'ProjectFile';

procedure TIDESettings.AppendToStringlist(SL: TStringList);
begin
  WriteString(SL, scProjectFile, ProjectFile);
end;

function TIDESettings.SetValue(const Option, Value: String): Boolean;
begin
  Result := True;
  if Option = scProjectFile then
    ProjectFile := Value
  else
    Result := False;
end;

end.

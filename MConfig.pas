//IDE configuration

//This file won't be present when compiling for running on device. It should never
//be 'used' by compiler source

//Settings for the compiler itself are in the Globals unit.
unit MConfig;

interface
uses Variables, Functions;

type TCompilerConfig = record
    PlatformName: String;

    //CompilerOptions

    //If true, variables can be auto-created by any assignment to an un-declared
    //variable. Explicit type declarations are allowed when this option is used.
    //If not type inference will be used. Note that type declarations are ONLY
    //allowed the first time the variable is assigned.
    AllowAutoCreation: Boolean;

    //If enable certain maths operations will be checked for overflow
    //Use the {$Q} compiler directive
    OverflowChecks: Boolean;

    DefaultVarStorage: TVarStorage;
    DefaultCallingConvention: TCallingConvention;

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
uses Classes, SysUtils;

const
  scPlatform = 'Platform';
  scAllowautoCreation = 'AllowAutoCreation';
  scOverflowChecks = 'OverflowChecks';
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
    case DefaultVarStorage of
      vsStatic: WriteString(SL, scDefaultVarStorage, scStorageAbsolute);
      vsStack: WriteString(SL, scDefaultVarStorage, scStorageRelative);
    else
      Assert(True);
    end;
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

end.

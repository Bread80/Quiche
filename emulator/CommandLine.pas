unit CommandLine;

interface
uses mHardware;



procedure DoCommandLine(Hardware: THardware);

implementation
uses SysUtils, Classes, mConfigFile;

type TCommandLine = class
  private
    Hardware: THardware;
  protected
  published
    constructor Create(AHardware: THardware);
    destructor Destroy;override;
    procedure LoadConfigFile(Filename: String);
    procedure Execute;
  end;

procedure DoCommandLine(Hardware: THardware);
var CL: TCommandLine;
begin
  CL := TCommandLine.Create(Hardware);
  CL.Execute;
  CL.Free;
end;

{ TCommandLine }

constructor TCommandLine.Create(AHardware: THardware);
begin
  inherited Create;
  Hardware := AHardware;
end;

destructor TCommandLine.Destroy;
begin
  inherited;
end;

procedure TCommandLine.Execute;
var P: String;
  I: Integer;
begin
  for I := 1 to ParamCount do
  begin
    P := ParamStr(I);
    if P.Chars[0] = '@' then
      LoadConfigFile(P)
    else
      raise Exception.Create('Illegal command line parameter: ' + P);
  end;
end;

procedure TCommandLine.LoadConfigFile(Filename: String);
var Config: TConfigFile;
begin
  Config := TConfigFile.Create(Hardware);
  Config.LoadConfigFile(Filename);
  Config.Free;
end;

end.

program quiche;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, FileUtil, LazFileutils,
  Def.Globals,
  Parse,
  IDE.Compiler, IDE.CommandLine;

type

  { TQuicheCC }

  TQuicheCC = class(TCustomApplication)
  private
  protected
    procedure SetCommandLineOptions(CommandLine: TCommandLine);
    function SelectPlatform(CommandLine: TCommandLine): Boolean;
    function SelectDeploy(CommandLine: TCommandLine): Boolean;
    function Compile(CommandLine: TCommandLine): Boolean;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp(CommandLine: TCommandLine);
  end;

{ TQuicheCC }

function TQuicheCC.Compile(CommandLine: TCommandLine): Boolean;
var SourceFile: String;
  Lines: TStringList;
begin
  SourceFile := CommandLine.GetNoOption.Value;
  writeln('Compiling file: ', SourceFile);

  Lines := TStringList.Create;
  try
    try
      Lines.LoadFromFile(SourceFile);
    except
      on E:Exception do
      begin
        writeln('Error: ', E.Message);
        EXIT(False);
      end;
    end;

    try
      IDE.Compiler.CompileStrings(Lines, btDefault, pmRootUnknown, False, True);

      if ParseErrorNo <> 0 then
      begin
        writeln(Lines[ParseErrorLine-1]);
        writeln(StringOfChar(' ', ParseErrorPos), '^');
        writeln(ParseErrorString);
        writeln(ParseErrorHelp);
      end
      else if AssembleError then
      begin
        writeln('Assembler error:');
        writeln(AssemblerLog);
      end
      else
      begin
        writeln('Compiled in ' + format('%.5f',[IDE.Compiler.CompileTime * 1000]), 'ms');
        if not Deploy(IDE.Compiler.GetBinaryFilename) then
          raise Exception.Create(IDE.Compiler.DeployError);
      end;
    except
      on E:Exception do
      begin
        writeln('Error: ', E.Message);
        EXIT(False);
      end;
    end;
  finally
    Lines.Free;
  end;

  Result := True;
end;

constructor TQuicheCC.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TQuicheCC.Destroy;
begin
  inherited Destroy;
end;

procedure TQuicheCC.DoRun;
var CommandLine: TCommandLine;
begin
  writeln('Quiche Language Compiler');
  writeln('  by Bread80 - http://bread80.com');
  writeln;

  CommandLine := TCommandLine.Create;
  try
    SetCommandLineOptions(CommandLine);
    CommandLine.Parse;

    //If the user asked for help we'll ignore any errors in the command line
    //to give assistance
    if CommandLine.HaveHelp then
    begin
      WriteHelp(CommandLine);
      Terminate;
      Exit;
    end;

    if CommandLine.HaveError then
    begin
      writeln('Error: ' + CommandLine.ErrorMsg);
      writeln;
      writeln('Run `quiche -h` for usage');
      Terminate;
      Exit;
    end;

    try
      IDE.Compiler.DefaultInitFolders;
      IDE.Compiler.Initialise(True, False);
    except
      on E:Exception do
      begin
        writeln('Error: ', E.Message);
        Terminate;
        Exit;
      end;
    end;

    if SelectPlatform(CommandLine) then
      if SelectDeploy(CommandLine) then
        if Compile(CommandLine) then
          //Deploy
          ;

  finally
    CommandLine.Free;
  end;

  // stop program loop
  Terminate;
end;

function TQuicheCC.SelectDeploy(CommandLine: TCommandLine): Boolean;
var Value: String;
  SL: TStringList;
  S: String;
begin
  if CommandLine.HasOption('deploy') then
  begin
    Value := CommandLine.GetValueOption('deploy').Value;
    if (Value <> '') and IDE.Compiler.SetDeploy(Value) then
        writeln('Deploy: ',Value)
    else
    begin
      if Value <> '' then
        writeln('Deploy ''',Value,''' not found');
      writeln('Available deploys for platform ''',ExtractFilename(GetPlatformFolder),''':');
      SL := TStringList.Create;
      try
        GetDeployList(SL);
        for S in SL do
          writeln(ExtractFilenameWithoutExt(ExtractFileName(S)));
      finally
        SL.Free;
      end;
      Exit(False);
    end;
  end;

  Result := True;
end;

function TQuicheCC.SelectPlatform(CommandLine: TCommandLine): Boolean;
var Value: String;
  SL: TStringList;
  S: String;
begin
  if CommandLine.HasOption('platform') then
  begin
    Value := CommandLine.GetValueOption('platform').Value;
    if (Value <> '') and IDE.Compiler.SetPlatform(Value) then
      writeln('Platform: ',Value)
    else
    begin
      if Value <> '' then
        writeln('Platform ''',Value,''' not found');
      writeln('Available platforms:');
      SL := TStringList.Create;
      try
        GetPlatformList(SL);
        for S in SL do
          writeln(ExtractFilename(S));
      finally
        SL.Free;
      end;
      EXIT(False);
    end;
  end;
  Result := True;
end;

procedure TQuicheCC.SetCommandLineOptions(CommandLine: TCommandLine);
begin
  CommandLine.AddMeta('<source-file>','',sdRequired, [sfRequired],
    'The source code file to compile', TCLNoOption);
  CommandLine.AddMeta('help','h',sdNone, [],
    'Show this message', TCLHelp);
  CommandLine.AddMeta('platform','p',sdOptional, [],
    'Select the target system. Leave value blank for a list of options', TCLValue);
  CommandLine.AddMeta('deploy','d',sdOptional, [],
      'Specify how to run the compiled code Leave value blank for a list of options', TCLValue);

  //Set command line options here
end;

procedure TQuicheCC.WriteHelp(CommandLine: TCommandLine);
begin
  writeln('Usage: quichecc <source-file> [Options]');
  writeln;
  writeln('Options:');
  writeln(CommandLine.GetOptionHelpText);
(*  writeln(' -h, --help - Show this message');
  writeln(' -p --platform <platform-name> - Set platforms (leave empty to list)');
  writeln(' -d --deploy <deploy-name> - Set deployment (leave empty to list)');
*)end;

var
  Application: TQuicheCC;
begin
  Application:=TQuicheCC.Create(nil);
  Application.Title:='Quiche Command Line Compiler';
  Application.Run;
  Application.Free;
end.


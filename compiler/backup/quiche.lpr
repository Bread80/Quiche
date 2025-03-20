program quiche;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, FileUtil, LazFileutils,
  Def.Globals,
  IDE.Compiler;

type

  { TQuicheCC }

  TQuicheCC = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TQuicheCC }

procedure TQuicheCC.DoRun;
const ShortOpts = 'dhp:';
  LongOpts: array of String = ('deploy','help','platform');
var
  ErrorMsg: String;
  Opts: TStringList;
  NonOptions: TStringArray;
  SourceFile: String;
  Lines: TStringList;
  Value: String;
  SL: TStringList;
  S: String;
begin
  writeln('Quiche Language Compiler');
  writeln('  by Bread80 - http://bread80.com');
  writeln;

  // quick check parameters
  ErrorMsg:=CheckOptions(ShortOpts, LongOpts);
  if ErrorMsg<>'' then
  begin
    writeln('Error: ', ErrorMsg);
    writeln;
    writeln('Run `quiche -h` for usage');
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  NonOptions := GetNonOptions(ShortOpts, LongOpts);
  if Length(NonOptions) = 0 then
    ErrorMsg := 'No source file specified'
  else if Length(NonOptions) > 1 then
    ErrorMsg := 'Too many source files specified'
  else
    SourceFile := NonOptions[0];

  if ErrorMsg <> '' then
  begin
    writeln('Error: ', ErrorMsg);
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

  writeln('Compiling file: ', SourceFile);

  if HasOption('p','platform') then
  begin
    Value := GetOptionValue('p','platform');
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
      Terminate;
      Exit;
    end;
  end;

  if HasOption('d','deploy') then
  begin
    Value := GetOptionValue('d','deploy');
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
      Terminate;
      Exit;
    end;
  end;

  Lines := TStringList.Create;
  try
    try
      Lines.LoadFromFile(SourceFile);
    except
      on E:Exception do
      begin
        writeln('Error: ', E.Message);
        Terminate;
        Exit;
      end;
    end;

    try
      IDE.Compiler.CompileStrings(Lines, btDefault, ptDeclarations, False, True);

      if ParseErrorNo <> 0 then
      begin
        write(ParseErrorString);
        writeln(' at line ', ParseErrorLine.ToString, ', ', ParseErrorPos.ToString);
        writeln(ParseErrorHelp);
      end
      else if AssembleError then
      begin
        writeln('Assembler error:');
        writeln(AssemblerLog);
      end
      else
        writeln('Compiled with no errors');
    except
      on E:Exception do
      begin
        writeln('Error: ', E.Message);
        Terminate;
        Exit;
      end;
    end;
  finally
    Lines.Free;
  end;

  // stop program loop
  Terminate;
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

procedure TQuicheCC.WriteHelp;
begin
  writeln('Usage: quichecc <source-file> [Options]');
  writeln;
  writeln('Options:');
  writeln(' -h, --help - Show this message');
  writeln(' -p --platform <platform-name> - Set platforms (leave empty to list)');
  writeln(' -d --deploy <deploy-name> - Set deployment (leave empty to list)');
end;

var
  Application: TQuicheCC;
begin
  Application:=TQuicheCC.Create(nil);
  Application.Title:='Quiche Command Line Compiler';
  Application.Run;
  Application.Free;
end.


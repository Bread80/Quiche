program quichecc;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
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
var
  ErrorMsg: String;
  NonOptions: TStringArray;
  SourceFile: String;
  Lines: TStringList;
begin
  writeln('Quiche Language Compiler');
  writeln('  by Bread80 - http://bread80.com');
  writeln;
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg = '' then
  begin
    NonOptions := GetNonOptions('h',['help']);
    if Length(NonOptions) = 0 then
      ErrorMsg := 'No source file specified'
    else if Length(NonOptions) > 1 then
      ErrorMsg := 'Too many source files specified'
    else
      SourceFile := NonOptions[0];
  end;

  // parse parameters
  if HasOption('h', 'help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if ErrorMsg<>'' then
  begin
    writeln('Error: ', ErrorMsg);
    writeln;
    writeln('Run `quiche -h` for usage');
    Terminate;
    Exit;
  end;

  //TODO: Do this properly
  SetQuicheFolder('c:\DropBox\Delphi\Quiche');
  Config.PlatformName := 'TestCase';
  OutputFolder := 'c:\RetroTools\Quiche';

  try
    IDE.Compiler.Initialise(True, False);
  except
    on E:Exception do
    begin
      writeln('Error: ', E.Message);
      Terminate;
      Exit;
  end;

  end;


  writeln('Compiling file: ', NonOptions[0]);

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
end;

var
  Application: TQuicheCC;
begin
  Application:=TQuicheCC.Create(nil);
  Application.Title:='Quiche Command Line Compiler';
  Application.Run;
  Application.Free;
end.


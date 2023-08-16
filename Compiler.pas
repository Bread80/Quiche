unit Compiler;

interface
uses Classes, MErrors;



var LastErrorNo: Integer;
var LastErrorLine: Integer;
var LastErrorPos: Integer;
//Output log of the assembler
var AssemblerLog: String;

var RunTimeError: Byte;
var RunTimeErrorAddress: Word;

//Base folder where Quiche data is stored
var QuicheFolder: String;
//Folder where output files will be written (assembler, binaries etc)
var OutputFolder: String;
//File of platform specific code to be included
var PlatformFileName: String;

//Fully qualified path - READ ONLY
var AssemblerFileName: String;
//Fully qualified path - READ ONLY
var BinaryFileName: String;

function LastErrorString: String;

type TCompileScope = (csBlock, csGlobal);


function CompileStrings(SL: TStrings;Scope: TCompileScope;InitDirectives: Boolean): Boolean;

//Compiles the given text to a binary file
//Returns true if everything succeeded
function CompileString(S: String;Scope: TCompileScope;InitDirectives: Boolean): Boolean;


procedure Initialise(InitDirectives: Boolean);

procedure SetOverflowChecks(Value: Boolean);

//Load source code
procedure LoadSourceFile(Filename: String);
procedure LoadSourceStrings(SL: TStrings);
procedure LoadSourceString(Source: String);

//Parse the code which has been loaded
//Returns True if parsing was successful, otherwise consult LastErrorNo and LastErrorString
function Parse(Scope: TCompileScope): Boolean;

//Return the IL data in text form
procedure GetILText(S: TStrings);

procedure GetScopeList(S: TStrings);

//Select a scope and make it the current scope
//(For use *only* when browsing compiled code)
//Returns True if the scope was found
function SelectScope(Name: String): Boolean;

//Dump the variables data to text form
procedure GetVarsText(S: TStrings;TypeSummary: Boolean);

procedure GetFunctionsText(S: TStrings);

procedure RunInterpreter;
procedure GetInterpreterOutput(S: TStrings);

procedure LoadCodeGenLibrary(Filename: String);

function DoCodeGen: Boolean;

procedure GetObjectCode(S: TStrings);

procedure SaveObjectCode(Filename: String);

function Assemble(Filename: String): Boolean;

function Emulate(Filename: String): Boolean;

implementation
uses SysUtils, IOUtils,
  Operators, PrimitivesEx, ILData, Variables, Parse, CodeGenZ80AsmEx,
  CodeLibrary, ILExec, Shell, ParserBase, Functions, Scopes;

var LastError: TAssembleError;

const scQuicheLibrary = 'Z80Code\quichecore.asm';
const scRAMDump = 'RAMDump.bin';  //In output folder

function LastErrorString: String;
begin
  if LastError = errCodeGen then
    Result := 'ERROR in CodeGen: ' + CodeGenErrorString
  else
    Result := 'ERROR: ' + LastErrorNo.ToString + ': ' + Errors[LastError] +
      ' at line ' + LastErrorLine.ToString + ' ' + LastErrorPos.ToString;
end;

procedure InitialiseDirectives;
begin
  optAllowAutoCreation := False;
  optOverflowChecks := True;
end;

procedure Initialise(InitDirectives: Boolean);
begin
  LastErrorNo := 0;
  LastError := errNone;
  if InitDirectives then
    InitialiseDirectives;

  InitialiseSkipMode;
  InitialiseVars;
  InitialiseScopes;
  InitialiseOperatorList;
  InitialisePrimitiveList;
  InitialiseCodeGen(TPath.Combine(QuicheFolder, PlatformFileName),
    TPath.Combine(QuicheFolder, scQuicheLibrary));
end;

procedure SetOverflowChecks(Value: Boolean);
begin
  optOverflowChecks := Value;
end;

procedure LoadSourceFile(Filename: String);
begin
  LoadFromFile(Filename);
end;

procedure LoadSourceStrings(SL: TStrings);
begin
  LoadFromStrings(SL);
end;

procedure LoadSourceString(Source: String);
begin
  LoadFromString(Source);
end;

procedure CodeGenCallback;
begin
  CodeGen(GetCurrentScope.Name, GetCurrentScope.Assembly);
end;

function Parse(Scope: TCompileScope): Boolean;
begin
  LastError := errNone;
  OnScopeDone := CodeGenCallback;

  if Scope = csGlobal then
    LastError := ParseGlobals(True)
  else
    while (LastError = errNone) and not ParserEOF do
      case Scope of
        csBlock: LastError := ParseBlock(bsSingle);
      else
        raise Exception.Create('Unknown compile scope in Compiler.Parse');
      end;
  LastErrorNo := Integer(LastError);
  LastErrorLine := ErrorLineNo;
  LastErrorPos := ErrorPos;
  Result := LastError = errNone;
end;

procedure GetILText(S: TStrings);
begin
  ILToStrings(S);
end;

procedure GetScopeList(S: TStrings);
begin
  ScopesToStrings(S);
end;

function SelectScope(Name: String): Boolean;
begin
  Result := ScopeSelectByName(Name);
end;

procedure GetVarsText(S: TStrings;TypeSummary: Boolean);
begin
  VarsToStrings(S, TypeSummary);
  if RunTimeError = 0 then
    S.Insert(0, 'Runtime error 0')
  else
  begin
    S.Insert(0,'');
    S.Insert(0, 'RUNTIME ERROR ' + IntToStr(RunTimeError) + ' at $' + IntToHex(RunTimeErrorAddress,4));
  end;
end;

procedure GetFunctionsText(S: TStrings);
begin
  FunctionsToStrings(S);
end;

procedure RunInterpreter;
begin
  Execute;
end;

procedure GetInterpreterOutput(S: TStrings);
begin
  S.Assign(ExecOutput);
end;


procedure LoadCodeGenLibrary(Filename: String);
begin
  LoadLibrary(Filename);
end;

function DoCodeGen: Boolean;
begin
  LastError := CodeGen(GetCurrentScope.Name, GetCurrentScope.Assembly);
  Result := LastError = errNone;
  LastErrorNo := Integer(LastError);
end;

procedure GetObjectCode(S: TStrings);
begin
  if Assigned(GetCurrentScope.Assembly) then
    S.Assign(GetCurrentScope.Assembly);
end;

procedure SaveObjectCode(Filename: String);
begin
  AssemblerFilename := Filename;

  SaveAssemblyFile(Filename);
end;

function Assemble(Filename: String): Boolean;
begin
  BinaryFilename := TPath.ChangeExtension(Filename, '.bin');
  AssemblerLog := Shell.Assemble(Filename);
  Result := not (AssemblerLog.Contains('failed') or AssemblerLog.Contains('Error'));
end;

//Compile source in parser
function CompileParser(Scope: TCompileScope): Boolean;
begin
  //CodeGen
  LoadCodeGenLibrary(TPath.Combine(QuicheFolder, 'Z80Code/Assembly.txt'));

  Result := Compiler.Parse(Scope);
  if not Result then
    EXIT;

  Result := DoCodeGen;
  SaveObjectCode(TPath.Combine(OutputFolder, 'quicheoutput.asm'));
  if not Result then
    EXIT;

  //Assemble
  Result := Compiler.Assemble(AssemblerFileName);
end;

function CompileStrings(SL: TStrings;Scope: TCompileScope;InitDirectives: Boolean): Boolean;
begin
  Initialise(InitDirectives);
  LoadSourceStrings(SL);

  Result := CompileParser(Scope);
end;

function CompileString(S: String;Scope: TCompileScope;InitDirectives: Boolean): Boolean;
begin
  //Initialise
  Initialise(InitDirectives);

  //Parse
  LoadSourceString(S);

  Result := CompileParser(Scope);
end;

function Emulate(Filename: String): Boolean;
begin
  Shell.Emulate(Filename);

  Variables.LoadVarsFromMemoryDump(TPath.Combine(OutputFolder, scRAMDump), {IX}$b000,
    RunTimeError, RunTimeErrorAddress);
  Result := RuntimeError = rerrNone;
end;

initialization
//  Initialise;
end.


unit Compiler;

interface
uses Classes, ParseErrors;

var LastErrorNo: Integer;
var LastErrorLine: Integer;
var LastErrorPos: Integer;
var WriteBuffer: String;    //Text written to output

//Output log of the assembler
var AssemblerLog: String;
  AssembleError: Boolean;

var RunTimeError: Byte;
var RunTimeErrorAddress: Word;

//Folder where output files will be written (assembler, binaries etc)
var OutputFolder: String;

//Fully qualified path - READ ONLY
var AssemblerFileName: String;
//Fully qualified path - READ ONLY
var BinaryFileName: String;

//Set the folder at the base of the Quiche directory tree
procedure SetQuicheFolder(const Folder: String);

procedure SetPlatform(const APlatform: String);

function GetTestsFolder: String;

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

procedure LoadFragmentsLibrary(Filename: String);

function DoCodeGen: Boolean;

procedure GetObjectCode(S: TStrings);

procedure SaveObjectCode(Filename: String);

function Assemble(Filename: String): Boolean;

function Emulate(Filename: String): Boolean;

implementation
uses SysUtils, IOUtils,
  Operators, PrimitivesEx, ILData, Variables, Parse, CodeGenZ80AsmEx,
  Fragments, ILExec, Shell, ParserBase, Functions, Scopes, Globals;

const
  FragmentsFilename = 'Data/Fragments.txt';
  OperatorsFilename = 'Data/Operators.csv';
  PrimitivesFilename = 'Data/Primitives.csv';

  QuicheCoreFilename = 'Assembler/QuicheCore.asm';
  PlatformsBaseFolder = 'Platforms';
  //Platform library is in PlatformFolder
  PlatformLibraryFilename = 'Main.asm';

const scRAMDump = 'RAMDump.bin';  //In output folder

//Base folder where Quiche data is stored
var QuicheFolder: String;
//File of platform specific code to be included
var PlatformFolder: String;
var PlatformName: String;

procedure SetQuicheFolder(const Folder: String);
begin
  QuicheFolder := Folder;
end;

procedure SetPlatform(const APlatform: String);
begin
  PlatformName := APlatform;
  PlatformFolder := TPath.Combine(QuicheFolder, PlatformsBaseFolder);
  PlatformFolder := TPath.Combine(PlatformFolder, PlatformName);
end;

function GetTestsFolder: String;
begin
  Result := TPath.Combine(QuicheFolder, 'Tests');
end;

function LastErrorString: String;
begin
  if LastError <> qeNone then
    Result := 'ERROR: ' + LastErrorNo.ToString + ': ' + LastErrorMessage +
      ' at line ' + LastErrorLine.ToString + ' ' + LastErrorPos.ToString
  else if CodeGenErrorString <> '' then
    Result := 'ERROR in CodeGen: ' + CodeGenErrorString
  else
    Result := '';
end;

procedure InitialiseDirectives;
begin
  optAllowAutoCreation := False;
  optOverflowChecks := True;
  optDefaultVarStorage := vsRelative;
end;

procedure Initialise(InitDirectives: Boolean);
begin
  LastErrorNo := 0;
  LastError := qeNone;
  if InitDirectives then
    InitialiseDirectives;

  InitialiseSkipMode;
  InitialiseVars;
  InitialiseScopes;
  InitialiseOperators;
  LoadOperatorsFile(TPath.Combine(QuicheFolder, OperatorsFilename));
  InitialiseFragments;
  LoadFragmentsLibrary(TPath.Combine(QuicheFolder, FragmentsFilename));
  InitialisePrimitives;
  LoadPrimitivesFile(TPath.Combine(QuicheFolder, PrimitivesFilename));

  InitialiseCodeGen(TPath.Combine(PlatformFolder, 'Assembler/' + PlatformName + '.asm'),
    TPath.Combine(QuicheFolder, QuicheCoreFilename));
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

function CodeGenCallback: Boolean;
var Scope: PScope;
begin
  Scope := GetCurrentScope;
  Result := CodeGen(Scope.Name, Scope.AsmCode, Scope.AsmData);
end;

function Parse(Scope: TCompileScope): Boolean;
begin
  LastError := qeNone;
  OnScopeDone := CodeGenCallback;

  if Scope = csGlobal then
    LastError := ParseGlobals(True)
  else
    while (LastError = qeNone) and not ParserEOF do
      case Scope of
        csBlock: LastError := ParseBlock(bsSingle, vsRelative);
      else
        raise Exception.Create('Unknown compile scope in Compiler.Parse');
      end;
  LastErrorNo := Integer(LastError);
  LastErrorLine := ErrorLineNo;
  LastErrorPos := ErrorPos;
  Result := LastError = qeNone;
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


procedure LoadFragmentsLibrary(Filename: String);
begin
  Fragments.LoadFragmentsFile(Filename);
end;

function DoCodeGen: Boolean;
var Scope: PScope;
begin
  Scope := GetCurrentScope;
  Result := CodeGen(Scope.Name, Scope.AsmCode, Scope.AsmData);
//  LastErrorNo := Integer(LastError);
end;

procedure GetObjectCode(S: TStrings);
var Scope: PScope;
begin
  Scope := GetCurrentScope;
  if Assigned(Scope.AsmCode) then
  begin
    S.Assign(Scope.AsmCode);
//    S.Assign(GetCurrentScope.Assembly);
    S.Append(Scope.AsmData.Text);
  end;
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
  AssembleError := AssemblerLog.Contains('failed') or AssemblerLog.Contains('Error');
  Result := not AssembleError;
end;

//Compile source in parser
function CompileParser(Scope: TCompileScope): Boolean;
begin
  //CodeGen
//  LoadFragmentsLibrary(TPath.Combine(QuicheFolder, FragmentsFilename));

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
const StackBase = $b000;
begin
  Shell.Emulate(Filename);

  WriteBuffer :=  Variables.LoadVarsFromMemoryDump(TPath.Combine(OutputFolder, scRAMDump), StackBase,
    RunTimeError, RunTimeErrorAddress);
  Result := RuntimeError = rerrNone;
end;

initialization
//  Initialise;
end.


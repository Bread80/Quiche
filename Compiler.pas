unit Compiler;

interface
uses Classes, ParseErrors, MConfig, Globals;

//====Errors and return values

//Parser
var LastErrorNo: Integer;
var LastErrorLine: Integer;
var LastErrorPos: Integer;
function LastErrorString: String;

//Output log of the assembler
var AssemblerLog: String;
  AssembleError: Boolean;

//Emulator
var WriteBuffer: String;    //Text written to output
var RunTimeError: Byte;
var RunTimeErrorAddress: Word;

//====Config
var Config: TCompilerConfig;

//Folder where output files will be written (assembler, binaries etc)
var OutputFolder: String;

//Fully qualified path - READ ONLY
var AssemblerFileName: String;
//Fully qualified path - READ ONLY
var BinaryFileName: String;

const DeployFolderName = 'Deploy';

var Deploy: TDeploy;

//Set the folder at the base of the Quiche directory tree
procedure SetQuicheFolder(const Folder: String);
function GetQuicheFolder: String;

procedure GetPlatformList(List: TStrings);
function GetPlatformFolder: String;

function GetTestsFolder: String;

//====Parser to Assembler

//Initialise system.
//Usually called as part of CompileString(s). Tester calls this to perform a cold
//init, calling CompileString(s) for each test with a warm init for speed reasons
//Entry:
//  InitDirectives, if True (recommended) resets directives to standard settings
//  WarmInit, if True (not recommended) skips various steps, such as loading various
//  data files (operators, primitives etc).
procedure Initialise(InitDirectives, WarmInit: Boolean);

type TParseType = (
  //Parse at the declarations level. Allows functions and globals. Requires BEGIN...END. block
  ptDeclarations,
  //Parse at the code level. Doesn't allow functions or globals. Doesn't require a block of any kind
  ptCode);

//'One click' compile to binary
//Returns False if there was an error
function CompileStrings(SL: TStrings;BlockType: TBlockType;ParseType: TParseType;
  InitDirectives, WarmInit: Boolean): Boolean;
function CompileString(S: String;BlockType: TBlockType;ParseType: TParseType;
  InitDirectives, WarmInit: Boolean): Boolean;

//Run the interpreter (no longer used or updated)
procedure RunInterpreter;
procedure GetInterpreterOutput(S: TStrings);

//Run the emulator
//Returns False if there was an error whilst trying to run the emulator
function Emulate(Filename: String): Boolean;

//====Query compiler data

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

procedure GetObjectCode(S: TStrings);

procedure SaveObjectCode(Filename: String);

implementation
uses SysUtils, IOUtils,
  Operators, PrimitivesEx, ILData, Variables, Parse, CodeGenZ80AsmEx,
  Fragments, ILExec, Shell, Emulator, ParserBase, Functions, Scopes;

//====Errors and return values

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

//====Config

const
  FragmentsFilename = 'Data/Fragments.txt';
  OperatorsFilename = 'Data/Operators.csv';
  OperatorsNGFilename = 'Data/OperatorsNG.csv';
  PrimitivesFilename = 'Data/Primitives.csv';
  PrimitivesNGFilename = 'Data/PrimitivesNG.csv';

  QuicheCoreFilename = 'Assembler/QuicheCore.asm';
  PlatformsBaseFolder = 'Platforms';
  //Platform library is in PlatformFolder
//  PlatformLibraryFilename = 'Main.asm';

const scRAMDump = 'RAMDump.bin';  //In output folder (when shelling to emulator as separate process)

//Base folder where Quiche data is stored
var QuicheFolder: String;
//File of platform specific code to be included
var PlatformFolder: String;

procedure SetQuicheFolder(const Folder: String);
begin
  QuicheFolder := Folder;
end;

function GetQuicheFolder: String;
begin
  Result := QuicheFolder;
end;

procedure GetPlatformList(List: TStrings);
var Dirs: TArray<String>;
  S: String;
begin
  Dirs := TDirectory.GetDirectories(TPath.Combine(QuicheFolder, PlatformsBaseFolder));
  List.Clear;
  for S in Dirs do
    List.Add(TPath.GetFilename(S));
end;

function GetPlatformFolder: String;
begin
  PlatformFolder := TPath.Combine(QuicheFolder, PlatformsBaseFolder);
  PlatformFolder := TPath.Combine(PlatformFolder, Config.PlatformName);

  Result := PlatformFolder;
end;

function GetTestsFolder: String;
begin
  Result := TPath.Combine(QuicheFolder, 'Tests');
end;

//====Initialisation

function CodeGenCallback: Boolean;
var Scope: PScope;
begin
  Scope := GetCurrentScope;
  Result := CodeGen(Scope, btDefault);
end;

procedure LoadFragmentsLibrary(Filename: String);
begin
  Fragments.LoadFragmentsFile(Filename);
end;

procedure DoInitDirectives;
begin
  optAllowAutoCreation := Config.AllowAutoCreation;
  optOverflowChecks := Config.OverflowChecks;
  optDefaultVarStorage := Config.DefaultVarStorage;
end;

procedure Initialise(InitDirectives, WarmInit: Boolean);
begin
  LastErrorNo := 0;
  LastError := qeNone;
  Parse.OnScopeDone := CodeGenCallback;
  if InitDirectives then
    DoInitDirectives;

  InitialiseSkipMode;
  InitialiseVars;
  InitialiseScopes;

  if not WarmInit then
  begin
    InitialiseOperators;
    LoadOperatorsFile(TPath.Combine(QuicheFolder, OperatorsFilename));
    LoadOperatorsFileNG(TPath.Combine(QuicheFolder, OperatorsNGFilename));
    InitialiseFragments;
    LoadFragmentsLibrary(TPath.Combine(QuicheFolder, FragmentsFilename));
    InitialisePrimitives;
    LoadPrimitivesFile(TPath.Combine(QuicheFolder, PrimitivesFilename));
    LoadPrimitivesNGFile(TPath.Combine(QuicheFolder, PrimitivesNGFilename));
  end;

  InitialiseCodeGen(TPath.Combine(GetPlatformFolder, 'Assembler/' + Config.PlatformName + '.asm'),
    TPath.Combine(QuicheFolder, QuicheCoreFilename));
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

//====Compilation

//Parse the code which has been loaded
//Returns True if parsing was successful, otherwise consult LastErrorNo and LastErrorString
function Parse(BlockType: TBlockType;ParseType: TParseType): Boolean;
begin
  LastError := qeNone;

  //Declaration level
  case ParseType of
    ptDeclarations:
      case BlockType of
        btDefault: LastError := ParseDeclarations(True, True, optDefaultVarStorage);
        btStatic:  LastError := ParseDeclarations(True, True, vsStatic);
        btStack:   LastError := ParseDeclarations(True, True, vsStack);
      end;
    ptCode:
    //Block level
    while (LastError = qeNone) and not ParserEOF do
      case BlockType of
        btDefault: LastError := ParseBlock(bsSingle, optDefaultVarStorage);
        btStack: LastError := ParseBlock(bsSingle, vsStack);
        btStatic: LastError := ParseBlock(bsSingle, vsStatic);
      else
        raise Exception.Create('Unknown compile scope in Compiler.Parse');
      end;
  end;

  LastErrorNo := Integer(LastError);
  LastErrorLine := ErrorLineNo;
  LastErrorPos := ErrorPos;
  Result := LastError = qeNone;
end;

function DoCodeGen(BlockType: TBlockType): Boolean;
var Scope: PScope;
begin
  Scope := GetCurrentScope;
  Result := CodeGen(Scope, BlockType);
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
  AssembleError := AssemblerLog.Contains('failed') or AssemblerLog.Contains('Error: [') or AssemblerLog.Contains('Warning: [');
  Result := not AssembleError;
end;

//Compile source in parser
function Compile(BlockType: TBlockType;ParseType: TParseType): Boolean;
begin
  //CodeGen
  Result := Compiler.Parse(BlockType, ParseType);
  if not Result then
    EXIT;

  Result := DoCodeGen(BlockType);
  SaveObjectCode(TPath.Combine(OutputFolder, 'quicheoutput.asm'));
  if not Result then
    EXIT;

  //Assemble
  Result := Compiler.Assemble(AssemblerFileName);
end;

function CompileStrings(SL: TStrings;BlockType: TBlockType;ParseType: TParseType;
  InitDirectives, WarmInit: Boolean): Boolean;
begin
  Initialise(InitDirectives, WarmInit);
  LoadSourceStrings(SL);

  Result := Compile(BlockType, ParseType);
end;

function CompileString(S: String;BlockType: TBlockType;ParseType: TParseType;
  InitDirectives, WarmInit: Boolean): Boolean;
begin
  //Initialise
  Initialise(InitDirectives, WarmInit);

  //Parse
  LoadSourceString(S);

  Result := Compile(BlockType, ParseType);
end;

//====Interpreter (deprecated) and Emulator

procedure RunInterpreter;
begin
  Execute;
end;

procedure GetInterpreterOutput(S: TStrings);
begin
  S.Assign(ExecOutput);
end;


function Emulate(Filename: String): Boolean;
const
  StackBase = $f000;
  StackFrameSize = 4; //Return address and previous IX
begin
  if Deploy.ConfigFile <> '' then
  begin //Use inbuilt emulator
    Emulator.Initialise(Deploy.ConfigFile);
    Emulator.RunToHalt;
    Emulator.TryReadByte('LAST_ERROR_CODE', RunTimeError);
    Emulator.TryReadWord('LAST_ERROR_ADDR', RunTimeErrorAddress);
    Emulator.GetVarData(VarGetParamsByteSize + StackFrameSize);
  end
  else
  begin
    if Deploy.Run = '' then
    begin
//    ShowMessage('No deployment specified');
      EXIT(False);
    end;

    Shell.Emulate(Deploy.Run, 'C:\');

    WriteBuffer :=  Variables.LoadVarsFromMemoryDump(TPath.Combine(OutputFolder, scRAMDump),
      StackBase - StackFrameSize, RunTimeError, RunTimeErrorAddress);
  end;

  Result := True;
end;


//====Query data (after compiling)

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

initialization
  //Default values for compiler options
  Config.AllowAutoCreation := False;
  Config.OverflowChecks := True;
  Config.DefaultVarStorage := vsStack;//vsAbsolute;
  Config.DefaultCallingConvention := ccStackLocal;
end.


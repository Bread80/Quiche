//{$ifndef fpc}
  {$define EMULATOR}
//{$endif}
//This file gives a 'high level' overview of the compiler, and gives a convenient
//interface for using it from the IDE.
//This is not part of the compiler 'proper'. Ie it won't be present in on-device
//builds.
//It provides access to the compiler, data structures, code generator, assembler
//emulator, running (deployments) etc., as well as making errors and error messages
//easy to access and understand.
unit IDE.Compiler;

interface
uses Classes,
  Def.Globals,
  Parse.Errors,
  Z80.GenProcs,
  IDE.Config;

//==================Errors and return values

//Parser
var ParseErrorNo: Integer;  //0 = no error
var ParseErrorLine: Integer;
var ParseErrorPos: Integer;
function ParseErrorString: String;
function ParseErrorHelp: String;

//Output log of the assembler
var AssemblerLog: String;
  AssembleError: Boolean;

//Emulator
var WriteBuffer: String;    //Text written to output
var RunTimeError: Byte;
var RunTimeErrorAddress: Word;


//==================Config

//Set the folder at the base of the Quiche directory tree
procedure SetQuicheFolder(const Folder: String);
function GetQuicheFolder: String;

//Returns the list of available Platforms
procedure GetPlatformList(List: TStrings);
function GetPlatformFolder: String;
function SetPlatform(const PlatformName: String): Boolean;
procedure SetPlatformToDefault;

//For the current platform
procedure GetDeployList(List: TStrings);
procedure ClearDeploy;
function SetDeploy(const DeployName: String): Boolean;
function GetDeployFolder: String;

function GetTestsFolder: String;

function GetConfigFilename: String;
function GetConfig: PCompilerConfig;

//The filename of the compiled output binary
function GetBinaryFilename: String;

//Call at startup to initialise default folder locations
procedure DefaultInitFolders;

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

const ParseTypeStrings: array[low(TParseType)..high(TParseType)] of String = (
  'Declarations', 'Code');

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
{$ifdef EMULATOR}
function Emulate(Filename: String): Boolean;
{$endif}

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
uses SysUtils, {$ifdef fpc}FileUtil,{$else}IOUtils,{$endif}
  Def.Functions, Def.IL, Def.Intrinsics, Def.Operators, Def.Scopes,
  Def.Variables, Def.Consts, Def.QTypes,
  Parse, Parse.Base,
  Lib.Data, Lib.Primitives,
  CodeGen, CG.Data,
  CleverPuppy, Z80.AlgoData,
  {$IFDEF EMULATOR}IDE.Emulator, {$endif}
  IDE.ILExec, IDE.Shell;

//====Errors and return values

function ParseErrorString: String;
begin
  if LastError <> qeNone then
    Result := 'ERROR: ' + ParseErrorNo.ToString + ': ' + LastErrorMessage +
      ' at line ' + ParseErrorLine.ToString + ' ' + ParseErrorPos.ToString
  else if CodeGenErrorString <> '' then
    Result := 'ERROR in CodeGen: ' + CodeGenErrorString
  else
    Result := '';
end;

function ParseErrorHelp: String;
begin
  if LastError <> qeNone then
    Result := LastErrorHelp
  else
    Result := '';
end;
//====Config

var ConfigFilename: String;
var Config: TCompilerConfig;

//Folder containing the compiler executable (ie us)
var BinFolder: String;

//Folder where output files will be written (assembler, binaries etc)
var OutputFolder: String;

//Fully qualified path - READ ONLY
var AssemblerFileName: String;
//Fully qualified path - READ ONLY
var BinaryFileName: String;

const DeployFolderName = 'Deploy';
  DeployExtension = '.deploy';

var Deploy: TDeploy;
const
  DefaultPlatformName = 'Default';

  ErrorsFilename = 'Errors.txt';
  FragmentsFilename = 'Fragments.txt';
  OperatorsFilename = 'OperatorsNG.csv';
  PrimitivesFilename = 'PrimitivesNG.csv';
  IntrinsicsFilename = 'Intrinsics.csv';
  AlgoDataFilename = 'AlgoData.txt';

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
{$ifndef fpc}
var Dirs: TArray<String>;
  S: String;
{$endif}
begin
{$ifdef fpc}
  FindAllDirectories(List, ConcatPaths([GetQuicheFolder, PlatformsBaseFolder]), False, ';');
{$else}
  Dirs := TDirectory.GetDirectories(TPath.Combine(GetQuicheFolder, PlatformsBaseFolder));
  List.Clear;
  for S in Dirs do
    List.Add(TPath.GetFilename(S));
{$endif}
end;

function GetPlatformFolder: String;
begin
{$ifdef fpc}
  PlatformFolder := ConcatPaths([QuicheFolder, PlatformsBaseFolder, Config.PlatformName]);
{$else}
  PlatformFolder := TPath.Combine(QuicheFolder, PlatformsBaseFolder);
  PlatformFolder := TPath.Combine(PlatformFolder, Config.PlatformName);
{$endif}
  Result := PlatformFolder;
end;

function SetPlatform(const PlatformName: String): Boolean;
begin
  Config.PlatformName := PlatformName;
  //TODO: Return False if folder does not exist
  Result := DirectoryExists(GetPlatformFolder);
end;

procedure SetPlatformToDefault;
begin
  SetPlatform(DefaultPlatformName);
end;

procedure GetDeployList(List: TStrings);
{$ifndef fpc}
var Dirs: TArray<String>;
  S: String;
{$endif}
begin
{$ifdef fpc}
  FindAllFiles(List, GetDeployFolder, '*' + DeployExtension, False);
{$else}
  Dirs := TDirectory.GetFiles(GetDeployFolder, '*' + DeployExtension);
  List.Clear;
  for S in Dirs do
    List.Add(TPath.GetFilename(S));
{$endif}
end;

procedure ClearDeploy;
begin
  Deploy.Clear;
end;

function SetDeploy(const DeployName: String): Boolean;
var Name: String;
begin
{$ifdef fpc}
  if ExtractFileExt(DeployName) <> '' then
{$else}
  if TPath.HasExtension(DeployName) then
{$endif}
    Name := DeployName
  else
    Name := DeployName + DeployExtension;

{$ifdef fpc}
  Name := ConcatPaths([GetDeployFolder, Name]);
{$else}
  Name := TPath.Combine(GetDeployFolder, Name);
{$endif}

  Result := FileExists(Name);
  if not Result then
    EXIT;

  ClearDeploy;
{$ifdef fpc}
  Deploy.LoadFromFile(Name);
{$else}
  Deploy.LoadFromFile(Name);
{$endif}
end;

function GetDeployFolder: String;
begin
{$ifdef fpc}
  Result := ConcatPaths([GetPlatformFolder, DeployFolderName]);
{$else}
  Result := TPath.Combine(IDE.Compiler.GetPlatformFolder, DeployFolderName)
{$endif}
end;


function GetTestsFolder: String;
begin
{$ifdef fpc}
  Result := ConcatPaths([BinFolder, 'Tests']);
{$else}
  Result := TPath.Combine(BinFolder, 'Tests');
{$endif}
end;

function GetConfigFilename: String;
begin
  Result := ConfigFilename;
end;

function GetConfig: PCompilerConfig;
begin
  Result := @Config;
end;

function GetBinaryFilename: String;
begin
  Result := BinaryFilename;
end;

procedure DefaultInitFolders;
var Folder: String;
begin
{$ifdef fpc}
//<Base>/redist/bin
  BinFolder := ProgramDirectory;// ExtractFilePath(ParamStr(0)));
//<Base>/redist
  SetQuicheFolder(ExpandFileName(ConcatPaths([BinFolder, '..\'])));
  ConfigFileName := ConcatPaths([GetQuicheFolder, 'Config','Compiler.cfg']);
{$else}
//<Base>/redist/bin
  BinFolder := TPath.GetFullPath(TPath.GetDirectoryName(ParamStr(0)));
  //<Base>/redist
  SetQuicheFolder(TPath.GetFullPath(TPath.Combine(BinFolder, '..\')));
  ConfigFilename := TPath.Combine(GetQuicheFolder, 'Config\Compiler.cfg');
{$endif}
  Config.LoadFromFile(ConfigFilename);

  //TODO
  IDE.Compiler.OutputFolder := 'C:\RetroTools\Quiche';
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
function DoParse(BlockType: TBlockType;ParseType: TParseType): Boolean;
var ParseMode: TParseMode;
begin
  LastError := qeNone;
  case ParseType of
    ptDeclarations: ParseMode := pmProgram;
    ptCode: ParseMode := pmRootUnknown;
  else
    Assert(False);
  end;

  try
    case BlockType of
      btDefault: LastError := ParseQuiche(ParseMode, optDefaultVarStorage);
      btStatic:  LastError := ParseQuiche(ParseMode, vsStatic);
      btStack:   LastError := ParseQuiche(ParseMode, vsStack);
    end;
  except
    on E:Exception do
      LastError := ErrBUG('(Exception/Assertion):'+#13+E.Message);
  end;

  ParseErrorNo := Integer(LastError);
  ParseErrorLine := ErrorLineNo;
  ParseErrorPos := ErrorPos;
  Result := LastError = qeNone;
end;

function DoCodeGen(BlockType: TBlockType): Boolean;
var Scope: PScope;
begin
  Scope := GetCurrentScope;
  if optCleverPuppy then
  begin
    Scope.CleverPuppy := TCleverPuppy.Create;
    Scope.CleverPuppy.ProcessSection;
  end;

  Result := CodeGenSection(Scope, BlockType);
//  LastErrorNo := Integer(LastError);
end;

function CodeGenCallback: Boolean;
begin
  Result := DoCodeGen(btDefault);
end;
(*var Scope: PScope;
begin
  Scope := GetCurrentScope;
  Result := CodeGenSection(Scope, btDefault);
end;
*)
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
{$ifdef fpc}
  BinaryFilename := ChangeFileExt(Filename, '.bin');
{$else}
  BinaryFilename := TPath.ChangeExtension(Filename, '.bin');
{$endif}
  AssemblerLog := IDE.Shell.Assemble(Filename);
  AssembleError := AssemblerLog.Contains('failed') or AssemblerLog.Contains('Error: [') or AssemblerLog.Contains('Warning: [');
  Result := not AssembleError;
end;

//Compile source in parser
function Compile(BlockType: TBlockType;ParseType: TParseType): Boolean;
begin
  //CodeGen
  Result := IDE.Compiler.DoParse(BlockType, ParseType);
  if not Result then
    EXIT;

  Result := DoCodeGen(BlockType);
{$ifdef fpc}
  SaveObjectCode(ConcatPaths([OutputFolder, 'quicheoutput.asm']));
{$else}
  SaveObjectCode(TPath.Combine(OutputFolder, 'quicheoutput.asm'));
{$endif}
  if not Result then
    EXIT;

  //Assemble
  Result := IDE.Compiler.Assemble(AssemblerFileName);
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

{$ifdef EMULATOR}
function Emulate(Filename: String): Boolean;
const //For inbuilt emulator
  StackBase = $f000;
  StackFrameSize = 4; //Return address and previous IX
begin
  if Deploy.Run = '' then
  begin //Use inbuilt emulator
    IDE.Emulator.Initialise(Deploy.GetConfigFile);
    IDE.Emulator.RunToHalt;
    IDE.Emulator.TryReadByte('LAST_ERROR_CODE', RunTimeError);
    IDE.Emulator.TryReadWord('LAST_ERROR_ADDR', RunTimeErrorAddress);
    IDE.Emulator.GetVarData(VarGetParamsByteSize + StackFrameSize);
  end
  else
  begin
{$ifdef fpc}
    raise Exception.Create('Shelling to an emulator is currently unsupported');
{$else}
    IDE.Shell.Emulate(Deploy.Run, 'C:\');
{$endif}
{
    WriteBuffer :=  Variables.LoadVarsFromMemoryDump(TPath.Combine(OutputFolder, scRAMDump),
      StackBase - StackFrameSize, RunTimeError, RunTimeErrorAddress);
}  end;

  Result := True;
end;
{$endif}

//====Query data (after compiling)

procedure GetILText(S: TStrings);
var Scope: PScope;
begin
  ILToStrings(S);
  Scope := GetCurrentScope;
  if Assigned(Scope.CleverPuppy) then
  begin
    S.Add(Scope.CleverPuppy.ToString);
    S.Add(Scope.CleverPuppy.Log.Text);
  end;
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


//====Initialisation

procedure LoadFragmentsLibrary(Filename: String);
begin
  LoadFragmentsFile(Filename);
end;

procedure DoInitDirectives;
begin
  optVarAutoCreate := Config.AllowAutoCreation;
  optOverflowChecks := Config.OverflowChecks;
  optRangeChecks := Config.RangeChecks;
  optDefaultVarStorage := Config.DefaultVarStorage;
  optDefaultCallingConvention := Config.DefaultCallingConvention;
  optDefaultSignedInteger := True;
  optDefaultSmallestInteger := False;
  optCleverPuppy := False;
end;

procedure Initialise(InitDirectives, WarmInit: Boolean);
begin
  ParseErrorNo := 0;
  LastError := qeNone;
  Parse.OnScopeDone := CodeGenCallback;
  if InitDirectives then
    DoInitDirectives;

  InitialiseSkipMode;
  InitialiseConsts;
  InitialiseVars;
  InitialiseScopes;

  if not WarmInit then
  begin
{$ifdef fpc}
    LoadErrorData(ConcatPaths([BinFolder, ErrorsFilename]));
{$else}
    LoadErrorData(TPath.Combine(BinFolder, ErrorsFilename));
{$endif}
    InitialiseOperators;
{$ifdef fpc}
    LoadOperatorsFile(ConcatPaths([BinFolder, OperatorsFilename]));
{$else}
    LoadOperatorsFile(TPath.Combine(BinFolder, OperatorsFilename));
{$endif}
    InitialiseFragments;
{$ifdef fpc}
    LoadFragmentsLibrary(ConcatPaths([BinFolder, FragmentsFilename]));
{$else}
    LoadFragmentsLibrary(TPath.Combine(BinFolder, FragmentsFilename));
{$endif}
    InitialisePrimitives;
{$ifdef fpc}
    LoadPrimitivesFile(ConcatPaths([BinFolder, PrimitivesFilename]));
{$else}
    LoadPrimitivesFile(TPath.Combine(BinFolder, PrimitivesFilename));
{$endif}

    Z80.GenProcs.Initialise;
  end;
  //Intrinsics are owned by the root Scope which is always cleared, so we must
  //reload for every run
  InitialiseIntrinsics;
{$ifdef fpc}
  LoadIntrinsicsFile(ConcatPaths([BinFolder, IntrinsicsFilename]));
{$else}
  LoadIntrinsicsFile(TPath.Combine(BinFolder, IntrinsicsFilename));
{$endif}

{$ifdef fpc}
  InitialiseCodeGen(ConcatPaths([GetPlatformFolder, 'Assembler/', Config.PlatformName + '.asm']),
    ConcatPaths([QuicheFolder, QuicheCoreFilename]));
{$else}
  InitialiseCodeGen(TPath.Combine(GetPlatformFolder, 'Assembler/' + Config.PlatformName + '.asm'),
    TPath.Combine(QuicheFolder, QuicheCoreFilename));
{$endif}
{$ifdef fpc}
  LoadAlgoData(ConcatPaths([BinFolder, AlgoDataFilename]));
{$else}
  LoadAlgoData(TPath.Combine(BinFolder, AlgoDataFilename));
{$endif}
end;

initialization
  //Default values for compiler options
  Config.AllowAutoCreation := False;
  Config.OverflowChecks := True;
  Config.DefaultVarStorage := vsStack;//vsAbsolute;
  Config.DefaultCallingConvention := ccStack;
end.


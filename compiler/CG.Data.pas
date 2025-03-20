unit CG.Data;

interface
uses Def.Scopes;

//===============Constants to hex strings

//Values are masked as needed to the given size
function ByteToStr(Value: Integer): String;
function WordToStr(Value: Integer): String;
function WordLoToStr(Value: Integer): String;

function GetCurrProcName: String;

//===============Write to assembler output file

procedure SaveAssemblyFile(FileName: String);

//Write an entire instruction line

procedure AsmLine(S: String);

procedure AsmLines(S: String);

procedure AsmInstr(S: String);

procedure AsmOpcode(const Op: String;const P1: String = '';const P2: String = '');

//Strictly for TESTING ONLY
function PeekAssembly: String;


procedure AsmDataLine(S: String);

var
  CodeGenErrorString: String;
  CurrErrorCount: Integer;  //In current routine
  TotalErrorCount: Integer; //In current build

procedure AsmError(const Msg: String);



function GetCodeGenScope: PScope;

procedure GenLabel(Name: String);

procedure InsertPreamble(PlatformFile, QuicheLibrary: String);

//Init for the current code block
//Sets CodeGenScope to Scope
procedure CGDataBlockInit(Scope: PScope);

//Init for a compile
procedure CGDataCompileInit;

implementation
uses SysUtils, Classes;


//===============Constants to hex strings

function ByteToStr(Value: Integer): String;
begin
  Result := '$' + IntToHex(Value and $ff, 2).ToLower
end;

function WordToStr(Value: Integer): String;
begin
  Result := '$' + IntToHex(Value and $ffff, 4).ToLower
end;

function WordLoToStr(Value: Integer): String;
begin
  Result := '$' + IntToHex(lo(Word(Value and $ffff)) and $ff, 2).ToLower
end;

//=============== Assembler output file

//The assembly name of the current procedure
var CurrProcName: String;

function GetCurrProcName: String;
begin
  Result := CurrProcName;
end;

var
  AsmCodeFull: TStringList;
  AsmCodeScope: TStringList;
  AsmDataFull: TStringList;
  AsmDataScope: TStringList;

procedure SaveAssemblyFile(FileName: String);
begin
  AsmCodeFull.Append(AsmDataFull.Text);
  AsmCodeFull.Add('__quiche_end:');

  AsmCodeFull.SaveToFile(Filename);
end;

procedure AsmLine(S: String);
begin
  AsmCodeFull.Add(S);
  if Assigned(AsmCodeScope) then
    AsmCodeScope.Add(S);
end;

procedure AsmLines(S: String);
begin
  AsmLine(S);
end;

//Write an entire instruction line
procedure AsmInstr(S: String);
begin
  AsmLine('  '+S);
end;

procedure AsmOpcode(const Op: String;const P1: String = '';const P2: String = '');
var S: String;
begin
  S := '  ' + Op;
  if P1 <> '' then
  begin
    S := S + ' ' + P1;
    if P2 <> '' then
      S := S + ',' + P2;
  end;
  AsmLine(S);
end;

function PeekAssembly: String;
var Line: String;
begin
  Result := '';
  for Line in AsmCodeFull do
  begin
    if Result <> '' then
      Result := Result + ':';
    Result := Result + Trim(Line);
  end;
end;

procedure AsmError(const Msg: String);
begin
  inc(CurrErrorCount);
  inc(TotalErrorCount);
  AsmLine('ERROR: ' + Msg + ' in ' + CurrProcName);
end;

procedure AsmDataLine(S: String);
begin
  AsmDataFull.Add(S);
  if Assigned(AsmDataScope) then
    AsmDataScope.Add(S);
end;

//=============
var  LabelIndex: Integer;  //Used to generate unique labels. Cleared at the start of each routine

function GetUniqueLabel: String;
begin
  Result := '.x'+IntToStr(LabelIndex);
  inc(LabelIndex);
end;

procedure GenLabel(Name: String);
begin
  AsmLine(Name + ':');
end;

var
  CodeGenScope: PScope;

function GetCodeGenScope: PScope;
begin
  Result := CodeGenScope;
end;

procedure InsertPreamble(PlatformFile, QuicheLibrary: String);
begin
  AsmCodeFull.Add(';Quiche object code');
  AsmCodeFull.Add(';Auto-created. Will be overwritten!');
  AsmCodeFull.Add(';Designed for RASM assembler');
  AsmCodeFull.Add('');
  AsmCodeFull.Add(';Insert platform specific code');
  AsmCodeFull.Add('include "' + PlatformFile + '"');
  AsmCodeFull.Add('');
  AsmCodeFull.Add(';Insert Quiche libraries');
  AsmCodeFull.Add('include "' + QuicheLibrary + '"');
  AsmCodeFull.Add('');
  AsmCodeFull.Add(';Generated code starts here');
  AsmCodeFull.Add('quiche:');
end;

procedure CGDataBlockInit(Scope: PScope);
begin
  CodeGenScope := Scope;

  CodeGenErrorString := '';

  CurrErrorCount := 0;
  AsmCodeScope := Scope.AsmCode;
  AsmDataScope := Scope.AsmData;

  CurrProcName := '_'+Scope.Name.ToLower;
  LabelIndex := 1;
end;

procedure CGDataCompileInit;
begin
  if AsmCodeFull = nil then
    AsmCodeFull := TStringList.Create
  else
    AsmCodeFull.Clear;
  AsmCodeScope := nil;
  if AsmDataFull = nil then
    AsmDataFull := TStringList.Create
  else
    AsmDataFull.Clear;
  AsmDataScope := nil;
end;

initialization
  AsmCodeFull := nil;
  AsmDataFull := nil;
finalization
  AsmCodeFull.Free;
  AsmDataFull.Free;
end.

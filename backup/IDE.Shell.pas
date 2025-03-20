unit IDE.Shell;

interface

function Assemble(Filename: String): String;
{$ifndef fpc}
function Emulate(const CommandLine, WorkingDir: String): String;
{$endif}

implementation
uses {$ifdef fpc}Process, {$else}Windows, IOUtils, {$endif}Classes, SysUtils;

{$ifndef fpc}
{ Run a DOS program and retrieve its output dynamically while it is running. }
//From: https://stackoverflow.com/questions/1454501/how-do-i-run-a-command-line-program-in-delphi
function GetDosOutput(CommandLine: string; Work: string = 'C:\'): string;  { Run a DOS program and retrieve its output dynamically while it is running. }
var
  SecAtrrs: TSecurityAttributes;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  StdOutPipeRead, StdOutPipeWrite: THandle;
  WasOK: Boolean;
  pCommandLine: array[0..255] of AnsiChar;
  BytesRead: Cardinal;
  WorkDir: string;
  Handle: Boolean;
begin
  Result := '';
  with SecAtrrs do begin
    nLength := SizeOf(SecAtrrs);
    bInheritHandle := True;
    lpSecurityDescriptor := nil;
  end;
  CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SecAtrrs, 0);
  try
    with StartupInfo do
    begin
      FillChar(StartupInfo, SizeOf(StartupInfo), 0);
      cb := SizeOf(StartupInfo);
      dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
      wShowWindow := SW_HIDE;
      hStdInput := GetStdHandle(STD_INPUT_HANDLE); // don't redirect stdin
      hStdOutput := StdOutPipeWrite;
      hStdError := StdOutPipeWrite;
    end;
    WorkDir := Work;
    Handle := CreateProcess(nil, PChar('cmd.exe /C ' + CommandLine),
                            nil, nil, True, 0, nil,
                            PChar(WorkDir), StartupInfo, ProcessInfo);
    CloseHandle(StdOutPipeWrite);
    if Handle then
      try
        repeat
          WasOK := windows.ReadFile(StdOutPipeRead, pCommandLine, 255, BytesRead, nil);
          if BytesRead > 0 then
          begin
            pCommandLine[BytesRead] := #0;
            Result := Result + pCommandLine;
          end;
        until not WasOK or (BytesRead = 0);
        WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
      finally
        CloseHandle(ProcessInfo.hThread);
        CloseHandle(ProcessInfo.hProcess);
      end;
  finally
    CloseHandle(StdOutPipeRead);
  end;
end;
{$endif}

//Separate assembly messages from map file data.
//Saves the map file data to <Filename>.map
//Returns the assembly messages
function ExtractMapFile(const Filename, Output: String): String;
var Map: TStringList;
  I: Integer;
begin
  Result := '';
  Map := TStringList.Create;
  try
    Map.Text := Output;
    I := 0;
    //Search for first line of
    while I < Map.Count do
      if Map[I].StartsWith('Bnk|') then
        Inc(I)
      else if (Length(Map[I]) > 0) and CharInSet(Map[I].Chars[0],
        ['a' .. 'z', 'A' .. 'Z']) then
      begin
        Result := Result + Map[I] + #13;
        Map.Delete(I);
      end
      else
        Inc(I);

{$ifdef fpc}
    Map.SaveToFile(ChangeFileExt(Filename, '.map'));
{$else}
    Map.SaveToFile(TPath.ChangeExtension(Filename, '.map'));
{$endif}
  finally
    Map.Free;
  end;
end;

function Assemble(Filename: String): String;
const
  Rasm = 'rasm_win64.exe';
  WorkDir = 'c:\';
var
  CommandLine: String;
begin
{$ifdef fpc}
  RunCommand(Rasm, ExpandFilename(Filename) + ' -oa -s -sa -map -eo', Result);
{$else}
  //-oa option takes output file path from input file path
  CommandLine := RASM + ' ' + TPath.GetFullPath(Filename) + ' -oa -s -sa -map -eo';
  Result := GetDosOutput(CommandLine, WorkDir);
{$endif}
  Result := ExtractMapFile(Filename, Result);
end;

{$ifndef fpc}
function Emulate(const CommandLine, WorkingDir: String): String;
{const Emulator = '..\..\..\Z80Emulator\Win32\Debug\z80Emulator.exe';
//  Config = '..\..\Z80Code\Config.txt';
  Config = 'C:\RetroTools\Quiche\Config.txt';
  WorkDir = 'c:\';
var
  CommandLine: String;
}begin
//  CommandLine := TPath.GetFullPath(Emulator) + ' @' + TPath.GetFullPath(Config);
  Result := GetDosOutput(CommandLine, WorkingDir);
end;
{$endif}

end.

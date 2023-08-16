unit Shell;

interface

function Assemble(Filename: String): String;

function Emulate(Filename: String): String;

implementation
uses Windows, IOUtils;

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

function Assemble(Filename: String): String;
const Rasm = 'rasm_x64.exe';
  WorkDir = 'c:\';
var
  CommandLine: String;
begin
  //-oa option takes output file path from input file path
  CommandLine := RASM + ' ' + TPath.GetFullPath(Filename) + ' -oa';
  Result := GetDosOutput(CommandLine, WorkDir);
end;

function Emulate(Filename: String): String;
const Emulator = '..\..\..\Z80Emulator\Win32\Debug\z80Emulator.exe';
//  Config = '..\..\Z80Code\Config.txt';
  Config = 'C:\RetroTools\Quiche\Config.txt';
  WorkDir = 'c:\';
var
  CommandLine: String;
begin
  CommandLine := TPath.GetFullPath(Emulator) + ' @' + TPath.GetFullPath(Config);
  Result := GetDosOutput(CommandLine, WorkDir);
end;

end.

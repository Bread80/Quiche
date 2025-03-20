unit mMemory;

interface
uses SysUtils, mHardware;

type TRAM = class(TMemory)
  private
    FRAM: array[0..$ffff] of byte;
//    FHookRead: TFunc<Word, Byte, Byte>;
//    FHookWrite: TProc<Word, Byte, Byte>;
  protected
    class function GetClassName: String;
  public
    procedure LoadBin(Filename: String;Addr: Word);

    procedure Reset;override;

    //Reads data from memory - hook the HookRead property if you want to see what
    //is being read and/or modify it.
    function TryWriteByte(Addr: Word;Data: Byte): Boolean;override;
    function TryReadByte(Addr: Word;var Data: Byte): Boolean;override;

    procedure DumpMemory(AFilename: String);override;

    //Sends Address, Old (existing) data byte at that address and
    //New byte being written to the address
//    property HookWrite: TProc<Word,Byte,Byte> read FHookWrite write FHookWrite;
    //Sends Address and memory data byte at that address
    //Returns data byte to be read by the z80 - this could be the original byte
    //or data modified by the hooking routine.
//    property HookRead: TFunc<Word,Byte,Byte> read FHookRead write FHookRead;

//    property Data[Addr: Word]: Byte read Read8 write Write8;default;
  end;

implementation
uses {$ifdef fpc}LazFileUtils, {$else}IOUtils, {$endif}Classes;

{ TMemory }

{$ifdef fpc}
procedure SaveByteArray(AByteArray: TBytes; const AFileName: string);
var
  AStream: TStream;
begin

  if FileExistsUTF8(AFileName) then DeleteFileUTF8(AFileName);

  AStream := TFileStream.Create(AFileName, fmCreate);
  try
     AStream.WriteBuffer(Pointer(AByteArray)^, Length(AByteArray));
  finally
     AStream.Free;
  end;
end;
{$endif}

procedure TRAM.DumpMemory(AFilename: String);
var Bytes: TBytes;
  Addr: Integer;
begin
  SetLength(Bytes, 65536);
  for Addr := 0 to $ffff do
    Bytes[Addr] := FRAM[Addr];
{$ifdef fpc}
  SaveByteArray(Bytes, AFilename);
{$else}
  TFile.WriteAllBytes(AFilename, Bytes);
{$endif}
end;

class function TRAM.GetClassName: String;
begin
  Result := 'RAM';
end;

function LoadByteArray(const AFileName: string): TBytes;
var
  AStream: TStream;
begin
  SetLength(result, 0);

//  if not FileExistsUTF8(AFileName) then exit;

  AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
     AStream.Position := 0;
     SetLength(result, AStream.Size);
     AStream.Read(PByte(result)^, AStream.Size);
  finally
     AStream.Free;
  end;

end;

procedure TRAM.LoadBin(Filename: String; Addr: Word);
var
  Bytes: TBytes;
  Src: Integer;
begin
{$ifdef fpc}
  Bytes := LoadByteArray(Filename);
{$else}
  Bytes := TFile.ReadAllBytes(Filename);
{$endif}
  for Src := 0 to Length(Bytes)-1 do
    FRAM[Addr+Src] := Bytes[Src];
end;
{
function TRAM.Read16(Addr: Word): Word;
begin
  Result := Read8(Addr) or (Read8(Addr+1) shl 8);
end;

function TRAM.Read8(Addr: word): byte;
begin
  if Assigned(HookRead) then
    Result := HookRead(Addr, FRAM[Addr])
  else
    Result := FRAM[Addr];
end;
}
procedure TRAM.Reset;
begin
  inherited;
  //Do nothing
end;

function TRAM.TryReadByte(Addr: Word; var Data: Byte): Boolean;
begin
  Result := True;
  Data := FRAM[Addr];
//  Data := $80;
end;

function TRAM.TryWriteByte(Addr: Word; Data: Byte): Boolean;
begin
  Result := True;
  FRam[Addr] := Data;
end;

{procedure TRAM.Write16(Addr, Value: Word);
begin
  Write8(Addr, Value and $ff);
  Write8(Addr+1, Value shr 8);
end;

procedure TRAM.Write8(Addr: word; Value: byte);
begin
  if Assigned(HookWrite) then
    HookWrite(Addr, FRAM[Addr], Value);

  FRAM[Addr] := Value;
end;
}
initialization
  Hardware.RegisterDevice(TRam.GetClassName, TRam);
end.

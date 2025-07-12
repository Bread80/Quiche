unit mMemory;

interface
uses SysUtils, mHardware;

type TRAM = class(TMemory)
  private
    FRAM: array[0..$ffff] of byte;
  protected
    //Reads data from memory - hook the HookRead property if you want to see what
    //is being read and/or modify it.
    function DoTryWriteByte(Addr: Word;Data: Byte): Boolean;override;
    function DoTryReadByte(Addr: Word;var Data: Byte): Boolean;override;
  public
    procedure LoadBin(Filename: String;Addr: Word);

    procedure Reset;override;

    procedure DumpMemory(AFilename: String);override;

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

function TRAM.DoTryReadByte(Addr: Word; var Data: Byte): Boolean;
begin
  Data := FRAM[Addr];
  Result := True;
end;

function TRAM.DoTryWriteByte(Addr: Word; Data: Byte): Boolean;
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
  Hardware.RegisterDevice(TRam.GetTypeID, TRam);
end.

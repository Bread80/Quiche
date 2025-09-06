unit mHardware;

interface
uses Generics.Collections, SysUtils, mZ80, mSymbols, mMapFile;

type TAbstractDevice = class;

{$ifdef fpc}
TReadHookProc = function(Device: TAbstractDevice;Addr: Word;Data: Byte): Byte;
TWriteHookProc = function(Device: TAbstractDevice;Addr: Word;Data, Data2: Byte): Byte;
{$else}
  //Address, Value being read from the device, Value to be returned to the CPU.
  //The latter may be the original value from the device or may be modified by
  //the hooking process.
  TReadHookProc = TFunc<TAbstractDevice, Word, Byte, Byte>;
  //Address, Value being written by the CPU, and value to write to memory or the
  //device.
  //Returns the value to be written to the device, which may be the same as the
  //data sent but could be modified by the hooking process
  TWriteHookProc = TFunc<TAbstractDevice, Word, Byte, Byte, Byte>;
{$endif}

  TAbstractDevice = class
  private
    FIsEnabled: Boolean;
    FName: String;

    FHookRead: TReadHookProc;
    FHookWrite: TWriteHookProc;
  protected
    class function GetTypeID: String;virtual;
    function DoTryWriteByte(Addr: Word;Data: Byte): Boolean;virtual;abstract;
    function DoTryReadByte(Addr: Word;var Data: Byte): Boolean;virtual;abstract;
  public
    constructor Create(const AName, ASettings: String);virtual;
    destructor Destroy;override;

    procedure Reset;virtual;abstract;

    function TryWriteByte(Addr: Word;Data: Byte): Boolean;
    function TryReadByte(Addr: Word;var Data: Byte): Boolean;

    property TypeID: String read GetTypeID;
    property Name: String read FName;
    property IsEnabled: Boolean read FIsEnabled write FIsEnabled;

    //Sends Address, and byte being written to the address
    property HookWrite: TWriteHookProc read FHookWrite write FHookWrite;
    //Sends Address and Returns data byte to be read by the z80 - this could be the original byte
    //or data modified by the hooking routine.
    property HookRead: TReadHookProc read FHookRead write FHookRead;
  end;

  TDeviceClass = class of TAbstractDevice;

type TIODevice = class(TAbstractDevice)
  protected
    function DoTryWriteByte(Addr: Word;Data: Byte): Boolean;override;
    function DoTryReadByte(Addr: Word;var Data: Byte): Boolean;override;
  public
    procedure Reset;override;
  end;

type TMemory = class(TAbstractDevice)
  private
    FSymbols: TSymbols;
    FSourceFilename: String;
    FBuildSettings: String;
  protected
    procedure SetSourceFilename(const Value: String;const ABuildSettings: String = '');
  public
    constructor Create(const AName, ASettings: String);override;
    destructor Destroy;override;
    procedure DumpMemory(AFilename: String);virtual;abstract;

    property Symbols: TSymbols read FSymbols;
    property SourceFilename: String read FSourceFilename;
  end;

type TDeviceData = class
    TypeID: String;
    DeviceType: TDeviceClass;
  end;

type TUpdateType = (utSystem, utDoneOpcode);
{$ifdef fpc}
  TUpdateProc = procedure(UpdateType: TUpdateType);
  TCharProc = procedure(Ch: Char);
  TIntegerFunc = function: Integer;
  TProcedure = procedure;
{$else}
  TUpdateProc = TProc<TUpdateType>;
  TCharProc = TProc<Char>;
  TIntegerFunc = TFunc<Integer>;
  TProcedure = TProc;
{$endif}
  TUpdateView = (uvQuiet, uvVerbose);

  TExecStatus = (esPaused, esRunning);

type THardware = class
  private
    Z80Exec: TZ80Executor;

    //List of devices types which can be instantiated
    FRegisteredDevices: TObjectList<TDeviceData>;
    //List of instantiated IO devices
    FIODevices: TObjectList<TIODevice>;
    //List of instantiated memory devices
    FMemoryDevices: TObjectList<TMemory>;

    FMapFile: TMapFile;
    FSymbols: TSymbols;
    FSourceFilename: String;
    FBuildSettings: String;

    FTerm1OutProc: TCharProc;
    FTerm1InProc: TIntegerFunc;

    FBreakpointProc: TProcedure;

    //List of procedures (i.e. for viewers) to call every time we update something
    FUpdateProcs: TList<TUpdateProc>;
    FExecStatus: TExecStatus;
    FUpdateView: TUpdateView;
    procedure SetExecStatus(const Value: TExecStatus);
    procedure SetUpdateView(const Value: TUpdateView);

    function FindMemoryDeviceByName(const AName: String): TMemory;
    function AddrToMemoryDevice(Addr: Word): TMemory;
  protected
  public
    constructor Create;
    destructor Destroy;override;
    //Full reset, removing all current hardware and connections
    procedure Restart;
    //Resets with the current config
    procedure Reset;

    procedure LoadConfigFile(const Filename: String);

    procedure RegisterDevice(const ATypeID: String;AClass: TDeviceClass);

    procedure CreateMemoryDevice(const ATypeID: String;const AName: String = '';
      const ASettings: String = '');
    procedure CreateIODevice(const ATypeID: String;const AName: String = '';
      const ASettings: String = '');

    procedure UpdateViewers(AUpdateType: TUpdateType = utSystem);

    procedure HookTerminal1Out(ATermProc: TCharProc);
    procedure HookTerminal1In(ATermProc: TIntegerFunc);
    procedure Term1Out(C: Char);
    function Term1In: Integer;

    function ReadIO(Addr: Word): Byte;
    procedure WriteIO(Addr: Word;Data: Byte);

    function ReadMemoryByte(Addr: Word): Byte;
    function ReadMemoryWord(Addr: Word): Word;
    procedure WriteMemoryByte(Addr: Word;Data: Byte);
    procedure WriteMemoryWord(Addr: Word;Data: Word);
    procedure DumpMemory(ADeviceName: String;Filename: String);

    //Where ADeviceName specifies an optional name of a memory device.
    //If ADeviceName is specified the binary will be loaded into that device, and only that device.
    //If no ADeviceName is specified (='') binary will be loaded into whatever memory is currently
    //enabled at that address.
    procedure LoadBinary(const AFilename: String;Addr: Word;const ADeviceName: String = '');
    procedure LoadMapFile(const Filename: String);
    //Where ADeviceName specifies an optional name of a memory device.
    //If ADeviceName is specified then the symbol data will be loaded into that specific memory device
    //and will only be used (later) if that device is enabled.
    //If ADeviceName is not specified (='') the symbols will be loaded into the global symbol table
    //and will be 'found' no matter what memory device is enabled at that address
    procedure LoadSymbols(const AFilename: String;const ADeviceName: String = '');
    procedure SetSourceFilename(const AFilename: String;const ABuildSettings: String = '';const ADeviceName: String = '');

    procedure AddUpdateProc(AProc: TUpdateProc);
    procedure RemoveUpdateProc(AProc: TUpdateProc);

    function TryAddrToSourceFile(Addr: Word;out Filename: String;out FileLine: Integer): Boolean;

    //Returns the first symbol <= Value.
    function TryAddrToSymbol(Addr: Word;var Symbol: String): Boolean;
    function TrySymbolToAddr(ASymbol: String;var Addr: Word): Boolean;
    //Returns True if the Addr is in the range of Prior bytes before ASymbol to After bytes after ASymbol
    // (ASymbol-Prior) <= Addr <= (ASymbol + After)
    function TryIsInRange(Addr: Word;ASymbol: String;Prior, After: Word;var InRange: Boolean): Boolean;

    procedure ExecOpcode;
    procedure SetPC(Addr: Word);

    procedure Run;
    procedure RunToHalt;
    procedure Pause;

    property MemoryDevices: TObjectList<TMemory> read FMemoryDevices;
    property IODevices: TObjectList<TIODevice> read FIODevices;

    property ExecStatus: TExecStatus read FExecStatus write SetExecStatus;
    property UpdateView: TUpdateView read FUpdateView write SetUpdateView;

    property Z80: TZ80Executor read Z80Exec;

    property Term1OutProc: TCharProc read FTerm1OutProc write FTerm1OutProc;
    property Term1InProc: TIntegerFunc read FTerm1InProc write FTerm1InProc;

    property BreakpointProc: TProcedure read FBreakpointProc write FBreakpointProc;
  end;

var Hardware: THardware;

implementation
uses {$ifdef fpc}LazFileUtils, {$else}IOUtils,{$endif} Classes, mConfigFile;

{ TAbstractDevice }

constructor TAbstractDevice.Create(const AName, ASettings: String);
begin
  inherited Create;

  FName := AName;
  FIsEnabled := True;
end;

destructor TAbstractDevice.Destroy;
begin

  inherited;
end;

class function TAbstractDevice.GetTypeID: String;
begin
{$ifdef fpc}
  Result := Copy(ClassName, 2, MaxInt);
{$else}
  Result := ClassName.Substring(1, MaxInt);
{$endif}
end;

function TAbstractDevice.TryReadByte(Addr: Word; var Data: Byte): Boolean;
begin
  Result := DoTryReadByte(Addr, Data);

  if Assigned(HookRead) then
    Data := HookRead(Self, Addr, $ff);
end;

function TAbstractDevice.TryWriteByte(Addr: Word; Data: Byte): Boolean;
begin
  if Assigned(HookWrite) then
    HookWrite(Self, Addr, $ff, Data);

  Result := DoTryWriteByte(Addr, Data);
end;

{ TMemory }

constructor TMemory.Create(const AName, ASettings: String);
begin
  inherited Create(AName, ASettings);
  FSymbols := TSymbols.Create;
end;

destructor TMemory.Destroy;
begin
  FSymbols.Free;
  inherited;
end;

procedure TMemory.SetSourceFilename(const Value, ABuildSettings: String);
begin
  FSourceFilename := Value;
  FBuildSettings := ABuildSettings;
end;

{ TIODevice }

function TIODevice.DoTryReadByte(Addr: Word; var Data: Byte): Boolean;
begin
  Result := False;
  Data := $ff;
end;

function TIODevice.DoTryWriteByte(Addr: Word; Data: Byte): Boolean;
begin
  Result := False;
end;

procedure TIODevice.Reset;
begin
  //Nothing to do
end;

{ THardware }

function THardware.AddrToMemoryDevice(Addr: Word): TMemory;
var Dummy: Byte;
  I: Integer;
begin
  for I := 0 to FMemoryDevices.Count-1 do
  begin
    Result := FMemoryDevices[I];
    if Result.IsEnabled and Result.TryReadByte(Addr, Dummy) then
      EXIT;
  end;

  Result := nil;
end;

procedure THardware.AddUpdateProc(AProc: TUpdateProc);
begin
  FUpdateProcs.Add(AProc);
end;

constructor THardware.Create;
begin
  inherited Create;

  FUpdateProcs := TList<TUpdateProc>.Create;

  FRegisteredDevices := TObjectList<TDeviceData>.Create;
  FIODevices := TObjectList<TIODevice>.Create;
  FMemoryDevices := TObjectList<TMemory>.Create;

  FMapFile := TMapFile.Create;
  FSymbols := TSymbols.Create;

  Z80Exec := TZ80Executor.Create;
  Z80Exec.ReadMemoryByte := ReadMemoryByte;
  Z80Exec.WriteMemoryByte := WriteMemoryByte;
  Z80Exec.ReadIO := ReadIO;
  Z80Exec.WriteIO := WriteIO;
end;

procedure THardware.CreateIODevice(const ATypeID, AName, ASettings: String);
var Device: TDeviceData;
  LName: String;
begin
  if AName = '' then
    LName := ATypeID
  else
    LName := AName;
  for Device in FRegisteredDevices do
    if CompareText(ATypeID, Device.TypeID) = 0 then
    begin
      if Device.DeviceType.InheritsFrom(TIODevice) then
        FIODevices.Add(TIODevice(Device.DeviceType.Create(LName, ASettings)))
      else
        raise Exception.Create('Device ''' + AName + ''' is not an I/O device.');
      EXIT;
    end;

  raise Exception.Create('No I/O device found called ''' + AName + '''');
end;

procedure THardware.CreateMemoryDevice(const ATypeID, AName, ASettings: String);
var Device: TDeviceData;
  LName: String;
begin
  if AName = '' then
    LName := ATypeID
  else
    LName := AName;
  for Device in FRegisteredDevices do
    if CompareText(ATypeID, Device.TypeID) = 0 then
    begin
      if Device.DeviceType.InheritsFrom(TMemory) then
        FMemoryDevices.Add(TMemory(Device.DeviceType.Create(LName, ASettings)))
      else
        raise Exception.Create('Device ''' + AName + ''' is not a memory device.');
      EXIT;
    end;

  raise Exception.Create('No memory device found called ''' + AName + '''');
end;

destructor THardware.Destroy;
begin
  FSymbols.Free;
  FMapFile.Free;
  FMemoryDevices.Free;
  FIODevices.Free;
  FRegisteredDevices.Free;

  inherited;
end;

procedure THardware.DumpMemory(ADeviceName: String;Filename: String);
var Mem: TMemory;
begin
  Mem:= FindMemoryDeviceByName(ADeviceName);
  if Mem = nil then
    raise Exception.Create('Unknown device: ' + ADeviceName);
  Mem.DumpMemory(Filename);
end;

procedure THardware.ExecOpcode;
begin
  Z80Exec.ExecOpCode;
//  Quiet;
  UpdateViewers(utDoneOpcode);
end;

function THardware.FindMemoryDeviceByName(const AName: String): TMemory;
begin
  for Result in FMemoryDevices do
    if CompareText(Result.Name, AName) = 0 then
      EXIT;
  Result := nil;
end;

procedure THardware.HookTerminal1In(ATermProc: TIntegerFunc);
begin
  FTerm1InProc := ATermProc;
end;

procedure THardware.HookTerminal1Out(ATermProc: TCharProc);
begin
  FTerm1OutProc := ATermProc;
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

procedure THardware.LoadBinary(const AFilename: String; Addr: Word;const ADeviceName: String);
var
  Bytes: TBytes;
  Src: Integer;
  Device: TMemory;
begin
  Device := nil;
  if ADeviceName <> '' then
  begin
    Device := FindMemoryDeviceByName(ADeviceName);
    if not Assigned(Device) then
      raise Exception.Create('Unknown memory device ''' + ADeviceName + '''');
  end;
{$ifdef fpc}
  Bytes := LoadByteArray(AFilename);
{$else}
  Bytes := TFile.ReadAllBytes(AFilename);
{$endif}
  for Src := 0 to Length(Bytes)-1 do
    if Assigned(Device) then
    begin
      if not Device.TryWriteByte(Addr+Src, Bytes[Src]) then
        raise Exception.Create('Write to memory device ''' + Device.Name + ''' failed at address $' + (Addr+Src).ToString);
    end
    else
      WriteMemoryByte(Addr+Src, Bytes[Src]);
end;

procedure THardware.LoadConfigFile(const Filename: String);
var Config: TConfigFile;
begin
  Config := TConfigFile.Create(Self);
  try
    Config.LoadConfigFile(Filename);
  finally
    Config.Free;
  end;
end;

procedure THardware.LoadMapFile(const Filename: String);
begin
  FMapFile.LoadFromFile(Filename);
end;

procedure THardware.LoadSymbols(const AFilename, ADeviceName: String);
var Device: TMemory;
begin
  if ADeviceName <> '' then
  begin
    Device := FindMemoryDeviceByName(ADeviceName);
    if not Assigned(Device) then
      raise Exception.Create('Unknown memory device ''' + ADeviceName + '''');
    Device.Symbols.LoadFile(AFilename);
  end
  else
    FSymbols.LoadFile(AFilename);
end;

procedure THardware.Pause;
begin
  ExecStatus := esPaused;
end;

function THardware.ReadIO(Addr: Word): Byte;
var
  I: Integer;
begin
  for I := 0 to FIODevices.Count-1 do
    if FIODevices[I].IsEnabled then
      if FIODevices[I].TryReadByte(Addr, Result) then
        EXIT;
  //!!No I/O device on port
end;

function THardware.ReadMemoryByte(Addr: Word): Byte;
var
  I: Integer;
begin
  for I := 0 to FMemoryDevices.Count-1 do
    if FMemoryDevices[I].IsEnabled then
      if FMemoryDevices[I].TryReadByte(Addr, Result) then
        EXIT;
  //!!No memory at address
end;

function THardware.ReadMemoryWord(Addr: Word): Word;
var
  I: Integer;
  Low, High: Byte;
begin
  for I := 0 to FMemoryDevices.Count-1 do
    if FMemoryDevices[I].IsEnabled then
      if FMemoryDevices[I].TryReadByte(Addr, Low) and
        FMemoryDevices[I].TryReadByte(Addr + 1, High) then
        EXIT((High shl 8) + Low);

  //!!No memory at address
  Result := 0;
end;

procedure THardware.RegisterDevice(const ATypeID: String; AClass: TDeviceClass);
var
  Data: TDeviceData;
begin
  Data := TDeviceData.Create;
  Data.TypeID := ATypeID;
  Data.DeviceType := AClass;
  FRegisteredDevices.Add(Data);
end;

procedure THardware.RemoveUpdateProc(AProc: TUpdateProc);
begin
  FUpdateProcs.Remove(AProc);
end;

procedure THardware.Reset;
var
  Device: TIODevice;
  Memory: TMemory;
begin
  ExecStatus := esPaused;
  Z80Exec.Z80.ColdReset;

  for Device in FIODevices do
    Device.Reset;
  for Memory in FMemoryDevices do
    Memory.Reset;

  UpdateViewers(utSystem);
end;

procedure THardware.Restart;
begin
  FSymbols.Clear;
  FMapFile.Clear;
  FMemoryDevices.Clear;
  FIODevices.Clear;
  Reset;
end;

procedure THardware.Run;
begin
  ExecStatus := esRunning;
end;

procedure THardware.RunToHalt;
const opcHALT = $76;
begin
  while ReadMemoryByte(Hardware.Z80.Z80.PC) <> opcHALT do
    ExecOpcode;
end;

procedure THardware.SetExecStatus(const Value: TExecStatus);
begin
  FExecStatus := Value;
  if Value = esPaused then
    UpdateViewers(utDoneOpcode);
end;

procedure THardware.SetPC(Addr: Word);
begin
    Z80Exec.Z80.PC := Addr;
end;

procedure THardware.SetSourceFilename(const AFilename, ABuildSettings, ADeviceName: String);
var Device: TMemory;
begin
  if ADeviceName <> '' then
  begin
    Device := FindMemoryDeviceByName(ADeviceName);
    if not Assigned(Device) then
      raise Exception.Create('Unknown memory device ''' + ADeviceName + '''');
    Device.SetSourceFilename(AFilename, ABuildSettings);
  end
  else
  begin
    FSourceFilename := AFilename;
    FBuildSettings := ABuildSettings;
  end;
end;

procedure THardware.SetUpdateView(const Value: TUpdateView);
begin
  FUpdateView := Value;
  UpdateViewers(utDoneOpcode);
end;

function THardware.Term1In: Integer;
begin
  if Assigned(FTerm1InProc) then
    Result := FTerm1InProc
  else
    Result := -1;
end;

procedure THardware.Term1Out(C: Char);
begin
  if Assigned(FTerm1OutProc) then
    FTerm1OutProc(C);
end;

function THardware.TryAddrToSourceFile(Addr: Word; out Filename: String;
  out FileLine: Integer): Boolean;
begin
  Result := FMapFile.TryAddrToSourceFile(Addr, Filename, FileLine);
end;

function THardware.TryAddrToSymbol(Addr: Word; var Symbol: String): Boolean;
var
  Device: TMemory;
begin
  Device := AddrToMemoryDevice(Addr);
  Result := False;
  if Assigned(Device) then
    Result := Device.Symbols.TryAddrToSymbol(Addr, Symbol);
  if not Result then
    Result := FSymbols.TryAddrToSymbol(Addr, Symbol);
end;

function THardware.TryIsInRange(Addr: Word; ASymbol: String; Prior, After: Word;
  var InRange: Boolean): Boolean;
var
  Device: TMemory;
begin
  Device := AddrToMemoryDevice(Addr);
  Result := False;
  if Assigned(Device) then
    Result := Device.Symbols.TryIsInRange(Addr, ASymbol, Prior, After, InRange);
  if not Result then
    Result := FSymbols.TryIsInRange(Addr, ASymbol, Prior, After, InRange);
end;

function THardware.TrySymbolToAddr(ASymbol: String; var Addr: Word): Boolean;
var
  Device: TMemory;
begin
  Device := AddrToMemoryDevice(Addr);
  Result := False;
  if Assigned(Device) then
    Result := Device.Symbols.TrySymbolToAddr(ASymbol, Addr);
  if not Result then
    Result := FSymbols.TrySymbolToAddr(ASymbol, Addr);
end;

procedure THardware.UpdateViewers(AUpdateType: TUpdateType);
var I: Integer;
  Update: Boolean;
begin
  case UpdateView of
    uvQuiet:
    case AUpdateType of
      utSystem: Update := True;
      utDoneOpcode: Update := ExecStatus = esPaused;
    else
      raise Exception.Create('Unhandled ExecState type');
    end;

    uvVerbose: Update := True;
  else
    raise Exception.Create('Unhandled UpdateView type');
  end;

  if Update then
    for I := 0 to FUpdateProcs.Count-1 do
      FUpdateProcs[I](AUpdateType);

  if Assigned(FBreakpointProc) then
    FBreakpointProc;
end;

procedure THardware.WriteIO(Addr: Word; Data: Byte);
var
  I: Integer;
begin
  for I := 0 to FIODEvices.Count-1 do
    if FIODevices[I].IsEnabled then
      FIODevices[I].TryWriteByte(Addr, Data);
end;

procedure THardware.WriteMemoryByte(Addr: Word; Data: Byte);
var
  I: Integer;
begin
  for I := 0 to FMemoryDEvices.Count-1 do
    if FMemoryDevices[I].IsEnabled then
      FMemoryDevices[I].TryWriteByte(Addr, Data);
end;

procedure THardware.WriteMemoryWord(Addr, Data: Word);
var
  I: Integer;
begin
  for I := 0 to FMemoryDevices.Count-1 do
    if FMemoryDevices[I].IsEnabled then
    begin
      FMemoryDevices[I].TryWriteByte(Addr, Data and $ff);
      FMemoryDevices[I].TryWriteByte(Addr + 1, (Data shr 8) and $ff);
    end;
end;

initialization
  Hardware := THardware.Create;
finalization
  Hardware.Free;
  Hardware := nil;
end.

unit devRC2014SIO2;

interface
uses SysUtils, mHardware;

type TSIOControl = array[0..7] of Byte;
  PSIOControl = ^TSIOControl;

type TRC2014SIO2 = class(TIODevice)
  private
    FBasePort: Word;
    FPortMask: Word;
    FHaveDataA: Boolean;
    FHaveDataB: Boolean;
    FChannelDataA: Byte;
    FChannelDataB: Byte;
{    FTermOutA: TProc<Char>;
    FTermOutB: TProc<Char>;
    FTermInA: TFunc<Integer>;
    FTermInB: TFunc<Integer>;
}
    FControlA: TSIOControl;
    FControlB: TSIOControl;
    procedure WriteControl(const Channel: Char;const Value: Byte);
    procedure WriteData(const Channel: Char;const Value: Byte);
    function ReadControl(const Channel: Char): Byte;
  public
    constructor Create(const AName, ASettings: String);override;
    destructor Destroy;override;

    procedure Reset;override;

    function TryWriteIO(Addr: Word;Data: Byte): Boolean;override;
    function TryReadIO(Addr: Word;var Data: Byte): Boolean;override;

    property PortMask: Word read FPortMask write FPortMask;
    property BasePort: Word read FBasePort write FBasePort;

//    property TermOutA: TProc<Char> read FTermOutA write FTermOutA;
//    property TermOutB: TProc<Char> read FTermOutB write FTermOutB;
//    property TermInA: TFunc<Integer> read FTermInA write FTermInA;
//    property TermInB: TFunc<Integer> read FTermInB write FTermInB;
   end;

implementation

{ TRC2014SIO2 }

constructor TRC2014SIO2.Create(const AName, ASettings: String);
begin
  inherited Create(AName, ASettings);
  PortMask := $00f8;
  BasePort := $0080;

  Reset;
end;

destructor TRC2014SIO2.Destroy;
begin
  inherited Destroy;
end;

//A1=B/A
//A0=C/D !!Inverted
function TRC2014SIO2.TryReadIO(Addr: Word;var Data: Byte): Boolean;
begin
  Result := (Addr and PortMask) = BasePort;
  if Result then
    case Addr and 3 of
      0: Data := ReadControl('A');//ChannelAControl;
      1:
      begin
        Data := FChannelDataA;//ChannelAData;
        FHaveDataA := False;
      end;
      2: Data := ReadControl('B');//ChannelBControl;
      3:
      begin
        Data := FChannelDataB;//ChannelBData;
        FHaveDataB := False;
      end;
    end;
end;


function TRC2014SIO2.TryWriteIO(Addr: Word;Data: Byte): Boolean;
begin
  Result := (Addr and PortMask) = BasePort;
  if Result then
    case Addr and 3 of
      0: WriteControl('A', Data);
      1: WriteData('A', Data);
      2: WriteControl('B', Data);
      3: WriteData('B', Data);
    end;
end;

function TRC2014SIO2.ReadControl(const Channel: Char): Byte;
var Data: Integer;
begin
  if Channel = 'A' then
  begin
    if not FHaveDataA then
//      if Assigned(TermInA) then
      begin
        Data := Hardware.Term1In;
//        Data := TermInA;
        FHaveDataA := Data >= 0;
        if FHaveDataA then
          FChannelDataA := Data and $ff;
      end;
    if FHaveDataA then
      Result := $05   //Out buffer empty, In buffer full
    else
      Result := $04;  //Out buffer empty, In buffer empty
  end
  else
  begin
    if not FHaveDataB then
//      if Assigned(TermInB) then
      begin
//        Data := TermInB;
        Data := Hardware.Term1In;   //Update to Term 2?
        FHaveDataB := Data >= 0;
        if FHaveDataB then
          FChannelDataB := Data and $ff;
      end;
    if FHaveDataB then
      Result := $05   //Out buffer empty, In buffer full
    else
      Result := $04;  //Out buffer empty, In buffer empty
  end;
end;

procedure TRC2014SIO2.Reset;
var I: Integer;
begin
  FHaveDataA := False;
  FChannelDataA := 0;
  FHaveDataB := False;
  FChannelDataB := 0;
  for I := 0 to 7 do
  begin
    FControlA[I] := 0;
    FControlB[I] := 0;
  end;
end;

procedure TRC2014SIO2.WriteControl(const Channel: Char; const Value: Byte);
var
  Control: PSIOControl;
begin
  if Channel = 'A' then
    Control := @FControlA
  else
    Control := @FControlB;

  if Value and $07 = 0 then
    Control[Control[0] and $07] := Value
  else
    Control[0] := Value;
end;

procedure TRC2014SIO2.WriteData(const Channel: Char;const Value: Byte);
begin
  if Channel = 'A' then
  begin
    Hardware.Term1Out(chr(Value));

//    FChannelDataA := Value;

//    if Assigned(TermOutA) then
//      TermOutA(chr(Value));
  end
  else
  begin
    Hardware.Term1Out(chr(Value));    //Update to Term 2?
//    FChannelDataB := Value;
//    if Assigned(TermOutB) then
//      TermOutB(chr(Value));
  end
end;

initialization
  Hardware.RegisterDevice('RC2014_SIO2', TRC2014SIO2);
end.

unit mSymbols;

interface
uses Generics.Collections;

type TSymbol = class
  Symbol: String;
  Value: Word;
end;

type TSymbols = class
  private
    Symbols: TList<TSymbol>;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Clear;

    procedure InsertSymbol(Symbol: TSymbol);
    procedure LoadFile(Filename: String);

    //Returns the first symbol <= Value.
    function TryAddrToSymbol(Addr: Word;var Symbol: String): Boolean;
    function TrySymbolToAddr(ASymbol: String;var Addr: Word): Boolean;
    //Returns True if the Addr is in the range of Prior bytes before ASymbol to After bytes after ASymbol
    // (ASymbol-Prior) <= Addr <= (ASymbol + After)
    function TryIsInRange(Addr: Word;ASymbol: String;Prior, After: Word;var InRange: Boolean): Boolean;
end;

implementation
uses {$ifdef fpc}LazFileUtils,{$else}IOUtils,{$endif} Classes, SysUtils, Math;

{ TSymbols }

procedure TSymbols.Clear;
begin
  Symbols.Clear;
end;

constructor TSymbols.Create;
begin
  inherited Create;

  Symbols := TList<TSymbol>.Create;
end;

destructor TSymbols.Destroy;
begin
  Symbols.Free;

  inherited;
end;

procedure TSymbols.InsertSymbol(Symbol: TSymbol);
var
  I: Integer;
begin
  I := 0;
  while (I < Symbols.Count-1) and (Symbols[I].Value < Symbol.Value) do
    inc(I);
  Symbols.Insert(I, Symbol);
end;

function TSymbols.TryIsInRange(Addr: Word; ASymbol: String; Prior,
  After: Word;var InRange: Boolean): Boolean;
var
  SymbolAddr: Word;
begin
  Result := TrySymbolToAddr(ASymbol, SymbolAddr);
  if not Result then
    EXIT;
  InRange := (Addr >= SymbolAddr - Prior) and (Addr <= SymbolAddr + After);
end;

procedure TSymbols.LoadFile(Filename: String);

  procedure ParseLine(L: String);
  var
    A: TArray<String>;
    S: TSymbol;
  begin
    A := L.Split([' ']);
    if Length(A) < 2 then
      raise Exception.Create('Error in Symbol file - not enough parameters');

    S := TSymbol.Create;
    S.Symbol := A[0];
    if A[1].StartsWith('#') then
      A[1] := A[1].Substring(1);
    S.Value := StrToInt('$'+A[1]);

    InsertSymbol(S);
    Symbols.Add(S);
  end;

var
  S: TStringList;
  L: String;
begin
  S := TStringList.Create;
  try
    S.LoadFromFile(Filename);
(*    S.Text := TFile.ReadAllText(Filename);
*)
    for L in S do
      ParseLine(L);
  finally
    S.Free;
  end;
end;

function TSymbols.TrySymbolToAddr(ASymbol: String;var Addr: Word): Boolean;
var
  S: TSymbol;
begin
  for S in Symbols do
    if CompareText(S.Symbol, ASymbol) = 0 then
    begin
      Addr := S.Value;
      EXIT(True);
    end;

  Addr := 0;
  Result := False;
end;

function TSymbols.TryAddrToSymbol(Addr: Word;var Symbol: String): Boolean;
var I: Integer;
begin
  Symbol := '';
  I := 0;
  while (I < Symbols.Count) and (Symbols[I].Value <= Addr) do
    inc(I);

  dec(I);
  Result := I >= 0;
  if Result then
    Symbol := Symbols[I].Symbol + '+' + IntToHex(Addr - Symbols[I].Value, 4);
end;

end.

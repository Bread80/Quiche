unit mMapFile;

interface
uses Generics.Collections, Classes;

type
  PMapItem = ^TMapItem;
  TMapItem = record
    Bank: Integer;
    RealAddr: Integer;
    LogicalAddr: Integer;
    Bytecode: String;
    Time: Integer;
    Source: String;
    Filename: String;
    FileLine: Integer;
  end;

type TMapFile = class
    FText: TStringList;
    FItems: TList<PMapItem>;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Clear;

    procedure LoadFromFile(const AFilename: String);

    function TryAddrToSourceFile(Addr: Word;out Filename: String;out FileLine: Integer): Boolean;
  end;

implementation
uses SysUtils;

{ TMapFile }

procedure TMapFile.Clear;
begin
  FText.Clear;
  FItems.Clear;
end;

constructor TMapFile.Create;
begin
  inherited;
  FText := TStringList.Create;
  FItems := TList<PMapItem>.Create;
end;

destructor TMapFile.Destroy;
begin
  FText.Free;
  FItems.Free;
  inherited;
end;

procedure TMapFile.LoadFromFile(const AFilename: String);

  procedure SetItemWithFileLink(Item: PMapItem;const Source: String);
  var I: Integer;
    Colon: Integer;
  begin
    I := Length(Source)-2; //Ignore ')'
    Colon := -1;
    while (I >= 0) and (Source.Chars[I] <> '(') do
    begin
      if Source.Chars[I] = ':' then
        Colon := I;
      dec(I);
    end;
    if (I < 0) or (Colon < 0) then
      raise Exception.Create('Error in map file.');
    Item.Source := Source.SubString(0, I);
    TryStrToInt(Source.Substring(I + 2, Colon-I-2), Item.FileLine);
    Item.Filename := Source.Substring(Colon+1, Length(Source)-Colon-2);
  end;

var Line: String;
  Item: PMapItem;
begin
  Clear;
  FText.LoadFromFile(AFilename);

  for Line in FText do
  begin
    if (Line.Length = 0) or
      Line.StartsWith('Bnk|') or
      Line.StartsWith('----') or
      Line.StartsWith(' IX') or
      Line.StartsWith(' IY') then
      //Skip
    else
    begin

      if Line.Chars[3] = '|' then
      //Code or label
      begin
        New(Item);
        FItems.Add(Item);

        TryStrToInt('$' + Line.Substring(0,3), Item.Bank);
        TryStrToInt('$' + Line.Substring(4,4), Item.RealAddr);
        if not TryStrToInt('$' + Line.Substring(9,4), Item.LogicalAddr) then
          Item.LogicalAddr := Item.RealAddr;
        Item.ByteCode := Line.Substring(15,11);
        if not TryStrToInt(Line.Substring(28,2), Item.Time) then
          Item.Time := 0;
        if Line.EndsWith(')') then
          SetItemWithFileLink(Item, Line.Substring(31))
        else
          Item.Source := Line.Substring(31);
      end;
{      else
        Item.Source := Line.Substring(31);
}    end;
  end;
end;

function TMapFile.TryAddrToSourceFile(Addr: Word; out Filename: String;
  out FileLine: Integer): Boolean;
var I: Integer;
begin
  for I := FItems.Count-1 downto 0 do
    if Addr >= FItems[I].RealAddr then
    begin
      Filename := FItems[I].Filename;
      FileLine := FItems[I].FileLine;
      EXIT(True);
    end;

  Result := False;
end;

end.

unit IDE.CommandLine;

interface
uses Generics.Collections;

type
  //Does this option accept or require a data value?
  TCLOptionData = (sdNone, sdRequired, sdOptional);
  //Is this option required? Can it be specified multiple times?
  TCLOptionFlags = set of (sfRequired, sfAllowMultiple);

type

  TCLOptionMeta = class;

 { TCLOption }

 TCLOption = class
  private
    //Default place to store error messages
    FErrorMsg: String;
    FMeta: TCLOptionMeta;
  public
    constructor Create(AMeta: TCLOptionMeta);
    //By default, returns FErrorMsg.
    //Can be overriden to provide custom validation.
    function Validate: String;virtual;

    property Meta: TCLOptionMeta read FMeta;
  end;

  TCLOptionClass = class of TCLOption;

  { TCLOptionMeta }

  TCLOptionMeta = class
  private
    FHelp: String;
    FShortName: String;
    FLongName: String;
    FFlags: TCLOptionFlags;
    FData: TCLOptionData;
    FOptionClass: TCLOptionClass;
    function GetIsNoOption: Boolean;
  public
    constructor Create(const ALongName, AShortName: String;AData: TCLOptionData;
      AFlags: TCLOptionFlags; const AHelp: String;AOptionClass: TCLOptionClass);

    function GetOptionHelpText: String;

    property IsNoOption: Boolean read GetIsNoOption;
    property LongName: String read FLongName;
    property ShortName: String read FShortName;
    property Data: TCLOptionData read FData;
    property Flags: TCLOptionFlags read FFlags;
    property Help: String read FHelp;
    property OptionClass: TCLOptionClass read FOptionClass;
  end;

  //Class for unknown (invalid) options
  TCLUnknown = class(TCLOption)
  public
    constructor Create(AMeta: TCLOptionMeta;const Name: String);
  end;

  //Class for help option
  TCLHelp = class(TCLOption)
  end;

  { TCLValue }

  TCLValue = class(TCLOption)
  private
    FValue: String;
  public
    procedure AddValue(const AValue: String);virtual;
    property Value: String read FValue;
  end;

  //A default option is one which does not have an option specifier (ie the - prefixed term).
  //This is used, eg, for the file name to process.
  TCLNoOption = class(TCLValue)
  end;

  { TCommandLine }

  TCommandLine = class
  private
    FFilename: String;
    FOptions: TObjectList<TCLOption>;
    FMeta: TObjectList<TCLOptionMeta>;
    FErrorMsg: String;
    function GetErrorMsg: String;
  protected
    function MakeOption(const Name: String): TCLOption;
    function MakeNoOption(const AValue: String): TCLOption;
    //Verify the arguments passed are are valid
    procedure Validate;
  public
    constructor Create;
    destructor Destroy;override;

    procedure AddMeta(const ALongName, AShortName: String;AData: TCLOptionData;
    AFlags: TCLOptionFlags; const AHelp: String; AClass: TCLOptionClass);
    //Returns False if there are any parameter errors
    procedure Parse;

    function HaveHelp: Boolean;
    function GetHelpOption: TCLHelp;
    function GetOptionHelpText: String;
    function HaveError: Boolean;
    function GetNoOption: TCLNoOption;
    function HasOption(const Name: String): Boolean;
    function GetValueOption(const Name: String): TCLValue;

    //This executable name (ParamStr(0))
    property Filename: String read FFilename;
    property Options: TObjectList<TCLOption> read FOptions;
    property Meta: TObjectList<TCLOptionMeta> read FMeta;

    property ErrorMsg: String read GetErrorMsg;
  end;

implementation
uses SysUtils;

{ TCLOptionMeta }

function TCLOptionMeta.GetIsNoOption: Boolean;
begin
  Result := (FLongName = '') and (FShortName = '');
end;

constructor TCLOptionMeta.Create(const ALongName, AShortName: String;
  AData: TCLOptionData; AFlags: TCLOptionFlags; const AHelp: String;
  AOptionClass: TCLOptionClass);
begin
  FLongName := ALongName;
  FShortName := AShortName;
  FData := AData;
  FFlags := AFlags;
  FHelp := AHelp;
  FOptionClass := AOptionClass;
end;

function TCLOptionMeta.GetOptionHelpText: String;
begin
  Result := '';
  if FHelp = '' then
    EXIT;

  if (LongName <> '') and (LongName.Chars[0] = '<') then
    Result := ' ' + LongName
  else
  begin
    if ShortName <> '' then
      Result := Result + ' -' + ShortName;
    if LongName <> '' then
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + ' -' + LongName;
    end;
  end;

(*
  case Data of

  end;
*)
(*  Flags
*)
  Result := Result + ' - ' + Help;
end;

{ TCLOption }

constructor TCLOption.Create(AMeta: TCLOptionMeta);
begin
  FMeta := AMeta;
end;

function TCLOption.Validate: String;
begin
  Result := FErrorMsg;
end;

{ TCLUnknown }

constructor TCLUnknown.Create(AMeta: TCLOptionMeta;const Name: String);
begin
  inherited Create(AMeta);
  FErrorMsg := 'Invalid option: ''' + Name + '''';
end;

{ TCLValue }

procedure TCLValue.AddValue(const AValue: String);
begin
  if FValue <> '' then
  begin
    if FErrorMsg = '' then
      if Assigned(Meta) then
        FErrorMsg := 'Multiple values given for option -' + Meta.LongName
      else
        FErrorMsg := 'Multiple values given for un-named option';
  end
  else
   FValue := AValue;
end;

{ TCommandLine }

procedure TCommandLine.AddMeta(const ALongName, AShortName: String;
  AData: TCLOptionData; AFlags: TCLOptionFlags; const AHelp: String; AClass: TCLOptionClass);
begin
  Meta.Add(TCLOptionMeta.Create(ALongName, AShortName,
  AData, AFlags, AHelp, AClass));
end;

constructor TCommandLine.Create;
begin
  inherited Create;
  FOptions := TObjectList<TCLOption>.Create(True);
  FMeta := TObjectList<TCLOptionMeta>.Create(True);
end;

destructor TCommandLine.Destroy;
begin
  Options.Free;
  Meta.Free;
  inherited;
end;

function TCommandLine.GetErrorMsg: String;
begin
  Result := FErrorMsg;
end;

function TCommandLine.MakeOption(const Name: String): TCLOption;
var AMeta: TCLOptionMeta;
begin
  for AMeta in Meta do
    if (CompareText(AMeta.LongName, Name) = 0) or
      (CompareText(AMeta.ShortName, Name) = 0) then
      EXIT(AMeta.OptionClass.Create(AMeta));

  Result := TCLUnknown.Create(nil, Name);
end;

function TCommandLine.MakeNoOption(const AValue: String): TCLOption;
var AMeta: TCLOptionMeta;
begin
  for AMeta in Meta do
    if TCLNoOption.InheritsFrom(AMEta.OptionClass) then
    begin
      Result := AMeta.OptionClass.Create(AMeta);
      Assert(Result is TCLValue);
      TCLValue(Result).AddValue(AValue);
      EXIT;
    end;

  Result := TCLUnknown.Create(nil, 'Unable to parse value ''' + AValue + '''');
end;

procedure TCommandLine.Parse;
var I: Integer;
  S: String;
  Option: TCLOption;
begin
  if ParamCount > 0 then
    FFilename := ParamStr(0);
  Option := nil;
  for I := 1 to ParamCount do
  begin
    S := ParamStr(I);
//    writeln('ParamStr: ' + S);

    if S.Chars[0] = '-' then
    begin
      Option := MakeOption(S.SubString(1, Maxint));
      Options.Add(Option);
//      writeln('Add Option');
    end
    else if Option = nil then
    begin
      Option := MakeNoOption(S);
      Options.Add(Option);
      Option := nil;
//      writeln('Add non-option');
    end
    else
    begin
//      writeln('Add Value: ''' + S + '''');
      Assert(Option is TCLValue);
      TCLValue(Option).AddValue(S);
    end;
//    writeln(Options.Count);
  end;

  //Validate
  for Option in Options do
  begin
//    writeln(Option.ClassName);
//    if Assigned(Option.Meta) then
//      writeln('  ', Option.Meta.LongName);
    FErrorMsg := Option.Validate;
    if FErrorMsg <> '' then
      EXIT;
  end;

  if not HaveError then
    Validate;
end;

function TCommandLine.HaveHelp: Boolean;
begin
  Result := GetHelpOption <> nil;
end;

function TCommandLine.GetHelpOption: TCLHelp;
var Option: TCLOption;
begin
  for Option in Options do
    if Option is TCLHelp then
      EXIT(Option as TCLHelp);

  Result := nil;
end;

function TCommandLine.GetOptionHelpText: String;
var AMeta: TCLOptionMeta;
begin
  Result := '';
  for AMeta in Meta do
    Result := Result + AMeta.GetOptionHelpText + #13#10;
end;

function TCommandLine.HaveError: Boolean;
begin
  Result := ErrorMsg <> '';
end;

function TCommandLine.GetNoOption: TCLNoOption;
var Option: TCLOption;
begin
  for Option in Options do
    if Option is TCLNoOption then
      EXIT(Option as TCLNoOption);

  Result := nil;
end;

function TCommandLine.HasOption(const Name: String): Boolean;
var Option: TCLOption;
begin
  for Option in Options do
    if (CompareText(Name, Option.Meta.LongName) = 0) or
      (CompareText(Name, Option.Meta.ShortName) = 0) then
      EXIT(True);

  Result := False;
end;

function TCommandLine.GetValueOption(const Name: String): TCLValue;
var Option: TCLOption;
begin
  for Option in Options do
    if Option is TCLValue and Assigned(Option.Meta) and
      ((CompareText(Name, Option.Meta.LongName) = 0) or
      (CompareText(Name, Option.Meta.ShortName) = 0)) then
      EXIT(Option as TCLValue);

  Result := nil;
end;

procedure TCommandLine.Validate;
var Count: Integer;
  AMeta: TCLOptionMeta;
  Option: TCLOption;
begin
  for AMeta in Meta do
  begin
    Count := 0;
    for Option in Options do
      if Assigned(Option.Meta) then
      begin
//        writeln(' ',Option.Meta.LongName);
        if Option.Meta = AMeta then
          inc(Count);
      end
      else if Option is AMeta.OptionClass then
      begin
//        writeln(' ',Option.Classname);
        inc(Count);
      end;
//    writeln(AMeta.LongName + ' ' + Count.ToString);

    if (Count = 0) and (sfRequired in AMeta.flags) then
    begin
      if (AMeta.LongName <> '') and (AMeta.LongName.Chars[0] = '<') then
        FErrorMsg := AMeta.LongName + ' not specified'
      else
        FErrorMsg := 'Required option -' + AMeta.LongName + ' not specified';
    end;
    if (Count > 1) and not (sfAllowMultiple in AMeta.Flags) then
    begin
      if (AMeta.LongName <> '') and (AMeta.LongName.Chars[0] = '<') then
        FErrorMsg := AMeta.LongName + ' given multiple times'
      else
        FErrorMsg := 'Option -' + AMeta.LongName + ' given multiple times';
    end;
  end;
end;

end.

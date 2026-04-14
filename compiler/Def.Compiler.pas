unit Def.Compiler;

interface
uses SysUtils, Classes, Generics.Collections,
  Def.Variables, Def.IL, Def.Scopes, Def.UserTypes,
  CleverPuppy;

type
  //An IILScope is a scope which stores IL code (as opposed to a parse scope which
  //is only relevant to the parser.
  //Also Marks the endpoint for a 'local' scope when searching up
  TILScope = class(TScope)
  private
    FILList: TILList;
    FAsmData: TStringList;
    FAsmCode: TStringList;
    FCleverPuppy: TCleverPuppy;
  public
    constructor Create(const AName: String;AOwner: TScope);
    destructor Destroy;override;

    function QualifiedName: String;
    function FindByName(const AName: String): TILScope;

    function GetLocalAddrMode: TAddrMode;virtual;

    //Probly broken at the moment!
    procedure RunCleverPuppy;

    //Returns a list of the names of all ILScopes.
    procedure ListILScopes(S: TStrings);

    property AsmCode: TStringList read FAsmCode;   //Assembly code for Code segment for this scope (from CodeGen)
    property AsmData: TStringList read FAsmData;   //Assembly code for Data segment (from CodeGen)
    property ILList: TILList read FILList;
    property CleverPuppy: TCleverPuppy read FCleverPuppy;  //Clever puppy codegen data (if any)
  end;

  //Declarations for records
  TRecord = class(TILScope)
  end;

  //Library units
  TUnit = class(TILScope)
  end;

  //The scope itself is the main program. FUnits describe any included units (including System)
  TProgram = class(TILScope)
  private
    FUnits: TObjectList<TUnit>;
  protected
    procedure WarmInit;
  public
    constructor Create;
    destructor Destroy;override;

    //Searched the user program and all units
    function SearchDownUnits<T: Class>(Func: TFunc<T, Boolean>): T;
    function SearchItemsUnits<T: Class>(Func: TFunc<T, Boolean>): T;

    property Units: TObjectList<TUnit> read FUnits;  //Of TUnit
  end;

function TheProgram: TProgram;

//If Wamr is True only the program data will be deleted (all libraries will
//be retained)
//Returns True if a warm init was actioned. A cold init will be performed if
//library data has yet to be created
function InitProgram(Warm: Boolean): Boolean;

implementation
uses Def.Globals;

var TheProgramVar: TProgram;

function TheProgram: TProgram;
begin
  Result := TheProgramVar;
end;

{ TILScope }

constructor TILScope.Create(const AName: String; AOwner: TScope);
begin
  inherited Create(AName, AOwner);

  FAsmCode := TStringlist.Create;
  FAsmData := TStringList.Create;
  FILList:= CreateILList;
  FCleverPuppy := nil;
end;

destructor TILScope.Destroy;
begin
  FAsmCode.Free;
  FAsmData.Free;
  ClearILList(ILList);
  FILList.Free;
  FCleverPuppy.Free;

  inherited;
end;

function TILScope.FindByName(const AName: String): TILScope;
begin
  if CompareText(AName, Name) = 0 then
    EXIT(Self);

  Result := SearchDown<TILScope>(
    function(Scope: TILScope): Boolean
    begin
      Result := CompareText(Scope.Name, AName) = 0;
    end);
end;

function TILScope.GetLocalAddrMode: TAddrMode;
begin
  Result := optDefaultAddrMode;
end;

procedure TILScope.ListILScopes(S: TStrings);
begin
  S.Add(QualifiedName);
  EachDown<TILScope>(
    procedure(Scope: TILScope)
    begin
      Scope.ListILScopes(S);
    end);
end;

function TILScope.QualifiedName: String;
begin
  if Assigned(Owner) and (Owner is TILScope) then
    if not (Owner is TProgram) then
    Result := TILScope(Owner).QualifiedName
  else
    Result := '';
  Result := Result + Name;
end;

procedure TILScope.RunCleverPuppy;
begin
  FCleverPuppy := TCleverPuppy.Create;
  CleverPuppy.ProcessSection;
end;

{ TProgram }

constructor TProgram.Create;
begin
  inherited Create('_Main', nil);
  FUnits := TObjectList<TUnit>.Create;
end;

destructor TProgram.Destroy;
begin
  FUnits.Free;
  inherited;
end;

function TProgram.SearchDownUnits<T>(Func: TFunc<T, Boolean>): T;
var LUnit: TScope;
begin
  Result := nil;
  for LUnit in Units do
  begin
    Result := LUnit.SearchDown<T>(Func);
    if Assigned(Result) then
      EXIT;
  end;
end;

function TProgram.SearchItemsUnits<T>(Func: TFunc<T, Boolean>): T;
var LUnit: TUnit;
begin
  Result := nil;
  for LUnit in Units do
  begin
    Result := LUnit.SearchItems<T>(Func);

    if Assigned(Result) then
      EXIT;
  end;
end;

procedure TProgram.WarmInit;
begin
  Items.Clear;
end;

function InitProgram(Warm: Boolean): Boolean;
begin
  Result := Warm;
  if Warm then
    TheProgram.WarmInit
  else
  begin
    if Assigned(TheProgramVar) then
      TheProgram.Free;
    TheProgramVar := TProgram.Create;
    Result := False;
  end;
end;

initialization
  InitProgram(False);
finalization
  if Assigned(TheProgramVar) then
    FreeAndNil(TheProgramVar);
end.

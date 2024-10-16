(*
  Compile time constants

  Currently repurposes the code and data structures for Variables
*)
unit Def.Consts;

interface
uses
  Generics.Collections,
  Def.Variables, Def.QTypes;

type
  PConst = ^TConst;
  TConst = record
    Name: String;
    VarType: TVarType;
    InScope: Boolean;
    Depth: Integer;
    Value: TImmValue;
  end;

type
  PConstList = ^TConstList;
  TConstList = record
  private
    Items: TList<PConst>;
    MarkPosition: Integer;
  public
    procedure Initialise;
    procedure Clear;
    //Scope depth has been DECrememented. Any in scope constants with higher scope depth
    //need to go out of scope
    procedure ScopeDepthDecced(NewDepth: Integer);

    function Add(const AName: String;AVarType: TVarType;const AValue: TImmValue): PConst;
    function FindByNameInScope(const AName: String): PConst;
  end;

//Currently scoped list of constants
var Consts: PConstList;

procedure InitialiseConsts;

//------------Scope related

//These routines are called by the Scopes routines to create, clear and set Scopes
function CreateConstList: PConstList;
procedure SetCurrentConstList(List: PConstList);

implementation
uses
  SysUtils, Classes,
  Def.Scopes;

//================SCOPE RELATED
(*
var
  Consts: TConstList;
  ConstMarkPosition: Integer;
*)
procedure InitialiseConsts;
begin
  Consts := nil;
//  ConstMarkPosition := -1;
end;

function CreateConstList: PConstList;
begin
  New(Result);
  Result.Initialise;
//  ConstMarkPosition := -1;
end;

procedure SetCurrentConstList(List: PConstList);
begin
  Consts := List;
end;

{ TConstList }


function TConstList.Add(const AName: String; AVarType: TVarType;
  const AValue: TImmValue): PConst;
begin
  New(Result);
  Items.Add(Result);
  Result.Name := AName;
  Result.VarType := AVarType;
  Result.InScope := True;
  Result.Depth := GetCurrentScope.Depth;
  Result.Value := AValue;
end;

procedure TConstList.Clear;
var V: PConst;
begin
  for V in Items do
    Dispose(V);
  Items.Clear;
  MarkPosition := -1;
end;

function TConstList.FindByNameInScope(const AName: String): PConst;
var I: Integer;
begin
  for I := 0 to Items.Count-1 do
    if (CompareText(Items[I].Name, AName) = 0) and Items[I].InScope then
      EXIT(Items[I]);

  Result := nil;
end;

procedure TConstList.Initialise;
begin
  Items := TList<PConst>.Create;
  MarkPosition := -1;
end;

procedure TConstList.ScopeDepthDecced(NewDepth: Integer);
var I: Integer;
begin
  I := Items.Count-1;
  while (I >= 0) and (Items[I].Depth > NewDepth) do
  begin
    Items[I].InScope := False;
    dec(I);
  end;
end;

initialization
  Consts := nil;
end.

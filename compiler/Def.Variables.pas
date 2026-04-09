unit Def.Variables;

interface
uses Classes, Generics.Collections, SysUtils,
  Def.VarTypes, Def.UserTypes, Def.Consts, Def.ScopesEX;

type
  TAddrMode = ( //Addressing mode for variable storage
    amStatic,   //Absolute, permanent memory location
                //VarAddr := <label>
                //VarValue := (<label>)
    amStack,    //Relative, offset from a base address (i.e in stack frame)
                //VarAddr := IX+<offset>
                //VarValue := (IX+<offset>)
    amStaticRef,  //The address of the variable data is stored at an absolute (static)
                //location. The variable *must* be of a Pointered type
                //VarAddr := (<label>)
                //VarValue := <addr> := (<label>). <value> := (<addr>)
    amStackRef  //The address of the variable data is stored on the stack at a location
                //relative to the stack pointer (IX)
                //VarAddr := (IX+<offset>)
                //VarValue := <addr> := (IX+<offset>). <value> := (<addr>)
    );

  EAddrMode = class(Exception)
    constructor Create;reintroduce;
  end;

  //Controls accessibility of variables and function parameters
  TVarAccess = (
    vaNone,   //Parameter not assigned or other error
    vaLocal);  //Local variable
//    vaGlobal, //Globally stored variable (but only accessible within Scope)

  TVariable = class(TTypedIdentifier)
  private
    FAddrMode: TAddrMode;
    FVersion: Integer;
    FTouched: Boolean;
    FAdjustVersionTo: Integer;
    FValue: TImmValue;
    FAdjustVersionFrom: Integer;
    FAccess: TVarAccess;
    FOffset: Integer;

    procedure Initialise(AAddrMode: TAddrMode);
  protected
    function GetIsConst: Boolean;virtual;
  public
    constructor Create(const AName: String; AOwner: TScope;AUserType: TUserType;AAddrMode: TAddrMode);
    constructor CreateUnnamed(AOwner: TScope; AUserType: TUserType; AAddrMode: TAddrMode);
    constructor CreateHidden(AOwner: TScope; AUserType: TUserType;AAddrMode: TAddrMode);

    function IdentType: TIdentType;override;

    //There are times when a variable must be created before the type is determined.
    //In such cases use this function will assign a type.
    //The method is to be used only with extreme caution.
    //This function will fail if UserType is already assigned
    procedure AssignType(AUserType: TUserType);

    //Increments the Version of a variable and returns the new value
    //Does not increment if SkipMode is enabled
    function IncVersion: Integer;virtual;

    //----Phi function processing

    //Mark a variable as 'touched'
    procedure Touch;

    //----Code generation

    //Returns the name of the variable used in Assemby code
    function GetAsmName: String;

    //Returns true if the variable requires storage, false if not.
    //Varaibles don't require storage include:
    // - those which have been optimised away
    // - ShadowOf variables
    function RequiresStorage: Boolean;

    //Returns the number of bytes to be allocated on the stack for storing this
    //variable as a local variable. Returns 0 if the variable's data is not passed via the
    //stack
    function GetStackBytesAsLocal: Integer;virtual;

    //---UI
    function Description: String;
    function ToString: String;override;

    //---Properties

    //Addressing mode - where is the data stored and how to access it
    property AddrMode: TAddrMode read FAddrMode;
    //Access type for parameters (paValue for locals)
    property Access: TVarAccess read FAccess;
    //True if the variable is a CONST argument. If so it must not be written to
    //and must not be passed as an argument to any function which may modify it's
    //value (ie. VAR or OUT access)
    property IsConst: Boolean read GetIsConst;
    //If the Storage is:
    //vsOffset, this is the offset from the stack base address
    //vsFixed: the offset from the start of the Data segment
    property Offset: Integer read FOffset write FOffset;

    //===Parse and execution time data

    //Version is incremented every time the variable is written to. This (along
    //with phi vars) enable the optimiser and code generator to track what is
    //stored in the variable at any point and optimise appropriately
    property Version: Integer read FVersion;

    //===Data used only for SSA/Phi variable generation

    //Temporary data used when generating phi functions
    property Touched: Boolean read FTouched write FTouched;
    //Temp data used while doing branch fixups.
    //If a variable read has the given version index...
    property AdjustVersionFrom: Integer read FAdjustVersionFrom write FAdjustVersionFrom;
    //...we need to change that read to reference this version.
    property AdjustVersionTo: Integer read FAdjustVersionTo write FAdjustVersionTo;

    //===Execution time only data

    //Value (read from emulator)
    property Value: TImmValue read FValue write FValue;
  end;

  TVars = class
  private
    FSkipMode: Boolean;
  public
    //Creates a new, uniquely named variable. If a variable with that name already exists
    //returns nil. Uses storage type for current Scope (and/or Func)
    class function Add(const AName: String;AUserType: TUserType): TVariable;
    class function AddWithAddrMode(const AName: String;AUserType: TUserType;AAddrMode: TAddrMode): TVariable;
    //Creates a variable with no name. The name *must* be assigned as soon as they
    //are known.
    class function AddUnnamed(AUserType: TUserType): TVariable;
    //Adds a hidden variable - one which the user is unaware of. Used within expressions etc.
    class function AddHidden(AUserType: TUserType): TVariable;
    class function AddHiddenWithAddrMode(AUserType: TUserType;AAddrMode: TAddrMode): TVariable;

    //--------------Finding/Accessing
    //Find a variable by name across all current scopes.
    class function FindByNameAllScopes(const AName: String): TVariable;

    //-----------------------------Phis and fixups

    //Clear the Touched flag for all variables in local scope
    class procedure ClearTouches;
    class procedure ClearAdjust;

    //=====
    //Skip mode enables IL generation to easily be skipped or rolled back by
    //stopping certain internal actions. Skip mode is used to avoid generating code
    //which will never be exectuted

    //Enable parameter enables code to conditionally enter skip mode without
    //caring whether the skip mode is actually needed
    //Enables Skip mode if Enable is true.
    //Does nothing if Enable is False
    //Either way, returns previous skip mode. This value MUST be passed to SkipModeEnd
    function SkipModeStart(Enable: Boolean): Boolean;
    //To be called at the end of Skip mode. PrevSkipMode MUST be the value returned by the
    //previous call to SkipModeStart
    procedure SkipModeEnd(PrevSkipMode: Boolean);

    property SkipMode: Boolean read FSkipMode;
  end;

procedure InitialiseVars;

var Vars: TVars;

implementation
uses {IOUtils,}
  Def.Globals, Def.Scopes, Def.IL,
  Parse.Base;

{ EAddrMode }

constructor EAddrMode.Create;
begin
  inherited Create('Invalid addressing mode');
end;

{ TVariable }

procedure TVariable.AssignType(AUserType: TUserType);
begin
  Assert(AUserType = UserType, 'Already done!!');
//  Assert(UserType = nil);
//  UpdateUserType(AUserType);
end;

constructor TVariable.Create(const AName: String; AOwner: TScope;AUserType: TUserType;
  AAddrMode: TAddrMode);
begin
  inherited Create(AName, AOwner, AUserType);
  Initialise(AAddrMode);
end;

constructor TVariable.CreateHidden(AOwner: TScope; AUserType: TUserType;
  AAddrMode: TAddrMode);
var LName: String;
begin
  LName := '_temp' + Integer(Self).ToString;
  Create(LName, AOwner, AUserType, AAddrMode);
end;

constructor TVariable.CreateUnnamed(AOwner: TScope; AUserType: TUserType;
  AAddrMode: TAddrMode);
begin
  Create('', AOwner, AUserType, AAddrMode);
end;

function TVariable.Description: String;
begin
  Result := UserType.Description + ' //' + GetAsmName + ': ';

//  Result := Result + AddrModeToString + ' ';
  case AddrMode of
    amStatic, amStaticRef:
      if Offset <> -1 then
      begin
        Result := Result + '@'+IntToHex(Offset, 4).Tolower;
        if Value.UserType <> nil then
          Result := Result + ' = ' + Value.ToString;
      end;
    amStack, amStackRef:
      //vsOffset
      if Offset < 0 then
        Result := Result + IntToStr(Offset)
      else
        Result := Result + '+' + IntToStr(Offset);
  else
    Assert(False);
  end;
end;

function TVariable.GetAsmName: String;
begin
  Result := '_v_' + Owner.Name + '.' + Name;
end;

function TVariable.GetIsConst: Boolean;
begin
  Result := False;
end;

function TVariable.GetStackBytesAsLocal: Integer;
begin
  if RequiresStorage then
    if Access in [vaLocal]then
      case AddrMode of
        amStatic, amStaticRef: Result := 0;
        amStack: Result :=  UserType.DataSize;
        amStackRef: Result := GetVarTypeSize(vtPointer);
      else
        raise EAddrMode.Create;
      end;
end;

function TVariable.IdentType: TIdentType;
begin
  Result := itVariable;
end;

function TVariable.IncVersion: Integer;
begin
  Result := Version + 1;
  if not Vars.SkipMode then
    FVersion := Result;
end;

procedure TVariable.Initialise(AAddrMode: TAddrMode);
begin
  FAddrMode := AAddrMode;
  FVersion := 0;
  FTouched := False;
  FOffset := -1;
  FAccess := vaLocal;
  Value.CreateTyped(UserType, 0);
end;

function TVariable.RequiresStorage: Boolean;
begin
  Result := True;
end;

function TVariable.ToString: String;
begin
  Result := 'var ' + Name + ': ' + Description;
end;

procedure TVariable.Touch;
begin
  FTouched := True;
end;

{ TVars }

class function TVars.Add(const AName: String; AUserType: TUserType): TVariable;
begin
  Result := AddWithAddrMode(AName, AUserType, GetCurrentScope.GetLocalAddrMode);
end;

class function TVars.AddHidden(AUserType: TUserType): TVariable;
begin
  Result := AddHiddenWithAddrMode(AUserType, GetCurrentScope.GetLocalAddrMode);
end;

class function TVars.AddHiddenWithAddrMode(AUserType: TUserType;
  AAddrMode: TAddrMode): TVariable;
var Scope: TScope;
begin
  Scope := GetCurrentScope.BlockScope;
  Result := TVariable.CreateHidden(Scope, AUserType, AAddrMode);
  Scope.Add(Result);
end;

class function TVars.AddUnnamed(AUserType: TUserType): TVariable;
var Scope: TScope;
begin
  Scope := GetCurrentScope.BlockScope;
  Result := TVariable.CreateUnnamed(Scope, AUserType, GetCurrentScope.GetLocalAddrMode);
  Scope.Add(Result);
end;

class function TVars.AddWithAddrMode(const AName: String; AUserType: TUserType;
  AAddrMode: TAddrMode): TVariable;
var Scope: TScope;
begin
  Assert(AName <> '');
  if GetCurrentScope.SearchUpLocal(AName).IdentType <> itUnknown then
    Result := nil
  else
  begin
    Scope := GetCurrentScope.BlockScope;
    Result := TVariable.Create(AName, Scope, AUserType, AAddrMode);
    Scope.Add(Result);
  end;
end;

class procedure TVars.ClearAdjust;
var Scope: TScope;
begin
  Scope := GetCurrentScope.BlockScope;
  Assert(Scope is TCodeBlock);
  (Scope as TCodeBlock).EachAllLevel<TVariable>(
    procedure(V: TVariable)
    begin
    V.AdjustVersionFrom := -1;
    V.AdjustVersionTo := -1;
    end);
end;

class procedure TVars.ClearTouches;
var Scope: TScope;
begin
  Scope := GetCurrentScope.BlockScope;
  Assert(Scope is TCodeBlock);
  (Scope as TCodeBlock).EachAllLevel<TVariable>(
    procedure(V: TVariable)
    begin
      V.Touched := False;
    end);
end;

class function TVars.FindByNameAllScopes(const AName: String): TVariable;
var IdentData: TIdentData;
begin
  IdentData := GetCurrentScope.SearchUpAll(AName);
  if IdentData.IdentType = itVariable then
    EXIT(IdentData.Value as TVariable);

  Result := nil;
end;

procedure TVars.SkipModeEnd(PrevSkipMode: Boolean);
begin
  //Only when disabling SkipMode
  if FSkipMode and not PrevSkipMode then
  begin
    ILRollback;
//    Vars.Rollback;
  end;
  FSkipMode := PrevSkipMode;
end;

function TVars.SkipModeStart(Enable: Boolean): Boolean;
begin
  //Only when turning on skip mode!
  if Enable and not SkipMode then
  begin //Mark current positions
    ILMark;
(*    Vars.Mark;*)
  end;
  Result := SkipMode;
  if Enable then
    FSkipMode := True;
end;

procedure InitialiseVars;
begin
  if Vars <> nil then
    Vars.Free;
  Vars := TVars.Create;
end;

initialization
  Vars := nil
end.

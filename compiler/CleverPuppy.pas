unit CleverPuppy;

interface
uses Classes, Generics.Collections,
  Def.IL, Def.Functions,
  Lib.Data,
  Z80.CPUState, Z80.AlgoData,
  Z80.Hardware, Z80.Algos;

type
  TKeepOrKill = (kkKeep, kkKill);

  TChunk = class;
  TChunkSet = class;
  TChunkSequence = class;
  TChunkIL = class;
  TLogProc = procedure(Sender: TChunk;const Msg: String) of object;

  //Abstract parent
  TChunk = class
  private
    FCosts: TCosts;
    FLogProc: TLogProc;
    FPrev: TChunk;
    FNext: TChunk;

    function GetRegState: PCPUState;virtual;abstract;
    function GetCount:Integer;virtual;abstract;
    procedure SetNext(const Value: TChunk);
    procedure SetPrev(const Value: TChunk);
  protected
    //Creates an opbect of the same class as Self
    //(I'm sure this can be done using classes, but this works for now)
    function Instantiate: TChunk;virtual;abstract;
    //Copies state from From into Self. From must be same class as Self.Class,
    //or a class descended from Self.Class
    procedure CopyFrom(From: TChunk);virtual;
    //Creates and returns a copy of Self (using Instantiate and CopyFrom)
    function Dup: TChunk;

    //Reduce any Combos to a single, most optimal Combo. (If there are multiple
    //optimatal Combos, then choose a single one by whatever method).
    procedure Flatten;virtual;

    procedure CalcCosts;virtual;abstract;

    //Descends the first sequence items and first set items to get the
    //first TChunkIL item.
    //If none exists returns nil
    function GetFirstChunkIL: TChunkIL;virtual;
    function GetLastChunkIL: TChunkIL;virtual;
  public
    constructor Create(ALogProc: TLogProc);
    property LogProc: TLogProc read FLogProc;

    property Prev: TChunk read FPrev write SetPrev;
    property Next: TChunk read FNext write SetNext;
    //Returns the RegState of the last item in a sequence
    property RegState: PCPUState read GetRegState;

    property FirstChunkIL: TChunkIL read GetFirstChunkIL;
    property LastChunkIL: TChunkIL read GetLastChunkIL;
    property Costs: TCosts read FCosts;
    property Count: Integer read GetCount;
  end;

  //Abstract parent
  //A chunk which contains a set of alternate, equivalent, combos
  //for a single IL step, only one of which will eventually be chosen
  //to generate
  TChunkSet = class(TChunk)
  private
    Combos: TObjectList<TChunk>;
    function GetCount: Integer;override;
  protected
    function Instantiate: TChunk;override;
    procedure CopyFrom(From: TChunk);override;

    procedure Flatten;override;

    procedure CalcCosts;override;
    function GetFirstChunkIL: TChunkIL;override;
    function GetLastChunkIL: TChunkIL;override;

    //Is there an alternative Combo which is more performant and with the
    //same end result (CPU state)?
    //If so returns True, so the caller can delete the given Combo
    function IsInferior(Combo: TChunk): TKeepOrKill;

    //Sub to DeDup
    //Searches all Combos after Index (> Index),
    //if they leave the same CPU state a Index then
    //if they're worse than us remove them,
    //if they're better than us exit with True so caller can remove us.
    function DeDupItem(Index: Integer): TKeepOrKill;
    //Compares all combos, if any leave the same state removes the worse one
    //(or a random one if they have the same performance)
    procedure DeDup;

    //To be called after a new chunk has been appended and permuted.
    //For each Combo,
    //* If a source value can be scavenged, add a combo which scavenges it.
    //* Extra Combos may also be added for other possible optimisations of source params
    //Any Combos which are ound be be invalid will be removed.
    procedure TailScavengeLoads;
    //Add a Combo to the Combos list
    procedure AddCombo(Combo: TChunk);

  public
    constructor Create(ALogProc: TLogProc);
    destructor Destroy;override;

    procedure AddCopyWithNewAlgo(Combo: TChunk;Step: TChunkIL;Param: PILParam;Algo: TAlgo;AlgoReg: TCPUReg);

    function ToString: String;override;
  end;

  //Sequence of chunks. Each chunk defines one or more steps
  TChunkSequence = class(TChunk)
  private
    Steps: TObjectList<TChunk>;
    function GetCount: Integer;override;
    function GetRegState: PCPUState;override;
    function GetLast: TChunk;
  protected
    function Instantiate: TChunk;override;
    procedure CopyFrom(From: TChunk);override;

    procedure CalcCosts;override;
    function GetFirstChunkIL: TChunkIL;override;
    function GetLastChunkIL: TChunkIL;override;

    procedure Flatten;override;

    //Implements TChunkSet.TailReduceCombos for a single Combo (which is a sequence)
    function TailReduceSequence: Boolean;
    procedure AddStep(Step: TChunk);

  public
    constructor Create(ALogProc: TLogProc);
    destructor Destroy;override;

    function IndexOf(AStep: TChunk): Integer;

    //Returns the last Chunk in the sequence. If no steps, returns nil
    property Last: TChunk read GetLast;
    function ToString: String;override;
  end;

  //A Chunk representing a single code Block, A code Block is a piece of
  //linear code with no branches in, out, or within.
  TChunkBlock = class(TChunkSequence)
  private
  protected
    function Instantiate: TChunk;override;

    //Generates a ChunkSet of all possible sequences for (Left, Right)
    //where Left and Right are subsequent steps.
    //Returns the new ChunkSet
    function Permute(Left, Right: TChunk): TChunkSet;
    //Process operations (which operate on data). Ignores assigns
    procedure MergeSteps;
  public
  end;

  TCleverPuppy = class
  private
    FBlocks: TChunkSequence;
    LogProc: TLogProc;
    FLog: TStringList;
  protected
    procedure AddLog(Sender: TChunk;const Msg: String);
    //Copy IL data into Chunks, and generate Combos for each
    procedure ImportSection;
    //Copy IL data from Chunks to the ILList, replacing previous data
    procedure ExportSection;
  public
    constructor Create;
    destructor Destroy;override;
    procedure ProcessSection;

    function ToString: String;override;

    property Log: TStringList read FLog;
  end;

  //A chunk for a single IL step (or Steps if extended)
  TChunkIL = class(TChunk)
  private
    FItemIndex: Integer;  //Index within IL
    Items: TArray<TILItem>;
    //Array elements are indexes into Params
    FSourceParams: TArray<Integer>;
    DestParams: TArray<Integer>;
    RegState: TCPUState;  //CPU state after the item

    //If any of the Items has a Prim assigned returns it, otherwise returns nil
    function GetPrim: PPrimitive;

    procedure AddItem(Item: PILItem);
    procedure AddSourceParam(ParamIndex: Integer);
    procedure AddDestParam(ParamIndex: Integer);
    function ParamCount: Integer;
    function GetParam(Index: Integer): PILParam;
    //Index into SourceParams array
    function GetSourceParam(Index: Integer): PILParam;
    function GetSourceParamInGenOrder(Order: Integer): PILParam;
    function GetDestParam(Index: Integer): PILParam;

    //Copy data for ILItem(s)
    procedure InitItems(AnItemIndex: Integer);
    function GetSourceCount: Integer;
    function GetSourceParams(Index: Integer): PILParam;
    function GetChunkIL: TChunkIL;
  protected
    function Instantiate: TChunk;override;
    procedure CopyFrom(From: TChunk);override;

    procedure CalcCosts;override;
    procedure UpdateRegStateForSourceParams;
    procedure UpdateRegStateForPrim;
    procedure UpdateRegStateForDestParams;

    function GetRegState: PCPUState;override;
  public
    //Create for the given IL step
    constructor Create(ALogProc: TLogProc;AnItemIndex: Integer);

    function IndexOfParam(AParam: PILParam): Integer;

    procedure UpdateRegState;overload;
    //Set the RegState field based on InitialRegState
    procedure UpdateRegState(const InitialRegState: TCPUState);overload;

    //The number of source params
    property SourceCount: Integer read GetSourceCount;
    property SourceParams[Index: Integer]: PILParam read GetSourceParams;
    function ToString: String;override;
  end;

  //A set where every Combo is an TChunkIL
  TChunkILSet = class(TChunkSet)
  private
    //Copy Base and add it to Combos
    function AddCopyCombo(Base: TChunkIL): TChunkIL;
    //Initialise Combos. Copy data for ILItem and it's Params
    procedure InitIntrinsic(Base: TChunkIL);

    //Set ending CPU state for every combo
    procedure SetCPUStates;

    procedure ComboParamImmediate(Base: TChunkIL;Index: Integer);
    procedure ComboParamVarDest(Base: TChunkIL;Index: Integer);
    procedure ComboParamVarSource(Base: TChunkIL;Index: Integer);
    procedure ComboParam(Base: TChunkIL;Index: Integer);
  protected
    function Instantiate: TChunk;override;

  public
    //Create for the given IL setp where the Op is an intrinsic
    constructor CreateIntrinsic(ALogProc: TLogProc;Index: Integer);
  end;

implementation
uses SysUtils,
  Def.Operators, Def.Variables, Def.VarTypes, Def.Scopes, Def.UserTypes,
  Lib.CPUState, Lib.Primitives,
  Z80.GenProcs,
  Puppy.Source;

{
  AddForPrim
  AddForParam(Param, Index)
    -> Recursively calls itself for each next param
  AddForDestAlgo
  AddForMoveAlgo
  AddForParamSourceAlgo inc AddForParamRegSource


  Each Prim
    Each DestAlgo
    Each MoveAlgo
      Each Param (normal order)
        Each SourceAlgo
        For saRegister, each MoveAlgo
      Each Param (swapped order)
        Each SourceAlgo
        For saRegister, each MoveAlgo



  TChunkILSet.Operation
  For each Prim:
    For each Param:
      Get set of SourceAlgos
      For saRegister, get list of Registers and MoveAlgos
    Get set of MoveAlgos
    Get set of DestAlgos
    GenCombo(ILItem, Prim, SourceAlgoList, MoveAlgos, DestAlgos);


    For Each Param1 Source (Variable, Pop, Reg)
      If it's viable:
        For Each Param2 Source (Variable, Pop, Reg)
          If it's viable

    For Each Dest memory (none, variable, push)
      if Its viable:
        Go
    For each Dest Reg
      Go

  Get list of suitable primitives
  For each primitive:
    For each input parameter, iterate the possible sources of data:
      Registers, Immediate, static, stack, pop etc
      Note range check requirements

}

{ TChunk }

procedure TChunk.CopyFrom(From: TChunk);
begin
  Prev := From.Prev;
  Next := From.Next;
end;

constructor TChunk.Create(ALogProc: TLogProc);
begin
  inherited Create;
  FLogProc := ALogProc;
end;

function TChunk.Dup: TChunk;
begin
  Result := Instantiate;
  Result.CopyFrom(Self);
end;

procedure TChunk.Flatten;
begin
  //Nothing
end;

function TChunk.GetFirstChunkIL: TChunkIL;
begin
  Result := nil;
end;

function TChunk.GetLastChunkIL: TChunkIL;
begin
  Result := nil;
end;

procedure TChunk.SetNext(const Value: TChunk);
begin
  Assert(Next = nil);
  FNext := Value;
end;

procedure TChunk.SetPrev(const Value: TChunk);
begin
  Assert(Prev = nil);
  FPrev := Value;
end;

{ TChunkSet }

procedure TChunkSet.AddCombo(Combo: TChunk);
begin
  Combos.Add(Combo);
end;

procedure TChunkSet.AddCopyWithNewAlgo(Combo: TChunk;Step: TChunkIL;Param: PILParam;
  Algo: TAlgo;AlgoReg: TCPUReg);
var NewCombo: TChunk;
  ChunkIL: TChunkIL;
  StepIndex: Integer;
  ParamIndex: Integer;
begin
  NewCombo := Combo.Dup;
  Combos.Add(NewCombo);
  ChunkIL := nil;
  if NewCombo is TChunkSequence then
  begin
    StepIndex := TChunkSequence(Combo).IndexOf(Step);
    ChunkIL := TChunkSequence(NewCombo).Steps[StepIndex] as TChunkIL;
  end
  else if NewCombo is TChunkIL then
    ChunkIL := TChunkIL(NewCombo);
  Assert(Assigned(ChunkIL));
  ParamIndex := Step.IndexOfParam(Param);
  ChunkIL.GetParam(ParamIndex).Algo := Algo;
  ChunkIL.GetParam(ParamIndex).AlgoReg := AlgoReg;
end;

procedure TChunkSet.CalcCosts;
var Combo: TChunk;
begin
  for Combo in Combos do
    Combo.CalcCosts;

  if Combos.Count = 1 then
    FCosts := Combos[0].Costs
  else
    FCosts.Init;
end;

procedure TChunkSet.CopyFrom(From: TChunk);
var Combo: TChunk;
begin
  Assert(From is TChunkSet);
  for Combo in TChunkSet(From).Combos do
    Combos.Add(Combo.Dup);
end;

constructor TChunkSet.Create(ALogProc: TLogProc);
begin
  inherited Create(ALogProc);
  Combos := TObjectList<TChunk>.Create(True);
end;

procedure TChunkSet.DeDup;
var I: Integer;
begin
  I := 0;
  while I < Combos.Count do
  begin
    //Returns True if there are better Combos available
    if DeDupItem(I) = kkKill then
      Combos.Delete(I)
    else
      inc(I);
  end;
end;

function TChunkSet.DeDupItem(Index: Integer): TKeepOrKill;
type TOptimiseFor = (ofSize, ofSpeed);
const OptimiseFor = ofSize; //TEMP
var J: Integer;
begin
  J := Index + 1;
  RegStateSet(Combos[Index].RegState^);
  while J < Combos.Count do
  begin
    //Does the other Combo result in the same state as us?
    if RegStateCompareAll(Combos[J].RegState^) then
    begin
      if OptimiseFor = ofSize then
      begin
        //If there's a better Combo available, signal caller to remove us
        if Combos[Index].Costs.Bytes > Combos[J].Costs.Bytes then
          EXIT(kkKill)
        else if Combos[Index].Costs.Bytes < Combos[J].Costs.Bytes then
          //If therwise it's a worse combo so remove it
          Combos.Delete(J)
        else  //Equal Bytes, compare cycles
          if Combos[Index].Costs.Cycles > Combos[J].Costs.Cycles then
            EXIT(kkKill)
          else //I.Cycles <= J.Cycles
            Combos.Delete(J);
      end
      else  //As above, but for speed
      begin
        if Combos[Index].Costs.Cycles > Combos[J].Costs.Cycles then
          EXIT(kkKill)
        else if Combos[Index].Costs.Cycles < Combos[J].Costs.Cycles then
          Combos.Delete(J)
        else
          if Combos[Index].Costs.Bytes > Combos[J].Costs.Bytes then
            EXIT(kkKill)
          else
            Combos.Delete(J)
      end
    end
    else
      inc(J)
  end;
  Result := kkKeep;
end;

destructor TChunkSet.Destroy;
begin
  Combos.Free;
  inherited;
end;

procedure TChunkSet.Flatten;
begin
  while Combos.Count > 1 do
    //if optimsing for size:
      if Combos[0].Costs.Bytes > Combos[1].Costs.Bytes then
        Combos.Delete(0)
      else
        Combos.Delete(1);
//    else  //Optimising for speed
{      if Combos[0].Costs.Cycles > Combos[1].Costs.Cycles then
        Combos.Delete(0)
      else
        Combos.Delete(1);
}
  Combos[0].Flatten;
end;

function TChunkSet.GetCount: Integer;
begin
  Result := Combos.Count;
end;

function TChunkSet.GetFirstChunkIL: TChunkIL;
begin
  if Combos.Count = 0 then
    EXIT(nil)
  else if Combos[0] is TChunkIL then
    Result := TChunkIL(Combos[0])
  else
    Result := Combos[0].FirstChunkIL;
end;

function TChunkSet.GetLastChunkIL: TChunkIL;
begin
  if Combos.Count = 0 then
    EXIT(nil)
  else if Combos[0] is TChunkIL then
    Result := TChunkIL(Combos[0])
  else
    Result := Combos[0].LastChunkIL;
end;

function TChunkSet.Instantiate: TChunk;
begin
  Result := TChunkSet.Create(FLogProc);
end;

function TChunkSet.IsInferior(Combo: TChunk): TKeepOrKill;
begin
  //TODO
  Assert(False, 'TODO');
  Result := kkKeep;
end;

procedure TChunkSet.TailScavengeLoads;
var I: Integer;
  SourcePuppy: TSourcePuppy;
begin
  SourcePuppy := TSourcePuppy.Create(Self);
  try
    I := 0;
    while I < Combos.Count do
    begin
      Assert(Combos[I] is TChunkSequence);

      //Returns Invalid if Combo should be removed
      if SourcePuppy.Process(Combos[I], Combos[I].LastChunkIL) = kkKill then
        Combos.Delete(I)
      else
        inc(I);
    end;
  finally
    SourcePuppy.Free;
  end;
  LogProc(Self, 'Scavenge loads, updated Combo count: ' + Count.ToString);
end;

function TChunkSet.ToString: String;
var I: Integer;
begin
  if Combos.Count = 1 then
    EXIT(Combos[0].ToString);

  Result := '['#13;
  for I := 0 to Combos.Count-1 do
    Result := Result + ',' + Combos[I].ToString + #13;
  Result := Result + ']';
end;

{ TChunkIL }

procedure TChunkIL.AddDestParam(ParamIndex: Integer);
begin
  SetLength(DestParams, Length(DestParams)+1);
  DestParams[Length(DestParams)-1] := ParamIndex;
end;

procedure TChunkIL.AddItem(Item: PILItem);
begin
  SetLength(Items, Length(Items)+1);
  Items[Length(Items)-1] := Item^;
end;

procedure TChunkIL.AddSourceParam(ParamIndex: Integer);
begin
  SetLength(FSourceParams, Length(FSourceParams)+1);
  FSourceParams[Length(FSourceParams)-1] := ParamIndex;
end;

procedure TChunkIL.CalcCosts;
var I: Integer;
  Param: PILParam;
  Prim: PPrimitive;
begin
  FCosts.Zero;
  Prim := GetPrim;
  if Assigned(Prim) then
    FCosts.Add(Prim.ProcMeta.Costs);

  for I := 0 to ParamCount-1 do
  begin
    Param := GetParam(I);
    FCosts.Add(AlgoData[Param.Algo].Fragment.ProcMeta.Costs);
    FCosts.Add(AlgoData[Param.OverflowAlgo].Fragment.ProcMeta.Costs);
  end;
end;

procedure TChunkIL.CopyFrom(From: TChunk);
var ChunkIL: TChunkIL;
begin
  inherited;
  Assert(From is TChunkIL);

  ChunkIL := TChunkIL(From);
  SetLength(Items, Length(ChunkIL.Items));
  {$IFDEF FPC}
  Items := Copy(ChunkIL.Items, 0, Length(ChunkIL.Items));
  {$ELSE}
  TArray.Copy<TILItem>(ChunkIL.Items, Items, 0, 0, Length(ChunkIL.Items));
  {$ENDIF}

  SetLength(FSourceParams, Length(ChunkIL.FSourceParams));
  if Length(ChunkIL.FSourceParams) > 0 then
    {$IFDEF FPC}
    FSourceParams := Copy(ChunkIL.FSourceParams, 0, Length(ChunkIL.FSourceParams));
    {$ELSE}
    TArray.Copy<Integer>(ChunkIL.FSourceParams, FSourceParams, 0, 0, Length(ChunkIL.FSourceParams));
    {$ENDIF}

  SetLength(DestParams, Length(ChunkIL.DestParams));
  if Length(ChunkIL.DestParams) > 0 then
    {$IFDEF FPC}
    DestParams := Copy(ChunkIL.DestParams, 0, Length(ChunkIL.DestParams));
    {$ELSE}
    TArray.Copy<Integer>(ChunkIL.DestParams, DestParams, 0, 0, Length(ChunkIL.DestParams));
    {$ENDIF}

  RegState := ChunkIL.RegState;
end;

constructor TChunkIL.Create(ALogProc: TLogProc;AnItemIndex: Integer);
begin
  inherited Create(ALogProc);
  FItemIndex := AnItemIndex;
  FCosts.Init;
  InitItems(AnItemIndex);
end;

function TChunkIL.GetChunkIL: TChunkIL;
begin
  Result := Self;
end;

function TChunkIL.GetDestParam(Index: Integer): PILParam;
begin
  Result := GetParam(DestParams[Index]);
end;

function TChunkIL.GetParam(Index: Integer): PILParam;
var ItemIndex: Integer;
begin
  ItemIndex := Index div 3;
  Assert(ItemIndex < Length(Items));

  Result := Items[ItemIndex].GetParam(Index mod 3 + 1);
end;

function TChunkIL.GetPrim: PPrimitive;
begin
  if Length(Items) <> 1 then
    Result := nil
  else
    Result := Items[0].Prim;
end;

function TChunkIL.GetRegState: PCPUState;
begin
  Result := @RegState;
end;

function TChunkIL.GetSourceCount: Integer;
begin
  Result := Length(FSourceParams);
end;

function TChunkIL.GetSourceParam(Index: Integer): PILParam;
begin
  Result := GetParam(FSourceParams[Index]);
end;

function TChunkIL.GetSourceParamInGenOrder(Order: Integer): PILParam;
var I: Integer;
begin
  for I := 0 to Length(FSourceParams)-1 do
  begin
    Result := GetSourceParam(I);
    if Result.GenOrder = Order then
      EXIT;
  end;

  Result := nil;
end;

function TChunkIL.GetSourceParams(Index: Integer): PILParam;
begin
  if Index >= SourceCount then
    raise Exception.Create('Invalid source param index');
  Result := GetParam(FSourceParams[Index]);
end;

function TChunkIL.IndexOfParam(AParam: PILParam): Integer;
var I: Integer;
begin
  for I := 0 to ParamCount-1 do
    if GetParam(I)= AParam then
      EXIT(I);

  Result := -1;
end;

procedure TChunkIL.InitItems(AnItemIndex: Integer);
var
  Item: PILItem;
  I: Integer;
  Param: PILParam;
begin
  //Copy Item (multiple Items if Extended)
  repeat
    Item := ILIndexToData(AnItemIndex);
    AddItem(Item);
    inc(AnItemIndex);
  until not (Item.Op in ExtendedOps);

  //Initialise params
  for I := 0 to ParamCount-1 do
  begin
    Param := GetParam(I);
    if (Param <> nil) and (Param.Kind <> pkNone) then
    begin
      Param.Algo := agUnspecified;
//      Param.MoveAlgo := agUnspecified;

      case Param.Kind of
        pkNone: ;
        pkVarSource, pkImmediate: AddSourceParam(I);
        pkVarDest: AddDestParam(I);
      else
        Assert(False);
      end;
    end;
  end;
end;

function TChunkIL.Instantiate: TChunk;
begin
  Result := TChunkIL.Create(FLogProc, FItemIndex);
end;

function TChunkIL.ParamCount: Integer;
begin
  Result := Length(Items) * 3;
end;

function TChunkIL.ToString: String;
var I: Integer;
begin
  if Length(Items) = 0 then
    EXIT('<Empty>');

  if GetPrim <> nil then
    Result := GetPrim.ProcName
  else
    Result := OpStrings[Items[0].Op];

  for I := 0 to ParamCount-1 do
    if GetParam(I).Kind <>  pkNone then
      Result := Result + ' ' + GetParam(I).ToString;

  Result := Result + '  $:' + FCosts.ToString;

  RegStateSet(RegState);
  Result := Result + {#13 +} '  ' + CPUStateToString(True);
end;

procedure TChunkIL.UpdateRegState(const InitialRegState: TCPUState);
begin
  //Redo before every update!
  RegStateSet(InitialRegState);
  UpdateRegState;
end;

procedure TChunkIL.UpdateRegState;
begin
  //Source Params
  UpdateRegStateForSourceParams;
  if GetPrim <> nil then
    UpdateRegStateForPrim;
  //Dest Params
  UpdateRegStateForDestParams;

  RegStateGet(RegState);
end;

procedure TChunkIL.UpdateRegStateForDestParams;
var ParamIndex: Integer;
  Param: PILParam;
begin
  for ParamIndex := 0 to Length(DestParams)-1 do
  begin
    Param := GetParam(DestParams[ParamIndex]);
    LibStateFromAlgo(Param.Algo, Param);
    LibStateFromAlgo(Param.OverflowAlgo, Param);
  end;
end;

procedure TChunkIL.UpdateRegStateForPrim;
begin
(*  if not Prim.HaveProcMeta then
    raise Exception.Create('No ProcMeta available for ''' + Prim.Name + '''. ProcMeta is com0ulsory for CleverPuppy');
*)
  if GetPrim <> nil then
    LibStateUpdateCPUState(GetPrim.ProcMeta);
  //Update State of Dest Reg to Result of Prim (if it sets a Reg)
  if Length(DestParams) >= 1 then
    LibStateFromDestParam(GetDestParam(0));
end;

procedure TChunkIL.UpdateRegStateForSourceParams;
var ParamIndex: Integer;
  Order: Integer;
  Param: PILParam;
begin
  if Length(FSourceParams) = 0 then
    EXIT;

  Param := GetSourceParam(0);
  if Param.GenOrder = 0 then
    //Natural load order
    for ParamIndex := 0 to Length(FSourceParams)-1 do
    begin
      Param := GetSourceParam(ParamIndex);
      Assert(Param.GenOrder = 0);
      LibStateFromAlgo(Param.Algo, Param);
    end
  else
    for Order := 1 to Length(FSourceParams) do
    begin //Explicit load order
      Param := GetSourceParamInGenOrder(Order);
      Assert(Param <> nil);
      LibStateFromAlgo(Param.Algo, Param);
    end;

  //Validate reg loads are correct (ie. nothing has been corrupted, everything got loaded)
  for ParamIndex := 0 to Length(FSourceParams)-1 do
  begin
    Param := GetSourceParam(ParamIndex);
    if Param.Reg <> rNone then
      case Param.Kind of
        pkImmediate:
          if not RegStateEqualsLiteral(Param.Reg, Param.Imm.ToInteger) then
          begin
            LogProc(Self, 'ERROR: Failed to load immediate (' + Param.Imm.ToInteger.ToString +
              ') to register ' + CPURegStrings[Param.Reg] + #13 + Self.ToString);
            raise Exception.Create('Failed to load immediate (' + Param.Imm.ToInteger.ToString +
              ') to register ' + CPURegStrings[Param.Reg] + #13 + Self.ToString);
          end;
        pkVarSource:
          if not RegStateEqualsVariable(Param.Reg, Param.Variable, Param.VarVersion, rskVarValue) then
          begin
            LogProc(Self, 'ERROR: Failed to load variable ''' + Param.Variable.Name +
              ''' to register ' + CPURegStrings[Param.Reg] + #13 + Self.ToString);
            raise Exception.Create('Failed to load variable ''' + Param.Variable.Name +
              ''' to register ' + CPURegStrings[Param.Reg] + #13 + Self.ToString);
          end;
      end;
  end;
end;

{ TChunkILSet }

function TChunkILSet.AddCopyCombo(Base: TChunkIL): TChunkIL;
begin
  Result := Base.Dup as TChunkIL;
  Combos.Add(Result);
  Result.CalcCosts;
end;

procedure TChunkILSet.ComboParam(Base: TChunkIL;Index: Integer);
var Param: PILParam;
begin
  if Index = Base.ParamCount then
  begin
    AddCopyCombo(Base);
    EXIT;
  end;

  Param := Base.GetParam(Index);

  if Param.Kind = pkNone then
  begin //End of Parameters. Add the current conbo
    AddCopyCombo(Base);
    EXIT;
  end;

  case Param.Kind of
    pkNone: ;
    pkImmediate:  ComboParamImmediate(Base, Index);
//    pkPhiVarSource:
//    pkPhiVarDest:
    pkVarSource:  ComboParamVarSource(Base, Index);
    pkVarDest:    ComboParamVarDest(Base, Index);
//    pkVarAddr:
//    pkPop:
//    pkPopByte:
//    pkPush:
//    pkPushByte:
//    pkBranch:
//    pkCondBranch:
  else
    Assert(False);
  end;
end;

procedure TChunkILSet.ComboParamImmediate(Base: TChunkIL; Index: Integer);
var Param: PILParam;
  Reg: TCPUReg;
begin
  Param := Base.GetParam(Index);
  Assert(Param.Kind = pkImmediate);

  for Reg in Param.SourceRegs do
  begin
    Param.Reg := Reg;
    case Reg of
      rA,rB,rC,rD,rE,rH,rL:
        Param.Algo := laImm8;
      rBC,rDE,rHL:
        Param.Algo := agImm16;
(* Untested
      rIX,rIY:
        Param.Algo := saImmXY;
      rCF:
        if Param.Imm.ToInteger <> 0 then
          Param.Algo := saSCF
        else
          Param.Algo := saClearCF;
*)
    else
      Assert(False);
    end;

    ComboParam(Base, Index+1);
  end;
end;

procedure TChunkILSet.ComboParamVarDest(Base: TChunkIL;Index: Integer);
var Param: PILParam;
  V: PVariable;
  Reg: TCPUReg;
begin
  Param := Base.GetParam(Index);

  V := Param.Variable;
  Assert((V.VarType = Param.CheckType) or not (pfRangeCheck in Param.Flags), 'TODO - Dest Range checking/type conversion');
  Assert(not Base.GetPrim.ProcMeta.ResultInLReg, 'TODO - Dest ResultInLReg');
  Assert(GetTypeDataSize(V.UserType) = 2, 'TODO - Dest TypeSize <> 2');

  case Base.GetPrim.ProcMeta.ResultLoc of
    plNone, plImmediate: Assert(False); //Invalid as Dest
    plStackVar, plStaticVar: ;  //Prim writes value to memory itself
    plRegister: //Value is in a register
    begin
      //Combo 1: Store directly to memory
      //TODO: We can ignore this if using result in next step (and subject
      //to variable meta data).
      Param.ResultRegs := Base.GetPrim.ProcMeta.ResultRegs;

      //Generate Combo for every possible register
      for Reg in Param.ResultRegs do
      begin
        Param.Reg := Reg;
        case V.AddrMode of
          amStatic:
            //Where the heuristics depend on the register :sigh:
            if Reg = rHL then
              Param.Algo := saStaticHL
            else
              Param.Algo := saStatic16;
          amStack:  Param.Algo := saStack16;
        else
          Assert(False);
        end;

        //Next Param
        ComboParam(Base, Index+1);
      end;

      //Combo 2: Push to stack ( *instead of store to variable* )
      //We need to check variable meta data and var map for this option

      //Move Combos:
      //For each ResultReg, Generate Combo moving to other regs,
      //and Combos of move algos for each

      //TODO Generate/update CPU state
    end;
  else
    Assert(False);
  end;
end;

procedure TChunkILSet.ComboParamVarSource(Base: TChunkIL;Index: Integer);
var Param: PILParam;
  V: PVariable;
  Reg: TCPUReg;
begin
  Param := Base.GetParam(Index);

  //TODO: Type conversion?? Range check??
  V := Param.Variable;
  Assert((V.VarType = Param.CheckType) or not (pfRangeCheck in Param.Flags), 'TODO - Source range checking/type conversion');
  Assert(GetTypeDataSize(V.UserType) = 2, 'TODO - Source TypeSize <> 2');

  //Generate Combo for every possible register
  for Reg in Param.SourceRegs do
  begin
    Param.Reg := Reg;
    case V.AddrMode of
      amStatic:
        //Heuristics depend on the register :sigh:
        if Reg = rHL then
          Param.Algo := laStaticHL
        else
          Param.Algo := laStatic16;
      amStack:  Param.Algo := laStack16;
    else
      Assert(False);
    end;

    //Next parameter
    ComboParam(Base, Index+1);
  end;
end;

constructor TChunkILSet.CreateIntrinsic(ALogProc: TLogProc;Index: Integer);
var Base: TChunkIL;
begin
  inherited Create(ALogProc);
  Base := TChunkIL.Create(ALogProc, Index);
  try
    InitIntrinsic(Base);
  finally
    Base.Free;
  end;
  LogProc(Self, 'Step ' + Index.ToString + ': Combos generated: ' + Count.ToString);
end;

procedure TChunkILSet.InitIntrinsic(Base: TChunkIL);
var
  SwapParams: Boolean;  //If the prim selector tells us to
  Prim: PPrimitive;
  Commutative: Boolean; //If the op is commutative and params ae not equal
  Param0: PILParam;
  Param1: PILParam;
  Intersection: TCPURegSet;
  DestIndex: Integer;
  Dest: PILParam;
  TempParam: TILParam;
  RegSet: TCPURegSet;
begin
  Assert(Length(Base.Items) > 0);
  Assert(not (Base.Items[0].Op in SystemOps));
  Param0 := nil;
  Param1 := nil;
  Dest := nil;

  //TODO: Get /list/ of primitives (ie Combo Primitives)
  Prim := PrimFindCodeGen(Base.Items[0], SwapParams);
  Base.Items[0].Prim := Prim;
  //If the primitive selector returned SwapParams then we only want to
  //try swapped params. If not then we want to try swapped and unswapped in the
  //Op is commutative.
  Commutative := Prim.Commutative and not SwapParams;

  //TODO: For each suitable primitive (where there are alternatives)

  //If any parameter, or result, requires a type conversion, set it
  if Prim.LType <> vtUnknown then
  begin
    Param0 := Base.GetParam(0);
    Param0.CheckType := Prim.LType;
    Param0.SourceLoc := Prim.ProcMeta.LLoc;
    Param0.SourceRegs := Prim.ProcMeta.LRegs;

    if Prim.RType <> vtUnknown then
    begin
      Param1 := Base.GetParam(1);
      Param1.CheckType := Prim.RType;
      Param1.SourceLoc := Prim.ProcMeta.RLoc;
      Param1.SourceRegs := Prim.ProcMeta.RRegs;

      //Is any regs are common to both params
      Intersection := Param1.SourceRegs * Param0.SourceRegs;
      if Intersection <> [] then
      begin
        //Is the resulting set a single value?
        //Does the left parameter only accept a single value?
        //(Error if not - there are no such Z80 instructions (probably))
        Assert(RegSetCount(Param0.SourceRegs) = 1, 'This shouldn''t happen');

        //Are both parameters the same?
        if Param0.KindMatch(Param1^) then
        begin //If so use the same Regs for both
          Param1.SourceRegs := Intersection;
          Param0.SourceRegs := Intersection;
          Commutative := False;  //No need to try swapping them
        end
        else  //Else remove LReg from RRegs
          Param1.SourceRegs := Param1.SourceRegs - Param0.SourceRegs;
      end;

      DestIndex := 2;
    end
    else
      DestIndex := 1;
    //TODO: CheckType for Result
  end
  else
    DestIndex := 0;

  //If HasResult???
  if Prim.ProcMeta.ResultLoc <> plNone then
  begin
    Dest := Base.GetParam(DestIndex);
    Dest.ResultLoc := Prim.ProcMeta.ResultLoc;
    Dest.ResultRegs := Prim.ProcMeta.ResultRegs;
    Dest.ResultInLReg := Prim.ProcMeta.ResultInLReg;

    if dfOverflowCheck in Base.Items[0].Flags then
      //TODO: Combo overflow check algorithms??
      if Prim.OverflowCheckProcName <> '' then
        Dest.OverflowAlgo := FragmentNameToAlgo(Prim.OverflowCheckProcName)
      else
        Dest.OverflowAlgo := GetOverflowAlgoForType(Prim.ResultType)
    else
      Dest.OverflowAlgo := agNone;
  end;


  if Prim.ResultTypeIsLType then
    Dest.CheckType := Prim.LType
  else if Prim.ResultTypeIsRType then
    Dest.CheckType := Prim.RType
  else
    Dest.CheckType := Prim.ResultType;

  //Try with unswapped params unless the primitive selector told us not to
  if not SwapParams then
    ComboParam(Base, 0);

  //Try with swapped params if the primitive selector told us to, or if that's an
  //option for this primitive and params
  if SwapParams or Commutative then
  begin
    TempParam := Param1^;
    Param1^ := Param0^;
    Param0^ := TempParam;

    //Restore Prim meta data
    //Unswap the Register options
    RegSet := Param0.SourceRegs;
    Param0.SourceRegs := Param1.SourceRegs;
    Param1.SourceRegs := RegSet;

    //Reapply the locations
    Param0.SourceLoc := Prim.ProcMeta.LLoc;
    Param1.SourceLoc := Prim.ProcMeta.RLoc;

    ComboParam(Base, 0)
  end;
  //End each prim loop

  //Set CPU state for every combo
  SetCPUStates;
end;

function TChunkILSet.Instantiate: TChunk;
begin
  Result := TChunkILSet.Create(FLogProc);
end;

procedure TChunkILSet.SetCPUStates;
var I: Integer;
  InitialRegState: TCPUState;
begin
  //Get Initial CPU state
  //TODO: Get CPU state from previous step
  //if no previous step:
  RegStateInitialise;
  ///Copy current CPU state (so we can set it back again below:

  //Get copy of initial state
  RegStateGet(InitialRegState);

  //Generate CPUState (for each Combo)
  for I := 0 to Combos.Count-1 do
    (Combos[I] as TChunkIL).UpdateRegState(InitialRegState);
end;

{ TChunkChunkList }

procedure TChunkSequence.AddStep(Step: TChunk);
begin
  Steps.Add(Step);
  if Steps.Count > 1 then
  begin
    Step.Prev := Steps[Steps.Count-2];
    Steps[Steps.Count-2].Next := Step;
  end;
end;

procedure TChunkSequence.CalcCosts;
var Step: TChunk;
begin
  FCosts.Zero;
  for Step in Steps do
  begin
    Step.CalcCosts;
    FCosts.Add(Step.Costs);
  end;
end;

procedure TChunkSequence.CopyFrom(From: TChunk);
var Step: TChunk;
begin
  Assert(From is TChunkSequence);
  inherited;
  for Step in TChunkSequence(From).Steps do
    Steps.Add(Step.Dup);
end;

constructor TChunkSequence.Create(ALogProc: TLogProc);
begin
  inherited;
  Steps := TObjectList<TChunk>.Create(True);
end;

destructor TChunkSequence.Destroy;
begin
  Steps.Free;
  inherited;
end;

procedure TChunkSequence.Flatten;
var Step: TChunk;
begin
  for Step in Steps do
    Step.Flatten;
end;

function TChunkSequence.GetCount: Integer;
begin
  Result := Steps.Count;
end;

function TChunkSequence.GetFirstChunkIL: TChunkIL;
begin
  if Steps.Count = 0 then
    EXIT(nil)
  else if Steps[0] is TChunkIL then
    Result := TChunkIL(Steps[0])
  else
    Result := Steps[0].FirstChunkIL;
end;

function TChunkSequence.GetLast: TChunk;
begin
  if Steps.Count = 0 then
    Result := nil
  else
    Result := Steps[Steps.Count-1];
end;

function TChunkSequence.GetLastChunkIL: TChunkIL;
begin
  if Steps.Count = 0 then
    EXIT(nil)
  else if Last is TChunkIL then
    Result := TChunkIL(Last)
  else
    Result := Last.LastChunkIL;
end;

function TChunkSequence.GetRegState: PCPUState;
begin
  Assert(Count > 0);
  Result := Steps[Steps.Count-1].GetRegState;
end;

function TChunkSequence.IndexOf(AStep: TChunk): Integer;
begin
  Result := Steps.IndexOf(AStep);
end;

function TChunkSequence.Instantiate: TChunk;
begin
  Result := TChunkSequence.Create(FLogProc);
end;

function TChunkSequence.TailReduceSequence: Boolean;
begin
  Assert(False, 'Unused??');
end;

function TChunkSequence.ToString: String;
var I: Integer;
begin
  if Steps.Count = 1 then
    EXIT(Steps[0].ToString);
  Result := '{'#13;
  for I := 0 to Steps.Count-1 do
    Result := Result + ':' + Steps[I].ToString + #13;
  Result := Result + '}';
  if Costs.Bytes <> -1 then
    Result := Result + '$:'+Costs.ToString;
end;

{ TChunkBlock }

function TChunkBlock.Instantiate: TChunk;
begin
  Result := TChunkBlock.Create(FLogProc);
end;

procedure TChunkBlock.MergeSteps;
var Combos: TChunkSet;
begin
  LogProc(Self, 'Merging steps for block ' + GetFirstChunkIL.Items[0].BlockID.ToString);

  while Steps.Count > 1 do
  begin
    LogProc(Self, 'Add Step: ' + Steps[1].GetFirstChunkIL.FItemIndex.ToString);

    //Generate all combinations of first and second steps
    Combos := Permute(Steps[0], Steps[1]);
    //Replace previous first step with newly permuted combinations...
    Steps[0] := Combos;
    //...and remove the step we've just included
    Steps.Delete(1);

    //Add extra Combos where source values can be scavenged
    Combos.TailScavengeLoads;

    Combos.CalcCosts;

    //Remove any Combos which have the same effect.
    Combos.DeDup;

    LogProc(Self, 'Trim duplicates, updated Combo count: ' + Combos.Count.ToString);
  end;
end;

function TChunkBlock.Permute(Left, Right: TChunk): TChunkSet;
var LCombo: TChunk;
  RCombo: TChunk;
  Sequence: TChunkSequence;
begin
  Result := TChunkSet.Create(FLogProc);

  if Left is TChunkSet then
  begin
    for LCombo in TChunkSet(Left).Combos do
      if Right is TChunkSet then
      begin //Left is Set, Right is Set
        for RCombo in TChunkSet(Right).Combos do
        begin
          if LCombo is TChunkSequence then
            Sequence := LCombo.Dup as TChunkSequence
          else
          begin
            Sequence := TChunkSequence.Create(FLogProc);
            Sequence.AddStep(LCombo.Dup);
          end;
          Sequence.AddStep(RCombo.Dup);
          Result.AddCombo(Sequence);
        end;
      end
      else  //Left is Set, Right is not Set
      begin
        if LCombo is TChunkSequence then
          Sequence := LCombo.Dup as TChunkSequence
        else
        begin
          Sequence := TChunkSequence.Create(FLogProc);
          Sequence.AddStep(LCombo.Dup);
        end;
        Sequence.AddStep(Right.Dup);
        Result.AddCombo(Sequence);
      end;
  end
  else if Right is TChunkSet then
  begin //Left is not Set, Right is Set
    for RCombo in TChunkSet(Next).Combos do
    begin
      if Left is TChunkSequence then
        Sequence := Left.Dup as TChunkSequence
      else
      begin
        Sequence := TChunkSequence.Create(FLogProc);
        Sequence.AddStep(Left.Dup);
      end;
      Sequence.AddStep(RCombo.Dup);
      Result.AddCombo(Sequence);
    end;
  end
  else
  begin //Neither is Set
    if Left is TChunkSequence then
      Sequence := Left.Dup as TChunkSequence
    else
    begin
      Sequence := TChunkSequence.Create(FLogProc);
      Sequence.AddStep(Left.Dup);
    end;
    Sequence.AddStep(Right.Dup);
    Result.AddCombo(Sequence);
  end;
  LogProc(Self, 'Permutations generated: ' + Result.Count.ToString);
end;

{ TCleverPuppy }

procedure TCleverPuppy.AddLog(Sender: TChunk; const Msg: String);
begin
  FLog.Add(Msg);
  //TODO: LogLevel: Add detailed info
end;

constructor TCleverPuppy.Create;
begin
  inherited;
  FLog := TStringList.Create;
  LogProc := AddLog;
  FBlocks := TChunkSequence.Create(LogProc);
end;

destructor TCleverPuppy.Destroy;
begin
  FBlocks.Free;
  FLog.Free;
  inherited;
end;

procedure TCleverPuppy.ExportSection;

  procedure ExportChunk(Chunk: TChunk);
  var I: Integer;
    ChunkItem: TILItem;
    ILItem: PILItem;
  begin
    if Chunk is TChunkSequence then
      for I := 0 to Chunk.Count-1 do
        ExportChunk(TChunkSequence(Chunk).Steps[I])
    else if Chunk is TChunkSet then
    begin
      Assert(Chunk.Count = 1, 'CleverPuppy data should have been Flattened');
      ExportChunk(TChunkSet(Chunk).Combos[0]);
    end
    else if Chunk is TChunkIL then
      for ChunkItem in TChunkIL(Chunk).Items do
      begin
        ILItem := ILAppend(opUnknown);
        ILItem^ := ChunkItem;
        ILItem.Prim := ChunkItem.Prim;
      end
    else
      Assert(False, 'Unknown Chunk type');
  end;

begin
  //Clear current list
  ClearILList(GetCurrentScope.ILList);
  ExportChunk(FBlocks)
end;

procedure TCleverPuppy.ImportSection;
var Index: Integer;
  Item: PILItem;
  Block: TChunkSequence;
begin
  LogProc(nil, 'Importing');
  Block := nil;

  Index := 0;
  //TODO: How do we handle blocks?

  while Index < ILGetCount do
  begin
    Item := ILIndexToData(Index);
    if (Item.BlockID >= 0) or (Block = nil) then
    begin
      LogProc(nil, 'Block ' + Item.BlockID.ToString);
      Block := TChunkBlock.Create(LogProc);
      FBlocks.AddStep(Block);
    end;

    case Item.Op of
      opUnknown: Assert(False);
//      opMove: ;
      opStoreImm: ; //TODO
//      opBranch: ;
//      opBoolVarBranch: ;
//      opNOTUSED1: ;
      opPhi: ;  //TODO
//      opRegLoad: ;
//      opRegLoadExtended: ;
//      opRegStore: ;
//      opRegStoreExtended: ;
//      opFuncCall: ;
//      opFuncCallExtended: ;
//      opFuncReturn: ;
    else  //Intrinsic Op
      Block.AddStep(TChunkILSet.CreateIntrinsic(LogProc, Index));
    end;
    Index := ILNextIndex(Index);
  end;
end;

procedure TCleverPuppy.ProcessSection;
var Scope: PScope;
  I: Integer;
begin
  Scope := GetCurrentScope;
  if Assigned(Scope.Func) then
    AddLog(nil, 'CleverPuppy processing: ' + Scope.Func.Name)
  else
    AddLog(nil, 'CleverPuppy processing: main');

  //Import all Steps, split into blocks
  ImportSection;

  //Process each block
  for I := 0 to FBlocks.Count-1 do
  begin
    Assert(FBlocks.Steps[I] is TChunkBlock);
    //Generate Combos for Source params and optimise
    TChunkBlock(FBlocks.Steps[I]).MergeSteps;
  end;

  //All done, choose a winner
  if FBlocks.Count = 1 then
    FBlocks.Steps[0].Flatten
  else
    Assert(False, 'TODO: Multiple blocks');

//TEMP Disable until we have codegen sorted
  ExportSection;
end;

function TCleverPuppy.ToString: String;
begin
  Result := #13'Clever Puppy data:'#13 + FBlocks.ToString;
end;

end.

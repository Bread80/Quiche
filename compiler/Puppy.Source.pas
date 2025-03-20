(*
Clever Puppy specialised for source parameters
*)
unit Puppy.Source;

interface
uses CleverPuppy;

type TSourcePuppy = class
  private
    //The set which this chunk belongs to. Allows us to add next Combos to the
    //set
    FSet: TChunkSet;
  protected
    //Set the params to load in reverse order.
    //Only for Steps with two source params
    procedure LoadPairInReverse(Step: TChunkIL);

    //Examine a Combo for any loads where the value can be scavenged from a register.
    //If so generate and add an extra combo (to the end of the list) for that option.
    //Index is the index of the Combo to examine
    procedure ScavengeLoads(Combo: TChunk;Step: TChunkIL);

    //Sub to SetSourceOrdering.
    //For Steps with two Params. Assumes pre-filtering by SetSourceOrdering has
    //ben performed already (to bypass easy cases)
    function SetSourceOrderingPair(Combo: TChunk;Step: TChunkIL): TKeepOrKill;
    //Reorders parameters if necessary so values load correctly
    function SetSourceOrdering(Combo: TChunk;Step: TChunkIL): TKeepOrKill;
  public
    constructor Create(ASet: TChunkSet);
    function Process(Combo: TChunk;Step: TChunkIL): TKeepOrKill;
  end;

implementation
uses Def.IL,
  Z80.Hardware, Z80.Algos, Z80.AlgoData, CG.CPUState.Z80;

{ TSourcePuppy }

constructor TSourcePuppy.Create(ASet: TChunkSet);
begin
  inherited Create;
  FSet := ASet;
end;

procedure TSourcePuppy.LoadPairInReverse(Step: TChunkIL);
begin
  Assert(Step.SourceCount = 2);
  Step.SourceParams[0].GenOrder := 2;
  Step.SourceParams[1].GenOrder := 1;
end;

function TSourcePuppy.Process(Combo: TChunk;Step: TChunkIL): TKeepOrKill;
begin
  //We can't do any optimising is there is no prior CPU state
  Assert(Step.Prev <> nil);

  ScavengeLoads(Combo, Step);

  Result := SetSourceOrdering(Combo, Step);
  if Result = kkKill then
    EXIT;

  //TODO: Validate reg load are correct (ie. nothing has been corrupted)
  Step.UpdateRegState;
end;

procedure TSourcePuppy.ScavengeLoads(Combo: TChunk;Step: TChunkIL);
var
  Param: PILParam;
  I: Integer;
  Reg: TCPUReg;
  NewCombo: TChunkSequence;
  StepIndex: Integer;
  Algo: TAlgo;
begin
  //Reg state at end of penultimate chunk
  RegStateSet(Step.Prev.RegState^);

  for I := 0 to Step.SourceCount-1 do
  begin
    Param := Step.SourceParams[I];
    //Only look at Params which are loads
    if Param.Algo < maPreserve then
      case Param.Kind of
        pkVarSource:
        begin
          //TK: Type conversions, partial loads
          Reg := RegStateFindVariable(Param.Variable, Param.VarVersion, rskVarValue);
          if Reg = rNone then
            //Can't scavenge
          else if Reg = Param.Reg then
            //Already loaded in target register
            FSet.AddCopyWithNewAlgo(Combo, Step, Param, maPreserve, rNone) //Retain as is
          else
          begin //Move to the target register
            Algo := agUnspecified;
(*            if Reg in CPUReg8Bit then
            begin
              Assert(Param.Reg in CPUReg8Bit);
              Algo := maFrom8;
            end
            else*) if Reg in CPURegPairs then
            begin
              if Param.Reg in CPURegPairs then
              begin
                //Move a pair
                Algo := maFromPair;

                //Can we also try EX HL,DE?
                if (Reg in [rHL,rDE]) and (Param.Reg in [rHL,rDE]) then
                  if Param.Reg = rHL then
                    FSet.AddCopyWithNewAlgo(Combo, Step, Param, agEXHLDE, rDE)
                  else  //Param.Reg is rDE
                    FSet.AddCopyWithNewAlgo(Combo, Step, Param, agEXHLDE, rHL)

              end
(*              else if Param.Reg in [rIX, rIY] then
                Algo := maPushPopXY
*)              else
                Assert(False);
            end
(*            else if Reg in [rIX, rIY] then
            begin
              if Param.Reg in CPURegPairs then
                Algo := maPushXYPop
              else if Param.Reg in [rIX, rIY] then
                Algo := maPushXYPopXY
              else
                Assert(False);
            end;
*)          else
              Assert(False,'Unvalid register');

            if Algo <> agUnspecified then
              FSet.AddCopyWithNewAlgo(Combo, Step, Param, Algo, Reg);
          end;
        end; //pkVarSource
        pkImmediate:
        begin
          //TODO
        end;  //pkImmediate
      else
        //Ignore
      end;
  end;
end;

function TSourcePuppy.SetSourceOrdering(Combo: TChunk;Step: TChunkIL): TKeepOrKill;
var
  Param: PILParam;
  I: Integer;
  SafetyCounts: array[low(TRegSafety)..high(TRegSafety)] of Integer;
  Safety: TRegSafety;
begin
  //No issues if zero or 1 source param
  if Step.SourceCount < 2 then
    EXIT(kkKeep);

  for Safety := low(TRegSafety) to high(TRegSafety) do
    SafetyCounts[Safety] := 0;

  for I := 0 to Step.SourceCount-1 do
  begin
    Param := Step.SourceParams[I];
    Safety := AlgoData[Param.Algo].RegSafety;
    inc(SafetyCounts[Safety]);
  end;

  //No clashes, exit early
  if SafetyCounts[rsSafe] + SafetyCounts[rsPreserve] = Step.SourceCount then
    EXIT(kkKeep);

  if Step.SourceCount = 2 then
    EXIT(SetSourceOrderingPair(Combo, Step));

  assert(False, 'TODO: More than two params');
end;

(*
Analysis matrix.
Note: Collateral is a separate, parallel, item which requires it's own data
Note: On entry the *only* exchanges we can have are EX HL,DE.

    Right->                                 **IGNORE COLLATERAL**
Left        Safe  Preserve  Copy  Exchange  Collateral
Safe        y     y         swap  swap      swap
Preserve    y     y         y     [1]       [2]
Copy        y     y         [3]   [4]       [5]
Exchange    y     [6]       [7]   [8]       [9]
**IGNORE COLLATERAL**
Collateral  y     [10]      [11]  [12]      [13]

Collateral is more complex than this. Ignore until we inlcude it in algos

Note on Exchanges: Prior to this code the only exchange in existence should be
EX HL,DE (possibly EX (SP),HL(?)). This limits the possible combinations.

[1] If L.Reg is R.Reg or R.AlgoReg then Fail
//[2] If L.Reg is R.Collateral then Fail
[3] If Excangge then mark one as Exchange and other as 'Exchanged by other param'
    If L.Reg is R.AlgoReg: Swap
    Otherwise, Okay
[4] If L.Reg is R.Reg or R.AlgoReg the Fail
//[5] If L.Reg is R.Collateral then Swap
[6] If L.Reg or L.AlgoReg is R.Reg then Fail
[7] If L.Reg or L.AlgoReg is R.Reg then Fail
    Otherwise, Swap
[8] Detect Mutual exchange. If so set Right.Algo 'Exchange by other param'
//[9] If L.Reg or L.AlgoReg is R.Collateral then Fail
//[10] If L.Collateral is R.Reg then Fail
//[11] If L.Collateral is R.AlgoReg then Swap
//[12] If L.Collateral is R.Reg or R.AlgoReg the Fail
//[13] If

(Fails may be resolvable with a more complete algorithm).

const Resolutions = array[rsSafe..rsExchange][rsSafe..rsExchange] of TResolution =
 [[resNone, resNone, resSwap, resSwap],
  [resNone, resNone, resNone, res1],
  [resNone, resNone, res3,    res4],
  [resNone, res5,    res7,    res8]];
*)

function TSourcePuppy.SetSourceOrderingPair(Combo: TChunk; Step: TChunkIL): TKeepOrKill;
var
  Left: PILParam;
  LeftSafety: TRegSafety;
  Right: PILParam;
  RightSafety: TRegSafety;
begin
  Assert(Step.SourceCount = 2);
  Left := Step.SourceParams[0];
  LeftSafety := AlgoData[Left.Algo].RegSafety;
  Right := Step.SourceParams[1];
  RightSafety := AlgoData[Right.Algo].RegSafety;

  Result := kkKeep;

  //TODO: This code assumes 16-bit only paramaters.
  //...which means we don't have to check for an 8 bit reg affecting a pair and
  //vice versa. Such code will be significantly more complex.
  //NOTE: No two params can be loading into the same register.
  case LeftSafety of
    rsSafe:
      case RightSafety of
        rsSafe, rsPreserve: ; //No clash
        rsCopy, rsExchange: //Load Right first
          LoadPairInReverse(Step);
      end;
    rsPreserve:
      case RightSafety of
        rsSafe, rsPreserve, rsCopy: ; //No clash
        rsExchange: //If we're exchanging a reg which must be preserved then
                    //this algo fails
          if Right.AlgoReg = Left.Reg then
            EXIT(kkKill);
      end;
    rsCopy:
      case RightSafety of
        rsSafe, rsPreserve: ; //No clash
        rsCopy: //Check for swaps
          if (Left.Reg = Right.AlgoReg) and (Left.AlgoReg = Right.Reg) then
          begin
            //We have a swap! - TODO: we'll need to permute the swap options
            FSet.LogProc(FSet, '!!!!!!!TODO: SourceOrdering: Swap detected and Combo deleted.'#13 + Step.ToString);
            EXIT(kkKill);
          end
          else
            if Right.AlgoReg = Left.Reg then
              LoadPairInReverse(Step);
        rsExchange:
          if Left.Reg = Right.AlgoReg then
            //we have a mutual exchange
            Left.Algo := agExchangedElsewhere
          //Does exchange affect lefts target reg?
          else if Right.AlgoReg = Left.Reg then
            //If so, do the copy (left) after the exchange (right)
            LoadPairInReverse(Step);
      end;
    rsExchange:
      case RightSafety of
        rsSafe: ; //No clash
        rsPreserve:
          if Left.Reg = Right.AlgoReg {or Left.Reg = Right.Reg TODO} then
            //Clash - Algo is invalid
            EXIT(kkKill);
        rsCopy:
          if (Left.Reg = Right.AlgoReg) and (Left.AlgoReg = Right.Reg) then
            //we have a mutual exchange
            Right.Algo := agExchangedElsewhere
          //Does exchange affect right's source reg?
          else if (Right.AlgoReg = Left.Reg) or (Right.AlgoReg = Left.Reg) then
            //If so, do the copy (right) before the exchange (left)
            LoadPairInReverse(Step);

        rsExchange:
          if (Left.Reg = Right.AlgoReg) and (Left.AlgoReg = Right.Reg) then
            //Mutual exchange
            Right.Algo := agExchangedElsewhere
          else
            Assert(False, 'This should never happen'#13 + Step.ToString);
      end;
  end;
end;

(*  Right->                                 **IGNORE COLLATERAL**
Left        Safe  Preserve  Copy  Exchange  Collateral
Safe        y     y         swap  swap      swap
Preserve    y     y         y     [1]       [2]
Copy        y     y         [3]   [4]       [5]
Exchange    y     [6]       [7]   [8]       [9]
**IGNORE COLLATERAL**
Collateral  y     [10]      [11]  [12]      [13]

Collateral is more complex than this. Ignore until we inlcude it in algos

Note on Exchanges: Prior to this code the only exchange in existence should be
EX HL,DE (possibly EX (SP),HL(?)). This limits the possible combinations.

[1] If L.Reg is R.Reg or R.AlgoReg then Fail
//[2] If L.Reg is R.Collateral then Fail
[3] If Excangge then mark one as Exchange and other as 'Exchanged by other param'
    If L.Reg is R.AlgoReg: Swap
    Otherwise, Okay
[4] If L.Reg is R.Reg or R.AlgoReg the Fail
//[5] If L.Reg is R.Collateral then Swap
[6] If L.Reg or L.AlgoReg is R.Reg then Fail
[7] If L.Reg or L.AlgoReg is R.Reg then Fail
    Otherwise, Swap
[8] Detect Mutual exchange. If so set Right.Algo 'Exchange by other param'
//[9] If L.Reg or L.AlgoReg is R.Collateral then Fail
//[10] If L.Collateral is R.Reg then Fail
//[11] If L.Collateral is R.AlgoReg then Swap
//[12] If L.Collateral is R.Reg or R.AlgoReg the Fail
//[13] If

(Fails may be resolvable with a more complete algorithm).

const Resolutions = array[rsSafe..rsExchange][rsSafe..rsExchange] of TResolution =
 [[resNone, resNone, resSwap, resSwap],
  [resNone, resNone, resNone, res1],
  [resNone, resNone, res3,    res4],
  [resNone, res5,    res7,    res8]];



  //====================For Ops with two parameters

  Assert(Length(ChunkIL.SourceParams) = 2, 'TODO: Loading more than two params');

  Left := ChunkIL.GetSourceParam(0);
  Right := ChunkIL.GetSourceParam(1);

  //Algos for Left which can't affect Right
  if AlgoData[Left.Algo].RegSafety in [rsSafe, rsPreserve, rsCopy] then
  begin
    //Algos for Right which can' be affected by Left
    if AlgoData[Right.Algo].RegSafety in [rsSafe, rsPreserve] then
      EXIT;
    if AlgoData[Right.Algo].RegSafety

  //Exchanges are painful
  //TODO: Other Algos which may affect the other param
  if (Left.Algo in [maEXHLDE]) or (Right.Algo in [maEXHLDE]) then
    LogProc(Self, 'Regload: EXHLDE');
(*    EXIT;
    Assert(False, 'TODO: Swaps');
*)
{  if AlgoData[Right.Algo].RegSafe then
    EXIT;

  if AlgoData[Left.Algo].RegSafe then
  begin //Swap param load ordering.
    Left.GenOrder := 2;
    Right.GenOrder := 1;
    EXIT;
  end;

  Assert(False, 'TODO: Unhandled'#13 + ChunkIL.ToString);
end;
}

end.

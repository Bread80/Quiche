(*
Data for the Algos declared in Z80.Algos
*)
unit Z80.AlgoData;

interface
uses Lib.Data,
  Z80.Algos;

//==================

//TODO: Range check algos, overflow check algos,
//including combined load/store and check algos

type TRegSafety = (
  rsUnknown,  //Null value. Error if this makes it to code gen
  rsSafe,     //Does not affect other registers and is not affected by other Algos.
              //Eg a load from memory
  rsPreserve, //Value to be preserved in register. This would be affected by
              //an Algo which has the register as collateral.
  rsCopy,     //Copy a value from another register
  rsExchange  //Two registers are swapped
  );

type TAlgoData = record
    Name: String;
    Fragment: PFragment;
    RegSafety: TRegSafety;  //How this Algo will affect non-target registers, or
                            //might be affected by other Algos.
//    Collateral: TCPUReg;  //TODO

    //If True the Algo doesn't not touch depend on or affect any
                      //registers than the intended target (for sources/destination).
                      //Used to determine whether parameter loading/storing sequence
                      //is important.
                      //NOTE: A register /move/ is NOT RegSafe - it requires the
                      //expected value to still be in the register (or to stay there
                      //for a dest param) and, therefore, ordering is still required.
  end;

var AlgoData: array[low(TAlgo)..high(TAlgo)] of TAlgoData;
(*
procedure UpdateCPUStateForAlgo(Algo: TAlgo);
*)
procedure LoadAlgoData(const Filename: String);

//Find the Algo from it's name
function FragmentNameToAlgo(const AName: String): TAlgo;

implementation
uses Parse.Source, SysUtils;
{
procedure UpdateCPUStateForAlgo(Algo: TAlgo);
begin
  if Algo in [agUnspecified, agNone] then
    EXIT;

  if not AlgoData[Algo].Fragment.HaveMeta then
    raise Exception.Create('ProcMeta not available for Algo ''' + AlgoData[Algo].Name + '''. ' +
      'Proc Meta data is compulsory for CleverPuppy code generation');
(*  if Assigned(AlgoData[Algo].Fragment.ProcMeta.StateProc) then
    AlgoData[Algo].Fragment.ProcMeta.StateProc(Param)
*)  CodeMetaUpdateCPUState(AlgoData[Algo].Fragment.ProcMeta);
end;
}

(*
:Section
=Name,Bytes,Cycles
Code
*)

procedure LoadAlgoData(const Filename: String);
var Parser: TGenericReader;

  function TryIdentToRegSafety(const Ident: String;out Safety: TRegSafety): Boolean;
  begin
    if CompareText(Ident, 'RegSafe') = 0 then
      Safety := rsSafe
    else if CompareText(Ident, 'Preserve') = 0 then
      Safety := rsPreserve
    else if CompareText(Ident, 'Copy') = 0 then
      Safety := rsCopy
    else if CompareText(Ident, 'Exchange') = 0 then
      Safety := rsExchange

    else
      EXIT(False);

    Result := True;
  end;

  procedure ParseAlgoData(var AlgoData: TAlgoData);
  var Ident: String;
  begin
    Assert(Parser.TestChar = '=');

    Parser.SkipWhiteChars;
    Parser.SkipChar;
    AlgoData.Name := Parser.ReadIdentifier;
    Parser.SkipWhiteChars;

    if Parser.TestChar <> ',' then
      raise Exception.Create('Missing Fragment name');
    Parser.SkipChar;
    Parser.SkipWhiteChars;
    Ident := Parser.ReadIdentifier;
    if Ident = '' then  //Only allowable for cgUnspecified and cgNone??
      AlgoData.Fragment := nil
    else
    begin
      AlgoData.Fragment := FindFragmentByName(Ident);
      if AlgoData.Fragment = nil then
        raise Exception.Create('Unknown fragment name: ' + Ident);
    end;
    Parser.SkipWhiteChars;

    if Parser.TestChar <> ',' then
      raise Exception.Create('Missing RegSafety field');
    Parser.SkipChar;
    Parser.SkipWhiteChars;
    AlgoData.RegSafety := rsUnknown;  //Assume dangerous

    Ident := Parser.ReadIdentifier;
    if not TryIdentToRegSafety(Ident, AlgoData.RegSafety) then
      raise Exception.Create('Unknown RegSafety value: ''' + Ident+'''');
  end;


var Ident: String;
  Algo: TAlgo;
begin
  Algo := agUnspecified;
  Parser := TGenericReader.Create;
  try
    Parser.OpenFile(Filename);

    while not Parser.EOF do
    begin
      Parser.SkipWhiteChars;
      if not Parser.EOLN then
        case Parser.TestChar of
          ';': ;  //Comment
          '=':    //Algo data
          begin
            ParseAlgoData(AlgoData[Algo]);
            if Algo = high(Algo) then
              Algo := agUnspecified
            else
              inc(Algo);

            Parser.SkipWhiteChars;
            if Parser.EOLN or (Parser.TestChar = ';') then
              //No error
            else
              raise Exception.Create('Text after algo data in AlgoData file');
          end;
        else
          raise Exception.Create('Invalid line in AlgoData file');
        end;
      Parser.NextLine;
    end;

    if Algo <> agUnspecified then
      raise Exception.Create(
        'No data specified, or incorrect item count in AlgoData file');
  finally
    Parser.Free;
  end;
end;

function FragmentNameToAlgo(const AName: String): TAlgo;
var Algo: TAlgo;
begin
  for Algo := low(TAlgo) to high(TAlgo) do
    if CompareText(AName, AlgoData[Algo].Fragment.Name) = 0 then
      EXIT(Algo);

  raise Exception.Create('Algo not found for ''' + AName + '''');
end;

end.

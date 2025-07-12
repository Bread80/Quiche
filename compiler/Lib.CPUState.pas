(*
Update CPU State from meta data for fragments and algos
*)
unit Lib.CPUState;

interface
uses Def.IL, Lib.Data,
  Z80.Algos;

//Update CPU state with state from the ProcMeta
procedure LibStateUpdateExitState(const Meta: TCodeProcMeta);

procedure LibStateUpdateCPUState(const Meta: TCodeProcMeta);


procedure LibStateFromAlgo(Algo: TAlgo;Param: PILParam);

procedure LibStateFromDestParam(Dest: PILParam);

implementation
uses Def.Consts,
  Z80.CPUState, Z80.Hardware, Z80.AlgoData, Z80.GenProcs;

//Update CPU state with state from the ProcMeta
procedure LibStateUpdateEntryState(const Meta: TCodeProcMeta);
var I: Integer;
begin
  for I := 0 to high(Meta.OnEntry) do
    if Meta.OnExit[I].Reg = rNone then
      EXIT
    else
      //Literal
      RegStateSetLiteral(Meta.OnEntry[I].Reg, Meta.OnEntry[I].Value);
end;

//Update CPU state with state from the ProcMeta
procedure LibStateUpdateExitState(const Meta: TCodeProcMeta);
var I: Integer;
begin
  for I := 0 to high(Meta.OnExit) do
    if Meta.OnExit[I].Reg = rNone then
      EXIT
    else
      //Literal
      RegStateSetLiteral(Meta.OnExit[I].Reg, Meta.OnExit[I].Value);
end;

procedure LibStateUpdateCPUState(const Meta: TCodeProcMeta);
begin
  LibStateUpdateEntryState(Meta);
  If Meta.HaveCorrupts then
    RegStateSetUnknowns(Meta.Corrupts);
  LibStateUpdateExitState(Meta);
end;


procedure LibStateFromAlgo(Algo: TAlgo;Param: PILParam);
var Meta: TCodeProcMeta;
begin
  if not (Algo in [agNone, agUnspecified]) then
  begin
    Assert(AlgoData[Algo].Fragment.HaveMeta, 'ProcMeta data not found. ProcMeta data is required then');

    Meta := AlgoData[Algo].Fragment.ProcMeta;
    if Meta.StateProc <> '' then
      FindStateProc(Meta.StateProc)(Param);
    if Meta.HaveCorrupts then
      RegStateSetUnknowns(Meta.Corrupts);
    LibStateUpdateExitState(Meta);
  end;
end;

procedure LibStateFromDestParam(Dest: PILParam);
begin
  if Dest.Kind = pkVarDest then
    if Dest.Reg <> rNone then
      RegStateSetVariable(Dest.Reg, Dest.Variable, Dest.VarVersion, rskVarValue);
end;

end.

unit CG.VarMap;

interface
uses Def.IL, Def.Variables;


type
  PVarData = ^TVarData;
  TVarData = record
    Variable: PVariable;
    Version: Integer; //Write version
    WriteILStep: Integer; //The IL step at which the value is assigned
    ReadCount: Integer; //The number of times this vaue is read
    FirstReadILStep: Integer; //The first IL step at which the value is read
                              //If ReadCount = 1 and FirstReadILStep = WriteILStep+1 then
                              //we can eliminate the write and keep the value in registers
    PhiWrite: Boolean;  //The first(!) write to the variable is a Phi function
    PhiRead: Boolean;   //The last read from this variable is a Phi function
    AddrOf: Boolean;    //The code references the address of this variable - optimiser beware!

    function ToString: String;  //For debugging etc.
  end;

procedure CreateVarMap;

function VarMapToString: String;

implementation
uses Generics.Collections, Classes, SysUtils,
  Def.Operators;

var VarMap: TList<PVarData>;

procedure ClearVarMap;
var I: Integer;
begin
  for I := 0 to VarMap.Count-1 do
    Dispose(VarMap[I]);
  VarMap.Clear;
end;

//Creates, initialises and adds to the map
function CreateVarData(Variable: PVariable;Version: Integer): PVarData;
begin
  New(Result);
  VarMap.Add(Result);
  Result.Variable := Variable;
  Result.Version := Version;
  Result.WriteILStep := -1;
  Result.ReadCount := 0;
  Result.FirstReadILStep := -1;
  Result.PhiRead := False;
  Result.PhiWrite := False;
  Result.AddrOf := False;
end;

function VarMapFind(Variable: PVariable;Version: Integer): PVarData;
var I: Integer;
begin
  for I := 0 to VarMap.Count-1 do
    if (VarMap[I].Variable = Variable) and (VarMap[I].Version = Version) then
      EXIT(VarMap[I]);

  Result := nil;
end;

//Add a variable to the VarMap and set the data for it's initial write
function AddVarMapWrite(Variable: PVariable;Version: Integer;Step: Integer): PVarData;
begin
  Result := VarMapFind(Variable, Version);
  //In loops the first write may happen /after/ the first read. We need to handle
  //that scenario
  if Assigned(Result) then
    Assert(Result.WriteILStep = -1, 'Variable written to multipe times')
  else
    Result := CreateVarData(Variable, Version);

  Result.WriteILStep := Step;

(*  begin
    New(Result);
    VarMap.Add(Result);
    Result.Variable := Variable;
    Result.Version := Version;
    Result.WriteILStep := Step;
    Result.ReadCount := 0;
    Result.FirstReadILStep := -1;
    Result.PhiRead := False;
    Result.PhiWrite := False;
    Result.AddrOf := False;
  end;
*)end;

function AddVarMapRead(Variable: PVariable;Version: Integer;Step: Integer): PVarData;
begin
  Result := VarMapFind(Variable, Version);
  if not Assigned(Result) then
    Result := CreateVarData(Variable, Version);
(*
  begin //Not found -> create (you have a read before writing, Mr Programmer!)
    New(Result);
    VarMap.Add(Result);
    Result.Variable := Variable;
    Result.Version := Version;
    Result.WriteILStep := -1; //Note: Parameters have a WriteILStep of -1, but /have/ been written
    Result.ReadCount := 0;
    Result.FirstReadILStep := -1;
    Result.PhiRead := False;
    Result.PhiWrite := False;
  end;
*)
  inc(Result.ReadCount);
  if Result.FirstReadILStep = -1 then
    Result.FirstReadILStep := Step;
end;

function AddVarMapAddrOf(Variable: PVariable): PVarData;
begin
  Result := VarMapFind(Variable, 0);
  if not Assigned(Result) then
    Result := CreateVarData(Variable, 0);

  Result.AddrOf := True;
end;

procedure AddVarMapParam(const Param, Dest: TILParam;Step: Integer);
var Data: PVarData;
begin
  case Param.Kind of
    pkNone, pkImmediate, pkBranch, pkCondBranch: ;
    pkVarSource:
      AddVarMapRead(Param.Variable, Param.VarVersion, Step);
    pkVarDest:
      AddVarMapWrite(Param.Variable, Param.VarVersion, Step);
    pkVarAddr:
      AddVarMapAddrOf(Param.Variable);
    pkVarPtr:
      AddVarMapRead(Param.Variable, Param.VarVersion, Step);
      //TODO: Also read (or write) memory(?)
    pkVarRef:
      AddVarMapRead(Param.Variable, Param.VarVersion, Step);
    pkPhiVarSource:
    begin
      Data := AddVarMapRead(Dest.PhiVar, Param.PhiSourceVersion, Step);
      Data.PhiRead := True;
    end;
    pkPhiVarDest:
    begin
      Data := AddVarMapWrite(Param.PhiVar, Param.PhiDestVersion, Step);
      Data.PhiWrite := True;
    end;
    pkPush: ; //TODO: Stack
    pkPushByte: ; //TODO Stack
  else
    Assert(False);
  end;
end;

procedure CreateVarMap;
var Step: Integer;
  ILItem: PILItem;
begin
  ClearVarMap;

  for Step := 0 to ILGetCount-1 do
  begin
    ILItem := ILIndexToData(Step);

    AddVarMapParam(ILItem.Param1, ILItem.Dest, Step);
    AddVarMapParam(ILItem.Param2, ILItem.Dest, Step);
    AddVarMapParam(ILItem.Param3, ILItem.Dest, Step);
  end;
end;

{ TVarData }

function TVarData.ToString: String;
begin
  Result := Variable.GetAsmName + '_' + Version.ToString +
    ' WStep: ' + WriteILStep.ToString + ' RCount: ' + ReadCount.ToString +
    ' 1stRStep: ' + FirstReadILStep.ToString;
  if PhiRead then
    Result := Result + ' PhiRead';
  if PhiWrite then
    Result := Result + ' PhiWrite';
end;

function VarMapToString: String;
var I: Integer;
begin
  Result := '';
  for I := 0 to VarMap.Count -1 do
    Result := Result + ';' + VarMap[I].ToString + #13;
end;

initialization
  VarMap := TList<PVarData>.Create;
finalization
  ClearVarMap;
  VarMap.Free;
end.

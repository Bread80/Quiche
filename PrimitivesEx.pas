unit PrimitivesEx;

interface
uses QTypes,  ILData;

type
  TPrimValidation = (pvYes, pvNo, pvEither);

  PPrimitive = ^TPrimitive;
  TCodeGenProc = procedure(ILItem: PILItem);
  TValidationProc = procedure(RH, RL: Char);
  TPrimitive= record
    OpIndex: Integer;         //The operation this routine handles
    ProcName: String;         //Name of the routine
    OpType: TOpType;          //The type of the operation
    FirstParamType: Char;     //Only needed for M16, M8 mixed parameter primitives.
                              //'s' if the first parameter is the Signed one,
                              //'u' if the first parameter is the unsigned one
                              //#0 if parameter order is irrelevent
                              //(the other parameter will always be the other type).
    IsBranch: Boolean;        //If true we're testing for a Branch, not an assignment
    DestType: TOpType;        //The type of the destination
    Validation: TPrimValidation;  //What validation can this routine handle?

    Param1Loc: TAllocLocSet;  //Valid sources for the first parameter
    Param2Loc: TAllocLocSet;  //Valid sources for the second parameter
    DestLoc: TAllocLoc;       //Where the destination (Output data will be found)
    Corrupts: TAllocLocSet;   //Which registers are corrupted. Includes any output (dest) register(s)

    Proc: TCodeGenProc;       //If there is a dynamic code generator for the operation,
                              //a reference to it's handler proc must be assigned here
                              //See the PrimSetProc routine below
    ValProc: TValidationProc; //If this is a validation routine
    ValProcName: String;      //Name of the proc to validate the result (if validation is needed)
    ValProcS8: String;        //Names of shortcut routines to use if converting the result to the specified type
    ValProcU8: String;
    ValProcS16: String;
    ValProcU16: String;
//    SnippetName: String;      //If there is code snippet for this operation, name it here
  end;


function ILItemToPrimitive(ILItem: PILItem): PPrimitive;

function PrimFindByProcName(AName: String): PPrimitive;

procedure PrimSetProc(Name: String;Proc: TCodeGenProc);
procedure PrimSetValProc(Name: String;Proc: TValidationProc);

procedure InitialisePrimitiveList;

implementation
uses Generics.Collections, Classes, SysUtils, Operators;

var PrimList: TList<PPrimitive>;

function PrimFindByProcName(AName: String): PPrimitive;
begin
  for Result in PrimList do
    if CompareText(Result.ProcName, AName) =  0 then
      EXIT;

  Result := nil;
end;

function OpTypeMatch(ProcType, ItemType: TOpType): Boolean;
begin
  if ItemType = ProcType then
    EXIT(True);
  if ProcType = rtUnknown then
    EXIT(True);

  case ProcType of
    rtX8: Result := ItemType in [rtS8, rtU8{, rtM8}];
    rtX16: Result := ItemType in [rtS16, rtU16, rtM16U16, rtM16S16];
  else
    Result := False;
  end;
end;

function OpOrderMatch(FirstParamType: Char;ILItem: PILItem): Boolean;
begin
  case FirstParamType of
    #0: Result := True;
    's': Result := ILParamToRawType(@ILitem.Param1) in [rtS8, rtS16];
    'u': Result := ILParamToRawType(@ILItem.Param1) in [rtU8, rtU16];
  else
    raise Exception.Create('Invalid FirstParamType in OpOrderMatch');
  end;
end;

function ValidationMatch(ProcValidation: TPrimValidation; CodeGenFlags: TCodeGenFlagSet): Boolean;
begin
  case ProcValidation of
    pvYes: Result := cgOverflowCheck in CodeGenFlags;
    pvNo: Result := not (cgOverflowCheck in CodeGenFlags);
    pvEither: Result := True;
  else
    raise Exception.Create('Unhandled Prim Validatio value');
  end;
end;

function ParamLocMatch(ProcLoc: TAllocLocSet;const ILParam: TILParam): Boolean;
begin
  if ILParam.Loc = locNone then
    EXIT(ProcLoc = []);

  if (ILParam.Loc = locImmediate) then
    EXIT(True);//ProcLoc = [plImm]);

  //If we can't find a suitable routine which accepts an Imm value then we'll
  //use one which doesn't and manually load it into a register.
  Result := (ProcLoc * [plA..plE,plH..plL, plHL..plBC]) <> [];
end;

function ILItemToPrimitive(ILItem: PILItem): PPrimitive;
begin
  for Result in PrimList do
    if ILItem.OpIndex = Result.OpIndex then
      if OpTypeMatch(Result.OpType, ILItem.OpType) and OpTypeMatch(Result.DestType, ILItem.ResultType) then
        if OpOrderMatch(Result.FirstParamType, ILItem) then
          if ValidationMatch(Result.Validation, ILItem.CodeGenFlags) then
            if ParamLocMatch(Result.Param1Loc, ILItem.Param1) and ParamLocMatch(Result.Param2Loc, ILItem.Param2) then
              if Result.IsBranch = (ILItem.DestType = dtCondBranch) then
              EXIT;

  Result := nil;
end;

procedure PrimSetProc(Name: String;Proc: TCodeGenProc);
var Prim: PPrimitive;
  Found: Boolean;
begin
  Found := False;
  for Prim in PrimList do
    if CompareText(Prim.ProcName, Name) = 0 then
    begin
      Prim.Proc := Proc;
      Found := True;
    end;

  if not Found then
    raise Exception.Create('Primitive not found: ' + Name);
end;

procedure PrimSetValProc(Name: String;Proc: TValidationProc);
var Prim: PPrimitive;
  Found: Boolean;
begin
  Found := False;
  for Prim in PrimList do
    if CompareText(Prim.ProcName, Name) = 0 then
    begin
      Prim.ValProc := Proc;
      Found := True;
    end;

  if not Found then
    raise Exception.Create('Validation primitive not found: ' + Name);
end;

const lutCharToParamLoc8: array['a'..'z'] of TAllocLoc = (
  plA, plB, plC, plD, plE, plFlags, plNone,  //A..G
  plH, plImm, plNone, plNone, plL, plNone,  //L..M
  plNone, plNone, plNone, plNone, plNone, plNone, plNone, plNone, //N..U
  plNone, plNone, plNone, plNone, plNone);   //V..Z

function StrToParamLoc8(C: Char;ForCorrupts: Boolean): TAllocLoc;
begin
  if (C = 'f') and not ForCorrupts then
    raise Exception.Create('Invalid paramLoc: ' + C);

  if C in ['a'..'z'] then
    Result := lutCharToParamLoc8[C]
  else
    raise Exception.Create('Invalid paramLoc: ' + C);
  if Result = plNone then
    raise Exception.Create('Invalid paramLoc: ' + C);
end;

function StrToParamLoc(S: String;ForCorrupts: Boolean): TAllocLoc;
begin
  if Length(S) = 0 then
    Result := plNone
  else if Length(S) = 1 then
    Result := StrToParamLoc8(S.Chars[0], ForCorrupts)
  else if CompareText(S, 'none') = 0 then
    Result := plNone
  else if CompareText(S, 'p1') = 0 then
    Result := plP1
  else if (S = 'bc') or (S = 'BC') then
    Result := plBC
  else if (S = 'de') or (S = 'DE') then
    Result := plDE
  else if (S = 'hl') or (S = 'HL') then
    Result := plHL
  else if (S = 'ix') or (S = 'IX') then
    Result := plIX
  else if (S = 'iy') or (S = 'IY') then
    Result := plIY
  else if (S = 'zf') or (S = 'ZF') then
    Result := plZF
  else if (S = 'zfa') or (S = 'ZFA') then
    Result := plZFA
  else if (S = 'nzf') or (S = 'NZF') then
    Result := plNZF
  else if (S = 'nzfa') or (S = 'NZFA') then
    Result := plNZFA
  else if (S = 'cf') or (S = 'CF') then
    Result := plCF
  else if (S = 'ncf') or (S = 'NCF') then
    Result := plNCF
  else if (S = 'cpla') or (S = 'CPLA') then
    Result := plCPLA
  else
    raise Exception.Create('Invalid paramLoc: ' + S);
end;

function StrToParamLocSet(S: String;ForCorrupts: Boolean): TAllocLocSet;
var X: String;
begin
  Result := [];
  for X in S.Split([';']) do
    Result := Result + [StrToParamLoc(X, ForCorrupts)];
end;

const
  fName           = 1;
  fOpType         = 2;
  fFirstParamType = 3;
  fDestType       = 4;
  fValidation     = 5;
  fProcName       = 6;
  //Column 7 empty
  fParam1Loc      = 8;
  fParam2Loc      = 9;
  fDestLoc        = 10;
  fCorrupts       = 11;
  fValProcName    = 12;
  fValProcS8      = 13;
  fValProcU8      = 14;
  fValProcS16     = 15;
  fValProcU16     = 16;


procedure InitialisePrimitiveList;
var Data: TStringList;
  Line: String;
  Fields: TArray<String>;
  Prim: PPrimitive;
begin
  if PrimList <> nil then
    PrimList.Free;

  PrimList := TList<PPrimitive>.Create;

  Data := TStringList.Create;
  Data.LoadFromFile('..\..\docs\PrimitivesEx.csv');

  for Line in Data do
    if (Length(Line) > 0) and (Line.Chars[0] <> ';') then
    begin
      Fields := Line.Split([',']);
      if Length(Fields) < 10 then
        raise Exception.Create('PrimitivesEx line too short: ' + Line);
      Prim := New(PPrimitive);
      PrimList.Add(Prim);

      if Fields[fName] = 'x' then
        Prim.OpIndex := -1
      else
      begin
        Prim.OpIndex := OpNameToIndex(Fields[fName]);
        if Prim.OpIndex = -1 then
          raise Exception.Create('Operation not found in ' + Line);
      end;


      if Fields[fOpType] = '' then
        Prim.OpType := rtUnknown
      else
        Prim.OpType := StringToOpType(Fields[fOpType]);

      if Fields[fFirstParamType] = '' then
        Prim.FirstParamType := #0
      else if Fields[fFirstParamType].Chars[0] in ['s','S','u','U'] then
        Prim.FirstParamType := Fields[fFirstParamType].ToLower.Chars[0]
      else
        raise Exception.Create('Invalid value for FirstParamType: ' + Fields[fFirstParamType]);

      Prim.IsBranch := False;
      Prim.DestType := rtUnknown;
      if Fields[fDestType] = '' then
        Prim.DestType := rtUnknown
      else if CompareText(Fields[fDestType], 'branch') = 0 then
        Prim.IsBranch := True
      else
        Prim.DestType := StringToOpType(Fields[fDestType]);

      if Length(Fields[fValidation]) <> 1 then
        raise Exception.Create('Error in PrimitivesEx.Validation field: ' + Fields[fValidation]);
      case Fields[fValidation].Chars[0] of
        'y','Y': Prim.Validation := pvYes;
        'n','N': Prim.Validation := pvNo;
        'x','X': Prim.Validation := pvEither;
      else
        raise Exception.Create('Error in PrimitivesEx.Validation field: ' + Fields[fValidation]);
      end;

      Prim.ProcName := Fields[fProcName];
      Prim.ValProcName := Fields[fValProcName];

      Prim.Param1Loc := StrToParamLocSet(Fields[fParam1Loc], False);
      Prim.Param2Loc := StrToParamLocSet(Fields[fParam2Loc], False);
      Prim.DestLoc :=   StrToParamLoc(Fields[fDestLoc], False);
      Prim.Corrupts :=  StrToParamLocSet(Fields[fCorrupts], True);

      Prim.ValProcName := Fields[fValProcName];
      Prim.ValProcS8 := Fields[fValProcS8];
      Prim.ValProcU8 := Fields[fValProcU8];
      Prim.ValProcS16 := Fields[fValProcS16];
      Prim.ValProcU16 := Fields[fValProcU16];

      Prim.Proc := nil;
      Prim.ValProc := nil;
    end;
end;

end.

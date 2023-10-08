unit PrimitivesEx;

interface
uses QTypes, ILData;

type
  TPrimValidation = (pvYes, pvNo, pvEither);

  TPrimFlag = (
    pfLoadRPLow,    //Load only the high byte of the (16-bit) parameter
    pfLoadRPHigh    //Load only high byte of the (16-bit) parameter
    );
  TPrimFlagSet = set of TPrimFlag;

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
    Flags: TPrimFlagSet;        //Assorted flags
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
  end;


function ILItemToPrimitive(const ILItem: TILItem): PPrimitive;

function PrimFindByProcName(AName: String): PPrimitive;

procedure PrimSetProc(Name: String;Proc: TCodeGenProc);
procedure PrimSetValProc(Name: String;Proc: TValidationProc);

procedure InitialisePrimitives;

procedure LoadPrimitivesFile(const Filename: String);

//Validates that generators are available for every primitive.
//Raises an exception if there is a problem
procedure ValidatePrimitives;

implementation
uses Generics.Collections, Classes, SysUtils, Operators, Variables, Fragments;

var PrimList: TList<PPrimitive>;

procedure ClearPrimList;
var Prim: PPrimitive;
begin
  for Prim in PrimList do
    Dispose(Prim);
  PrimList.Clear;
end;

procedure InitialisePrimitives;
begin
  ClearPrimList;
end;

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

function OpOrderMatch(FirstParamType: Char;const ILItem: TILItem): Boolean;
begin
  case FirstParamType of
    #0: Result := True;
    's': Result := ILItem.Param1.GetRawType in [rtS8, rtS16];
    'u': Result := ILItem.Param1.GetRawType in [rtU8, rtU16];
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
    raise Exception.Create('Unhandled Prim Validation value');
  end;
end;

function ParamLocMatch(ProcLoc: TAllocLocSet;const ILParam: TILParam): Boolean;
var V: PVariable;
begin
  case ILParam.Loc of
    locNone: EXIT(ProcLoc = []);
    locImmediate: EXIT(True);//ProcLoc = [plImm]);
    locVar: //Special cases which can handle variable types directly
    begin
      V := ILParam.ToVariable;
      if plAbsVar in ProcLoc then
        EXIT(V.Storage = vsAbsolute);
      if plRelVar in ProcLoc then
        EXIT(V.Storage = vsRelative);
    end;
  else
      //Nothing
  end;

  //If we can't find a suitable routine which accepts an Imm value then we'll
  //use one which doesn't and manually load it into a register.
  Result := (ProcLoc * [plA..plE,plH..plL, plHL..plBC]) <> [];
end;

function DestLocMatch(const Prim: PPrimitive; const ILItem: TILItem): Boolean;
var V: PVariable;
begin
//  Result := (Prim.IsBranch and (ILItem.DestType = dtCondBranch)) or not Prim.IsBranch then
  if Prim.IsBranch then
    EXIT(ILItem.DestType = dtCondBranch);

  if ILItem.DestType = dtData then
  begin
    //For an assign to a variable (Anything other than Assign is handled after the Primitive)
    V := ILItem.Dest.ToVariable;
    if Prim.DestLoc = plAbsVar then
      EXIT(V.Storage = vsAbsolute);
    if Prim.DestLoc = plRelVar then
      EXIT(V.Storage = vsRelative);
    EXIT(True);
  end
  else  //DestType <> dtData - return true because value can be massaged for branches
    EXIT(True);

  Result := False;
end;

function ILItemToPrimitive(const ILItem: TILItem): PPrimitive;
begin
  for Result in PrimList do
    if ILItem.OpIndex = Result.OpIndex then
      if OpTypeMatch(Result.OpType, ILItem.OpType) and OpTypeMatch(Result.DestType, ILItem.ResultType) then
        if OpOrderMatch(Result.FirstParamType, ILItem) then
          if ValidationMatch(Result.Validation, ILItem.CodeGenFlags) then
            if ParamLocMatch(Result.Param1Loc, ILItem.Param1) and ParamLocMatch(Result.Param2Loc, ILItem.Param2) then
              if DestLocMatch(Result, ILItem) then
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

function StrToPrimFlagSet(S: String): TPrimFlagSet;
var X: String;
begin
  Result := [];
  for X in S.Split([';']) do
    if CompareText(X, 'loadrplow') = 0 then
      Result := Result + [pfLoadRPLow]
    else if CompareText(X, 'loadrphigh') = 0 then
      Result := Result + [pfLoadRPHigh]
    else
      raise Exception.Create('Unknown prim flag: ' + X);
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
  else if CompareText(S, 'absvar') = 0 then
    Result := plAbsVar
  else if CompareText(S, 'relvar') = 0 then
    Result := plRelVar
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
  fFlags          = 7;
  fParam1Loc      = 8;
  fParam2Loc      = 9;
  fDestLoc        = 10;
  fCorrupts       = 11;
  fValProcName    = 12;
  fValProcS8      = 13;
  fValProcU8      = 14;
  fValProcS16     = 15;
  fValProcU16     = 16;


procedure LoadPrimitivesFile(const Filename: String);
var Data: TStringList;
  Line: String;
  Fields: TArray<String>;
  Prim: PPrimitive;
begin
  Data := TStringList.Create;
  Data.LoadFromFile(Filename);

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

      Prim.Flags := StrToPrimFlagSet(Fields[fFlags]);

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

//Validate that the specified proc exists in our codebase
procedure ValidateProc(S: String);
begin
  if S = '' then
    EXIT;
  if CompareText(S, 'empty') = 0 then
    EXIT;
  if CompareText(S, 'error') = 0 then
    EXIT;
  if S.Chars[0] = ':' then
    EXIT;

  if not Assigned(Fragments.FindFragmentByName(S)) then
    raise Exception.Create('Load primitives: Fragment not found: ' + S);
end;

procedure ValidatePrimitive(Prim: PPrimitive);
begin
  if not Assigned(Prim.Proc) then
    ValidateProc(Prim.ProcName);

  ValidateProc(Prim.ValProcName);
  ValidateProc(Prim.ValProcS8);
  ValidateProc(Prim.ValProcU8);
  ValidateProc(Prim.ValProcS16);
  ValidateProc(Prim.ValProcU16);
end;

procedure ValidatePrimitives;
var Prim: PPrimitive;
begin
  for Prim in PrimList do
    ValidatePrimitive(Prim);
end;

initialization
  PrimList := TList<PPrimitive>.Create;
end.

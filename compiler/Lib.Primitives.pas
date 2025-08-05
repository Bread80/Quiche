(*
Primitives are routines available to the code generator
*)
unit Lib.Primitives;

interface
uses Def.IL, Def.Operators, Def.QTypes, Def.Variables, Def.UserTypes,
  Parse.Literals,
  Lib.Data,
  Z80.Hardware;

//---Parse-time

//To be used by the parser. NOT to be used by the code generator
//Searches the available list of primitives for the Operator to find a suitable
//match based on the supplied operand types.
//If a routine is found which consumes LType and RType -> selects it.
//If not the types will be 'expanded' (if that is an option for the given
//types) until a suitably matching routine is found.
//Returns the result type that the selected routine will return.
//If no suitable routine was found returns vtUnknown
//If the parameter type were expanded then LType and RType return the expanded type(s)
function PrimFindParse(Op: TOperator;const Left, Right: TExprSlug;
  out LeftType, RightType: PUserType;var ResultType: PUserType): Boolean;
function PrimFindParseUnary(Op: TOperator;const Left: TExprSlug;
  out LeftType: PUserType;var ResultType: PUserType): Boolean;

//---Codegen time

function ILItemToPrimitive(const ILItem: TILItem;out SwapParams: Boolean): PPrimitive;

function PrimFindByProcName(AName: String): PPrimitive;

//---Initialisation-time

//Where a Primitive uses a Proc (a software CodeGen) these routines will search the
//Primitives list and substitute any such Proc names with the appropriate call-back
//routine
//procedure PrimSetProc(Name: String;Proc: TCodeGenProc);
procedure PrimSetValProc(Name: String;Proc: TValidationProc);

procedure InitialisePrimitives;

procedure LoadPrimitivesFile(const Filename: String);

//Validates that generators are available for every primitive.
//Raises an exception if there is a problem
procedure ValidatePrimitives;

implementation
uses Generics.Collections, Classes, SysUtils;

type TValProcMap = record
    Name: String;
    Proc: TValidationProc;
  end;

var ValProcs: TArray<TValProcMap>;

procedure AddValProc(AName: String;AProc: TValidationProc);
begin
  SetLength(ValProcs, length(ValProcs)+1);
  ValProcs[Length(ValProcs)-1].Name := AName;
  ValProcs[Length(ValProcs)-1].Proc := AProc;
end;

function FindValProc(AName: String): TValidationProc;
var I: Integer;
begin
  for I := 0 to Length(ValProcs)-1 do
    if CompareText(ValProcs[I].Name, AName) = 0 then
    begin
      Result := ValProcs[I].Proc;
      EXIT;
    end;

  Result := nil;
end;

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
  SetLength(ValProcs, 0);
end;


//==================NG Primitive matching & type matching

function ParamRegMatch(PrimLoc: TCodeProcLoc;AvailableRegs: TCPURegSet;
  Kind: TILParamKind;AddrMode: TAddrMode): Boolean;
begin
  if Kind = pkNone then
    EXIT(True);

  case PrimLoc of
    plNone:      Result := False;
    plImmediate: Result := Kind = pkImmediate;
    plStaticVar: Result := AddrMode = amStatic;
    plStackVar:  Result := AddrMode = amStack;
    plRegister:  Result := (AvailableRegs * [rA..rE,rH..rL, rHL..rBC]) <> [];
  else
    raise Exception.Create('Undefined PrimLoc');
  end;
end;

function IfOverflowCheckingMatch(const Prim: PPrimitive;const ILItem: PILItem): Boolean;
begin
  case Prim.IfOverflowChecking of
    pvYes: EXIT(cgOverflowCheck in ILItem.Flags) ;
    pvNo: EXIT(not (cgOverFlowCheck in ILItem.Flags));
    pvEither: EXIT(True); //Always okay
  else
    raise Exception.Create('Unknown primitive validation option');
  end;
end;

function DestMatch(Prim: PPrimitive;ILItem: PILItem): Boolean;
var V: PVariable;
begin
  Result := False;
  case ILItem.Dest.Kind of
    pkNone, pkPush, pkPushByte: Result := True;
    pkCondBranch: Result := Prim.ResultType in [vtBoolean, vtFlag];
    pkVarDest: //We need a result suitable for writing to a variable
    begin
      V := ILItem.Dest.ToVariable;
      case Prim.ProcMeta.ResultLoc of
        plNone:      Result := False;
        plRegister:  Result := True;
        plStaticVar: Result := V.AddrMode = amStatic;
        plStackVar:  Result := V.AddrMode = amStack;
      else
        Assert(False);
      end;
    end;
  else
    raise Exception.Create('Undefined Dest.Kind');
  end;
end;

type TPrimSearchRec = record
    Op: TOperator;
    GenTime: Boolean; //Are we code generating? If so does deeper matching.
                      //Parse time only needs to know a routine is available.
                      //CodeGen time needs to select the exact routine
    ILItem: PILItem;  //Only required at code gen time

    IsSigned: Boolean;  //Use signed fitness values
    IsPointer: Boolean; //Use pointer fitness values (i.e. unsigned)

    LUserType: PUserType;
    LType: TVarType;
    LIsRange: Boolean;
    LRange: TNumberRange;
    LKind: TILParamKind;
    LAddrMode: TAddrMode;

    RUserType: PUserType;
    RType: TVarType;
    RIsRange: Boolean;
    RRange: TNumberRange;
    RKind: TILParamKind;
    RAddrMode: TAddrMode;

    ResultKind: TILParamKind; //Only for GenTime??
    ResultAddrMode: TAddrMode; //Only for GenTime??


    MatchResultType: TVarType;  //Match if <> vtUnknown
    ResultType: TVarType;

    //If these are non-nil then these values should be
    //returned (PrimParse), otherwise the values passed
    //in should be retained
    ReturnLUserType: PUserType;
    ReturnRUserType: PUserType;
    ReturnResultUserType: PUserType;

    PrimIndex: Integer;
    SwapParams: Boolean;
  end;

function CalcTypeFitness(const SearchRec: TPrimSearchRec;Prim: PPrimitive;Swap: Boolean): Integer;
var
  Fitness: Integer;
  Signed: Boolean;
  ResultType: TVarType;
begin
  Signed := SearchRec.IsSigned;// and not SearchRec.IsPointer;

  if Swap then
  begin
    if SearchRec.LIsRange then
      Fitness := GetFitnessTypeRange(SearchRec.LRange, Prim.RType, Signed)
    else
      Fitness := GetFitnessTypeType(SearchRec.LType, Prim.RType, Signed);
  end
  else
  begin
    if SearchRec.LIsRange then
      Fitness := GetFitnessTypeRange(SearchRec.LRange, Prim.LType, Signed)
    else
      Fitness := GetFitnessTypeType(SearchRec.LType, Prim.LType, Signed);
  end;
  if Fitness < 0 then
    EXIT(-1);
  Result := Fitness;

  if Swap then
  begin
    if SearchRec.RIsRange then
      Fitness := GetFitnessTypeRange(SearchRec.RRange, Prim.LType, Signed)
    else
      Fitness := GetFitnessTypeType(SearchRec.RType, Prim.LType, Signed);
  end
  else
  begin
    if SearchRec.RIsRange then
      Fitness := GetFitnessTypeRange(SearchRec.RRange, Prim.RType, Signed)
    else
      Fitness := GetFitnessTypeType(SearchRec.RType, Prim.RType, Signed);
  end;
  if Fitness < 0 then
    EXIT(-1);

  Result := Result + Fitness;

  if SearchRec.MatchResultType <> vtUnknown then
  begin
    if Prim.ResultTypeIsLType then
      ResultType := SearchRec.LType
    else if Prim.ResultTypeIsRType then
      ResultType := SearchRec.RType
    else
      ResultType := Prim.ResultType;

    Fitness := GetFitnessTypeType(SearchRec.MatchResultType, ResultType, SearchRec.IsSigned);
    if Fitness < 0 then
      EXIT(-1);
    Result := Result + Fitness;
  end;
end;

function CalcFitness(const SearchRec: TPrimSearchRec;Prim: PPrimitive;Swap: Boolean): Integer;
begin
  Result := CalcTypeFitness(SearchRec, Prim, Swap);
  if Result = -1 then
    EXIT;
  //We want a pointer result so bias the result against signed result types
  if SearchRec.IsPointer then
  begin
    if IsSignedVarType(Prim.ResultType) then
      Result := Result + 10;
  end
  else if SearchRec.IsSigned then
    if not IsSignedVarType(Prim.ResultType) then
      Result := Result + 10;

  //Validate any other param compatibility issues
  if SearchRec.GenTime then
  begin
    if Swap then
    begin
      if not ParamRegMatch(Prim.ProcMeta.LLoc, Prim.ProcMeta.LRegs, SearchRec.RKind, SearchRec.RAddrMode) then
        EXIT(-1);
      if (Prim.RType <> vtUnknown) and
        not ParamRegMatch(Prim.ProcMeta.RLoc, Prim.ProcMeta.RRegs, SearchRec.LKind, SearchRec.LAddrMode) then
        EXIT(-1);
    end
    else
    begin
      if not ParamRegMatch(Prim.ProcMeta.LLoc, Prim.ProcMeta.LRegs, SearchRec.LKind, SearchRec.LAddrMode) then
        EXIT(-1);
      if (Prim.RType <> vtUnknown) and
        not ParamRegMatch(Prim.ProcMeta.RLoc, Prim.ProcMeta.RRegs, SearchRec.RKind, SearchRec.RAddrMode) then
        EXIT(-1);
    end;

    if not IfOverflowCheckingMatch(Prim, SearchRec.ILItem) then
      EXIT(-1);
    if not DestMatch(Prim, SearchRec.ILItem) then
      EXIT(-1);

    //Primitives which read/write directly to/from variables
(*    if SearchRec.ResultKind = pkVarDest then
    begin
      if pfnDestStaticVar in Prim.Flags then
        if SearchRec.ResultStorage <> vsStatic then
          EXIT(-1);
      if pfnDestRelVar in Prim.Flags then
        if SearchRec.ResultStorage <> vsStack then
          EXIT(-1);
    end;
*)  end;
end;

function PrimSearch(var SearchRec: TPrimSearchRec): Boolean;
var
  Fitness: Integer;
  Index: Integer;
  BestFitness: Integer;
  BestIndex: Integer;
  SwapParams: Boolean;
  Prim: PPrimitive;
begin
  //If both sides are enumerations then both types must be the same type, or OfType
  if (SearchRec.LType = vtEnumeration) then
    if SearchRec.LType = SearchRec.RType then
      if RemoveSubRange(SearchRec.LUserType) <> RemoveSubRange(SearchRec.RUserType) then
        EXIT(False);

  SearchRec.ReturnLUserType := nil;
  SearchRec.ReturnRUserType := nil;
  SearchRec.ReturnResultUserType := nil;

  BestFitness := MaxInt;
  BestIndex := -1;
  SwapParams := False;

  //First entry for this operator
  Index := Operations[SearchRec.Op].FirstPrimIndex;

  SearchRec.SwapParams := False;
  while (Index < PrimList.Count) and (PrimList[Index].Op = SearchRec.Op)
    and (BestFitness <> 0) do
  begin
    Prim := PrimList[Index];

    SearchRec.ResultType := Prim.ResultType;
    //TODO: Parameterised operators (how??)

    Fitness := CalcFitness(SearchRec, Prim, False);
    if (Fitness >= 0) and (Fitness < BestFitness) then
    begin
      BestFitness := Fitness;
      BestIndex := Index;
      SwapParams := False;
    end;

    if Prim.Commutative then
    begin
      Fitness := CalcFitness(SearchRec, Prim, True);
      if (Fitness >= 0) and (Fitness < BestFitness) then
      begin
        BestFitness := Fitness;
        BestIndex := Index;
        SwapParams := True;
      end;
    end;

    Inc(Index);
  end;
  Result := BestIndex >= 0;
  if not Result then
    EXIT;

  SearchRec.PrimIndex := BestIndex;
  Prim := PrimList[SearchRec.PrimIndex];

  if Prim.ResultTypeIsLType then
    SearchRec.ReturnResultUserType := SearchRec.LUserType
  else if Prim.ResultTypeIsRType then
    SearchRec.ReturnResultUserType := SearchRec.RUserType
  else if Prim.ResultType <> vtUnknown then
  begin
    SearchRec.ReturnResultUserType := GetSystemType(Prim.ResultType);
    Assert(Assigned(SearchRec.ReturnResultUserType));  //Unsuitable Prim.ResultType.
          //For non-system types you probablu want to specify a ResultType of
          //LType or RType in Primitives.csv
  end;

  //Numeric types can be expanded to match available primitives if required
  if IsNumericType(Prim.LType) then
    SearchRec.ReturnLUserType := GetSystemType(Prim.LType);
  if IsNumericType(Prim.RType) then
    SearchRec.ReturnRUserType := GetSystemType(Prim.RType);

  SearchRec.SwapParams := SwapParams;
end;

function PrimFindParse(Op: TOperator;const Left, Right: TExprSlug;
  out LeftType, RightType: PUserType;var ResultType: PUserType): Boolean;
var SearchRec: TPrimSearchRec;
  TempType: PUserType;
begin
  SearchRec.Op := Op;
  SearchRec.GenTime := False;
  //Fields not used for parse-time searches.
  SearchRec.ILItem := nil;
  SearchRec.LAddrMode := amStack;  //Don't care at parse time
  SearchRec.RAddrMode := amStack;  //Don't care at parse time

  SearchRec.MatchResultType := UTToVT(ResultType);

  SearchRec.LUserType := Left.ResultType;
  SearchRec.LType := UTToVT(Left.ResultType);
  if Left.ILItem <> nil then
    SearchRec.LKind := pkVarSource
  else
    SearchRec.LKind := Left.Operand.Kind;

  SearchRec.RUserType := Right.ResultType;
  SearchRec.RType := UTToVT(Right.ResultType);
  if Right.ILItem <> nil then
    SearchRec.RKind := pkVarSource
  else
    SearchRec.RKind := Right.Operand.Kind;

  SearchRec.LIsRange := (SearchRec.LKind = pkImmediate) and IsNumericType(SearchRec.LType);
  if SearchRec.LIsRange then
    SearchRec.LRange := IntToNumberRange(Left.Operand.Imm.IntValue);

  SearchRec.RIsRange := (SearchRec.RKind = pkImmediate) and IsNumericType(SearchRec.RType);
  if SearchRec.RIsRange then
    SearchRec.RRange := IntToNumberRange(Right.Operand.Imm.IntValue);

  if Operations[Op].SignCombine then
  begin
    Assert(not (SearchRec.LIsRange and SearchRec.RIsRange),'Can''t have both parameters ' +
      'as immediates - use compile time evaluation for that!');

    if SearchRec.LIsRange then
      SearchRec.IsSigned := (SearchRec.LRange in SignedRanges) or IsSignedVarType(SearchRec.RType)
    else if SearchRec.RIsRange then
      SearchRec.IsSigned := IsSignedVarType(SearchRec.LType) or (SearchRec.RRange in SignedRanges)
    else
      SearchRec.IsSigned := IsSignedVarType(SearchRec.LType) or IsSignedVarType(SearchRec.RType);
  end
  else
    SearchRec.IsSigned := False;

  SearchRec.IsPointer := (not SearchRec.LIsRange and (SearchRec.LType in [vtPointer, vtTypedPointer])) or
    (not SearchRec.RIsRange and (SearchRec.RType in [vtPointer, vtTypedPointer]));

  {SEARCH}
  Result := PrimSearch(SearchRec);

  if SearchRec.ReturnLUserType <> nil then
    LeftType := SearchRec.ReturnLUserType;
  if SearchRec.ReturnRUserType <> nil then
    RightType := SearchRec.ReturnRUserType;
  if SearchRec.SwapParams then
  begin
    TempType := LeftType;
    LeftType := RightType;
    RightType := TempType;
  end;

  if SearchRec.ReturnResultUserType <> nil then
  begin
    ResultType := SearchRec.ReturnResultUserType;
    if SearchRec.IsPointer then
      if ResultType.VarType = vtWord then
        ResultType := GetSystemType(vtPointer);

    if (ResultType.VarType in [vtReal, vtString]) or (SearchRec.LType in [vtReal, vtString]) or
      (SearchRec.RType in [vtReal, vtString]) then
      Assert(False);  //TODO

    if ResultType.VarType = vtFlag then
      ResultType := GetSystemType(vtBoolean);
  end;
end;

function PrimFindParseUnary(Op: TOperator;const Left: TExprSlug;
  out LeftType: PUserType;var ResultType: PUserType): Boolean;
var SearchRec: TPrimSearchRec;
begin
  SearchRec.Op := Op;
  SearchRec.GenTime := False;
  //Fields not used for parse-time search. This stops the compiler warnings.
  SearchRec.LKind := pkVarSource;
  SearchRec.RKind := pkVarSource;
  SearchRec.LAddrMode := amStack;
  SearchRec.RAddrMode := amStack;
  SearchRec.ILItem := nil;

  if Op = opStoreImm then
    Assert(False);

  SearchRec.MatchResultType := UTToVT(ResultType);

  SearchRec.LUserType := Left.ResultType;
  SearchRec.LType := UTToVT(Left.ResultType);
  if Left.ILItem <> nil then
    SearchRec.LKind := pkVarSource
  else
    SearchRec.LKind := Left.Operand.Kind;

  SearchRec.RUserType := nil;
  SearchRec.RType := vtUnknown;

  SearchRec.LIsRange := (SearchRec.LKind = pkImmediate) and IsNumericType(SearchRec.LType);
  if SearchRec.LIsRange then
    SearchRec.LRange := IntToNumberRange(Left.Operand.Imm.IntValue);
  SearchRec.RIsRange := False;

  if SearchRec.LIsRange then
    SearchRec.IsSigned := SearchRec.LRange in SignedRanges
  else
    SearchRec.IsSigned := IsSignedVarType(SearchRec.LType);
  SearchRec.IsPointer := not SearchRec.LIsRange and (SearchRec.LType in [vtPointer, vtTypedPointer]);

  Result := PrimSearch(SearchRec);

  if SearchRec.ReturnLUserType <> nil then
    LeftType := SearchRec.ReturnLUserType;

  if SearchRec.ReturnResultUserType <> nil then
  begin
    ResultType := SearchRec.ReturnResultUserType;
    if SearchRec.IsPointer then
      if ResultType.VarType = vtWord then
        ResultType := GetSystemType(vtPointer);

    if (ResultType.VarType in [vtReal, vtString]) or (SearchRec.LType in [vtReal, vtString]) then
      Assert(False);  //TODO

    if ResultType.VarType = vtFlag then
      ResultType := GetSystemType(vtBoolean);
  end;
end;

//Matching Primitives
//Match operands by expanding as per ParseTime matching.
//Once we've established the LType and RType we can match on the other parameters:
// * If either is immediate, look for a variant taking immediates
// * (Future TODO: if variable look for variant taking variable - indirect or indexed as appropriate
// * If we need flag for branching, look for such
// * Otherwise look for ResultType matching OpType (this may be part of the above)
// * Match validation
// * Match Locations (Not sure this should be here?)
function ILItemToPrimitive(const ILItem: TILItem;out SwapParams: Boolean): PPrimitive;
var
  SearchRec: TPrimSearchRec;
  V: PVariable;
begin
  //This routine is not valid for these operators
  Assert(ILItem.Op >= Def.Operators.opAdd, 'Cannot use this function with this operator');

  SearchRec.Op := ILItem.Op;
  SearchRec.PrimIndex := -1;
  SearchRec.GenTime := True;
  SearchRec.ILItem := @ILItem;
  //Deduce the initial types to search for
  SearchRec.LUserType := ILItem.Param1.GetUserType;
  SearchRec.LType := ILItem.Param1.GetVarType;
  SearchRec.LIsRange := False;
  SearchRec.RUserType := ILItem.Param2.GetUserType;
  SearchRec.RType := ILItem.Param2.GetVarType;
  SearchRec.RIsRange := False;
  SearchRec.IsSigned := Operations[ILItem.Op].SignCombine and (IsSignedVarType(SearchRec.LType) or IsSignedVarType(SearchRec.RType));
  SearchRec.IsPointer := Operations[ILItem.Op].SignCombine and
    ((SearchRec.LType in [vtPointer, vtTypedPointer]) or (SearchRec.RType in [vtPointer, vtTypedPointer]));
  //Do we need a boolean result in a flag (for a jump) or register (for assigning)?
  if ILItem.ResultVarType in [vtBoolean, vtFlag] then
    SearchRec.MatchResultType := ILItem.ResultVarType
  else
    SearchRec.MatchResultType := vtUnknown;

  SearchRec.LKind := ILItem.Param1.Kind;
  if SearchRec.LKind in [pkVarSource, pkVarAddr] then
  begin
    V := ILItem.Param1.ToVariable;
    SearchRec.LAddrMode := V.AddrMode;
  end;

  SearchRec.RKind := ILItem.Param2.Kind;
  if SearchRec.RKind in [pkVarSource, pkVarAddr] then
  begin
    V := ILItem.Param2.ToVariable;
    SearchRec.RAddrMode := V.AddrMode;
  end;

  SearchRec.ResultKind := ILItem.Dest.Kind;
  if SearchRec.ResultKind = pkVarDest then
  begin
    V := ILItem.Dest.ToVariable;
    SearchRec.ResultAddrMode := V.AddrMode;
  end;

  if PrimSearch(SearchRec) then
  begin
    Result := PrimList[SearchRec.PrimIndex];
    SwapParams := SearchRec.SwapParams;
  end
  else
    Result := nil;
end;

//================OTHER

function PrimFindByProcName(AName: String): PPrimitive;
begin
  for Result in PrimList do
    if CompareText(Result.ProcName, AName) =  0 then
      EXIT;

  Result := nil;
end;

procedure PrimSetValProc(Name: String;Proc: TValidationProc);
var Prim: PPrimitive;
//  Found: Boolean;
begin
  AddValProc(Name, Proc);
//  Found := False;
  for Prim in PrimList do
    if CompareText(Prim.ProcName, Name) = 0 then
    begin
      Prim.ValidateProc := Proc;
//      Found := True;
    end;
//  if not Found then
//    raise Exception.Create('ValProc not used by any primitives: ' + Name);
end;


function StrToCPURegSet(S: String;ForCorrupts: Boolean): TCPURegSet;
var X: String;
begin
  Result := [];
  for X in S.Split([';']) do
    Result := Result + [StrToCPURegAll(X, ForCorrupts)];
end;

procedure StrToPrimLoc(const S: String;out Loc: TCodeProcLoc;out Regs: TCPURegSet);
begin
  Regs := [];
  if S = '' then
    Loc := plNone
  else if CompareText(S, 'none') = 0 then
    Loc := plNone
  else if CompareText(S, 'imm') = 0 then
    Loc := plImmediate
  else if CompareText(S, 'static') = 0 then
    Loc := plStaticVar
  else if CompareText(S, 'stack') = 0 then
    Loc := plStackVar
  else
  begin
    Loc := plRegister;
    Regs := StrToCPURegSet(S, False);
  end;
end;

function StrToLoadParamType(S: String): TLoadParamType;
begin
  if S = '' then
    Result := lptNormal
  else if CompareText(S, 'load_rp_low') = 0 then
    Result := lptLow
  else if CompareText(S, 'load_rp_high') = 0 then
    Result := lptHigh
  else
    raise Exception.Create('Unknown LoadParamType: ' + S);
end;

const
  //Primitive selection data
  fName           = 1;
  fLType          = 2;
  fRType          = 3;
  fCommutative    = 4;
  fResultType     = 5;
  fIfOverflowChecking = 6;

  //Primitive data (hopefully to evenetually be migrated into the fragment and
  //library files
  fFlags          = 7;
  fProcName       = 8;

  //Empty columns to give half-decent presentation in Excel
  fLRegs          = 11; //Being deprecated
  fRRegs          = 12; //Being deprecated
  fResultRegs      = 13; //Being deprecated
  fCorrupts       = 14; //Being deprecated

  fOverflowCheckProc   = 15;
  fRangeCheckToS8   = 16;
  fRangeCheckToU8   = 17;
  fRangeCheckToS16  = 18;
  fRangeCheckToU16  = 19;

procedure LoadPrimitivesFile(const Filename: String);
var Data: TStringList;
  Line: String;
  Fields: TArray<String>;
  Prim: PPrimitive;
  I: Integer;
begin
  Data := TStringList.Create;
  Data.LoadFromFile(Filename);

  for Line in Data do
    if (Length(Line) > 0) and (Line.Chars[0] <> ';') then
    begin
      if Line.StartsWith('END') then
        EXIT;

      Fields := Line.Split([',']);
      if Fields[fName] <> '' then
      begin
        if Length(Fields) < 17 then
          raise Exception.Create('PrimitivesNG line too short: ' + Line);
        for I:=0 to Length(Fields)-1 do
          Fields[I] := Fields[I].Trim;

        Prim := New(PPrimitive);
        PrimList.Add(Prim);

        //Primitive selection data
        Prim.Op := IdentToAnyOperator(Fields[fName]);
        if Prim.Op = opUnknown then
          raise Exception.Create('Operation not found for primitive in ' + Line);
        if Operations[Prim.Op].FirstPrimIndex < 0 then
          Operations[Prim.Op].FirstPrimIndex := PrimList.Count-1;

        Prim.ProcName := Fields[fProcName];
        Prim.Fragment := FindFragmentByName(Prim.ProcName);

        if CompareText(Fields[fLType], 'none') = 0 then
          Prim.LType := vtUnknown
        else
        begin
          Prim.LType := StringToVarType(Fields[fLType]);
          if Prim.LType = vtUnknown then
            raise Exception.Create('Unknown LType: ' + Fields[fLType]);
        end;

        if CompareText(Fields[fRType], 'none') = 0 then
          Prim.RType := vtUnknown
        else
        begin
          Prim.RType := StringToVarType(Fields[fRType]);
          if Prim.RType = vtUnknown then
            raise Exception.Create('Unknown RType: ' + Fields[fRType]);
        end;
        Prim.Commutative := StringToBoolean(Fields[fCommutative]);

        Prim.ResultTypeIsLType := CompareText(Fields[fResultType], 'LType') = 0;
        Prim.ResultTypeIsRType := CompareText(Fields[fResultType], 'RType') = 0;
        if Prim.ResultTypeIsLType or Prim.ResultTypeIsRType then
          Prim.ResultType := vtUnknown
        else if CompareText(Fields[fResultType], 'None') = 0 then
          Prim.ResultType := vtUnknown
        else
        begin
          Prim.ResultType := StringToVarType(Fields[fResultType]);
          if Prim.ResultType = vtUnknown then
            raise Exception.Create('Unknown ResultType: ' + Fields[fResultType]);
        end;
        //--END

        if Length(Fields[fIfOverFlowChecking]) <> 1 then
          raise Exception.Create('Error in PrimitivesNG.OverflowChecking field: ' + Fields[fIfOverflowChecking]);
        case Fields[fIfOverflowChecking].Chars[0] of
          'y','Y': Prim.IfOverflowChecking := pvYes;
          'n','N': Prim.IfOverflowChecking := pvNo;
          'x','X': Prim.IfOverflowChecking := pvEither;
        else
          raise Exception.Create('Error in PrimitivesNG.Validation field: ' + Fields[fIfOverflowChecking]);
        end;


        //Primitive usage data
        Prim.LLoadType := StrToLoadParamType(Fields[fFlags]);

        //If data is specified by the fragment
        if Assigned(Prim.Fragment) and Prim.Fragment.HaveMeta then
        begin
          //TODO: Change ProcMeta to a pointer and just copy the pointer
          Prim.ProcMeta := Prim.Fragment.ProcMeta;

          if (Fields[fLRegs] <> '') or (Fields[fRRegs] <> '') or
            (Fields[fCorrupts] <> '') then
            raise Exception.Create('Register or Corrupts data specified but Fragment (etc) has already specified this data.');
        end
        else
        begin
          Prim.ProcMeta.Init;
          StrToPrimLoc(Fields[fLRegs], Prim.ProcMeta.LLoc, Prim.ProcMeta.LRegs);
          StrToPrimLoc(Fields[fRRegs], Prim.ProcMeta.RLoc, Prim.ProcMeta.RRegs);
        end;

        //Option to override Results Reg in fragment.
        //This is used if the fragment or proc returns multiple results (e.g div/mod)
        //or the result can be interpreted in multiple ways, eg the result of
        //a comparison might be in ZF or CF depending on the test to be performed
        if Fields[fResultRegs] <> '' then
        begin
          Prim.ProcMeta.ResultInLReg := CompareText(Fields[fResultRegs], 'param1') = 0;
          if Prim.ProcMeta.ResultInLReg then
          begin
            Prim.ProcMeta.ResultLoc := Prim.ProcMeta.LLoc;
            Prim.ProcMeta.ResultRegs := Prim.ProcMeta.LRegs;
          end
          else
          begin
            StrToPrimLoc(Fields[fResultRegs], Prim.ProcMeta.ResultLoc, Prim.ProcMeta.ResultRegs);
            if Prim.ProcMeta.ResultLoc = plImmediate then
              raise Exception.Create('Invalid ResultLoc: Immediate');
          end;
//          Prim.Preserves := [];//TODO not StrToCPURegSet(Fields[fCorrupts], True);
        end;

        Prim.OverflowCheckProcName := Fields[fOverflowCheckProc];
        Prim.RangeCheckToS8 := Fields[fRangeCheckToS8];
        Prim.RangeCheckToU8 := Fields[fRangeCheckToU8];
        Prim.RangeCheckToS16 := Fields[fRangeCheckToS16];
        Prim.RangeCheckToU16 := Fields[fRangeCheckToU16];

        Prim.ValidateProc := nil;
      end;
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

  if not Assigned(FindFragmentByName(S)) then
    raise Exception.Create('Load primitives: Fragment not found: ''' + S + '''');
end;

procedure ValidatePrimitive(Prim: PPrimitive);
begin
  if Prim.ProcName = '' then
//  if not Assigned(Prim.Proc) then
    ValidateProc(Prim.ProcName);

//  ValidateProc(PrimNG.ValidateProcName);
//  ValidateProc(PrimNG.ValidateToS8);
//  ValidateProc(PrimNG.ValidateToU8);
//  ValidateProc(PrimNG.ValidateToS16);
//  ValidateProc(PrimNG.ValidateToU16);
end;

procedure ValidatePrimitives;
var Prim: PPrimitive;
begin
  //TODO: Verify that Fragments and library routines are also available

  for Prim in PrimList do
    ValidatePrimitive(Prim);
end;

initialization
  PrimList := TList<PPrimitive>.Create;
end.

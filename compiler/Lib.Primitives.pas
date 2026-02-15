(*
Primitives are routines available to the code generator
*)
unit Lib.Primitives;

interface
uses Def.IL, Def.Operators, Def.VarTypes, Def.Variables, Def.UserTypes,
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

//---Codegen-time

//Matching Primitives
//Match operands by expanding as per ParseTime matching.
//Once we've established the LType and RType we can match on the other parameters:
// * If either is immediate, look for a variant taking immediates
// * (Future TODO: if variable look for variant taking variable - indirect or indexed as appropriate
// * If we need flag for branching, look for such
// * Otherwise look for ResultType matching OpType (this may be part of the above)
// * Match validation
// * Match Locations (Not sure this should be here?)
function PrimFindCodeGen(const ILItem: TILItem;out SwapParams: Boolean): PPrimitive;

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


//================== Primitive matching & type matching

//Does the primitive match the given addressing mode and available set of registers to use?
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
    plStaticPtrVar:  Result := AddrMode = amStaticRef;
    plStackPtrVar:  Assert(False);//Result := AddrMode = amStackPtr;

    plRegister:  Result := (AvailableRegs * [rA..rE,rH..rL, rHL..rBC]) <> [];
  else
    raise Exception.Create('Undefined PrimLoc');
  end;
end;

//Do the overflow checking and range checking of the ILItem and Primitive match?
function IfErrorCheckingMatch(const Prim: PPrimitive;const ILItem: PILItem): Boolean;

  function FlagMatch(Check: TPrimValidation;Flag: Boolean): Boolean;
  begin
    case Check of
      pvYes: EXIT(Flag) ;
      pvNo: EXIT(not Flag);
      pvEither: EXIT(True); //Always okay
    else
      raise Exception.Create('Unknown primitive validation option');
    end;
  end;

begin
  Result := FlagMatch(Prim.IfOverflowChecking, dfOverFlowCheck in ILItem.Flags) and
    FlagMatch(Prim.IfBoundsChecking, (dfBoundsCheck in ILItem.Flags));
end;

//Does the result/dest of the ILItem match that of the Primitive?
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
        plStaticPtrVar: Result := V.AddrMode = amStaticRef;
        plStackPtrVar:  Assert(False);//Result := V.AddrMode = amStackPtr;
      else
        Assert(False);
      end;
    end;
  else
    raise Exception.Create('Undefined Dest.Kind');
  end;
end;

type
  PSearchRecParam = ^TSearchRecParam;
  TSearchRecParam = record
    UserType: PUserType;
    VarType: TVarType;
    IsRange: Boolean;
    Range: TNumberRange;
    Kind: TILParamKind;
    AddrMode: TAddrMode;
  end;

type TPrimSearchRec = record
    Op: TOperator;
    GenTime: Boolean; //Are we code generating? If so does deeper matching.
                      //Parse time only needs to know a routine is available.
                      //CodeGen time needs to select the exact routine
    ILItem: PILItem;  //Only required at code gen time

    IsSigned: Boolean;  //Use signed fitness values
    IsPointer: Boolean; //Use pointer fitness values (i.e. unsigned)

    Left: TSearchRecParam;  //Left/Param 1 data
    Right: TSearchRecParam; //Right/Param 2 data

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

//----------------------Fitness

//Takes an integer value and returns which of the above ranges if best fits
function IntToNumberRange(Value: Integer): TNumberRange;
begin
  for Result := low(TNumberRange) to high(TNumberRange) do
    if Value <= NumberRangeBounds[Result] then
      EXIT;
  Result := high(TNumberRange);
end;


//Fitness values for (<primitive-type>,<code-type>)
const FitnessVarTypeVarType: array[vtInt8..vtReal,vtInt8..vtReal] of Integer =
//From. This axis is the parameter type. Other is primitive argument type
//To	    Int8	Int	Byt	Wrd	Ptr	TPtr Real
{Int8}	  ((0,	10,	10,	30,	30,	-1, 30),		//Otherwise:
{Integer}	(1,	  0,	3,	10,	10,	-1, 10),		//	If both parameters are unsigned set Byte to Integer to 3
{Byte}	  (20,	30,	0,	20,	20,	-1, 40),		//If SignCombine is set for an operator:
{Word}	  (10,	20,	2,	0,	1,	-1, 20),		//If SignCombine is set for an operator:
{Pointer}	(10,	20,	3,	1,	0,	0,  20),		//	If either parameter is signed, set Byte to Integer to 1
{TPointer}(-1,  -1, -1, -1, -1, -1, -1),    //Typed Pointer to Pointer is allowed. All others fail.
{Real}	  (4,	  4,	4,	4,	4,	-1, 0));    //**This is handled in code in GetFitnessTypeType


//NOTE: FINAL VALUES TODO
const FitnessNumberRangeVarTypeUnsigned:
  array[vtInt8..vtReal,low(TNumberRange)..high(TNumberRange)] of Integer =
//	From. This axis is the parameter type. Other is primitive argument type
//To	  NR_Real	S16	S8	Any	S16U8	S16U16	U16
{Int8}	  ((40,	30,	0,	0,	10,	  20,	  30),
{Integer}	(20,  0,	1,	2,	2,	  1,	  10),
{Byte}	  (30,  20,	10,	0,	0,	  10,	  20),
{Word}	  (10,  10,	20,	1,	1,	  0,	  0),
{Pointer}	(10,  10,	20,	1,	1,	  0,	  0),
{TPointer}(-1,  -1, -1, -1, -1,   -1,   -1),
{Real}	  (0,	  4,	4,	4,	4,	  4,	  4));

//NOTE: FINAL VALUES TODO
const FitnessNumberRangeVarTypeSigned:
  array[vtInt8..vtReal,low(TNumberRange)..high(TNumberRange)] of Integer =
//	From. This axis is the parameter type. Other is primitive argument type
//To	  NR_Real	S16	S8	Any	S16U8	S16U16	U16
{Int8}	  ((40,	30,	0,	0,	10,	  20,	  30),
{Integer}	(20,  0,	1,	1,	1,	  0,	  10), //-1 from Any S16U8 S16u16
{Byte}	  (30,  20,	10,	0,	0,	  10,	  20),
{Word}	  (10,  10,	20,	2,	2,	  1,	  0), //+1 to Any, S16U8, S16u16
{Pointer}	(10,  10,	20,	2,	2,	  1,	  0), //+1 to Any, S16U8, S16u16
{TPointer}(-1,  -1, -1, -1, -1,   -1,   -1),
{Real}	  (0,	  4,	4,	4,	4,	  4,	  4));

//See the PrimSelection.xlsx spreadsheet for calculations, validations, and notes
//on the above fitness values.

//Assesses the compatibility level between the code (variable) type and primitive's
//parameter type.
//  <0: totally incompatible
//   0: exact match
//1..9: data will need to be expanded. The lower the number the less costly the
//      expansion
//>=10: data will need to be shrunk - note all values in the original type can
//      be represented in the target type. Values will need to be validated
//      (if validation is enabled). Result may fail (run-time error) or be incorrect
//      The higher the value the greater the incompatibility


//CodeType is the Type of the value (from a variable)
//PrimType is the type of the primitive's argument
//Signed should be true if SignCombine is set for the operator AND the other parameter
//is a signed type[1].
//Signed should be false in all other cases.
//[1] If the other parameter is a constant it's type should be established (via
//GetFitnessTypeRange) before calling this function
function GetFitnessNumericType(CodeType, PrimType: TVarType;Signed: Boolean): Integer;
begin
  Assert(IsNumericVarType(PrimType));

  if not IsNumericVarType(CodeType) then
    EXIT(-1);

  if Signed and (CodeType = vtByte) and (PrimType = vtInteger) then
    Result := 1
  else
    Result := FitnessVarTypeVarType[PrimType, CodeType];
end;

//As GetFitnessTypeType but where the parameter is a constant AND a numerical type
function GetFitnessNumericRange(CodeRange: TNumberRange; PrimType: TVarType;Signed: Boolean): Integer;
begin
  //Ranges only apply to numeric types
  Assert(IsNumericVarType(PrimType));

  if Signed then
    Result := FitnessNumberRangeVarTypeSigned[PrimType, CodeRange]
  else
    Result := FitnessNumberRangeVarTypeUnsigned[PrimType, CodeRange];
end;

//Assess fitness between types, for non-numeric types
function GetFitnessArrays(const SearchParam: TSearchRecParam; const PrimArrayDef: TArrayDef): Integer;
var Fail: Boolean;
var SearchDef: PArrayDef;
begin
  Assert(SearchParam.UserType <> nil);
  SearchDef := @SearchParam.UserType.ArrayDef;

  //***To be refined
  Fail := False;
  case PrimArrayDef.ArrayType of
    atUnknown: ;  //Matches anything
    atArray: Fail := SearchDef.ArrayType <> atArray;
    atVector: Fail := SearchDef.ArrayType <> atVector;
    atList: Fail := SearchDef.ArrayType <> atList;
  else
    raise EVarType.Create;
  end;
  if Fail then
    EXIT(-1);

  case PrimArrayDef.ArraySize of
    asUnknown: ;
    asShort: Fail := SearchDef.ArraySize <> asShort;
    asLong: Fail := SearchDef.ArraySize <> asLong;
  else
    raise EVarType.Create;
  end;
  if Fail then
    EXIT(-1);

  //Unbounded?

  //ElementSize?

  Result := 10;
end;

//Assess fitness between types, for non-numeric types
function GetFitnessOtherType(CodeType, PrimType: TVarType): Integer;
begin
  Assert(not IsNumericVarType(PrimType));

  if CodeType = PrimType then
    Result := 0
  else if IsBooleanVarType(CodeType) and IsBooleanVarType(PrimType) then
    Result := 5
  else
    Result := -1;
end;

//Calculate the fitness of a single parameter to the primitive
function CalcParamFitness(const SearchParam: TSearchRecParam;PrimType: TVarType;const PrimArrayDef: TArrayDef;Signed: Boolean): Integer;
begin
  if IsNumericVarType(PrimType) then
  begin
    if SearchParam.IsRange then
      Result := GetFitnessNumericRange(SearchParam.Range, PrimType, Signed)
    else
      Result := GetFitnessNumericType(SearchParam.VarType, PrimType, Signed);
  end
  else  //Not numeric types
    if (SearchParam.VarType = vtArrayType) and (PrimType = vtArrayType) then
      Result := GetFitnessArrays(SearchParam, PrimArrayDef)
    else
      Result := GetFitnessOtherType(SearchParam.VarType, PrimType);

end;

//Calculate the fitness between the types of the search parameters and the primitive.
//If Swap if True parameter ordering will be swapped.
function CalcTypeFitness(const SearchRec: TPrimSearchRec;Prim: PPrimitive;Swap: Boolean): Integer;
var
  Fitness: Integer;
  Signed: Boolean;
  ResultType: TVarType;
begin
  Signed := SearchRec.IsSigned;

  //Left parameter against primitive left (right is swapped)
  if Swap then
    Fitness := CalcParamFitness(SearchRec.Left, Prim.RType, Prim.RArrayDef, Signed)
  else
    Fitness := CalcParamFitness(SearchRec.Left, Prim.LType, Prim.LArrayDef, Signed);

  if Fitness < 0 then
    EXIT(-1);
  Result := Fitness;

  //Right parameter agains primitive right (left is swapped)
  if Swap then
    Fitness := CalcParamFitness(SearchRec.Right, Prim.LType, Prim.LArrayDef, Signed)
  else
    Fitness := CalcParamFitness(SearchRec.Right, Prim.RType, Prim.RArrayDef, Signed);

  if Fitness < 0 then
    EXIT(-1);

  Result := Result + Fitness;

  //Result (if it matters)
  if SearchRec.MatchResultType <> vtUnknown then
  begin
    if Prim.ResultTypeIsLType then
      ResultType := SearchRec.Left.VarType
    else if Prim.ResultTypeIsRType then
      ResultType := SearchRec.Right.VarType
    else
      ResultType := Prim.ResultType;

    if IsNumericVarType(ResultType) then
      Fitness := GetFitnessNumericType(SearchRec.MatchResultType, ResultType, SearchRec.IsSigned)
    else
      Fitness := GetFitnessOtherType(SearchRec.MatchResultType, ResultType);

    if Fitness < 0 then
      EXIT(-1);
    Result := Result + Fitness;
  end;
end;

//Calculate the fitness between the search parameters and the primitive
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
      if not ParamRegMatch(Prim.ProcMeta.LLoc, Prim.ProcMeta.LRegs, SearchRec.Right.Kind, SearchRec.Right.AddrMode) then
        EXIT(-1);
      if (Prim.RType <> vtUnknown) and
        not ParamRegMatch(Prim.ProcMeta.RLoc, Prim.ProcMeta.RRegs, SearchRec.Left.Kind, SearchRec.Left.AddrMode) then
        EXIT(-1);
    end
    else
    begin
      if not ParamRegMatch(Prim.ProcMeta.LLoc, Prim.ProcMeta.LRegs, SearchRec.Left.Kind, SearchRec.Left.AddrMode) then
        EXIT(-1);
      if (Prim.RType <> vtUnknown) and
        not ParamRegMatch(Prim.ProcMeta.RLoc, Prim.ProcMeta.RRegs, SearchRec.Right.Kind, SearchRec.Right.AddrMode) then
        EXIT(-1);
    end;

    if not IfErrorCheckingMatch(Prim, SearchRec.ILItem) then
      EXIT(-1);
    if not DestMatch(Prim, SearchRec.ILItem) then
      EXIT(-1);

  end;
end;

//Search the primitives
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
  if (SearchRec.Left.VarType = vtEnumeration) then
    if SearchRec.Left.VarType = SearchRec.Right.VarType then
      if RemoveSubRange(SearchRec.Left.UserType) <> RemoveSubRange(SearchRec.Right.UserType) then
        EXIT(False);

  SearchRec.ReturnLUserType := nil;
  SearchRec.ReturnRUserType := nil;
  SearchRec.ReturnResultUserType := nil;

  BestFitness := MaxInt;
  BestIndex := -1;
  SwapParams := False;

  //First entry for this operator
  Index := Operations[SearchRec.Op].FirstPrimIndex;
  Assert(Index >= 0, 'No Primitives declared for operation '''+OpStrings[SearchRec.Op]+'''');

  SearchRec.SwapParams := False;
  while (Index < PrimList.Count) and (PrimList[Index].Op = SearchRec.Op)
    and (BestFitness <> 0) do
  begin
    Prim := PrimList[Index];

    SearchRec.ResultType := Prim.ResultType;

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
    SearchRec.ReturnResultUserType := SearchRec.Left.UserType
  else if Prim.ResultTypeIsRType then
    SearchRec.ReturnResultUserType := SearchRec.Right.UserType
  else if (SearchRec.MatchResultType = vtUnknown) and (Prim.ResultType <> vtUnknown) then
  //If we matched a Result Type then nowt to do, otherwise we need to set the result
  //type to that of the prim
  begin
    SearchRec.ReturnResultUserType := GetSystemType(Prim.ResultType);
    Assert(Assigned(SearchRec.ReturnResultUserType));  //Unsuitable Prim.ResultType.
          //For non-system types you probably want to specify a ResultType of
          //LType or RType in Primitives.csv
  end;

  //Numeric types can be expanded to match available primitives if required
  if IsNumericVarType(Prim.LType) then
    SearchRec.ReturnLUserType := GetSystemType(Prim.LType);
  if IsNumericVarType(Prim.RType) then
    SearchRec.ReturnRUserType := GetSystemType(Prim.RType);

  SearchRec.SwapParams := SwapParams;
end;

//=== Set up the search

//Set parameter search data for parse time search
procedure ParseSetParam(const Slug: TExprSlug;Search: PSearchRecParam);
begin
  Search.AddrMode := amStack;

  Search.UserType := Slug.ResultType;
  Search.VarType := Slug.ResultVarType;
  if Slug.ILItem <> nil then
    Search.Kind := pkVarSource
  else
    Search.Kind := Slug.Operand.Kind;

  Search.IsRange := (Search.Kind = pkImmediate) and IsNumericVarType(Search.VarType);
  if Search.IsRange then
    Search.Range := IntToNumberRange(Slug.Operand.Imm.IntValue);
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

  ParseSetParam(Left, @SearchRec.Left);
  ParseSetParam(Right, @SearchRec.Right);
  SearchRec.MatchResultType := UTToVT(ResultType);

  if Operations[Op].SignCombine then
  begin
    Assert(not (SearchRec.Left.IsRange and SearchRec.Right.IsRange),'Can''t have both parameters ' +
      'as immediates - use compile time evaluation for that!');

    if SearchRec.Left.IsRange then
      SearchRec.IsSigned := (SearchRec.Left.Range in SignedRanges) or IsSignedVarType(SearchRec.Right.VarType)
    else if SearchRec.Right.IsRange then
      SearchRec.IsSigned := IsSignedVarType(SearchRec.Left.VarType) or (SearchRec.Right.Range in SignedRanges)
    else
      SearchRec.IsSigned := IsSignedVarType(SearchRec.Left.VarType) or IsSignedVarType(SearchRec.Right.VarType);
  end
  else
    SearchRec.IsSigned := False;

  SearchRec.IsPointer :=
    (not SearchRec.Left.IsRange and (SearchRec.Left.VarType in [vtPointer, vtTypedPointer])) or
    (not SearchRec.Right.IsRange and (SearchRec.Right.VarType in [vtPointer, vtTypedPointer]));

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

    if (ResultType.VarType in [vtReal]) or
      (SearchRec.Left.VarType in [vtReal]) or
      (SearchRec.Right.VarType in [vtReal]) then
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
  Assert(Op <> opStoreImm);
  SearchRec.GenTime := False;

  ParseSetParam(Left, @SearchRec.Left);

  //Fields not used for parse-time search. This stops the compiler warnings.
  SearchRec.Right.Kind := pkVarSource;
  SearchRec.Right.AddrMode := amStack;
  SearchRec.ILItem := nil;

  SearchRec.MatchResultType := UTToVT(ResultType);
  SearchRec.Right.UserType := nil;
  SearchRec.Right.VarType := vtUnknown;
  SearchRec.Right.IsRange := False;

  if SearchRec.Left.IsRange then
    SearchRec.IsSigned := SearchRec.Left.Range in SignedRanges
  else
    SearchRec.IsSigned := IsSignedVarType(SearchRec.Left.VarType);
  SearchRec.IsPointer := not SearchRec.Left.IsRange and (SearchRec.Left.VarType in [vtPointer, vtTypedPointer]);

  Result := PrimSearch(SearchRec);

  if SearchRec.ReturnLUserType <> nil then
    LeftType := SearchRec.ReturnLUserType;

  if SearchRec.ReturnResultUserType <> nil then
  begin
    ResultType := SearchRec.ReturnResultUserType;
    if SearchRec.IsPointer then
      if ResultType.VarType = vtWord then
        ResultType := GetSystemType(vtPointer);

    if (ResultType.VarType in [vtReal]) or (SearchRec.Left.VarType in [vtReal]) then
      Assert(False);  //TODO

    if ResultType.VarType = vtFlag then
      ResultType := GetSystemType(vtBoolean);
  end;
end;

//Set parameter search data for codegen time search
procedure CodeGenSetParam(ILParam: PILParam;Search: PSearchRecParam);
var V: PVariable;
begin
  //Deduce the initial types to search for
  Search.UserType := ILParam.GetUserType;
  Search.VarType := ILParam.GetVarType;
  Search.IsRange := False;
  Search.Kind := ILParam.Kind;
  if Search.Kind in [pkVarSource, pkVarAddr] then
  begin
    V := ILParam.ToVariable;
    Search.AddrMode := V.AddrMode;
    if V.VarType = vtTypedPointer then
      if V.AddrMode = amStaticRef then
        Search.AddrMode := amStaticRef;
  end;
end;

function PrimFindCodeGen(const ILItem: TILItem;out SwapParams: Boolean): PPrimitive;
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

  CodeGenSetParam(@ILItem.Param1, @SearchRec.Left);
  CodeGenSetParam(@ILItem.Param2, @SearchRec.Right);

  //Combined
  SearchRec.IsSigned := Operations[ILItem.Op].SignCombine and
    (IsSignedVarType(SearchRec.Left.VarType) or IsSignedVarType(SearchRec.Right.VarType));
  SearchRec.IsPointer := Operations[ILItem.Op].SignCombine and
    ((SearchRec.Left.VarType in [vtPointer, vtTypedPointer]) or
    (SearchRec.Right.VarType in [vtPointer, vtTypedPointer]));
  //Do we need a boolean result in a flag (for a jump) or register (for assigning)?
  if ILItem.ResultVarType in [vtBoolean, vtFlag] then
    SearchRec.MatchResultType := ILItem.ResultVarType
  else
    SearchRec.MatchResultType := vtUnknown;

  //Result
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

//=================INITIALISATION

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
  else if CompareText(S, 'staticptr') = 0 then
    Loc := plStaticPtrVar
  else if CompareText(S, 'stackptr') = 0 then
    Loc := plStackPtrVar
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

function StringToChecking(const S: String): TPrimValidation;
begin
  if Length(S) <> 1 then
    raise Exception.Create('Error in Primitives Overflow or Range Checking field: ''' + S + '''');

  case S.Chars[0] of
    'y','Y': Result := pvYes;
    'n','N': Result := pvNo;
    'x','X': Result := pvEither;
  else
    raise Exception.Create('Error in Primitives Overflow or Range Checking field: ''' + S + '''');
  end;
end;

const
  //Primitive selection data
  fName           = 1;
  fLType          = 2;
  fRType          = 3;
  fCommutative    = 4;
  fResultType     = 5;
  fIfOverflowChecking = 6;
  fIfBoundsChecking = 7;

  //Primitive data (hopefully to evenetually be migrated into the fragment and
  //library files
  fFlags          = 8;
  fProcName       = 9;

  //Empty columns to give half-decent presentation in Excel
  fLRegs          = 12; //Being deprecated
  fRRegs          = 13; //Being deprecated
  fResultRegs     = 14; //Being deprecated
  fCorrupts       = 15; //Being deprecated

  fOverflowCheckProc  = 16;
  fRangeCheckToS8   = 17;
  fRangeCheckToU8   = 18;
  fRangeCheckToS16  = 19;
  fRangeCheckToU16  = 20;

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
          raise Exception.Create('Operation not found for primitive in ' + Line +
            #10#13'Operations need to be defined in both Primitives.csv and Operations.csv');
        if Operations[Prim.Op].FirstPrimIndex < 0 then
          Operations[Prim.Op].FirstPrimIndex := PrimList.Count-1;

        Prim.ProcName := Fields[fProcName];
        Prim.Fragment := FindFragmentByName(Prim.ProcName);

        if CompareText(Fields[fLType], 'none') = 0 then
          Prim.LType := vtUnknown
        else
        begin
          Prim.LType := StringToType(Fields[fLType], Prim.LArrayDef);
          if Prim.LType = vtUnknown then
            raise Exception.Create('Unknown LType: ' + Fields[fLType]);
        end;

        if CompareText(Fields[fRType], 'none') = 0 then
          Prim.RType := vtUnknown
        else
        begin
          Prim.RType := StringToType(Fields[fRType], Prim.RArrayDef);
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
          Prim.ResultType := StringToType(Fields[fResultType], Prim.ResultArrayDef);
          if Prim.ResultType = vtUnknown then
            raise Exception.Create('Unknown ResultType: ' + Fields[fResultType]);
        end;
        //--END

        Prim.IfOverflowChecking := StringToChecking(Fields[fIfOverflowChecking]);
        Prim.IfBoundsChecking := StringToChecking(Fields[fIfBoundsChecking]);

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

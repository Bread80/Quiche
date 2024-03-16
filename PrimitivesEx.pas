unit PrimitivesEx;

interface
uses QTypes, ILData, Operators, ParseExpr;

type
  TPrimValidation = (pvYes, pvNo, pvEither);

  TPrimFlag = (
    pfLoadRPLow,  //Load only the high byte of the (16-bit) parameter
    pfLoadRPHigh, //Load only high byte of the (16-bit) parameter
    pfP1StaticVar,  //Load Param1 from static var
    pfP1RelVar,     //Load Param2 from relative var
    pfDestStaticVar,  //Store to a static variable
    pfDestRelVar      //Store to an IX relative variable
    );
  TPrimFlagSet = set of TPrimFlag;

  PPrimitive = ^TPrimitive;
  TCodeGenProc = procedure(ILItem: PILItem);
  TValidationProc = procedure(RH, RL: Char);
  TPrimitive= record
    Op: TOperator;         //The operation this routine handles
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
    Flags: TPrimFlagSet;      //Assorted flags
    Param1Regs: TCPURegSet;    //Valid sources for the first parameter
    Param2Regs: TCPURegSet;    //Valid sources for the second parameter
    DestReg: TCPUReg;         //Where the destination (Output data will be found)
    Corrupts: TCPURegSet;     //Which registers are corrupted. Includes any output (dest) register(s)

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

  TPrimFlagNG = (
    pfnLoadRPLow,  //Load only the high byte of the (16-bit) parameter
    pfnLoadRPHigh //Load only high byte of the (16-bit) parameter
{    pfP1StaticVar,  //Load Param1 from static var
    pfP1RelVar,     //Load Param2 from relative var
    pfDestStaticVar,  //Store to a static variable
    pfDestRelVar      //Store to an IX relative variable
}    );
  TPrimFlagSetNG = set of TPrimFlagNG;
  PPrimitiveNG = ^TPrimitiveNG;
  TPrimitiveNG = record
    //Fields to use for primitive selection
    Op: TOperator;
    ProcName: String;     //Name of the Proc (code), Fragment, or Subroutine used
                          //during code generation
    LType: TVarType;      //Left operand type (base type)
    RType: TVarType;      //Right operand type (base type)
    Commutative: Boolean; //If True the left and right operators can be swapped
    ResultType: TVarType; //Destination (Result) type, unless...
    ResultTypeIsLType: Boolean;
    ResultTypeIsRType: Boolean;
    Validation: TPrimValidation;  //Can this routine be used when validation is on? off? either?

    //Fields for primitive use
    Flags: TPrimFlagSetNG;//Flags
    LRegs: TCPURegSet;    //What registers can the left parameter accept?
    RRegs: TCPURegSet;    //What registers can the right operator accept?
    ResultInLReg: Boolean;  //If True the result is returned in the same register
                          //as used for LReg. If False, see DRegs
    ResultReg: TCPUReg;   //What register is the result returned in? (if ResultInLReg is False)
    Corrupts: TCPURegSet; //What registers are corrupted by this routine
                          //(Should this include Result register?)
    ValidateProcName: String; //Used when we need to validate the result value
                          //If empty a generic validation routine will be used
                          //Set this value if there is a alternate routine available.
                          //Examples:
                          //A generic routine for signed values might check the
                          //overflow flag. But if a subroutine returns errors in
                          //the carry flag a routine to test the carry flag will
                          //need to be specified here
    ValidateToS8: String; //As with ValidateProc, generic routines are used to
                          //validate type conversions. If a more specific or
                          //optimised routine is available it can be specified
                          //here. For example a generic routine to convert signed
                          //to unsigned will need to do an operation to test the
                          //sign flag. If the primitive already sets that flag
                          //an optimised routine can skip the test operation.
    ValidateToU8: String;
    ValidateToS16: String;
    ValidateToU16: String;

    //Possibly add data regiarding code bytesize and execution cycles.

    Proc: TCodeGenProc;   //Proc to generate code (if generated by code)
    ValidateProc: TValidationProc;  //Proc to generate validation code (if generated in code)
  end;

function ILItemToPrimitive(const ILItem: TILItem): PPrimitive;
function ILItemToPrimitiveNG(const ILItem: TILItem;out SwapParams: Boolean): PPrimitiveNG;

function PrimFindByProcName(AName: String): PPrimitive;

//To be used by the parser. NOT to be used by the code generator
//Searches the available list of primitives for the Operator to find a suitable
//match based on the supplied operand types.
//If a routine is found which consumes LType and RType -> selects it.
//If not not the types will be 'expanded' (if that is an option for the given
//types) until a suitably matching routine is found.
//Returns the result type that the selected routine will return.
//If no suitable routine was found returns vtUnknown
//If the parameter type were expanded then LType and RType return the expanded type(s)
function PrimFindParse(Op: TOperator;const Left, Right: TExprSlug;
  out LeftType, RightType: TVarType;var ResultType: TVarType): Boolean;
function PrimFindParseUnary(Op: TOperator;const Left: TExprSlug;
  out LeftType: TVarType;var ResultType: TVarType): Boolean;

procedure PrimSetProc(Name: String;Proc: TCodeGenProc);
procedure PrimSetValProc(Name: String;Proc: TValidationProc);

procedure InitialisePrimitives;

procedure LoadPrimitivesFile(const Filename: String);
procedure LoadPrimitivesNGFile(const Filename: String);

//Validates that generators are available for every primitive.
//Raises an exception if there is a problem
procedure ValidatePrimitives;

implementation
uses Math, Generics.Collections, Classes, SysUtils, Variables, Fragments;

var PrimList: TList<PPrimitive>;
  PrimListNG: TList<PPrimitiveNG>;

procedure ClearPrimList;
var Prim: PPrimitive;
  PrimNG: PPrimitiveNG;
begin
  for Prim in PrimList do
    Dispose(Prim);
  PrimList.Clear;

  for PrimNG in PrimListNG do
    Dispose(PrimNG);
  PrimListNG.Clear;
end;

procedure InitialisePrimitives;
begin
  ClearPrimList;
end;


//==================NG Primitive matching & type matching


//Range types are used within the parser to assess how to store, expand and find
//primitives for numeric types
//NOTE: The compiler could be adapted for targets with different bitness by modifying
//this table and/or the constants which go with it (NumberRangeBounds)
//However, adaptations will almost certainly be required elsewhere.
type TNumberRange = (
    rgReal,   //..-32769:     Real                      n/a                      Real
    rgS16,    //-32768..-129: Unsigned 16               Real                     S16
    rgS8,     //-128..-1:     Signed 8 or Signed 16     Signed 16                S8
    rgAny,    //0..127:       Signed 8 or unsigned 8    Signed 16 or Unsigned 16 Any
    rgS16U8,  //128..255:     Signed 16 or unsigned 8   Signed 16 or Unsigned 16 S16U8
    rgS16U16, //256..32767:   Signed 16 or Unsigned 16  Real or Unsigned 16      S16U16
    rgU16     //32768..65535: Unsigned 16               Real                     U16
//  (Real)      65536..:      Real                      n/a                      Real
    );
const SignedRanges = [rgReal, rgS16, rgS8];

const NumberRangeBounds: array[low(TNumberRange)..high(TNumberRange)] of Integer = (
      -32769, -129,      -1,     127,    255,       32767,     65535);
    NumberRangeToSignedType: array[low(TNumberRange)..high(TNumberRange)] of TVarType = (
      vtReal, vtInteger, vtInt8, vtInt8, vtInteger, vtInteger, vtReal);
    NumberRangeToUnSignedType: array[low(TNumberRange)..high(TNumberRange)] of TVarType = (
      vtReal, vtReal,    vtReal, vtByte, vtByte,    vtWord,    vtWord);

function IntToNumberRange(Value: Integer): TNumberRange;
begin
  for Result := low(TNumberRange) to high(TNumberRange) do
    if Value <= NumberRangeBounds[Result] then
      EXIT;
  Result := high(TNumberRange);
end;

function RangeToType(Op: TOperator;AType: TVarType;ARange: TNumberRange): TVarType;
begin
  if Operations[Op].SignCombine then
  begin
    if IsSignedType(AType) or (ARange in [rgS16, rgS8]) then
      Result := NumberRangeToSignedType[ARange]
    else
      Result := NumberRangeToUnsignedType[ARange];
  end
  else if ARange < rgAny then //Negative constant
    Result := NumberRangeToSignedType[ARange]
  else
    Result := NumberRangeToUnsignedType[ARange];
end;



function ParamRegMatch(Prim: PPrimitiveNG;AvailableRegs: TCPURegSet;
  Kind: TILParamKind;Storage: TVarStorage): Boolean;
var V: PVariable;
begin
  if Kind = pkNone then
    EXIT(True);

  if AvailableRegs = [rImm] then
    EXIT(Kind = pkImmediate);

  if AvailableRegs = [rIndirect] then
    EXIT(Storage = vsStatic);

  if AvailableRegs = [rOffset] then
    EXIT(Storage = vsStack);

  //If we can't find a suitable routine which accepts an Imm value then we'll
  //use one which doesn't and manually load it into a register.
  Result := (AvailableRegs * [rA..rE,rH..rL, rHL..rBC]) <> [];
end;

function ValidationMatchNG(const Prim: PPrimitiveNG;const ILItem: PILItem): Boolean;
begin
  case Prim.Validation of
    pvYes: EXIT(cgOverflowCheck in ILItem.CodeGenFlags) ;
    pvNo: EXIT(not (cgOverFlowCheck in ILItem.CodeGenFlags));
    pvEither: EXIT(True); //Always okay
  else
    Assert(False, 'Unknown primitive validation option');
  end;
end;

function DestRegMatch(const Prim: PPrimitiveNG;const ILDest: TILDest): Boolean;
var V: PVariable;
begin
  case ILDest.Kind of
    pkNone: EXIT(True);
    pkStack:
      //TODO: Update this to have more regs available
      if Prim.ResultInLReg then
        Result := [rHL,rDE,rBC] * Prim.LRegs <> []
      else
        Result := Prim.ResultReg in [rHL, rDE, rBC];
    pkStackByte:
      //TODO: Update this to have more regs available
      if Prim.ResultInLReg then
        Result := [rA,rB,rD,rH, rZFA,rNZFA,rCPLA] * Prim.LRegs <> []
      else
        Result := Prim.ResultReg in [rA,rB,rD,rH, rZFA,rNZFA,rCPLA];
    pkVar:
    begin //We need a result suitable for writing to a variable
//      if ILDest.Kind in [pkVar] then
      begin
        if ((Prim.ResultInLReg) and (Prim.LRegs = [rIndirect])) or (Prim.ResultReg = rIndirect) then
        begin
          V := ILDest.ToVariable;
          EXIT(V.Storage = vsStatic);
        end;

        if ((Prim.ResultInLReg) and (Prim.LRegs = [rOffset])) or (Prim.ResultReg = rOffset) then
        begin
          V := ILDest.ToVariable;
          EXIT(V.Storage = vsStack);
        end;

        //If it's anything else then we can convert it as needed
        Result := True;
      end
    end;
  else
    Assert(False, 'ToDo (Branches?)');
  end;
end;

function DestMatch(Prim: PPrimitiveNG;ILItem: PILItem): Boolean;
begin
  case ILItem.DestType of
    dtCondBranch: Result := Prim.ResultType in [vtBoolean, vtFlag];
    dtData: Result := DestRegMatch(Prim, ILItem.Dest);
    dtNone: Result := True;
  else
    Assert(False);
  end;
end;

//Fitness values for (<primitive-type>,<code-type>)
const FitnessVarTypeVarType: array[vtWord..vtReal,vtWord..vtReal] of Integer =
//From. This axis is the parameter type. Other is primitive argument type
//    To	Word  Byt Ptr I8	Int	    Real
{Word}	  ((0,  2,  1,  10,	20,	-1, 20),		//Otherwise:
{Byte}	  (20,  0,  20, 20,	30,	-1, 40),		//	If both parameters are unsigned set Byte to Integer to 3
{Pointer}	(1,   2,  0,  10,	20,	-1, 20),		//	Use the same values as 'unsigned'
{Int8}	  (30,  10, 30, 0,	10,	-1, 30),		//If SignCombine is set for an operator:
{Integer}	(10,  3,  10, 1,	0,	-1, 10),		//	If either parameter is signed, set Byte to Integer to 1
{Unused}  (-1,  -1, -1, -1, -1, -1, -1),
{Real}	  (4,   4,  4,  4,	4,	-1, 0));    //**This is handled in code in GetFitnessTypeType

//NOTE: FINAL VALUES TODO
const FitnessNumberRangeVarTypeUnsigned:
  array[vtWord..vtReal,low(TNumberRange)..high(TNumberRange)] of Integer =
//	From. This axis is the parameter type. Other is primitive argument type
//To	NR_Real	S16	S8	Any	S16U8	S16U16	U16
{Word}	  ((10,	10,	20,	1,	1,	0,	0),
{Byte}	  (30,	20,	10,	0,	0,	10,	20),
{Pointer}	(10,	10,	20,	1,	1,	0,	0),
{Int8}	  (40,	30,	0,	0,	10,	20,	30),
{Integer}	(20,	0,	1,	2,	2,	1,	10),
{Unused}  (-1,  -1, -1, -1, -1, -1, -1),
{Real}	  (0,	  4,	4,	4,	4,	4,	4));

//NOTE: FINAL VALUES TODO
const FitnessNumberRangeVarTypeSigned:
  array[vtWord..vtReal,low(TNumberRange)..high(TNumberRange)] of Integer =
//	From. This axis is the parameter type. Other is primitive argument type
//To	NR_Real	S16	S8	Any	S16U8	S16U16	U16
{Word}	  ((10,	10,	20,	2,	2,	1,	0),
{Byte}	  (30,	20,	10,	0,	0,	10,	20),
{Pointer}	(10,	10,	20,	2,	2,	1,	0),
{Int8}	  (40,	30,	0,	0,	10,	20,	30),
{Integer}	(20,	0,	1,	1,	1,	0,	10),
{Unused}  (-1,  -1, -1, -1, -1, -1, -1),
{Real}	  (0,	  4,	4,	4,	4,	4,	4));

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
function GetFitnessTypeType(CodeType, PrimType: TVarType;Signed: Boolean): Integer;
begin
  if IsNumericType(CodeType) then
  begin
    if not IsNumericType(PrimType) then
      Result := -1
    else
      if Signed and (CodeType = vtByte) and (PrimType = vtInteger) then
        Result := 1
      else
        Result := FitnessVarTypeVarType[PrimType, CodeType];
  end
  else
    if CodeType = PrimType then
      Result := 0
    else
      Result := -1;
end;

//As GetFitnessTypeType but where the parameter is a constant AND a numerical type
function GetFitnessTypeRange(CodeRange: TNumberRange; PrimType: TVarType;Signed: Boolean): Integer;
begin
  //Ranges only apply to numeric types
  if not IsNumericType(PrimType) then
    EXIT(-1);

  if Signed then
    Result := FitnessNumberRangeVarTypeSigned[PrimType, CodeRange]
  else
    Result := FitnessNumberRangeVarTypeUnsigned[PrimType, CodeRange];
end;

type TPrimSearchRec = record
    Op: TOperator;
    GenTime: Boolean; //Are we code generating? If so does deeper matching.
                      //Parse time only needs to know a routine is available.
                      //CodeGen time needs to select the exact routine
    ILItem: PILItem;  //Only required at code gen time

    IsSigned: Boolean;  //Use signed fitness values
    IsPointer: Boolean; //Use pointer fitness values (i.e. unsigned)

    LType: TVarType;
    LIsRange: Boolean;
    LRange: TNumberRange;
    LKind: TILParamKind;
    LStorage: TVarStorage;

    RType: TVarType;
    RIsRange: Boolean;
    RRange: TNumberRange;
    RKind: TILParamKind;
    RStorage: TVarStorage;

    MatchResultType: TVarType;  //Match if <> vtUnknown
    ResultType: TVarType;

    PrimIndex: Integer;
    SwapParams: Boolean;
  end;

function CalcTypeFitness(const SearchRec: TPrimSearchRec;Prim: PPrimitiveNG;Swap: Boolean): Integer;
var
  Fitness: Integer;
  Signed: Boolean;
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
    Fitness := GetFitnessTypeType(SearchRec.MatchResultType, Prim.ResultType, SearchRec.IsSigned);
    if Fitness < 0 then
      EXIT(-1);
    Result := Result + Fitness;
  end;
end;

function CalcFitness(const SearchRec: TPrimSearchRec;Prim: PPrimitiveNG;Swap: Boolean): Integer;
begin
  Result := CalcTypeFitness(SearchRec, Prim, Swap);
  if Result = -1 then
    EXIT;
  //We want a pointer result so bias the result against signed result types
  if SearchRec.IsPointer then
  begin
    if IsSignedType(Prim.ResultType) then
      Result := Result + 10;
  end
  else if SearchRec.IsSigned then
    if not IsSignedType(Prim.ResultType) then
      Result := Result + 10;

  //Validate any other param compatibility issues
  if SearchRec.GenTime then
  begin
    if Swap then
    begin
      if not ParamRegMatch(Prim, Prim.LRegs, SearchRec.RKind, SearchRec.RStorage) then
        EXIT(-1);
      if (Prim.RType <> vtUnknown) and
        not ParamRegMatch(Prim, Prim.RRegs, SearchRec.LKind, SearchRec.LStorage) then
        EXIT(-1);
    end
    else
    begin
      if not ParamRegMatch(Prim, Prim.LRegs, SearchRec.LKind, SearchRec.LStorage) then
        EXIT(-1);
      if (Prim.RType <> vtUnknown) and
        not ParamRegMatch(Prim, Prim.RRegs, SearchRec.RKind, SearchRec.RStorage) then
        EXIT(-1);
    end;

    if not ValidationMatchNG(Prim, SearchRec.ILItem) then
      EXIT(-1);
    if not DestMatch(Prim, SearchRec.ILItem) then
      EXIT(-1);
  end;
end;

function PrimSearch(var SearchRec: TPrimSearchRec): Boolean;
var
  Fitness: Integer;
  Index: Integer;
  BestFitness: Integer;
  BestIndex: Integer;
  SwapParams: Boolean;
  Prim: PPrimitiveNG;
begin
  BestFitness := MaxInt;
  BestIndex := -1;
  SwapParams := False;

  //First entry for this operator
  Index := Operations[SearchRec.Op].FirstPrimIndex;

  SearchRec.SwapParams := False;
  while (Index < PrimListNG.Count) and (PrimListNG[Index].Op = SearchRec.Op)
    and (BestFitness <> 0) do
  begin
    Prim := PrimListNG[Index];

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
  Prim := PrimListNG[SearchRec.PrimIndex];
  if Prim.ResultTypeIsLType then
    SearchRec.ResultType := SearchRec.LType
  else if Prim.ResultTypeIsRType then
    SearchRec.ResultType := SearchRec.RType
  else
    SearchRec.ResultType := Prim.ResultType;
  SearchRec.LType := Prim.LType;
  SearchRec.RType := Prim.RType;

  SearchRec.SwapParams := SwapParams;
end;

function PrimFindParse(Op: TOperator;const Left, Right: TExprSlug;
  out LeftType, RightType: TVarType;var ResultType: TVarType): Boolean;
var SearchRec: TPrimSearchRec;
begin
//  Assert(ResultType = vtUnknown, 'Can''t search by ResultType yet');
  SearchRec.Op := Op;
  SearchRec.GenTime := False;
  //Fields not used for parse-time searches.
  SearchRec.ILItem := nil;
  SearchRec.LStorage := vsStack;  //Don't care at parse time
  SearchRec.RStorage := vsStack;  //Don't care at parse time

  SearchRec.MatchResultType := ResultType;

  SearchRec.LType := Left.ResultType;
  if Left.ILItem <> nil then
    SearchRec.LKind := pkVar
  else
    SearchRec.LKind := Left.Operand.Kind;

  SearchRec.RType := Right.ResultType;
  if Right.ILItem <> nil then
    SearchRec.RKind := pkVar
  else
    SearchRec.RKind := Right.Operand.Kind;

  SearchRec.LIsRange := (SearchRec.LKind = pkImmediate) and IsNumericType(SearchRec.LType);
  if SearchRec.LIsRange then
    SearchRec.LRange := IntToNumberRange(Left.Operand.immValueInt);

  SearchRec.RIsRange := (SearchRec.RKind = pkImmediate) and IsNumericType(SearchRec.RType);
  if SearchRec.RIsRange then
    SearchRec.RRange := IntToNumberRange(Right.Operand.immValueInt);

  if Operations[Op].SignCombine then
  begin
    Assert(not (SearchRec.LIsRange and SearchRec.RIsRange),'Can''t have both parameters ' +
      'as immediates - use compile time evaluation for that!');

    if SearchRec.LIsRange then
      SearchRec.IsSigned := (SearchRec.LRange in SignedRanges) or IsSignedType(SearchRec.RType)
    else if SearchRec.RIsRange then
      SearchRec.IsSigned := IsSignedType(SearchRec.LType) or (SearchRec.RRange in SignedRanges)
    else
      SearchRec.IsSigned := IsSignedType(SearchRec.LType) or IsSignedType(SearchRec.RType);
  end
  else
    SearchRec.IsSigned := False;

  SearchRec.IsPointer := (not SearchRec.LIsRange and (SearchRec.LType = vtPointer)) or
    (not SearchRec.RIsRange and (SearchRec.RType = vtPointer));

  {SEARCH}
  Result := PrimSearch(SearchRec);

  if SearchRec.SwapParams then
  begin
    LeftType := SearchRec.RType;
    RightType := SearchRec.LType;
  end
  else
  begin
    LeftType := SearchRec.LType;
    RightType := SearchRec.RType;
  end;
  if SearchRec.IsPointer then
    if SearchRec.ResultType = vtWord then
      SearchRec.ResultType := vtPointer;

  if (SearchRec.ResultType in [vtReal, vtString]) or (SearchRec.LType in [vtReal, vtString]) or
    (SearchRec.RType in [vtReal, vtString]) then
    Assert(False);

  if SearchRec.ResultType = vtFlag then
    ResultType := vtBoolean
  else
    ResultType := SearchRec.ResultType;
end;

function PrimFindParseUnary(Op: TOperator;const Left: TExprSlug;
  out LeftType: TVarType;var ResultType: TVarType): Boolean;
var SearchRec: TPrimSearchRec;
begin
  SearchRec.Op := Op;
  SearchRec.GenTime := False;
  //Fields not used for parse-time search. This stops the compiler warnings.
  SearchRec.LKind := pkVar;
  SearchRec.RKind := pkVar;
  SearchRec.LStorage := vsStack;
  SearchRec.RStorage := vsStack;
  SearchRec.ILItem := nil;

  SearchRec.MatchResultType := ResultType;

  SearchRec.LType := Left.ResultType;
  if Left.ILItem <> nil then
    SearchRec.LKind := pkVar
  else
    SearchRec.LKind := Left.Operand.Kind;  SearchRec.RType := vtUnknown;

  SearchRec.LIsRange := (SearchRec.LKind = pkImmediate) and IsNumericType(SearchRec.LType);
  if SearchRec.LIsRange then
    SearchRec.LRange := IntToNumberRange(Left.Operand.immValueInt);
  SearchRec.RIsRange := False;

  if SearchRec.LIsRange then
    SearchRec.IsSigned := SearchRec.LRange in SignedRanges
  else
    SearchRec.IsSigned := IsSignedType(SearchRec.LType);
  SearchRec.IsPointer := not SearchRec.LIsRange and (SearchRec.LType = vtPointer);

  Result := PrimSearch(SearchRec);

  LeftType := SearchRec.LType;
  if (SearchRec.ResultType in [vtReal, vtString]) or (SearchRec.LType in [vtReal, vtString]) or
    (SearchRec.RType in [vtReal, vtString]) then
    Result := False;

  if SearchRec.IsPointer and (SearchRec.ResultType = vtWord) then
    ResultType := vtPointer
  else if SearchRec.ResultType = vtFlag then
    ResultType := vtBoolean
  else
    ResultType := SearchRec.ResultType;
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
function ILItemToPrimitiveNG(const ILItem: TILItem;out SwapParams: Boolean): PPrimitiveNG;
var
  Found: Boolean;
  Prim: PPrimitiveNG;
  SearchRec: TPrimSearchRec;
  V: PVariable;
begin
  SearchRec.Op := ILItem.Op;
  SearchRec.PrimIndex := -1;
  SearchRec.GenTime := True;
  SearchRec.ILItem := @ILItem;
  //Deduce the initial types to search for
  SearchRec.LType := ILItem.Param1.GetVarType;
  SearchRec.LIsRange := False;
  SearchRec.RType := ILItem.Param2.GetVarType;
  SearchRec.RIsRange := False;
  SearchRec.IsSigned := Operations[ILItem.Op].SignCombine and (IsSignedType(SearchRec.LType) or IsSignedType(SearchRec.RType));
  SearchRec.IsPointer := Operations[ILItem.Op].SignCombine and ((SearchRec.LType = vtPointer) or (SearchRec.RType = vtPointer));
  SearchRec.MatchResultType := vtUnknown;

  SearchRec.LKind := ILItem.Param1.Kind;
  if SearchRec.LKind = pkVar then
  begin
    V := ILItem.Param1.ToVariable;
    SearchRec.LStorage := V.Storage;
  end;

  SearchRec.RKind := ILItem.Param2.Kind;
  if SearchRec.RKind = pkVar then
  begin
    V := ILItem.Param2.ToVariable;
    SearchRec.RStorage := V.Storage;
  end;

  if PrimSearch(SearchRec) then
  begin
    Result := PrimListNG[SearchRec.PrimIndex];
    SwapParams := SearchRec.SwapParams;
  end
  else
    Result := nil;
end;


//================================OG Prim searching and matching (and other stuff)

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

function ParamLocMatch(const Prim: PPrimitive;AvailableRegs: TCPURegSet;const ILParam: TILParam): Boolean;
var V: PVariable;
begin
  case ILParam.Kind of
    pkNone: EXIT(AvailableRegs = []);
    pkImmediate: EXIT(True);//ProcLoc = [plImm]);
    pkVar: //Special cases which can handle variable types directly
    begin
      V := ILParam.ToVariable;
      if pfP1StaticVar in Prim.Flags then
        EXIT(V.Storage = vsStatic);
      if pfP1RelVar in Prim.Flags then
        EXIT(V.Storage = vsStack);
    end;
  else
      //Nothing
  end;

  //If we can't find a suitable routine which accepts an Imm value then we'll
  //use one which doesn't and manually load it into a register.
  Result := (AvailableRegs * [rA..rE,rH..rL, rHL..rBC]) <> [];
end;

function DestLocMatch(const Prim: PPrimitive; const ILItem: TILItem): Boolean;
var V: PVariable;
begin
//  Result := (Prim.IsBranch and (ILItem.DestType = dtCondBranch)) or not Prim.IsBranch then
  if Prim.IsBranch then
    EXIT(ILItem.DestType = dtCondBranch);

  if ILItem.DestType = dtData then
  begin
    case ILItem.Dest.Kind of
      pkVar:
      begin
        //For an assign to a variable (Anything other than Assign is handled after the Primitive)
        V := ILItem.Dest.ToVariable;
        if pfDestStaticVar in Prim.Flags then
          EXIT(V.Storage = vsStatic);
        if pfDestRelVar in Prim.Flags then
          EXIT(V.Storage = vsStack);

        EXIT(True);
      end;
      pkStack, pkStackByte: EXIT(True);
    end;
  end
  else  //DestType <> dtData - return true because value can be massaged for branches
    EXIT(True);

  Result := False;
end;

function ILItemToPrimitive(const ILItem: TILItem): PPrimitive;
var Prim: PPrimitive;
begin
  for Prim in PrimList do
    if ILItem.Op = Prim.Op then
      if OpTypeMatch(Prim.OpType, ILItem.OpType) and OpTypeMatch(Prim.DestType, ILItem.ResultType) then
        if OpOrderMatch(Prim.FirstParamType, ILItem) then
          if ValidationMatch(Prim.Validation, ILItem.CodeGenFlags) then
            if ParamLocMatch(Prim, Prim.Param1Regs, ILItem.Param1) and
              ParamLocMatch(Prim, Prim.Param2Regs, ILItem.Param2) then
              if DestLocMatch(Prim, ILItem) then
                  EXIT(Prim);

  Result := nil;
end;

procedure PrimSetProc(Name: String;Proc: TCodeGenProc);
var Prim: PPrimitive;
  PrimNG: PPrimitiveNG;
  Found: Boolean;
begin
  Found := False;
  for Prim in PrimList do
    if CompareText(Prim.ProcName, Name) = 0 then
    begin
      Prim.Proc := Proc;
      Found := True;
    end;

  for PrimNG in PrimListNG do
    if CompareText(PrimNG.ProcName, Name) = 0 then
    begin
      PrimNG.Proc := Proc;
      Found := True;
    end;

  if not Found then
    raise Exception.Create('Primitive not found: ' + Name);
end;

procedure PrimSetValProc(Name: String;Proc: TValidationProc);
var Prim: PPrimitive;
  PrimNG: PPrimitiveNG;
  Found: Boolean;
begin
  Found := False;
  for Prim in PrimList do
    if CompareText(Prim.ProcName, Name) = 0 then
    begin
      Prim.ValProc := Proc;
      Found := True;
    end;

  for PrimNG in PrimListNG do
    if CompareText(PrimNG.ProcName, Name) = 0 then
    begin
      PrimNG.ValidateProc := Proc;
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
    else if CompareText(X, 'p1staticvar') = 0 then
      Result := Result + [pfP1StaticVar]
    else if CompareText(X, 'p1relvar') = 0 then
      Result := Result + [pfP1RelVar]
    else if CompareText(X, 'deststaticvar') = 0 then
      Result := Result + [pfDestStaticVar]
    else if CompareText(X, 'destrelvar') = 0 then
      Result := Result + [pfDestRelVar]    else
      raise Exception.Create('Unknown prim flag: ' + X);
end;

const lutCharToCPUReg: array['a'..'z'] of TCPUReg = (
  rA, rB, rC, rD, rE, rFlags, rNone,  //A..G
  rH, rImm, rNone, rNone, rL, rNone,  //L..M
  rNone, rNone, rNone, rNone, rNone, rNone, rNone, rNone, //N..U
  rNone, rNone, rNone, rNone, rNone);   //V..Z

function CharToCPUReg(C: Char;ForCorrupts: Boolean): TCPUReg;
begin
  if (C = 'f') and not ForCorrupts then
    raise Exception.Create('Invalid register: ' + C);

  if C in ['a'..'z'] then
    Result := lutCharToCPUReg[C]
  else
    raise Exception.Create('Invalid register: ' + C);
  if Result = rNone then
    raise Exception.Create('Invalid register: ' + C);
end;

function StrToCPUReg(S: String;ForCorrupts: Boolean): TCPUReg;
begin
  if Length(S) = 0 then
    Result := rNone
  else if Length(S) = 1 then
    Result := CharToCPUReg(S.Chars[0], ForCorrupts)
  else if CompareText(S, 'none') = 0 then
    Result := rNone
  else if CompareText(S, 'imm') = 0 then
    Result := rImm
  else if CompareText(S, 'p1') = 0 then
    Result := rP1
  else if (S = 'bc') or (S = 'BC') then
    Result := rBC
  else if (S = 'de') or (S = 'DE') then
    Result := rDE
  else if (S = 'hl') or (S = 'HL') then
    Result := rHL
  else if (S = 'ix') or (S = 'IX') then
    Result := rIX
  else if (S = 'iy') or (S = 'IY') then
    Result := rIY
  else if (S = 'zf') or (S = 'ZF') then
    Result := rZF
  else if (S = 'zfa') or (S = 'ZFA') then
    Result := rZFA
  else if (S = 'nzf') or (S = 'NZF') then
    Result := rNZF
  else if (S = 'nzfa') or (S = 'NZFA') then
    Result := rNZFA
  else if (S = 'cf') or (S = 'CF') then
    Result := rCF
  else if (S = 'ncf') or (S = 'NCF') then
    Result := rNCF
  else if (S = 'cpla') or (S = 'CPLA') then
    Result := rCPLA
  else
    raise Exception.Create('Invalid register: ' + S);
end;

function StrToCPURegSet(S: String;ForCorrupts: Boolean): TCPURegSet;
var X: String;
begin
  Result := [];
  for X in S.Split([';']) do
    Result := Result + [StrToCPUReg(X, ForCorrupts)];
end;

const
  fName           = 1;
  fOpType         = 2;
  fFirstParamType = 3;
  fDestType       = 4;
  fValidation     = 5;
  fProcName       = 6;
  fFlags          = 7;
  fParam1Regs     = 8;
  fParam2Regs     = 9;
  fDestReg        = 10;
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
  I: Integer;
begin
  Data := TStringList.Create;
  Data.LoadFromFile(Filename);

  for Line in Data do
    if (Length(Line) > 0) and (Line.Chars[0] <> ';') then
    begin
      Fields := Line.Split([',']);
      if Length(Fields) < 10 then
        raise Exception.Create('PrimitivesEx line too short: ' + Line);
      for I:=0 to Length(Fields)-1 do
        Fields[I] := Fields[I].Trim;
      Prim := New(PPrimitive);
      PrimList.Add(Prim);

      if (Fields[fName] = 'x') or (Fields[fName] = 'unknown') then
        Prim.Op := opUnknown
      else
      begin
        Prim.Op := IdentToOperator(Fields[fName]);
        if Prim.Op = opUnknown then
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

      Prim.Param1Regs := StrToCPURegSet(Fields[fParam1Regs], False);
      Prim.Param2Regs := StrToCPURegSet(Fields[fParam2Regs], False);
      Prim.DestReg :=   StrToCPUReg(Fields[fDestReg], False);
      Prim.Corrupts :=  StrToCPURegSet(Fields[fCorrupts], True);

      Prim.ValProcName := Fields[fValProcName];
      Prim.ValProcS8 := Fields[fValProcS8];
      Prim.ValProcU8 := Fields[fValProcU8];
      Prim.ValProcS16 := Fields[fValProcS16];
      Prim.ValProcU16 := Fields[fValProcU16];

      Prim.Proc := nil;
      Prim.ValProc := nil;
    end;
end;

function StrToPrimFlagSetNG(S: String): TPrimFlagSetNG;
var X: String;
begin
  Result := [];
  for X in S.Split([';']) do
    if CompareText(X, 'load_rp_low') = 0 then
      Result := Result + [pfnLoadRPLow]
    else if CompareText(X, 'load_rp_high') = 0 then
      Result := Result + [pfnLoadRPHigh]
{    else if CompareText(X, 'p1staticvar') = 0 then
      Result := Result + [pfP1StaticVar]
    else if CompareText(X, 'p1relvar') = 0 then
      Result := Result + [pfP1RelVar]
    else if CompareText(X, 'deststaticvar') = 0 then
      Result := Result + [pfDestStaticVar]
    else if CompareText(X, 'destrelvar') = 0 then
      Result := Result + [pfDestRelVar]
}    else
      raise Exception.Create('Unknown prim flag NG: ' + X);
end;

const
  //Primitive selection data
  fNGName           = 1;
  fNGLType          = 2;
  fNGRType          = 3;
  fNGCommutative    = 4;
  fNGResultType     = 5;
  fNGValidation     = 6;

  //Primitive data (hopefully to evenetually be migrated into the fragment and
  //library files
  fNGFlags          = 7;
  fNGProcName       = 8;
  //Empty columns to give half-decent presentation in Excel
  fNGLRegs          = 11;
  fNGRRegs          = 12;
  fNGResultReg      = 13;
  fNGCorrupts       = 14;
  fNGValidateProc   = 15;
  fNGValidateToS8   = 16;
  fNGValidateToU8   = 17;
  fNGValidateToS16  = 18;
  fNGValidateToU16  = 19;

procedure LoadPrimitivesNGFile(const Filename: String);
var Data: TStringList;
  Line: String;
  Fields: TArray<String>;
  Prim: PPrimitiveNG;
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

        Prim := New(PPrimitiveNG);
        PrimListNG.Add(Prim);

        //Primitive selection data
        Prim.Op := IdentToOperator(Fields[fNGName]);
        if Prim.Op = opUnknown then
          raise Exception.Create('Operation not found in ' + Line);
        if Operations[Prim.Op].FirstPrimIndex < 0 then
          Operations[Prim.Op].FirstPrimIndex := PrimListNG.Count-1;

        Prim.ProcName := Fields[fNGProcName];
        Prim.LType := StringToVarType(Fields[fNGLType]);
        if Prim.LType = vtUnknown then
          raise Exception.Create('Unknown LType: ' + Fields[fNGLType]);
        if CompareText(Fields[fNGRType], 'none') = 0 then
          Prim.RType := vtUnknown
        else
        begin
          Prim.RType := StringToVarType(Fields[fNGRType]);
          if Prim.RType = vtUnknown then
            raise Exception.Create('Unknown RType: ' + Fields[fNGRType]);
        end;
        Prim.Commutative := StringToBoolean(Fields[fNGCommutative]);

        Prim.ResultTypeIsLType := CompareText(Fields[fNGResultType], 'LType') = 0;
        Prim.ResultTypeIsRType := CompareText(Fields[fNGResultType], 'RType') = 0;
        if Prim.ResultTypeIsLType or Prim.ResultTypeIsRType then
          Prim.ResultType := vtUnknown
        else if CompareText(Fields[fNGResultType], 'None') = 0 then
          Prim.ResultType := vtUnknown
        else
        begin
          Prim.ResultType := StringToVarType(Fields[fNGResultType]);
          if Prim.ResultType = vtUnknown then
            raise Exception.Create('Unknown ResultType: ' + Fields[fNGResultType]);
        end;

        if Length(Fields[fNGValidation]) <> 1 then
          raise Exception.Create('Error in PrimitivesNG.Validation field: ' + Fields[fNGValidation]);
        case Fields[fNGValidation].Chars[0] of
          'y','Y': Prim.Validation := pvYes;
          'n','N': Prim.Validation := pvNo;
          'x','X': Prim.Validation := pvEither;
        else
          raise Exception.Create('Error in PrimitivesNG.Validation field: ' + Fields[fNGValidation]);
        end;


        //Primitive usage data
        Prim.Flags := StrToPrimFlagSetNG(Fields[fFlags]);

        Prim.LRegs := StrToCPURegSet(Fields[fNGLRegs], False);
        if rImm in Prim.LRegs then
          Assert(Prim.LRegs = [rImm])
        else if rIndirect in Prim.LRegs then
          Assert(Prim.LRegs = [rIndirect])
        else if rOffset in Prim.LRegs then
          Assert(Prim.LRegs = [rOffset]);

        Prim.RRegs := StrToCPURegSet(Fields[fNGRRegs], False);
        if rImm in Prim.RRegs then
          Assert(Prim.RRegs = [rImm])
        else if rIndirect in Prim.RRegs then
          Assert(Prim.RRegs = [rIndirect])
        else if rOffset in Prim.RRegs then
          Assert(Prim.RRegs = [rOffset]);

        Prim.ResultInLReg := CompareText(Fields[fNGResultReg], 'param1') = 0;
        if not Prim.ResultInLReg then
          Prim.ResultReg :=   StrToCPUReg(Fields[fNGResultReg], False);
        Prim.Corrupts :=  StrToCPURegSet(Fields[fNGCorrupts], True);

        Prim.ValidateProcName := Fields[fNGValidateProc];
        Prim.ValidateToS8 := Fields[fNGValidateToS8];
        Prim.ValidateToU8 := Fields[fNGValidateToU8];
        Prim.ValidateToS16 := Fields[fNGValidateToS16];
        Prim.ValidateToU16 := Fields[fNGValidateToU16];

        Prim.Proc := nil;
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

procedure ValidatePrimitiveNG(PrimNG: PPrimitiveNG);
begin
  if not Assigned(PrimNG.Proc) then
    ValidateProc(PrimNG.ProcName);

  ValidateProc(PrimNG.ValidateProcName);
  ValidateProc(PrimNG.ValidateToS8);
  ValidateProc(PrimNG.ValidateToU8);
  ValidateProc(PrimNG.ValidateToS16);
  ValidateProc(PrimNG.ValidateToU16);
end;

procedure ValidatePrimitives;
var Prim: PPrimitive;
  PrimNG: PPrimitiveNG;
begin
  for Prim in PrimList do
    ValidatePrimitive(Prim);
  for PrimNG in PrimListNG do
    ValidatePrimitiveNG(PrimNG);
end;

initialization
  PrimList := TList<PPrimitive>.Create;
  PrimListNG := TList<PPrimitiveNG>.Create;
end.

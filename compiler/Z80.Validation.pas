//Generates code for runtime error checks

unit Z80.Validation;

interface
uses Def.QTypes, Def.UserTypes,
  Lib.Data,
  Z80.Hardware, Z80.GenProcs;

//Procedure to be used for ramge checking the value in a register
type TRangeCheckProc = procedure(Reg: TCPUReg;Options: TMoveOptionSet);

//If the Primitive specifies an optimised range check procedure for the
//required conversion this routine will return the Proc.
//If not, or if this routine is not relevant (e.g due to the primitive's Result type),
//it will return nil.
//Also, primitive may explicitly specify NO range checks (eg for a typecast) in
//which case this routine will again return nil
//TODO: Do we really want FromType?? Should this be ToType??
function GetOptimisedRangeCheckProc(Prim: PPrimitive;FromType: TVarType): TRangeCheckProc;

//Generate range checking code
//Reg is the CPU register containing the value to be checked.
//FromType is the current type
//ToType is the type it is being converted to
//Prim is the Primitive used for the operation (or nil). This should *only* be given
//  if the value being tested is the Result of the primtive operation.
//  If non-nil the GenRangeCheck will examine the RangeCheckToxx properties
//  for a suitable optimised routine to use.
//Options specifies limitaions as to which registers can be used/must be preserved.
//  If the operation can't be performed without trashing these registers an assertion
//  will be raised. (This parmeter si mainly for validation of the code generator/regiser allocater);
procedure GenRangeCheck(Reg: TCPUReg;FromType, ToType: PUserType;Prim: PPrimitive;Options: TMoveOptionSet);

//These two routines operate the same as the above, but operate on individual bytes
//of a 16-bit value. They can be used to optimise range checking the loading of a
//16-bit value into an 8-bit register by allowing the two bytes to be loaded and tested
//separately.
//* Reg must be an 8-bit register
//* FromType must be a 16-bit integer type
//* ToType must be an 8-bit integer type
//* Prim is not needed here - that is only required when checking Stores
procedure GenRangeCheckHighByte(Reg: TCPUReg;FromType, ToType: PUserType;Options: TMoveOptionSet);
procedure GenRangeCheckLowByte(Reg: TCPUReg;FromType, ToType: PUserType;Options: TMoveOptionSet);

//These two routines perform a High9NEQRangeCheck in two parts - useful when bytes
//are being loded separately, eg when loading a 16-bit value into an 8-bit register.
//WARNING: Carry flag *must* be preserved between Part 1 and Part 2
//Low byte must be in A register
procedure GenRangeCheckIntegerToInt8Part1;
//High byte must be in A register.
procedure GenRangeCheckIntegerToInt8Part2;

implementation
uses SysUtils,
  CG.Data,
  Z80.CPUState, Z80.Assembler;

//===================================Range Checking
//aka overflow checking

procedure GenNZRangeCheck(Reg: TCPUReg;Options: TMoveOptionSet);
begin
  AsmInstr('jp nz,raise_range');
end;

procedure GenMRangeCheck(Reg: TCPUReg;Options: TMoveOptionSet);
begin
  AsmInstr('jp m,raise_range');
end;

procedure GenPRangeCheck(Reg: TCPUReg;Options: TMoveOptionSet);
begin
  AsmInstr('jp p,raise_range');
end;

//Raise an overflow error if bit 7 of the DestAlloc register is set
//Preserves all registers. Corrupts flags
procedure GenBit7SetRangeCheck(Reg: TCPUReg;Options: TMoveOptionSet);
begin
  Assert(Reg in CPUReg8Bit);

  if Reg = rA then
  begin //A reg
    Assert(Options * [moPreserveOtherFlags, moPreserveCF] = []);

    AsmInstr('and a');
    AsmInstr('jp m,raise_range');
    RegStateSetUnknowns([rZF, rFlags]);
    RegStateSetLiteral(rCF, 0); //AND clears carry
  end
  else
  begin
    Assert(Options * [moPreserveOtherFlags] = []);

    AsmOpCode('bit','7',CPURegStrings[Reg]);
    AsmInstr('jp nz,raise_range');
    RegStateSetUnknowns([rFlags]);  //Carry unchanged, other flags unknown
    RegStateSetLiteral(rZF, 1); //Zero numst be set. Not really useful but best to play safe
  end;
end;

//Raise an overflow error if bit 15 of the DestAlloc register is set
//Preserves all registers. Corrupts flags
procedure GenBit15SetRangeCheck(Reg: TCPUReg;Options: TMoveOptionSet);
begin
  if Reg in CPUReg8Bit then
    GenBit7SetRangeCheck(Reg, Options)
  else
  begin
    Assert(Reg in CPURegPairs);
    GenBit7SetRangeCheck(CPURegPairToHigh[Reg], Options);
  end;
end;

//Raise range error unless all 9 of the high bits are equal.
//I.e all set or all clear. Used to validate conversion from an
//signed 16 bit to signed 8 bit where we can only accept values -$80 to +$7f
//Used when downsizing signed values
//Corrupts: a,f
procedure GenHigh9NEQRangeCheck(Reg: TCPUReg;Options: TMoveOptionSet);
begin
  Assert(Reg in CPURegPairs);
  Assert(Options * [moPreserveOtherFlags, moPreserveCF, moPreserveA] = []);

  OpLD(rA, CPURegPairToLow[Reg]);
  AsmInstr('rla');               //Bit 7 of low byte into carry flag
  OpLD(rA, CPURegPairToHigh[Reg]);
  AsmInstr('adc a,$00');         //$ff + carry set gives $00, $00 + carry clear gives $00
  AsmInstr('jp nz,raise_range'); //All others overflow
  RegStateSetUnknowns([rFlags, rCF]);
  RegStateSetLiteral(rA, 0);
  RegStateSetLiteral(rZF, 1);
end;

//Raise overflow unless all bits are equal.
//I.e all set or all clear. Used to validate conversion from an
//signed 16 bit to signed 8 bit where we can only accept values -$80 to +$7f
//Used when downsizing signed values
//Corrupts: a,f
procedure GenByteNEQRangeCheck(Reg: TCPUReg;Options: TMoveOptionSet);
begin
  Assert(Reg in CPUReg8Bit);
  Assert(Options * [moPreserveOtherFlags, moPreserveCF, moPreserveA] = []);

  if Reg <> rA then
    OpLD(rA, Reg);
  AsmInstr('rlca');               //Bit 7 of low byte into carry flag, and to bit 0 of A
  AsmInstr('adc a,$00');         //$ff + carry set gives $00, $00 + carry clear gives $00
  AsmInstr('jp nz,raise_range'); //All others overflow
  RegStateSetUnknowns([rFlags, rCF]);
  RegStateSetLiteral(rA, 0);
  RegStateSetLiteral(rZF, 1);
end;

//Raise range error unless all bits of the high byte are equal.
//I.e all set or all clear. Used to validate conversion from an
//signed 16 bit to signed 8 bit where we can only accept values -$80 to +$7f
//Used when downsizing signed values
//Corrupts: a,f
procedure GenHighNEQRangeCheck(Reg: TCPUReg;Options: TMoveOptionSet);
begin
  Assert(Reg in CPURegPairs);

  GenByteNEQRangeCheck(CPURegPairToHigh[Reg], Options);
end;

//Raise an overflow error unless all of the highest nine bits of the
//destination register pair are zero
//Invalid if negative or > $7f (1)
//of > $7f (2)
//I.e top nine bits must be clear
//Corrupts: a,f
procedure GenHigh9NZRangeCheck(Reg: TCPUReg;Options: TMoveOptionSet);
begin
  Assert(Reg in CPURegPairs);
  Assert(Options * [moPreserveOtherFlags, moPreserveCF, moPreserveA] = []);

  OpLD(rA, CPURegPairToLow[Reg]);
  AsmInstr('and $80');			      //Mask all but top bit of low byte
  AsmOpcode('or', CPURegStrings[CPURegPairToHigh[Reg]]);
  AsmInstr('jp nz,raise_range');

  RegStateSetUnknowns([rA, rFlags]);
  RegStateSetLiteral(rCF, 0); //OR clears carry
  RegStateSetLiteral(rZF, 1);
end;

//Test if the high byte of the dest is non-zero. If so raise an overflow error
//Corrupts: a,f
procedure GenHighNZRangeCheck(Reg: TCPUReg;Options: TMoveOptionSet);
begin
  Assert(Reg in CPURegPairs);
  Assert(Options * [moPreserveOtherFlags, moPreserveCF] = []);

  OpLD(rA, CPURegPairToHigh[Reg]);
  AsmInstr('and a');
  AsmInstr('jp nz,raise_range');

  RegStateSetUnknown(rFlags);
  RegStateSetLiteral(rA, 0);  //A must be zero because we errored if it wasn't
  RegStateSetLiteral(rCF, 0); //AND clears CF
  RegStateSetliteral(rZF, 1);
end;

//Test if the byte is non-zero. If so raise an overflow error
//Corrupts: a,f
procedure GenByteNZRangeCheck(Reg: TCPUReg;Options: TMoveOptionSet);
begin
  Assert(Reg in CPUReg8Bit);
  Assert(Options * [moPreserveOtherFlags, moPreserveCF] = []);

  if Reg <> rA then
  begin
    Assert(Options * [moPreserveA] = []);
    OpLD(rA, Reg);
  end;
  AsmInstr('and a');
  AsmInstr('jp nz,raise_range');

  RegStateSetUnknown(rFlags);
  RegStateSetLiteral(rA, 0);  //A must be zero because we errored if it wasn't
  RegStateSetLiteral(rCF, 0); //AND clears CF
  RegStateSetliteral(rZF, 1);
end;
(*
Originally for testing Negate when overflow checking was off, but that proved
difficult so these reoutines have been redacted

//Test if a 16-bit value is non zero. If so raises a range check error
procedure Gen16BitNZRangeCheck(Reg: TCPUReg;Options: TMoveOptionSet);
begin
  Assert(Reg in CPURegPairs);
  Assert(Options * [moPreserveOtherFlags, moPreserveCF, moPreserveA] = []);

  OpLD(rA, CPURegPairToLow[Reg]);
  OpcodeOR(CPURegPairToHigh[Reg]);
  Instr('jp nz,raise_range');

  RegStateSetUnknown(rFlags);
  RegStateSetLiteral(rA, 0);  //A must be zero because we errored if it wasn't
  RegStateSetLiteral(rCF, 0); //OR clears CF
  RegStateSetliteral(rZF, 1);
end;

//Test whether 16-bit value is a non-zero positive value. If so raises a range check error
procedure Gen16BitPositiveNZRangeCheck(Reg: TCPUReg;Options: TMoveOptionSet);
begin
  Assert(Reg in CPURegPairs);
  Assert(Options * [moPreserveOtherFlags, moPreserveCF, moPreserveA] = []);

  //If bit 15 is set, no error
  //Otherwise all bits must be zero
  Assert(False);
end;
*)
function RangeCheckStrToProc(const Name: String): TRangeCheckProc;
begin
  Result := nil;
  if Name = 'empty' then
    Result := nil

  else if Name = 'nz_range' then
    Result := GenNZRangeCheck
  else if Name = 'm_range' then
    Result := GenMRangeCheck
  else if Name = 'p_range' then
    Result := GenPRangeCheck
  else if Name = 'bit7_set_range' then
    Result := GenBit7SetRangeCheck
  else if Name = 'bit15_set_range' then
    Result := GenBit15SetRangeCheck
  else if Name = 'high9_neq_range' then
    Result := GenHigh9NEQRangeCheck
  else if Name = 'high_neq_range' then
    Result := GenHighNEQRangeCheck
  else if Name = 'high9_nz_range' then
    Result := GenHigh9NZRangeCheck
  else if Name = 'high_nz_range' then
    Result := GenHighNZRangeCheck
(*
  else if Name = '16bit_nz_range' then
    Result := Gen16BitNZRangeCheck
  else if Name = '16bit_positive_nz_range' then
    Result := Gen16BitPositiveNZRangeCheck
*)
  else
    Assert(False, 'Unknown Range Check proc name')
end;

//If the Primitive specifies an optimised range check procedure for the
//required conversion this routine will return the Proc.
//If not, or if this routine is not relevant (e.g due to the primitives Result type
//it will return nil.
//Also, primitive may explicitly specify NO range checks (eg for a typecast) in
//which case this routine will again return nil
//TODO: Do we rally want FromType?? Should this be ToType??
function GetOptimisedRangeCheckProc(Prim: PPrimitive;FromType: TVarType): TRangeCheckProc;
var ProcName: String;
begin
  if not IsNumericType(FromType) then
    EXIT(nil);

  Assert(Assigned(Prim));

  if FromType = vtTypedPointer then
    FromType := vtPointer;

  ProcName := '';
  //Do we have a special case validation routine for conversion to said type?
  case FromType of
    vtInt8: ProcName := Prim.RangeCheckToS8;
    vtByte: ProcName := Prim.RangeCheckToU8;
    vtInteger: ProcName := Prim.RangeCheckToS16;
    vtWord, vtPointer: ProcName := Prim.RangeCheckToU16;
  else
    raise Exception.Create('Unhandled numeric type GenRangeCheck');
  end;
  if ProcName <> '' then
    Result := RangeCheckStrToProc(ProcName)
  else
    Result := nil;
end;

//Where ToType is a SubRange. Generate code to range check it's assignment from ToType
//Parameters as GenRangeCheck
procedure GenSubRangeCheck(Reg: TCPUReg;FromType, ToType: PUserType;Prim: PPrimitive;Options: TMoveOptionSet);
begin
  Assert(Assigned(FromType));
  Assert(Assigned(ToType));
  Assert(Assigned(ToType.OfType));
  Assert(FromType.VarType = vtSubRange);
(*
  ToBaseType := ToType.OfType;

//TODO: **** All Ordinal types to specify Low and High value in their type
  if ToType.Low <> FromType.Low then
    case FromType.VarType of
      vtByte:
        if ToType.Low <> 0 then
        begin
          if Reg <> rA then
            //LD A,Reg && State
          //CP A,ToType.Low-1
          //JP NC,range_error



    //Special case.
    Assert(False)
{
If From Unsigned -> Do nothing
If From Signed -> Error is negative
}
  else
    Assert(False)
{
If From Unsigned -> Check low value
If From Signed -> Check low value
}
  end;
  if ToType.High <> FromType.High then
{
If From Unsigned -> Check high value
If From Signed -> Check high value
*)


  Assert(False);
end;

(*const //Routine names to keep the table source code size reasonable
  //Error if bit 7 set
  b7s: TRangeCheckProc = GenBit7SetRangeCheck;
  //Error if bit 15 is set
  b15s: TRangeCheckProc = GenBit15SetRangeCheck;
  //Error unless all of the highest 9 bits are equal (ie. all set or all clear)
  h9neq: TRangeCheckProc = GenHigh9NEQRangeCheck;
  //Raise an error unless the highest 9 bits are all zero
  b9nz: TRangeCheckProc = GenHigh9NZRangeCheck;
  //Error if the high byte is non-zero
  hbnz: TRangeCheckProc = GenHighNZRangeCheck;
*)
//Specifies the routine to use to validate a conversion from the type given in the row
//to the type given in the column.
//'' (empty): no validation is necessary for this conversion
//If a more optimised routine is available this can be specified in the primitives
//table ('Special validations on type conversion'). If so that routine will be used
//in preference to this table.
const ValidationMatrix: array[vtInt8..vtPointer,vtInt8..vtPointer] of TRangeCheckProc =
//        To:
//From:   Int8                    Integer                Byte                  Word                   Pointer
{Int8}    ((nil,                  nil,                   GenBit7SetRangeCheck, GenBit7SetRangeCheck,  GenBit7SetRangeCheck),
{Integer} (GenHigh9NEQRangeCheck, nil,                   GenHighNZRangeCheck,  GenBit15SetRangeCheck, GenBit15SetRangeCheck),
{Byte}    (GenBit7SetRangeCheck,  nil,                   nil,                  nil,                   nil),
{Word}    (GenHigh9NZRangeCheck,  GenBit15SetRangeCheck, GenHighNZRangeCheck,  nil,                   nil),
{Pointer} (GenHigh9NZRangeCheck,  GenBit15SetRangeCheck, GenHighNZRangeCheck,  nil,                   nil));

procedure GenRangeCheck(Reg: TCPUReg;FromType, ToType: PUserType;Prim: PPrimitive;Options: TMoveOptionSet);
var Proc: TRangeCheckProc;
  Optimised: TRangeCheckProc;
  FType, TType: TVarType;
begin
  Assert(Assigned(FromType));
  Assert(Assigned(ToType));

  FType := FromType.VarType;
  TType := ToType.VarType;
  if FType = vtTypedPointer then
    FType := vtPointer;
  if TType = vtTypedPointer then
    TType := vtPointer;

  //Filter out anything we don't need to range check.
  //This will need to be updated at some point for array, enumeration, etc.
  if FromType = ToType then
    EXIT;
  if TType = vtSubRange then
  begin //Process conversions to SubRange
    GenSubRangeCheck(Reg, FromType, ToType, Prim, Options);
    EXIT;
  end;
  if not IsNumericType(FType) then
    EXIT;
  if not IsNumericType(TType) then
    EXIT;
  Assert(TType <> vtUnknown);

  //Assign generic default
  Proc := ValidationMatrix[FType, TType];
  //May be overridden with an optimised proc
  Optimised := nil;
  if Assigned(Prim) then
    Optimised := GetOptimisedRangeCheckProc(Prim, FType);

  if Assigned(Optimised) then
    Optimised(Reg, Options)
  else if Assigned(Proc) then
    Proc(Reg, Options);
end;

procedure GenRangeCheckHighByte(Reg: TCPUReg;FromType, ToType: PUserType;Options: TMoveOptionSet);
var FType, TType: TVarType;
begin
  Assert(Assigned(FromType));
  Assert(Assigned(ToType));

  //Filter out anything we don't need to range check.
  //This will need to be updated at some point for array, enumeration, etc.
  if FromType = ToType then
    EXIT;

  FType := FromType.VarType;
  TType := ToType.VarType;
  if not IsNumericType(FType) then
    EXIT;
  if not IsNumericType(TType) then
    EXIT;

  Assert(Reg in CPUReg8Bit);
  Assert(FType in [vtInteger, vtWord, vtPointer]);
  Assert(TType in [vtInt8, vtByte]);

  case TType of
    vtByte: GenByteNZRangeCheck(Reg, Options);
    vtInt8:
      if FType = vtInteger then
        GenByteNEQRangeCheck(Reg, Options)
      else
        GenByteNZRangeCheck(Reg, Options);
  else
    Assert(False);
  end;
end;

procedure GenRangeCheckLowByte(Reg: TCPUReg;FromType, ToType: PUserType;Options: TMoveOptionSet);
var FType, TType: TVarType;
begin
  Assert(Assigned(FromType));
  Assert(Assigned(ToType));

  //Filter out anything we don't need to range check.
  //This will need to be updated at some point for array, enumeration, etc.
  if FromType = ToType then
    EXIT;

  FType := FromType.VarType;
  TType := ToType.VarType;
  if not IsNumericType(FType) then
    EXIT;
  if not IsNumericType(TType) then
    EXIT;

  Assert(Reg in CPUReg8Bit);
  Assert(FType in [vtInteger, vtWord, vtPointer]);
  Assert(TType in [vtInt8, vtByte]);
  Assert(not ((FType = vtInteger) and (TType = vtInt8)), 'Use specialises routines');

  //From    To    Check?
  //Integer Int8  ??  - Use another routine
  //Integer Byte  No  - We've tested Integer was positive via high byte
  //Word    Int8  Yes - Avoid accidental negatives
  //Word    Byte  No
  //If both types have the same signed/unsigned status, no need for checks
  //If both types have different signed/unsigned status we require bit 7 to be clear
  if IsSignedType(TType) and not IsSignedType(FType) then
    GenBit7SetRangeCheck(Reg, Options);
end;

procedure GenRangeCheckIntegerToInt8Part1;
begin
  AsmInstr('rla');
end;

procedure GenRangeCheckIntegerToInt8Part2;
begin
  AsmInstr('adc a,$00');
  AsmInstr('jp nz,raise_range');
  RegStateSetLiteral(rA, 0);
  RegStateSetUnknowns([rFlags, rCF]);
end;

end.

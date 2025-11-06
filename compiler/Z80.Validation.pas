//Generates code for runtime error checks

unit Z80.Validation;

interface
uses Def.VarTypes, Def.UserTypes,
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

//These routines operate the same as the above, but operate on individual bytes
//of a 16-bit value. They can be used to optimise range checking the loading of a
//16-bit value into an 8-bit register by allowing the two bytes to be loaded and tested
//separately.
//* Reg must be an 8-bit register
//* FromType must be a 16-bit integer type
//* ToType must be an 8-bit integer type
//* Prim is not needed here - that is only required when checking Stores
procedure GenRangeCheckHighByte(Reg: TCPUReg;FromType, ToType: PUserType;Options: TMoveOptionSet);
procedure GenRangeCheckLowByte(Reg: TCPUReg;FromType, ToType: PUserType;Options: TMoveOptionSet);

//Used when loading a 16-bit value into an 8-bit value
//!!!On entry the high byte/bits have been validated as within range for the OfType (Byte or Int8)
//This routine valdates that the Low byte (in Reg) is within range of ToType
procedure GenSubRangeCheckLowByte(Reg: TCPUReg;FromType, ToType: PUserType;Options: TMoveOptionSet);


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
  Lib.GenFragments,
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

  OpMOV(rA, CPURegPairToLow[Reg]);
  AsmInstr('rla');               //Bit 7 of low byte into carry flag
  OpMOV(rA, CPURegPairToHigh[Reg]);
  AsmInstr('adc a,$00');         // $ff + carry set gives $00, $00 + carry clear gives $00
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
    OpMOV(rA, Reg);
  AsmInstr('rlca');               //Bit 7 of low byte into carry flag, and to bit 0 of A
  AsmInstr('adc a,$00');         // $ff + carry set gives $00, $00 + carry clear gives $00
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

  OpMOV(rA, CPURegPairToLow[Reg]);
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

  OpMOV(rA, CPURegPairToHigh[Reg]);
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
    OpMOV(rA, Reg);
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

//================================SubRanges

//As SubRangeCheck for unsigned 8-bit variable to SubRange
procedure GenSubRangeCheckU8(Reg: TCPUReg;FromType, ToType: PUserType;Options: TMoveOptionSet);
var AMoved: Boolean;
begin
  Assert(Reg in CPUReg8Bit);
  Assert(GetTypeSize(FromType) = 1);
  Assert(not IsSignedType(FromType));
  Assert(Options * [moPreserveA, moPreserveCF, moPreserveOtherFlags] = []);

  AMoved := False;

  //Lower bound
  if ToType.Low > FromType.Low then
  begin
    //Move Reg to rA
    if (Reg <> rA) and not AMoved then
    begin
      GenRegMove(Reg, rA, False, Options);
      AMoved := True;
    end;

    Assert((ToType.Low >= 0) and (ToType.Low <= 255)); //Byte values only
    AsmInstr('cp ' + ByteToStr(ToType.Low));
    AsmInstr('jp c,raise_range');
    RegStateSetUnknown(rFlags);
    RegStateSetLiteral(rCF, 0);
  end;

  //Upper bound
  if ToType.High < FromType.High then
  //Test value in A <= ToType.High
  begin
    //Move Reg to rA
    if (Reg <> rA) and not AMoved then
      GenRegMove(Reg, rA, False, Options);

     //(Test code errors out if ToType.High = Max(Byte))
    Assert((ToType.High >= 0) and (ToType.High < 255)); //Byte values only
    AsmInstr('cp ' + ByteToStr(ToType.High+1));
    AsmInstr('jp nc,raise_range');
    RegStateSetUnknown(rFlags);
    RegStateSetLiteral(rCF, 1);
  end;
end;

//As SubRangeCheck for signed 8-bit variable to SubRange
procedure GenSubRangeCheckS8(Reg: TCPUReg;FromType, ToType: PUserType;Options: TMoveOptionSet);
var AMoved: Boolean;
begin
  Assert(Reg in CPUReg8Bit);
  Assert(GetTypeSize(FromType) = 1);
  Assert(IsSignedType(FromType));
  Assert(Options * [moPreserveA, moPreserveCF, moPreserveOtherFlags] = []);

  AMoved := False;

  //Lower bound
  if ToType.Low > FromType.Low then
  begin
    //Move Reg to rA
    if (Reg <> rA) and not AMoved then
    begin
      GenRegMove(Reg, rA, False, Options);
      AMoved := True;
    end;

    //Test value if rA >= ToType.Low
    if ToType.Low = 0 then
    begin //Special case - error if negative
      AsmInstr('and a');
      AsmInstr('jp m,raise_range');
    end
    else
    begin
      Assert((ToType.Low >= -128) and (ToType.Low <= 127)); //Int8 values only
      AsmInstr('cp ' + ByteToStr(ToType.Low));
      GenLibraryProc(':less_than_signed_range', nil);
    end;
    RegStateSetUnknown(rFlags);
  end;

  //Upper bound
  if ToType.High < FromType.High then
  begin
    //Move Reg to rA
    if (Reg <> rA) and not AMoved then
      GenRegMove(Reg, rA, False, Options);

    //Test value in A <= ToType.High
    //(Test code errors out if ToType.High = Max(Int8))
    Assert((ToType.High >= -128) and (ToType.High < 127)); //Byte values only
    AsmInstr('cp ' + ByteToStr(ToType.High+1));
    GenLibraryProc(':greater_than_equal_signed_range', nil);
    RegStateSetUnknown(rFlags);
  end;
end;

//As SubRangeCheck for unsigned 16-bit variable to SubRange
procedure GenSubRangeCheckU16(Reg: TCPUReg;FromType, ToType: PUserType;Options: TMoveOptionSet);
var LowPass: String;  //Labels
  TestCarry: String;
  HighPass: String;
begin
  //****************************************
  //There's probably easier ways to do this but the rest of the code generator
  //allows us to corrupt A and F and nothing else. Therefore we can't do a subtraction
  //or call a subroutine with value in register. So we'll do it the long way
  //and test bytes individually
  //****************************************
  Assert(Reg in CPURegPairs);
  Assert(GetTypeSize(FromType) = 2);
  Assert(not IsSignedType(FromType));
  Assert(Options * [moPreserveA, moPreserveCF, moPreserveOtherFlags] = []);

  //Lower bound
  if ToType.Low > FromType.Low then
  begin //Test value in Reg >= ToType.Low
    Assert((ToType.Low >= 0) and (ToType.Low <= 65535)); //Byte values only
    GenRegMove(CPURegPairToHigh[Reg], rA, False, Options);  //High byte of value to A
    if hi(ToType.Low) = 0 then  //If SubRange.Low <= 255 we need Hi byte to be zero
    begin //SubRange.Low in [$0000..$00ff]
      AsmInstr('and a');              //If high byte if non-zero...
      LowPass := GetUniqueLabel;
      AsmInstr('jr nz,'+LowPass);        //...can't be an error

      GenRegMove(CPURegPairToLow[Reg], rA, False, Options);  //Low byte of value to A
      AsmInstr('cp ' + ByteToStr(Lo(ToType.Low)));  //Test low byte
      AsmInstr('jp c,raise_range');   //Error if too low
      AsmLabel(LowPass);
      RegStateSetUnknown(rFlags);
    end
    else  //SubRange.Low >= $0100
    begin
      AsmInstr('cp ' + ByteToStr(Hi(ToType.Low)));  //Test high byte
      if Lo(ToType.Low) <> 0 then //If Lo byte of SubRange is zero we only need test high byte
      begin
        TestCarry := GetUniqueLabel;
        AsmInstr('jr nz,'+TestCarry);                 //If non-zero we can ignore low byte
        GenRegMove(CPURegPairToLow[Reg], rA, False, Options);  //Low byte of value to A
        AsmInstr('cp ' + ByteToStr(Lo(ToType.Low)));  //Test low byte
        AsmLabel(TestCarry);
      end;
      AsmInstr('jp c,raise_range');
      RegStateSetUnknown(rFlags);
      RegStateSetLiteral(rCF, 0);
    end;
  end;

  //Upper bound
  if ToType.High < FromType.High then
  begin
    //(Test code errors out if ToType.High = Max(Word))
    Assert(ToType.High >= 0);
    if ToType.High < 65535 then  //MaxWord can't be out of range
    begin
      GenRegMove(CPURegPairToHigh[Reg], rA, False, Options);  //High byte of value to A
      if hi(ToType.High) = 0 then  //If SubRange.High <= 255 we need Hi byte to be zero
      begin //SubRange.High in [$0000..$00ff]
        AsmInstr('and a');              //If high byte is non-zero...
        AsmInstr('jp nz,raise_range');  //...error
        RegStateSetUnknown(rFlags);
        if lo(ToType.High) = $ff then
          //SubRange.High = $00ff
          RegStateSetLiteral(rZF, 1)
        else  //SubRange.High < $00ff
        begin
          GenRegMove(CPURegPairToLow[Reg], rA, False, Options);  //Low byte of value to A
          AsmInstr('cp ' + ByteToStr(Lo(ToType.High)+1));  //Test low byte
          AsmInstr('jp nc,raise_range');   //Error if too high
          RegStateSetLiteral(rCF, 1);
        end;
      end
      else  //SubRange.High >= $0100
      begin
        if Lo(ToType.High) = $ff then
        begin //If lo byte is $ff we only need to test high byte
          AsmInstr('cp ' + ByteToStr(Hi(ToType.High)+1));  //Test high byte
          AsmInstr('jp nc,raise_range');
          RegStateSetUnknown(rFlags);
          RegStateSetLiteral(rCF, 1);
        end
        else  //Otherwise test both bytes
        begin
          AsmInstr('cp ' + ByteToStr(Hi(ToType.High)));  //Test high byte
          HighPass := GetUniqueLabel;
          AsmInstr('jr c,'+HighPass);     //Guaranteed pass
          AsmInstr('jp nz,raise_range');  //Guaranteed fail

          GenRegMove(CPURegPairToLow[Reg], rA, False, Options);  //Low byte of value to A
          AsmInstr('cp ' + ByteToStr(Lo(ToType.High)+1));  //Test low byte
          AsmInstr('jp nc,raise_range');
          AsmLabel(HighPass);
          RegStateSetUnknown(rFlags);
          RegStateSetLiteral(rCF, 1);
        end;
      end;
    end;
  end;
end;

//As SubRangeCheck for unsigned 16-bit variable to SubRange
procedure GenSubRangeCheckS16(Reg: TCPUReg;FromType, ToType: PUserType;Options: TMoveOptionSet);
var LowPass: String;  //Labels
  TestCarry: String;
  LowPassP: String;
  HighPassM: String;
  HighPass: String;
begin
  //****************************************
  //There's probably easier ways to do this but the rest of the code generator
  //allows us to corrupt A and F and nothing else. Therefore we can't do a subtraction
  //or call a subroutine with value in register. So we'll do it the long way
  //and test bytes individually
  //****************************************
  Assert(Reg in CPURegPairs);
  Assert(GetTypeSize(FromType) = 2);
  Assert(IsSignedType(FromType));
  Assert(Options * [moPreserveA, moPreserveCF, moPreserveOtherFlags] = []);

  //Lower bound
  if ToType.Low > FromType.Low then
  begin
    //Test sign of Value. Either ->
    //  * shortcut tests (depending on range bounds),
    //  * or jump to only generate unsigned tests

    Assert((ToType.Low >= -32768) and (ToType.Low <= 32767)); //Integer values only
    GenRegMove(CPURegPairToHigh[Reg], rA, False, Options);  //High byte of value to A
    AsmInstr('and a');              //Test high byte
    RegStateSetUnknown(rFlags);
    if ToType.Low >= 0 then
    begin
      //Error if value is negative
      AsmInstr('jp m,raise_range');
      //We're testing a positve value against a positive literal
      //We can reuse unsigned lower bound test!!!
      //...
      if hi(ToType.Low) = 0 then  //If SubRange.Low <= 255 we need Hi byte to be zero
      begin //SubRange.Low in [$0000..$00ff]
                                          //If high byte if non-zero...
        LowPass := GetUniqueLabel;
        AsmInstr('jp nz,'+LowPass);       //...can't be an error

        GenRegMove(CPURegPairToLow[Reg], rA, False, Options);  //Low byte of value to A
        AsmInstr('cp ' + ByteToStr(Lo(ToType.Low)));  //Test low byte
        AsmInstr('jp c,raise_range');   //Error if too low
        AsmLabel(LowPass);
        RegStateSetLiteral(rCF, 0);
      end
      else  //SubRange.Low >= $0100
      begin
        AsmInstr('cp ' + ByteToStr(Hi(ToType.Low)));  //Test high byte
        if Lo(ToType.Low) <> 0 then //If Lo byte of SubRange is zero we only need test high byte
        begin
          TestCarry := GetUniqueLabel;
          AsmInstr('jr nz,'+TestCarry);                 //If non-zero we can ignore low byte
          GenRegMove(CPURegPairToLow[Reg], rA, False, Options);  //Low byte of value to A
          AsmInstr('cp ' + ByteToStr(Lo(ToType.Low)));  //Test low byte
          AsmLabel(TestCarry);
        end;
        AsmInstr('jp c,raise_range');
        RegStateSetUnknown(rFlags);
        RegStateSetLiteral(rCF, 0);
      end;
    end
    else  //ToType.Low < 0
    begin
      //Guaranteed pass if Value is positive
      LowPassP := GetUniqueLabel;
      AsmInstr('jp p,'+LowPassP);       //...can't be an error
      //Testing negative value against a negative literal
      //Can we do this unsigned???
      //...

      begin
        AsmInstr('cp ' + ByteToStr(Hi(ToType.Low)));  //Test high byte
        if Lo(ToType.Low) <> 0 then //If Lo byte of SubRange is zero we only need test high byte
        begin
          TestCarry := GetUniqueLabel;
          AsmInstr('jr nz,'+TestCarry);                 //If non-zero we can ignore low byte
          GenRegMove(CPURegPairToLow[Reg], rA, False, Options);  //Low byte of value to A
          AsmInstr('cp ' + ByteToStr(Lo(ToType.Low)));  //Test low byte
          AsmLabel(TestCarry);
        end;
        AsmInstr('jp c,raise_range');
        RegStateSetUnknown(rFlags);
        RegStateSetLiteral(rCF, 0);
      end;
      AsmLabel(LowPassP);
    end;
  end;

  //Upper Bound
  if ToType.High < FromType.High then
  begin
    //Test sign of Value. Either ->
    //  * shortcut tests (depending on range bounds),
    //  * or jump to only generate unsigned tests

    Assert((ToType.Low >= -32768) and (ToType.Low <= 32767)); //Integer values only
    GenRegMove(CPURegPairToHigh[Reg], rA, False, Options);  //High byte of value to A
    AsmInstr('and a');              //Test high byte
    RegStateSetUnknown(rFlags);
    if ToType.High >= 0 then
    begin
      //Guaranteed pass if value is negative
      HighPassM := GetUniqueLabel;
      AsmInstr('jp m,'+HighPassM);
      //We're testing a positive value against a positive literal
      //We can reuse unsigned lower bound test!!!
      //...

      if Lo(ToType.High) = $ff then
      begin //If lo byte is $ff we only need to test high byte
        AsmInstr('cp ' + ByteToStr(Hi(ToType.High)+1));  //Test high byte
        AsmInstr('jp nc,raise_range');
        RegStateSetUnknown(rFlags);
        RegStateSetLiteral(rCF, 1);
      end
      else  //Otherwise test both bytes
      begin
        AsmInstr('cp ' + ByteToStr(Hi(ToType.High)));  //Test high byte
        HighPass := GetUniqueLabel;
        AsmInstr('jr c,'+HighPass);     //Guaranteed pass
        AsmInstr('jp nz,raise_range');  //Guaranteed fail

        GenRegMove(CPURegPairToLow[Reg], rA, False, Options);  //Low byte of value to A
        AsmInstr('cp ' + ByteToStr(Lo(ToType.High)+1));  //Test low byte
        AsmInstr('jp nc,raise_range');
        AsmLabel(HighPass);
        RegStateSetUnknown(rFlags);
        RegStateSetLiteral(rCF, 1);
      end;

      AsmLabel(HighPassM);
    end
    else  //ToType.High < 0
    begin
      //Fail if value >= 0
      AsmInstr('jp p,raise_range');
      //Testing negative value against a negative literal
      //Can we do this unsigned???
      //...

      if Lo(ToType.High) = $ff then
      begin //If lo byte is $ff we only need to test high byte
        AsmInstr('cp ' + ByteToStr(Hi(ToType.High)+1));  //Test high byte
        AsmInstr('jp nc,raise_range');
        RegStateSetUnknown(rFlags);
        RegStateSetLiteral(rCF, 1);
      end
      else  //Otherwise test both bytes
      begin
        AsmInstr('cp ' + ByteToStr(Hi(ToType.High)));  //Test high byte
        HighPass := GetUniqueLabel;
        AsmInstr('jr c,'+HighPass);     //Guaranteed pass
        AsmInstr('jp nz,raise_range');  //Guaranteed fail

        GenRegMove(CPURegPairToLow[Reg], rA, False, Options);  //Low byte of value to A
        AsmInstr('cp ' + ByteToStr(Lo(ToType.High)+1));  //Test low byte
        AsmInstr('jp nc,raise_range');
        AsmLabel(HighPass);
        RegStateSetUnknown(rFlags);
        RegStateSetLiteral(rCF, 1);
      end;
    end;
  end;
end;

//Where ToType is a SubRange. Generate code to range check it's assignment from ToType
//Parameters as GenRangeCheck
procedure GenSubRangeCheck(Reg: TCPUReg;FromType, ToType: PUserType;Options: TMoveOptionSet);
begin
  Assert(Assigned(FromType));
  Assert(Assigned(ToType));
  Assert(ToType.IsSubRange);
  Assert(Assigned(ToType.OfType));

  //We're testing an 8-bit value
  case GetTypeSize(FromType) of
  1:
//  if Reg in CPUReg8Bit then
    if IsSignedType(FromType) then
      GenSubRangeCheckS8(Reg, FromType, ToType, Options)
    else
      GenSubRangeCheckU8(Reg, FromType, ToType, Options);
  2:
//  else  //16-bit value
      if IsSignedType(FromType) then
      GenSubRangeCheckS16(Reg, FromType, ToType, Options)
    else
      GenSubRangeCheckU16(Reg, FromType, ToType, Options);
  else
    Assert(False);
  end;
end;

//===================================

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
  if ToType.IsSubRange then
  begin //Process conversions to SubRange
    GenSubRangeCheck(Reg, FromType, ToType, Options);
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

//Note: Subrange types should have already been removed from FromType and ToType
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


//Used when loading a 16-bit value into an 8-bit value
//!!!On entry the high byte/bits have been validated as within range for the OfType (Byte or Int8)
//This routine valdates that the Low byte (in Reg) is within range of ToType
procedure GenSubRangeCheckLowByte(Reg: TCPUReg;FromType, ToType: PUserType;Options: TMoveOptionSet);
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
  if not IsOrdinalType(FType) then
    EXIT;
  if not IsOrdinalType(TType) then
    EXIT;

  Assert(Reg in CPUReg8Bit);

  //Move Reg to rA
  if Reg <> rA then
    GenRegMove(Reg, rA, False, Options);

  //Lower bound
  if ToType.Low > FromType.Low then
    if IsSignedType(FromType) then
    begin //Signed comparison
      //Test value if rA >= ToType.Low
      if ToType.Low = 0 then
      begin //Special case - error if negative
        AsmInstr('and a');
        AsmInstr('jp m,raise_range');
      end
      else
      begin
        Assert(ToType.Low >= -128); //Int8 values only
        if ToType.Low < 127 then
        begin
          AsmInstr('cp ' + ByteToStr(ToType.Low));
          GenLibraryProc(':less_than_signed_range', nil);
        end;
      end;
    end
    else //Unsigned comparison
    begin
      Assert((ToType.Low >= 0) and (ToType.Low <= 255)); //Byte values only
      AsmInstr('cp ' + ByteToStr(ToType.Low));
      AsmInstr('jp c,raise_range');
    end;

  //Upper bound
  if ToType.High < FromType.High then
    //Test value in A <= ToType.High
    if IsSignedType(FromType) then
    begin //Signed comparison
      //(Test code errors out if ToType.High = Max(Int8))
      Assert((ToType.High >= -128) and (ToType.High < 127)); //Byte values only
      AsmInstr('cp ' + ByteToStr(ToType.High+1));
      GenLibraryProc(':greater_than_equal_signed_range', nil);
    end
    else //Unsigned comparison
    begin
      //(Test code errors out if ToType.High = Max(Byte))
      Assert(ToType.High >= 0); //Byte values only
      if ToType.High < 255 then
      begin
        AsmInstr('cp ' + ByteToStr(ToType.High+1));
        AsmInstr('jp nc,raise_range');
      end;
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
  if IsSignedVarType(TType) and not IsSignedVarType(FType) then
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

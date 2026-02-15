(*
Routines to assist with generating assembler code for the Z80
*)
unit Z80.Assembler;

interface
uses
  Def.Variables, Def.Consts,
  Z80.Hardware;

//If ByteIndex is zero,
//Generates (IX+<full-varname>)
//otherwise,
//Generates (IX+<full-varname>+<ByteOffset>)
function OffsetToStr(Reg: TCPUReg;Variable: PVariable;ByteIndex: Integer = 0): String;
function OffsetHiToStr(Reg: TCPUReg;Variable: PVariable): String;

//---Loads from memory
procedure OpLOAD(Dest: TCPUReg;Source: String);overload;
procedure OpLOAD(Dest: TCPUReg;SourceV: PVariable);overload;
//ByteIndex allows to specify high or low byte. Reg must be 8-bit
procedure OpLOAD(Dest: TCPUReg;SourceV: PVariable;ByteIndex: Integer);overload;
//From index register - can move both 8 and 16 bit values
procedure OpLOAD(Dest, SourceXY: TCPUReg;SourceV: PVariable;ByteIndex: Integer = 0);overload;

//Where Dest is 8-bit and Source is 16-bit (pointer)
//LD <Dest>,(<Source>)
procedure OpLOAD(Dest, Source: TCPUReg);overload;

//---Moves between registers, and loads of immediate data
procedure OpMOV(Dest: TCPUReg;Source: String);overload;
//Convenience routines to generate specific opcodes
procedure OpMOV(Dest, Source: TCPUReg);overload;
//Converts Source to 8 or 16-bit string depending on the Dest register size.
//If source is a Char, might also be converted to characters literal
procedure OpMOV(Dest: TCPUReg;Source: TImmValue);overload;
procedure OpMOV(Dest: TCPUReg;Value: Integer);overload;

//---Stores to memory
//To/from variable
procedure OpSTO(DestV: PVariable;Source: TCPUReg);overload;
//ByteIndex allows to specify high or low byte. Reg must be 8-bit
procedure OpSTO(DestV: PVariable;ByteIndex: Integer;Source: TCPUReg);overload;
//To index register - can move both 8 and 16 bit values
procedure OpSTO(DestXY: TCPUReg;DestV: PVariable;Source: TCPUReg;ByteIndex: Integer = 0);overload;
//8-bit store ONLY
procedure OpSTO(DestXY: TCPUReg;DestV: PVariable;Value: Integer;ByteIndex: Integer = 0);overload;

//Where Dest is 16-bit (pointer) and Source is 8-bit
//LD (<Dest>),<Source>
procedure OpSTO(Dest, Source: TCPUReg);overload;
//Dest must be HL
//LD (<Dest>),<immediate>
procedure OpSTO(Dest: TCPUReg; Source: TImmValue);overload;
//8-bit immediate store ONLY
procedure OpSTO(Dest: TCPUReg; Value: Integer);overload;

procedure OpLDIR;

procedure OpPUSH(Reg: TCPUReg);
procedure OpPOP(Reg: TCPUReg);

procedure OpEXHLDE;

procedure OpADD(RAcc, RAdd: TCPUReg);
procedure OpADDHLSP;
procedure OpAND(Reg: TCPUReg);
procedure OpCP(Value: Integer);
procedure OpDEC(Reg: TCPUReg);
procedure OpINC(Reg: TCPUReg);
procedure OpOR(Reg: TCPUReg);
procedure OpSBC(RAcc, rSub: TCPUReg);


implementation
uses SysUtils,
  CG.Data;


function OffsetToStr(Reg: TCPUReg;Variable: PVariable;ByteIndex: Integer = 0): String;
begin
  Assert(Variable.AddrMode in [amStack, amStackRef]);

  Result := '('+CPURegStrings[Reg];
  if Variable.Offset < 0 then
    Result := Result + '-'
  else
    Result := Result + '+';
  Result := Result + Variable.GetAsmName;

  if ByteIndex <> 0 then
    Result := Result + ' +' + ByteIndex.ToString;
  Result := Result + ')';
end;

function OffsetHiToStr(Reg: TCPUReg;Variable: PVariable): String;
begin
  Result := OffsetToStr(Reg, Variable, 1);
end;

procedure OpLOAD(Dest: TCPUReg;Source: String);overload;
begin
  Assert((Dest = rA) or (Dest in CPUReg16Bit));
  AsmOpcode('ld', CPURegStrings[Dest], '('+Source+')');
end;

procedure OpLOAD(Dest: TCPUReg;SourceV: PVariable);overload;
begin
  Assert(SourceV.AddrMode in [amStatic, amStaticRef]);
  AsmOpcode('ld', CPURegStrings[Dest], '('+SourceV.GetAsmName+')');
end;

procedure OpLOAD(Dest, SourceXY: TCPUReg;SourceV: PVariable;ByteIndex: Integer = 0);overload;
begin
  Assert(SourceXY in [rIX, rIY]);
  Assert(SourceV.AddrMode in [amStack, amStackRef]);

  if Dest in CPURegPairs then
  begin
    AsmOpcode('ld', CPURegStrings[CPURegPairToLow[Dest]], OffsetToStr(SourceXY, SourceV, ByteIndex));
    AsmOpcode('ld', CPURegStrings[CPURegPairToHigh[Dest]], OffsetToStr(SourceXY, SourceV, ByteIndex +1));
  end
  else
    AsmOpcode('ld', CPURegStrings[Dest], OffsetToStr(SourceXY, SourceV, ByteIndex));
end;

procedure OpLOAD(Dest: TCPUReg;SourceV: PVariable;ByteIndex: Integer);overload;
var S: String;
begin
  Assert(SourceV.AddrMode = amStatic);
  S := '('+SourceV.GetAsmName;
  if ByteIndex <> 0 then
    S := S +' +' +ByteIndex.ToString;
  S := S + ')';
  AsmOpcode('ld', CPURegStrings[Dest], S);
end;

//LD <dest>,(<source>) where dest is an 8-bit and source is a 16-bit
procedure OpLOAD(Dest, Source: TCPUReg);
var S: String;
begin
  Assert(Source in CPUReg16Bit);
  Assert(Dest in CPUReg8Bit);
  Assert((Dest = rA) or (Source = rHL));  //Unacceptable combinations

  S := '(' + CPURegStrings[Source] + ')';
  AsmOpcode('ld', CPURegStrings[Dest], S);
end;

procedure OpMOV(Dest: TCPUReg;Source: String);overload;
begin
  AsmOpcode('ld', CPURegStrings[Dest], Source);
end;

procedure OpMOV(Dest, Source: TCPUReg);overload;
begin
  if Source in [rIX, rIY] then
  begin //Mov via 'illegal' instructions
    Assert(Dest in [rBC, rDE]); //Can't move to other index reg or HL :sigh:
    AsmOpCode('ld',CPURegStrings[CPURegPairToLow[Dest]],CPURegStrings[Source] + 'l');
    AsmOpCode('ld',CPURegStrings[CPURegPairToHigh[Dest]],CPURegStrings[Source] + 'h');
  end
  else
    AsmOpcode('ld',CPURegStrings[Dest],CPURegStrings[Source]);
end;

procedure OpMOV(Dest: TCPUReg; Source: TImmValue);overload;
begin
  if Dest in CPUReg8Bit then
    AsmOpcode('ld',CPURegStrings[Dest], Source.ToStringByte)
  else if Dest in CPUReg16Bit then
    AsmOpcode('ld',CPURegStrings[Dest], Source.ToStringWord)
  else
    Assert(False);
end;

procedure OpMOV(Dest: TCPUReg;Value: Integer);overload;
begin
  if Dest in CPUReg8Bit then
    AsmOpcode('ld',CPURegStrings[Dest],ByteToStr(Value))
  else
    AsmOpcode('ld',CPURegStrings[Dest],WordToStr(Value));
end;

procedure OpSTO(DestXY: TCPUReg;DestV: PVariable;Source: TCPUReg;ByteIndex: Integer = 0);overload;
begin
  Assert(DestXY in [rIX, rIY]);
  Assert(DestV.AddrMode in [amStack, amStackRef]);

  if Source in CPURegPairs then
  begin
    AsmOpcode('ld',OffsetToStr(DestXY, DestV, ByteIndex), CPURegStrings[CPURegPairToLow[Source]]);
    AsmOpcode('ld',OffsetToStr(DestXY, DestV, ByteIndex +1), CPURegStrings[CPURegPairToHigh[Source]]);
  end
  else
    AsmOpcode('ld',OffsetToStr(DestXY, DestV, ByteIndex), CPURegStrings[Source]);
end;

procedure OpSTO(DestXY: TCPUReg;DestV: PVariable;Value: Integer;ByteIndex: Integer = 0);overload;
begin
  Assert(DestXY in [rIX, rIY]);
  Assert(DestV.AddrMode = amStack);
  Assert(Value in [0..255]);

  AsmOpcode('ld',OffsetToStr(DestXY, DestV, ByteIndex), ByteToStr(Value));
end;

procedure OpSTO(DestV: PVariable;Source: TCPUReg);overload;
begin
  Assert(DestV.AddrMode in [amStatic, amStaticRef]);
  AsmOpcode('ld', '('+DestV.GetAsmName+')', CPURegStrings[Source]);
end;

procedure OpSTO(DestV: PVariable;ByteIndex: Integer;Source: TCPUReg);overload;
var S: String;
begin
  Assert(DestV.AddrMode = amStatic);

  S := '('+DestV.GetAsmName;
  if ByteIndex <> 0 then
    S := S +' +' +ByteIndex.ToString;
  S := S + ')';
  AsmOpcode('ld', S, CPURegStrings[Source]);
end;

//LD (<dest>),<source> where dest is a 16-bit and source is an 8-bit
procedure OpSTO(Dest, Source: TCPUReg);
var D: String;
begin
  Assert(Source in CPUReg8Bit);
  Assert(Dest in CPUReg16Bit);
  Assert((Source = rA) or (Dest = rHL));  //Unacceptable combinations

  D := '(' + CPURegStrings[Dest] + ')';
  AsmOpcode('ld', D, CPURegStrings[Source]);
end;

procedure OpSTO(Dest: TCPUReg; Source: TImmValue);overload;
var D: String;
begin
  Assert(Dest = rHL);

  D := '(' + CPURegStrings[Dest] + ')';
  AsmOpcode('ld', D, Source.ToStringByte);
end;

procedure OpSTO(Dest: TCPUReg; Value: Integer);overload;
var D: String;
begin
  Assert(Dest = rHL);
  Assert(Value in [0..255]);

  D := '(' + CPURegStrings[Dest] + ')';
  AsmOpcode('ld', D, ByteToStr(Value));
end;

procedure OpLDIR;
begin
  AsmOpcode('ldir');
end;

procedure OpPUSH(Reg: TCPUReg);
begin
  Assert(Reg in (CPUReg16Bit + [rAF]));
  AsmOpcode('push',CPURegStrings[Reg]);
end;

procedure OpPOP(Reg: TCPUReg);
begin
  Assert(Reg in (CPUReg16Bit + [rAF]));
  AsmOpcode('pop',CPURegStrings[Reg]);
end;

procedure OpEXHLDE;
begin
  AsmInstr('ex hl,de');
end;

procedure OpADD(RAcc, RAdd: TCPUReg);
begin
  AsmOpcode('add',CPURegStrings[RAcc],CPURegStrings[RAdd]);
end;

procedure OpADDHLSP;
begin
  AsmOpcode('add','hl','sp');
end;

procedure OpAND(Reg: TCPUReg);
begin
  AsmOpcode('and',CPURegStrings[Reg]);
end;

procedure OpCP(Value: Integer);
begin
  AsmOpcode('cp',ByteToStr(Value));
end;

procedure OpDEC(Reg: TCPUReg);
begin
  AsmOpcode('dec',CPURegStrings[Reg]);
end;

procedure OpINC(Reg: TCPUReg);
begin
  AsmOpcode('inc',CPURegStrings[Reg]);
end;

procedure OpOR(Reg: TCPUReg);
begin
  AsmOpcode('or',CPURegStrings[Reg]);
end;

procedure OpSBC(RAcc, RSub: TCPUReg);
begin
  AsmOpcode('sbc',CPURegStrings[RAcc],CPURegStrings[RSub]);
end;


end.

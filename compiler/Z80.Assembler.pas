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


//Convenience routines to generate specific opcodes
procedure OpLD(Dest, Source: TCPUReg);overload;
//Converts Source to 8 or 16-bit string depending on the Dest register size.
//If source is a Char, might also be converted to characters literal
procedure OpLD(Dest: TCPUReg; Source: TImmValue);overload;
procedure OpLD(Dest: TCPUReg;Value: Integer);overload;
//To/from index register - can move both 8 and 16 bit values
procedure OpLD(DestXY: TCPUReg;DestV: PVariable;Source: TCPUReg;ByteIndex: Integer = 0);overload;
procedure OpLD(Dest, SourceXY: TCPUReg;SourceV: PVariable;ByteIndex: Integer = 0);overload;
//To/from variable
procedure OpLD(DestV: PVariable;Source: TCPUReg);overload;
procedure OpLD(Dest: TCPUReg;SourceV: PVariable);overload;
//ByteIndex allows to specify high or low byte. Reg must be 8-bit
procedure OpLD(Dest: TCPUReg;SourceV: PVariable;ByteIndex: Integer);overload;
procedure OpLD(DestV: PVariable;ByteIndex: Integer;Source: TCPUReg);overload;
procedure OpLD(Dest: TCPUReg;Source: String);overload;

//Where Dest is 8-bit and Source is 16-bit (pointer)
procedure OpLDFromIndirect(Dest, Source: TCPUReg);//overload;
//Where Dest is 16-bit (pointer) and Source is 8-bit
procedure OpLDToIndirect(Dest, Source: TCPUReg);//overload;

procedure OpPUSH(Reg: TCPUReg);
procedure OpPOP(Reg: TCPUReg);

procedure OpEXHLDE;

procedure OpADD(RAcc, RAdd: TCPUReg);
procedure OpINC(Reg: TCPUReg);
procedure OpSBC(RAcc, rSub: TCPUReg);
procedure OpDEC(Reg: TCPUReg);

procedure OpcodeOR(Reg: TCPUReg);

procedure OpANDA;

implementation
uses SysUtils,
  CG.Data;


function OffsetToStr(Reg: TCPUReg;Variable: PVariable;ByteIndex: Integer = 0): String;
begin
  Assert(Variable.AddrMode = amStack);

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

procedure OpLD(Dest, Source: TCPUReg);overload;
begin
  AsmOpcode('ld',CPURegStrings[Dest],CPURegStrings[Source]);
end;

procedure OpLD(Dest: TCPUReg; Source: TImmValue);overload;
begin
  if Dest in CPUReg8Bit then
    AsmOpcode('ld',CPURegStrings[Dest], Source.ToStringByte)
  else if Dest in CPUReg16Bit then
    AsmOpcode('ld',CPURegStrings[Dest], Source.ToStringWord)
  else
    Assert(False);
end;

procedure OpLD(Dest: TCPUReg;Value: Integer);overload;
begin
  if Dest in CPUReg8Bit then
    AsmOpcode('ld',CPURegStrings[Dest],ByteToStr(Value))
  else
    AsmOpcode('ld',CPURegStrings[Dest],WordToStr(Value));
end;

procedure OpLD(DestXY: TCPUReg;DestV: PVariable;Source: TCPUReg;ByteIndex: Integer = 0);overload;
begin
  Assert(DestXY in [rIX, rIY]);
  Assert(DestV.AddrMode = amStack);

  if Source in CPURegPairs then
  begin
    AsmOpcode('ld',OffsetToStr(DestXY, DestV, ByteIndex), CPURegStrings[CPURegPairToLow[Source]]);
    AsmOpcode('ld',OffsetToStr(DestXY, DestV, ByteIndex +1), CPURegStrings[CPURegPairToHigh[Source]]);
  end
  else
    AsmOpcode('ld',OffsetToStr(DestXY, DestV, ByteIndex), CPURegStrings[Source]);
end;

procedure OpLD(Dest, SourceXY: TCPUReg;SourceV: PVariable;ByteIndex: Integer = 0);overload;
begin
  Assert(SourceXY in [rIX, rIY]);
  Assert(SourceV.AddrMode = amStack);

  if Dest in CPURegPairs then
  begin
    AsmOpcode('ld', CPURegStrings[CPURegPairToLow[Dest]], OffsetToStr(SourceXY, SourceV, ByteIndex));
    AsmOpcode('ld', CPURegStrings[CPURegPairToHigh[Dest]], OffsetToStr(SourceXY, SourceV, ByteIndex +1));
  end
  else
    AsmOpcode('ld', CPURegStrings[Dest], OffsetToStr(SourceXY, SourceV, ByteIndex));
end;

procedure OpLD(DestV: PVariable;Source: TCPUReg);overload;
begin
  Assert(DestV.AddrMode = amStatic);
  AsmOpcode('ld', '('+DestV.GetAsmName+')', CPURegStrings[Source]);
end;

procedure OpLD(Dest: TCPUReg;SourceV: PVariable);overload;
begin
  Assert(SourceV.AddrMode = amStatic);
  AsmOpcode('ld', CPURegStrings[Dest], '('+SourceV.GetAsmName+')');
end;

procedure OpLD(Dest: TCPUReg;SourceV: PVariable;ByteIndex: Integer);overload;
var S: String;
begin
  Assert(SourceV.AddrMode = amStatic);
  S := '('+SourceV.GetAsmName;
  if ByteIndex <> 0 then
    S := S +' +' +ByteIndex.ToString;
  S := S + ')';
  AsmOpcode('ld', CPURegStrings[Dest], S);
end;

procedure OpLD(DestV: PVariable;ByteIndex: Integer;Source: TCPUReg);overload;
var S: String;
begin
  Assert(DestV.AddrMode = amStatic);

  S := '('+DestV.GetAsmName;
  if ByteIndex <> 0 then
    S := S +' +' +ByteIndex.ToString;
  S := S + ')';
  AsmOpcode('ld', S, CPURegStrings[Source]);
end;

procedure OpLD(Dest: TCPUReg;Source: String);overload;
begin
  AsmOpcode('ld', CPURegStrings[Dest], Source);
end;

//LD <dest>,(<source>) where dest is an 8-bit and source is a 16-bit
procedure OpLDFromIndirect(Dest, Source: TCPUReg);
var S: String;
begin
  Assert(Source in CPUReg16Bit);
  Assert(Dest in CPUReg8Bit);
  Assert((Dest = rA) or (Source = rHL));  //Unacceptable combinations

  S := '(' + CPURegStrings[Source] + ')';
  AsmOpcode('ld', CPURegStrings[Dest], S);
end;

//LD (<dest>),<source> where dest is a 16-bit and source is an 8-bit
procedure OpLDToIndirect(Dest, Source: TCPUReg);
var D: String;
begin
  Assert(Source in CPUReg8Bit);
  Assert(Dest in CPUReg16Bit);
  Assert((Source = rA) or (Dest = rHL));  //Unacceptable combinations

  D := '(' + CPURegStrings[Dest] + ')';
  AsmOpcode('ld', D, CPURegStrings[Source]);
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

procedure OpINC(Reg: TCPUReg);
begin
  AsmOpcode('inc',CPURegStrings[Reg]);
end;

procedure OpSBC(RAcc, RSub: TCPUReg);
begin
  AsmOpcode('sbc',CPURegStrings[RAcc],CPURegStrings[RSub]);
end;

procedure OpDEC(Reg: TCPUReg);
begin
  AsmOpcode('dec',CPURegStrings[Reg]);
end;

procedure OpcodeOR(Reg: TCPUReg);
begin
  AsmOpcode('or',CPURegStrings[Reg]);
end;

procedure OpANDA;
begin
  AsmOpcode('and','a');
end;

end.

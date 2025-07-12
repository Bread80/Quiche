unit Z80.LoadStoreMove.Test;

interface

implementation
uses TestFrameWork,
  Def.IL, Def.QTypes, Def.Consts, Def.Scopes, Def.Variables, Def.UserTypes,
  Lib.Data,
  CodeGen, CG.Data,
  Z80.LoadStoreMove, Z80.Load, Z80.Store,
  Z80.CPUState, Z80.Hardware, Z80.GenProcs;


type TTestZ80LoadStoreMove = class(TTestCase)
	protected
		procedure SetUp;override;
		procedure TearDown;override;
  end;

type TTestZ80LoadLiteral = class(TTestZ80LoadStoreMove)
  protected
    //Loading A with zero
    procedure DoTestLoadRegLiteral(Reg: TCPUReg;Value: TImmValue;Options: TMoveOptionSet;
      InState,
      OutCode, OutState: String);
	published
    //Loading literals into 8 bit registers
		procedure TestLoadRegLiteral8Bit;
    //Hacks load values into 8 bit registers
		procedure TestLoadRegLiteral8BitHacks;
    //Loading zero into A reg
    procedure TestLoadRegLiteralAZero;
    //Other hacks using A reg
    procedure TestLoadRegLiteralAHacks;
    //Literals into 16 bit registers
    procedure TestLoadRegLiteral16Bit;
    //Hacks to load values into 16-bit registers
    procedure TestLoadRegLiteral16BitHacks;
    //Literals into flags
    procedure TestLoadRegLiteralFlags;
end;

type TTestZ80LoadVariable = class(TTestZ80LoadStoreMove)
  protected
    procedure DoTestLoadVariable(Reg: TCPUReg;LoadType: TLoadParamType;Options: TMoveOptionSet;
      VarType: TVarType;VarAddrMode: TAddrMode;InState: String;
      RangeCheck: Boolean; ToType: TVarType;
      OutCode, OutState: String);
  published
    //Load static variable value into register
    procedure TestLoadRegStatic16;
    procedure TestLoadRegStatic8;
    procedure TestLoadRegStaticExtend;
    procedure TestLoadRegStaticHighOrLow;
    //Load stack variable value into register
    procedure TestLoadRegStack16;
    procedure TestLoadRegStack8;
    procedure TestLoadRegStackExtend;
    procedure TestLoadRegStackHighOrLow;
    //Scavenge variable values from other registers
    procedure TestLoadRegScavenge16;
    procedure TestLoadRegScavenge8;
    procedure TestLoadRegScavengeExtend;
    procedure TestLoadRegScavengeHighOrLow;

    //Load variable with type conversion and  range checking
    procedure TestLoadRegStaticConvertTo8Bit;
    procedure TestLoadRegStaticConvertTo16Bit;

    procedure TestLoadRegStackConvertTo8Bit;
    procedure TestLoadRegStackConvertTo16Bit;

    procedure TestLoadRegScavengeConvertTo8Bit;
    procedure TestLoadRegScavengeConvertTo16Bit;
end;

type TTestZ80StoreVariable = class(TTestZ80LoadStoreMove)
  protected
    procedure DoTestStoreVariable(Reg: TCPUReg;Options: TMoveOptionSet;
      RangeCheck: Boolean; FromType: TVarType;
      VarType: TVarType;VarAddrMode: TAddrMode;InState: String;
      OutCode, OutState: String);
  published
    //Store register value into static variable value
    procedure TestStoreRegStatic16;
    procedure TestStoreRegStatic8;
    //Extend with no range checking
    procedure TestStoreRegStaticExtend;
    //Store register value into  stack variable
    procedure TestStoreRegStack16;
    procedure TestStoreRegStack8;
    //Extend with no range checking
    procedure TestStoreRegStackExtend;

    //TODO: Store values from flags
    //TODO: Other result types (ie conditional branches, push etc.)

    //Store value with type conversion and range checking
    procedure TestStoreRegStaticConvertTo8Bit;
    procedure TestStoreRegStaticConvertTo16Bit;

    procedure TestStoreRegStackConvertTo8Bit;
    procedure TestStoreRegStackConvertTo16Bit;
end;


{ TTestZ80LoadStoreMove }

procedure TTestZ80LoadStoreMove.Setup;
begin
  inherited;
  InitialiseCodeGen('','');
  RegStateInitialise;
end;

procedure TTestZ80LoadStoreMove.TearDown;
begin
  inherited;

end;

{ TTestZ80LoadLiteral }

procedure TTestZ80LoadLiteral.DoTestLoadRegLiteral(Reg: TCPUReg;
  Value: TImmValue; Options: TMoveOptionSet; InState, OutCode, OutState: String);
var Code: String;
  State: String;
  Compare: String;
begin
  Setup;
  CPUStringToState(InState);
  RegStateClearModified;
  GenLoadRegLiteral(Reg, Value, Options);
  Code := PeekAssembly;
  State := CPUStateToString;
  Check(Code = OutCode, 'Expected ' + OutCode + ', got ' + Code + ' with ' + State);
  Compare := CPUStateCompare(OutState);
  Check(Compare = '', Compare + 'Expected ' + OutState + ' got ' + State + ' with ' + Code);
end;

procedure TTestZ80LoadLiteral.TestLoadRegLiteral16Bit;
begin
  //No optimisations
  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($1234), [],
    '','ld bc,$1234','bc:!$1234 b:!$12 c:!$34');
  DoTestLoadRegLiteral(rDE, TImmValue.CreateInteger($5678), [],
    '','ld de,$5678','de:!$5678 d:!$56 e:!$78');
  DoTestLoadRegLiteral(rHL, TImmValue.CreateInteger($9abc), [],
    '','ld hl,$9abc','hl:!$9abc h:!$9a l:!$bc');
  DoTestLoadRegLiteral(rIX, TImmValue.CreateInteger($def0), [],
    '','ld ix,$def0','ix:!$def0');
  DoTestLoadRegLiteral(rIY, TImmValue.CreateInteger($1234), [],
    '','ld iy,$1234','iy:!$1234');

  //Already loaded
  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($1234), [],
    'bc:$1234','','bc:_$1234 b:_$12 c:_$34');
  DoTestLoadRegLiteral(rDE, TImmValue.CreateInteger($5678), [],
    'de:$5678','','de:_$5678 d:_$56 e:_$78');
  DoTestLoadRegLiteral(rHL, TImmValue.CreateInteger($9abc), [],
    'hl:$9abc','','hl:_$9abc h:_$9a l:_$bc');
  DoTestLoadRegLiteral(rIX, TImmValue.CreateInteger($def0), [],
    'ix:$def0','','ix:_$def0');
  DoTestLoadRegLiteral(rIY, TImmValue.CreateInteger($1234), [],
    'iy:$1234','','iy:_$1234');
end;

procedure TTestZ80LoadLiteral.TestLoadRegLiteral16BitHacks;
begin
  //Moves
  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($1234), [],
    'de:$1234','ld c,e:ld b,d','bc:!$1234 b:!$12 c:!$34');
  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($1234), [],
    'bc:$3412','ld bc,$1234','bc:!$1234 b:!$12 c:!$34');  //Swapping pairs will fail
  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($1234), [],
    'b:$12 de:$1234','ld c,e','bc:!$1234 b:!$12 c:!$34');  //One half already loaded
  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($1234), [],
    'c:$34 de:$1234','ld b,d','bc:!$1234 b:!$12 c:!$34');  //Other half already loaded
  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($1234), [],
    'c:$12 de:$1234','ld b,c:ld c,e','bc:!$1234 b:!$12 c:!$34');  //Wrong half already loaded
  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($1234), [],
    'b:$34 de:$1234','ld c,b:ld b,d','bc:!$1234 b:!$12 c:!$34');  //Other wrong half already loaded
  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($1212), [],
    'a:$12','ld c,a:ld b,a','bc:!$1212 b:!$12 c:!$12'); //Both halves same value

  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($1234), [],
    'b:$11 de:$1234','ld c,e:ld b,d','bc:!$1234 b:!$12 c:!$34');  //Move preferred over INC/DEC
  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($1234), [],
    'c:$35 de:$1234','ld c,e:ld b,d','bc:!$1234 b:!$12 c:!$34');  //Move preferred over INC/DEC

  //EX HL,DE
  DoTestLoadRegLiteral(rHL, TImmValue.CreateInteger($1234), [],
    'de:$1234','ex hl,de','hl:!$1234 de:!? h:!$12 l:!$34 d:!? e:!?');
  DoTestLoadRegLiteral(rDE, TImmValue.CreateInteger($1234), [],
    'hl:$1234','ex hl,de','de:!$1234 hl:!? d:!$12 e:!$34 h:!? l:!?');
  DoTestLoadRegLiteral(rHL, TImmValue.CreateInteger($1234), [moPreserveHLDE],
    'de:$1234','ld l,e:ld h,d','hl:!$1234 de:_$1234 h:!$12 l:!$34 d:_$12 e:_$34'); //Not allowed
  DoTestLoadRegLiteral(rDE, TImmValue.CreateInteger($1234), [moPreserveHLDE],
    'hl:$1234','ld e,l:ld d,h','de:!$1234 hl:_$1234 d:!$12 e:!$34 h:_$12 l:_$34'); //Not allowed
  DoTestLoadRegLiteral(rHL, TImmValue.CreateInteger($1234), [],
    'de:$1234 hl:$5678','ex hl,de','hl:!$1234 de:!$5678 h:!$12 l:!$34 d:!$56 e:!$78');  //Other reg moved

  //INC
  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($1200), [],
    'bc:$11ff','inc bc','bc:!$1200 b:!$12 c:!$00'); //Rollover to high bit will be INC rr
  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($1234), [],
    'bc:$1233','inc c','bc:!$1234 b:!$12 c:!$34'); //Inc high byte only
  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($1234), [moPreserveOtherFlags],
    'bc:$1233','inc bc','bc:!$1234 b:!$12 c:!$34'); //16-bit if preserve
  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($1234), [moPreserveCF],
    'bc:$1233','inc c','bc:!$1234 b:!$12 c:!$34'); //8-bit if preserve CF only
  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($1234), [],
    'bc:$1134','inc b','bc:!$1234 b:!$12 c:!$34'); //INC high byte
  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($1234), [],
    'bc:$1133','inc c:inc b','bc:!$1234 b:!$12 c:!$34'); //INC both
  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($1234), [moPreserveOtherFlags],
    'bc:$1133','ld bc,$1234','bc:!$1234 b:!$12 c:!$34'); //Preserve, can't INC

  //DEC
  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($11ff), [],
    'bc:$1200','dec bc','bc:!$11ff b:!$11 c:!$ff'); //Rollover to high bit will be DEC rr
  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($1234), [],
    'bc:$1235','dec c','bc:!$1234 b:!$12 c:!$34'); //Dec high byte only
  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($1234), [moPreserveOtherFlags],
    'bc:$1235','dec bc','bc:!$1234 b:!$12 c:!$34'); //16-bit if preserve
  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($1234), [moPreserveCF],
    'bc:$1235','dec c','bc:!$1234 b:!$12 c:!$34'); //8-bit if preserve CF only
  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($1234), [],
    'bc:$1334','dec b','bc:!$1234 b:!$12 c:!$34'); //DEC high byte
  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($1234), [],
    'bc:$1335','dec c:dec b','bc:!$1234 b:!$12 c:!$34'); //DEC both
  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($1234), [moPreserveOtherFlags],
    'bc:$1335','ld bc,$1234','bc:!$1234 b:!$12 c:!$34'); //Preserve, can't DEC

  //Combos
  //If either half has to be loaded then don't optimise other - load both (unless other remains)
  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($1234), [],
    'bc:$ff35','ld bc,$1234','bc:!$1234 b:!$12 c:!$34'); //Don't DEC
  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($1234), [],
    'bc:$13ff','ld bc,$1234','bc:!$1234 b:!$12 c:!$34'); //Don't DEC
  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($1234), [],
    'bc:$ffff a:$12','ld bc,$1234','bc:!$1234 b:!$12 c:!$34'); //Don't move
  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($1234), [],
    'bc:$ffff a:$34','ld bc,$1234','bc:!$1234 b:!$12 c:!$34'); //Don't move

  //Combo move and inc or dec
  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($1234), [],
    'bc:$ff35 a:$12','dec c:ld b,a','bc:!$1234 b:!$12 c:!$34'); //DEC and move
  DoTestLoadRegLiteral(rBC, TImmValue.CreateInteger($1234), [],
    'bc:$11ff a:$34','ld c,a:inc b','bc:!$1234 b:!$12 c:!$34'); //INC and move
end;

procedure TTestZ80LoadLiteral.TestLoadRegLiteral8Bit;
begin
  //No optimisations
  DoTestLoadRegLiteral(rB, TImmValue.CreateInteger(0), [],
    '','ld b,$00','b:!$00 bc:!?');
  DoTestLoadRegLiteral(rC, TImmValue.CreateInteger(0), [],
    '','ld c,$00','C:!$00 bc:!?');
  DoTestLoadRegLiteral(rD, TImmValue.CreateInteger(0), [],
    '','ld d,$00','D:!$00 de:!?');
  DoTestLoadRegLiteral(rE, TImmValue.CreateInteger(0), [],
    '','ld e,$00','E:!$00 de:!?');
  DoTestLoadRegLiteral(rH, TImmValue.CreateInteger(0), [],
    '','ld h,$00','H:!$00 hl:!?');
  DoTestLoadRegLiteral(rL, TImmValue.CreateInteger(0), [],
    '','ld l,$00','L:!$00 HL:!?');

  //Move from another reg
  DoTestLoadRegLiteral(rB, TImmValue.CreateInteger(0), [],
    'c:0','ld b,c','B:!$00 C:_$00 bc:!0');
  DoTestLoadRegLiteral(rC, TImmValue.CreateInteger(0), [],
    'd:0','ld c,d','C:!$00 D:_$00 bc:!?');
  DoTestLoadRegLiteral(rD, TImmValue.CreateInteger(0), [],
    'e:0','ld d,e','D:!$00 E:_$00 de:!0');
  DoTestLoadRegLiteral(rE, TImmValue.CreateInteger(0), [],
    'h:0','ld e,h','E:!$00 H:_$00 de:!?');
  DoTestLoadRegLiteral(rH, TImmValue.CreateInteger(0), [],
    'l:0','ld h,l','H:!$00 L:_$00 hl:!0');
  DoTestLoadRegLiteral(rL, TImmValue.CreateInteger(0), [],
    'a:0','ld l,a','L:!$00 A:_$00 hl:!?');

  //No optimisations
  DoTestLoadRegLiteral(rB, TImmValue.CreateInteger(10), [],
    '','ld b,$0a','b:!$0a bc:!?');
  DoTestLoadRegLiteral(rC, TImmValue.CreateInteger(10), [],
    '','ld c,$0a','C:!$0a bc:!?');
  DoTestLoadRegLiteral(rD, TImmValue.CreateInteger(10), [],
    '','ld d,$0a','D:!$0a de:!?');
  DoTestLoadRegLiteral(rE, TImmValue.CreateInteger(10), [],
    '','ld e,$0a','E:!$0a de:!?');
  DoTestLoadRegLiteral(rH, TImmValue.CreateInteger(10), [],
    '','ld h,$0a','H:!$0a hl:!?');
  DoTestLoadRegLiteral(rL, TImmValue.CreateInteger(10), [],
    '','ld l,$0a','L:!$0a hl:!?');

  //Move from another reg
  DoTestLoadRegLiteral(rB, TImmValue.CreateInteger(10), [],
    'c:10','ld b,c','B:!$0a C:_$0a bc:!$0a0a');
  DoTestLoadRegLiteral(rC, TImmValue.CreateInteger(10), [],
    'd:10','ld c,d','C:!$0a D:_$0a bc:!?');
  DoTestLoadRegLiteral(rD, TImmValue.CreateInteger(10), [],
    'e:10','ld d,e','D:!$0a E:_$0a de:!$0a0a');
  DoTestLoadRegLiteral(rE, TImmValue.CreateInteger(10), [],
    'h:10','ld e,h','E:!$0a H:_$0a de:!?');
  DoTestLoadRegLiteral(rH, TImmValue.CreateInteger(10), [],
    'l:10','ld h,l','H:!$0a L:_$0a hl:!$0a0a');
  DoTestLoadRegLiteral(rL, TImmValue.CreateInteger(10), [],
    'a:10','ld l,a','L:!$0a A:_$0a hl:!?');

  DoTestLoadRegLiteral(rD, TImmValue.CreateInteger($80), [],
    'de:$7f80','ld d,e','D:!$80 de:!$8080');

  //Already set, nothing to do
  DoTestLoadRegLiteral(rB, TImmValue.CreateInteger(10), [],
    'b:10','','B:_$0a');
  DoTestLoadRegLiteral(rC, TImmValue.CreateInteger(10), [],
    'c:10','','C:_$0a');
  DoTestLoadRegLiteral(rD, TImmValue.CreateInteger(10), [],
    'd:10','','D:_$0a');
  DoTestLoadRegLiteral(rE, TImmValue.CreateInteger(10), [],
    'e:10','','E:_$0a');
  DoTestLoadRegLiteral(rH, TImmValue.CreateInteger(10), [],
    'h:10','','H:_$0a');
  DoTestLoadRegLiteral(rL, TImmValue.CreateInteger(10), [],
    'l:10','','l:_$0a');
end;

procedure TTestZ80LoadLiteral.TestLoadRegLiteral8BitHacks;
begin
  //INC
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($f1), [moPreserveCF],
    'a:$f0','inc a','A:!$f1 CF:_ Flags:!?');
  DoTestLoadRegLiteral(rB, TImmValue.CreateInteger(1), [moPreserveCF],
    'b:0','inc b','B:!1 bc:!? CF:_ Flags:!?');
  DoTestLoadRegLiteral(rC, TImmValue.CreateInteger(0), [],
    'c:$ff','inc c','C:!0 bc:!? CF:_ Flags:!?');
  DoTestLoadRegLiteral(rD, TImmValue.CreateInteger($80), [],
    'de:$7f81','inc d','D:!$80 de:!$8081 CF:_ Flags:!?');
  DoTestLoadRegLiteral(rE, TImmValue.CreateInteger($81), [],
    'e:$80','inc e','E:!$81 de:!? CF:_ Flags:!?');
  DoTestLoadRegLiteral(rH, TImmValue.CreateInteger(2), [],
    'h:1 l:0','inc h','H:!2 hl:!$0200 CF:_ Flags:!?');
  DoTestLoadRegLiteral(rL, TImmValue.CreateInteger($82), [],
    'l:$81','inc l','L:!$82 hl:!? CF:_ Flags:!?');

  DoTestLoadRegLiteral(rB, TImmValue.CreateInteger(1), [moPreserveOtherFlags],
    'b:0','ld b,$01','B:!1 bc:!? CF:_ Flags:_');

  //DEC
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($f1), [moPreserveCF],
    'a:$f2','dec a','A:!$f1 CF:_ Flags:!?');
  DoTestLoadRegLiteral(rB, TImmValue.CreateInteger($ff), [moPreserveCF],
    'b:0','dec b','B:!$ff bc:!? CF:_ Flags:!?');
  DoTestLoadRegLiteral(rC, TImmValue.CreateInteger($fe), [],
    'c:$ff','dec c','C:!$fe bc:!? CF:_ Flags:!?');
  DoTestLoadRegLiteral(rD, TImmValue.CreateInteger($7e), [],
    'de:$7f81','dec d','D:!$7e de:!$7e81 CF:_ Flags:!?');
  DoTestLoadRegLiteral(rE, TImmValue.CreateInteger($7f), [],
    'e:$80','dec e','E:!$7f de:!? CF:_ Flags:!?');
  DoTestLoadRegLiteral(rH, TImmValue.CreateInteger(0), [],
    'h:1 l:2','dec h','H:!0 hl:!$0002 CF:_ Flags:!?');
  DoTestLoadRegLiteral(rL, TImmValue.CreateInteger($80), [],
    'l:$81','dec l','L:!$80 hl:!? CF:_ Flags:!?');

  DoTestLoadRegLiteral(rB, TImmValue.CreateInteger($ff), [moPreserveOtherFlags],
    'b:0','ld b,$ff','B:!$ff bc:!? CF:_ Flags:_');
end;

procedure TTestZ80LoadLiteral.TestLoadRegLiteralAHacks;
begin
  //CPL
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($f0), [],
    'a:$0f','cpl','A:!$f0 CF:_ Flags:_');
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($f0), [moPreserveCF],
    'a:$0f','cpl','A:!$f0 CF:_ Flags:_');
  //Only trashes H and N - we don't care about those
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($f0), [moPreserveOtherFlags],
    'a:$0f','cpl','A:!$f0 CF:_ Flags:_');

  //RLA
  //!!RLA requires CF be a literal value
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($f0), [moPreserveOtherFlags],
    'a:$78 cf:0','rla','A:!$f0 CF:!0 Flags:_'); //Test CF out
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($f1), [moPreserveOtherFlags],
    'a:$78 cf:1','rla','A:!$f1 CF:!0 Flags:_'); //Test CF out
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($f0), [moPreserveCF],
    'a:$78','ld a,$f0','A:!$f0 CF:_ Flags:_');  //Fail when unknown CF in
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($f0), [moPreserveCF, moPreserveOtherFlags],
    'a:$78 CF:0','rla','A:!$f0 CF:!0 Flags:_'); //Preserve CF in when CF=0
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($f1), [moPreserveCF, moPreserveOtherFlags],
    'a:$f8 CF:1','rla','A:!$f1 CF:!1 Flags:_'); //Preserv CF in when CF=1
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($f1), [moPreserveCF, moPreserveOtherFlags],
    'a:$f8 CF:0','ld a,$f1','A:!$f1 CF:_ Flags:_');  //Fail when incorrect CF in

  //rRRA
  //!!RLA requires CF be a literal value
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($f0), [],
    'a:$e0 cf:1','rra','A:!$f0 CF:!0 Flags:_'); //Test CF out
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($f0), [],
    'a:$e1 cf:1','rra','A:!$f0 CF:!1 Flags:_'); //Test CF out
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($f0), [moPreserveCF],
    'a:$e0 cf:?','ld a,$f0','A:!$f0 CF:_ Flags:_');  //Unknown CF in
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($f0), [moPreserveCF, moPreserveOtherFlags],
    'a:$e1 CF:1','rra','A:!$f0 CF:!1 Flags:_');
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($70), [moPreserveCF, moPreserveOtherFlags],
    'a:$e0 CF:0','rra','A:!$70 CF:!0 Flags:_');
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($f0), [moPreserveCF, moPreserveOtherFlags],
    'a:$e0 CF:1','ld a,$f0','A:!$f0 CF:_ Flags:_');

  //RLCA
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($f0), [],
    'a:$78 cf:?','rlca','A:!$f0 CF:!0 Flags:_');  //CF out 0
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($f1), [moPreserveOtherFlags],
    'a:$f8 cf:?','rlca','A:!$f1 CF:!1 Flags:_');  //CF out 1
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($f0), [moPreserveCF],
    'a:$78 cf:?','ld a,$f0','A:!$f0 CF:_ Flags:_');  //Unknown CF in fails
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($f0), [moPreserveCF, moPreserveOtherFlags],
    'a:$78 CF:1','ld a,$f0','A:!$f0 CF:_ Flags:_');  //Can't preserve
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($f1), [moPreserveCF, moPreserveOtherFlags],
    'a:$f8 CF:0','ld a,$f1','A:!$f1 CF:_ Flags:_');  //Can't presereve

  //RRCA
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($f0), [],
    'a:$e1 cf:?','rrca','A:!$f0 CF:!1 Flags:_');  //CF out 1
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($70), [moPreserveOtherFlags],
    'a:$e0 cf:?','rrca','A:!$70 CF:!0 Flags:_');  //CF out 0
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($f0), [moPreserveCF],
    'a:$e1 cf:?','ld a,$f0','A:!$f0 CF:_ Flags:_');  //Unknown CF in fails
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($70), [moPreserveCF, moPreserveOtherFlags],
    'a:$e0 CF:1','ld a,$70','A:!$70 CF:_ Flags:_');  //Can't preserve
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($e0), [moPreserveCF, moPreserveOtherFlags],
    'a:$e1 CF:0','ld a,$e0','A:!$e0 CF:_ Flags:_');  //Can't presereve

  //ADD
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($33), [],
    'a:$11 b:$22','add a,b','A:!$33 CF:!0 Flags:!?');  //CF out = 0
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($33), [moPreserveCF],
    'a:$11 b:$22','ld a,$33','A:!$33 CF:_ Flags:_');  //Nope
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($33), [moPreserveOtherFlags],
    'a:$11 b:$22','ld a,$33','A:!$33 CF:_ Flags:_');  //Nope

  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($03), [],
    'a:$f0 b:$13','add a,b','A:!$03 CF:!1 Flags:!?');  //CF out = 1

  //ADC
  //IF CF is 0 we'll see an ADD, so don't test that option
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($33), [],
    'a:$11 b:$21 cf:1','adc a,b','A:!$33 CF:!0 Flags:!?');  //CF out = 0
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($33), [moPreserveCF],
    'a:$11 b:$21 cf:1','ld a,$33','A:!$33 CF:_ Flags:_');  //Nope
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($33), [moPreserveOtherFlags],
    'a:$11 b:$21 cf:1','ld a,$33','A:!$33 CF:_ Flags:_');  //Nope

  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($03), [],
    'a:$f0 b:$12 cf:1','adc a,b','A:!$03 CF:!1 Flags:!?');  //CF out = 1

  //SUB
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($33), [],
    'a:$44 b:$11','sub b','A:!$33 CF:!0 Flags:!?');  //CF out = 0
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($33), [moPreserveCF],
    'a:$44 b:$11','ld a,$33','A:!$33 CF:_ Flags:_');  //Nope
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($33), [moPreserveOtherFlags],
    'a:$44 b:$11','ld a,$33','A:!$33 CF:_ Flags:_');  //Nope

  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($f0), [],
    'a:$10 b:$20','sub b','A:!$f0 CF:!1 Flags:!?');  //CF out = 1

  //SBC
  //IF CF is 0 we'll see an SUB, so don't test that option
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($33), [],
    'a:$44 b:$10 cf:1','sbc a,b','A:!$33 CF:!0 Flags:!?');  //CF out = 0
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($33), [moPreserveCF],
    'a:$44 b:$10 cf:1','ld a,$33','A:!$33 CF:_ Flags:_');  //Nope
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($33), [moPreserveOtherFlags],
    'a:$44 b:$10 cf:1','ld a,$33','A:!$33 CF:_ Flags:_');  //Nope

  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger($f0), [],
    'a:$11 b:$20 cf:1','sbc a,b','A:!$f0 CF:!1 Flags:!?');  //CF out = 1
end;

procedure TTestZ80LoadLiteral.TestLoadRegLiteralAZero;
begin
  //XOR A
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger(0), [],
    '','xor a','A:!$00 CF:!0 Flags:!?');

  //Boring fallback preserves all flags
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger(0), [moPreserveOtherFlags],
    '','ld a,$00','A:!$00 CF:_ Flags:_');

  //Copy from another reg
  DoTestLoadRegLiteral(rA, TImmValue.CreateInteger(0), [],
    'b:$00','ld a,b','A:!$00 B:$00 CF:_ Flags:_');
 end;

procedure TTestZ80LoadLiteral.TestLoadRegLiteralFlags;
begin
  //Set
  DoTestLoadRegLiteral(rCF, TImmValue.CreateInteger(1), [],
    'cf:1','','CF:_');  //Already set

  //SCF
  DoTestLoadRegLiteral(rCF, TImmValue.CreateInteger(1), [],
    '','scf','CF:!1');

  //Clear
  DoTestLoadRegLiteral(rCF, TImmValue.CreateInteger(0), [],
    'cf:0','','CF:_');  //Already clear

  //CCF
  DoTestLoadRegLiteral(rCF, TImmValue.CreateInteger(0), [],
    'cf:1','ccf','CF:!0');  //If CF is set

  //SCF:CCF
  DoTestLoadRegLiteral(rCF, TImmValue.CreateInteger(0), [moPreserveOtherFlags],
    '','scf:ccf','CF:!0');  //If preserving flags

  //AND A
  DoTestLoadRegLiteral(rCF, TImmValue.CreateInteger(0), [],
    '','and a','CF:!0 Flags:!?');  //Fallback
end;


{ TTestZ80LoadVariable }

procedure TTestZ80LoadVariable.DoTestLoadVariable(Reg: TCPUReg;
  LoadType: TLoadParamType; Options: TMoveOptionSet; VarType: TVarType;VarAddrMode: TAddrMode;
  InState: String;RangeCheck: Boolean;ToType: TVarType;
  OutCode, OutState: String);
var Param: TILParam;
  V: PVariable;
  Code: String;
  State: String;
  Compare: String;
begin
  Setup;
  InitialiseScopes;
  RegStateInitialise;

  V := VarCreate('Test1', GetSystemType(VarType));
  V.AddrMode := VarAddrMode;
  V.Offset := 0;  //Needed??
  V.Version := 10; //VarVersion Needed??
  Param.Reg := Reg;
  Param.Kind := pkVarSource;
  Param.Variable := V;
  Param.VarVersion := 10;
  Param.Flags := [];
  Param.LoadType := LoadType;
  if RangeCheck then
    Param.Flags := Param.Flags + [cgRangeCheck];

  CPUStringToState(InState);
  RegStateClearModified;

  GenLoadParam(Param, GetSystemType(ToType), Options);  //<--- This is the big call
  Code := PeekAssembly;
  State := CPUStateToString;
  Check(Code = OutCode, 'Expected ' + OutCode + ', got ' + Code + ' with ' + State);
  Compare := CPUStateCompare(OutState);
  Check(Compare = '', Compare + 'Expected ' + OutState + ' got ' + State + ' with ' + Code);
end;

procedure TTestZ80LoadVariable.TestLoadRegScavenge16;
begin
  //16-bit value already in place
  DoTestLoadVariable(rHL, lptNormal, [],
    vtInteger, amStack, 'hl:%Test1#10',
    False, vtUnknown,
    '', 'hl:_%_v__Global_Test1#10 h:_/_v__Global_Test1#10 l:_\_v__Global_Test1#10');
  //16-bit scavenge with EX HL,DE
  DoTestLoadVariable(rHL, lptNormal, [],
    vtInteger, amStack, 'de:%Test1#10',
    False, vtUnknown,
    'ex hl,de', 'hl:!%_v__Global_Test1#10 h:!/_v__Global_Test1#10 l:!\_v__Global_Test1#10 de:!? d:!? e:!?');
  //16-bit scavenge without EX HL,DE
  DoTestLoadVariable(rHL, lptNormal, [moPreserveHLDE],
    vtInteger, amStack, 'de:%Test1#10',
    False, vtUnknown,
    'ld l,e:ld h,d', 'hl:!%_v__Global_Test1#10 h:!/_v__Global_Test1#10 l:!\_v__Global_Test1#10 de:_');
end;

procedure TTestZ80LoadVariable.TestLoadRegScavenge8;
begin
  //8-bit value already in place
  DoTestLoadVariable(rL, lptNormal, [],
    vtByte, amStack, 'l:%Test1#10',
    False, vtUnknown,
    '', 'l:_%_v__Global_Test1#10');
  //8-bit scavenged
  DoTestLoadVariable(rA, lptNormal, [],
    vtByte, amStack, 'l:%Test1#10',
    False, vtUnknown,
    'ld a,l', 'a:!%_v__Global_Test1#10 l:_');
  DoTestLoadVariable(rL, lptNormal, [],
    vtByte, amStack, 'a:%Test1#10',
    False, vtUnknown,
    'ld l,a', 'l:!%_v__Global_Test1#10 a:_');
  //And an Int8
  DoTestLoadVariable(rL, lptNormal, [],
    vtInt8, amStack, 'a:%Test1#10',
    False, vtUnknown,
    'ld l,a', 'l:!%_v__Global_Test1#10 a:_');
end;

procedure TTestZ80LoadVariable.TestLoadRegScavengeExtend;
begin
  //Zero extended
  DoTestLoadVariable(rHL, lptNormal, [],
    vtByte, amStack, 'd:%Test1#10',
    False, vtUnknown,
    'ld l,d:ld h,$00', 'hl:!? h:!$00 l:!%_v__Global_Test1#10 de:_');
  //Sign extended
  DoTestLoadVariable(rHL, lptNormal, [],
    vtInt8, amStack, 'd:%Test1#10',
    False, vtUnknown,
    'ld l,d:ld a,l:rla:sbc a,a:ld h,a', 'hl:!? h:!? l:!%_v__Global_Test1#10 de:_');

  //Zero extend with scavenged value
  DoTestLoadVariable(rHL, lptNormal, [],
    vtByte, amStack, 'd:%Test1#10 b:$00',
    False, vtUnknown,
    'ld l,d:ld h,b', 'hl:!? h:!$00 l:!%_v__Global_Test1#10 de:_');
  //...but zero extend won't scavenge overwritten value
  DoTestLoadVariable(rHL, lptNormal, [],
    vtByte, amStack, 'd:%Test1#10 l:$00',
    False, vtUnknown,
    'ld l,d:ld h,$00', 'hl:!? h:!$00 l:!%_v__Global_Test1#10 de:_');
  //However, unchanged high byte should be accounted for
  DoTestLoadVariable(rHL, lptNormal, [],
    vtByte, amStack, 'd:%Test1#10 h:$00',
    False, vtUnknown,
    'ld l,d', 'hl:!? h:_$00 l:!%_v__Global_Test1#10 de:_');
end;

procedure TTestZ80LoadVariable.TestLoadRegScavengeHighOrLow;
begin
  //VarValueHigh to 8 bit; to 16-bit zero extend
  DoTestLoadVariable(rA, lptHigh, [],
    vtInteger, amStack, 'hl:%Test1#10',
    False, vtUnknown,
    'ld a,h', 'a:!/_v__Global_Test1#10 hl:_');
  //VarValueLow to 8-bit
  DoTestLoadVariable(rA, lptLow, [],
    vtInteger, amStack, 'hl:%Test1#10',
    False, vtUnknown,
    'ld a,l', 'a:!\_v__Global_Test1#10 de:_');
  //VarValueHigh to 16-bit, zero extend
  DoTestLoadVariable(rBC, lptHigh, [],
    vtInteger, amStack, 'hl:%Test1#10',
    False, vtUnknown,
    'ld c,h:ld b,$00', 'b:!$00 c:!/_v__Global_Test1#10 bc:!?');
  //VarValueLow to 16-bit, zero extend
  DoTestLoadVariable(rBC, lptLow, [],
    vtInteger, amStack, 'hl:%Test1#10',
    False, vtUnknown,
    'ld c,l:ld b,$00', 'b:!$00 c:!\_v__Global_Test1#10 bc:!?');
end;

procedure TTestZ80LoadVariable.TestLoadRegStack16;
begin
  //Basic loads 16-bit variable to 16-bit registers
  DoTestLoadVariable(rHL, lptNormal, [],
    vtInteger, amStack, '',
    False, vtUnknown,
    'ld l,(ix+_v__Global_Test1):ld h,(ix+_v__Global_Test1 +1)',
      'hl:!%_v__Global_Test1#10 h:!/_v__Global_Test1#10 l:!\_v__Global_Test1#10');
  DoTestLoadVariable(rDE, lptNormal, [],
    vtInteger, amStack, '',
    False, vtUnknown,
    'ld e,(ix+_v__Global_Test1):ld d,(ix+_v__Global_Test1 +1)',
      'de:!%_v__Global_Test1#10 d:!/_v__Global_Test1#10 e:!\_v__Global_Test1#10');
  DoTestLoadVariable(rBC, lptNormal, [],
    vtInteger, amStack, '',
    False, vtUnknown,
    'ld c,(ix+_v__Global_Test1):ld b,(ix+_v__Global_Test1 +1)',
      'bc:!%_v__Global_Test1#10 b:!/_v__Global_Test1#10 c:!\_v__Global_Test1#10');

  //(Aside: We can't load IX or IY from stack)

  //Words and pointers - no troubles expected here
  DoTestLoadVariable(rHL, lptNormal, [],
    vtWord, amStack, '',
    False, vtUnknown,
    'ld l,(ix+_v__Global_Test1):ld h,(ix+_v__Global_Test1 +1)',
      'hl:!%_v__Global_Test1#10 h:!/_v__Global_Test1#10 l:!\_v__Global_Test1#10');
  DoTestLoadVariable(rDE, lptNormal, [],
    vtPointer, amStack, '',
    False, vtUnknown,
    'ld e,(ix+_v__Global_Test1):ld d,(ix+_v__Global_Test1 +1)',
      'de:!%_v__Global_Test1#10 d:!/_v__Global_Test1#10 e:!\_v__Global_Test1#10');
end;

procedure TTestZ80LoadVariable.TestLoadRegStack8;
begin
  //8-bit loads to 8-bit registers
  DoTestLoadVariable(rA, lptNormal, [],
    vtByte, amStack, '',
    False, vtUnknown,
    'ld a,(ix+_v__Global_Test1)', 'a:!%_v__Global_Test1#10');
  DoTestLoadVariable(rB, lptNormal, [],
    vtByte, amStack, '',
    False, vtUnknown,
    'ld b,(ix+_v__Global_Test1)', 'b:!%_v__Global_Test1#10');
  DoTestLoadVariable(rC, lptNormal, [],
    vtByte, amStack, '',
    False, vtUnknown,
    'ld c,(ix+_v__Global_Test1)', 'c:!%_v__Global_Test1#10');
  DoTestLoadVariable(rD, lptNormal, [],
    vtByte, amStack, '',
    False, vtUnknown,
    'ld d,(ix+_v__Global_Test1)', 'd:!%_v__Global_Test1#10');
  DoTestLoadVariable(rE, lptNormal, [],
    vtByte, amStack, '',
    False, vtUnknown,
    'ld e,(ix+_v__Global_Test1)', 'e:!%_v__Global_Test1#10');
  DoTestLoadVariable(rH, lptNormal, [],
    vtByte, amStack, '',
    False, vtUnknown,
    'ld h,(ix+_v__Global_Test1)', 'h:!%_v__Global_Test1#10');
  DoTestLoadVariable(rL, lptNormal, [],
    vtByte, amStack, '',
    False, vtUnknown,
    'ld l,(ix+_v__Global_Test1)', 'l:!%_v__Global_Test1#10');

  //And Int8s
  DoTestLoadVariable(rA, lptNormal, [],
    vtInt8, amStack, '',
    False, vtUnknown,
    'ld a,(ix+_v__Global_Test1)', 'a:!%_v__Global_Test1#10');
  DoTestLoadVariable(rB, lptNormal, [],
    vtChar, amStack, '',
    False, vtUnknown,
    'ld b,(ix+_v__Global_Test1)', 'b:!%_v__Global_Test1#10');
end;


procedure TTestZ80LoadVariable.TestLoadRegStackExtend;
begin
  //Unsigned extending
  DoTestLoadVariable(rHL, lptNormal, [],
    vtByte, amStack, '',
    False, vtUnknown,
    'ld l,(ix+_v__Global_Test1):ld h,$00',
      'hl:!? h:!$00 l:%_v__Global_Test1#10');
  DoTestLoadVariable(rDE, lptNormal, [],
    vtByte, amStack, '',
    False, vtUnknown,
    'ld e,(ix+_v__Global_Test1):ld d,$00',
      'de:!? d:!$00 e:!%_v__Global_Test1#10');
  DoTestLoadVariable(rBC, lptNormal, [],
    vtByte, amStack, '',
    False, vtUnknown,
    'ld c,(ix+_v__Global_Test1):ld b,$00',
      'bc:!? b:!$00 c:!%_v__Global_Test1#10');

  //Signed extending
  DoTestLoadVariable(rHL, lptNormal, [],
    vtInt8, amStack, '',
    False, vtUnknown,
    'ld l,(ix+_v__Global_Test1):ld a,l:rla:sbc a,a:ld h,a',
      'a:!? flags:!? cf:!? hl:!? h:!? l:!%_v__Global_Test1#10');
end;

procedure TTestZ80LoadVariable.TestLoadRegStackHighOrLow;
begin
  //Loading high or low byte of 16-bit value
  DoTestLoadVariable(rA, lptLow, [],
    vtWord, amStack, '',
    False, vtUnknown,
    'ld a,(ix+_v__Global_Test1)', 'a:!\_v__Global_Test1#10');
  DoTestLoadVariable(rH, lptHigh, [],
    vtInteger, amStack, '',
    False, vtUnknown,
    'ld h,(ix+_v__Global_Test1 +1)', 'h:!/_v__Global_Test1#10');

  //Loading high or low byte of 16-bit value **and extend**. We must zero extend!
  DoTestLoadVariable(rBC, lptLow, [],
    vtInteger, amStack, '',
    False, vtUnknown,
    'ld c,(ix+_v__Global_Test1):ld b,$00', 'c:!\_v__Global_Test1#10 b:!$00 bc:!?');
  DoTestLoadVariable(rDE, lptHigh, [],
    vtInteger, amStack, '',
    False, vtUnknown,
    'ld e,(ix+_v__Global_Test1 +1):ld d,$00', 'e:!/_v__Global_Test1#10 d:!$00 de:!?');
  //Scavenge the high byte
  DoTestLoadVariable(rBC, lptLow, [],
    vtInteger, amStack, 'a:$00',
    False, vtUnknown,
    'ld c,(ix+_v__Global_Test1):ld b,a', 'c:!\_v__Global_Test1#10 b:!$00 bc:!?');
  //The high byte can be scavenged. But verify we don't scavenge from an overwritten register
  DoTestLoadVariable(rBC, lptLow, [],
    vtInteger, amStack, 'c:$00',
    False, vtUnknown,
    'ld c,(ix+_v__Global_Test1):ld b,$00', 'c:!\_v__Global_Test1#10 b:!$00 bc:!?');
  //This time high byte already contains target value so leave it
  DoTestLoadVariable(rBC, lptLow, [],
    vtInteger, amStack, 'b:$00',
    False, vtUnknown,
    'ld c,(ix+_v__Global_Test1)', 'c:!\_v__Global_Test1#10 b:_$00 bc:!?');
end;

procedure TTestZ80LoadVariable.TestLoadRegStatic16;
begin
  //Basic loads 16-bit variable to 16-bit registers
  DoTestLoadVariable(rHL, lptNormal, [],
    vtInteger, amStatic, '',
    False, vtUnknown,
    'ld hl,(_v__Global_Test1)', 'hl:!%_v__Global_Test1#10 h:!/_v__Global_Test1#10 l:!\_v__Global_Test1#10');
  DoTestLoadVariable(rDE, lptNormal, [],
    vtInteger, amStatic, '',
    False, vtUnknown,
    'ld de,(_v__Global_Test1)', 'de:!%_v__Global_Test1#10');
  DoTestLoadVariable(rBC, lptNormal, [],
    vtInteger, amStatic, '',
    False, vtUnknown,
    'ld bc,(_v__Global_Test1)', 'bc:!%_v__Global_Test1#10');
  DoTestLoadVariable(rIX, lptNormal, [],
    vtInteger, amStatic, '',
    False, vtUnknown,
    'ld ix,(_v__Global_Test1)', 'ix:!%_v__Global_Test1#10');
  DoTestLoadVariable(rIY, lptNormal, [],
    vtInteger, amStatic, '',
    False, vtUnknown,
    'ld iy,(_v__Global_Test1)', 'iy:!%_v__Global_Test1#10');

  //Words and pointers - no troubles exppected here
  DoTestLoadVariable(rHL, lptNormal, [],
    vtWord, amStatic, '',
    False, vtUnknown,
    'ld hl,(_v__Global_Test1)', 'hl:!%_v__Global_Test1#10');
  DoTestLoadVariable(rHL, lptNormal, [],
    vtPointer, amStatic, '',
    False, vtUnknown,
    'ld hl,(_v__Global_Test1)', 'hl:!%_v__Global_Test1#10');
end;

procedure TTestZ80LoadVariable.TestLoadRegStatic8;
begin
  //8-bit loads to 8-bit registers
  DoTestLoadVariable(rA, lptNormal, [],
    vtByte, amStatic, '',
    False, vtUnknown,
    'ld a,(_v__Global_Test1)', 'a:!%_v__Global_Test1#10');
  //8-bit loads need to go via the A register
  DoTestLoadVariable(rB, lptNormal, [],
    vtByte, amStatic, '',
    False, vtUnknown,
    'ld a,(_v__Global_Test1):ld b,a', 'a:!%_v__Global_Test1#10 b:!%_v__Global_Test1#10 bc:!?');
  DoTestLoadVariable(rC, lptNormal, [],
    vtByte, amStatic, '',
    False, vtUnknown,
    'ld a,(_v__Global_Test1):ld c,a', 'a:!%_v__Global_Test1#10 c:!%_v__Global_Test1#10 bc:!?');
  DoTestLoadVariable(rD, lptNormal, [],
    vtByte, amStatic, '',
    False, vtUnknown,
    'ld a,(_v__Global_Test1):ld d,a', 'a:!%_v__Global_Test1#10 d:!%_v__Global_Test1#10 de:!?');
  DoTestLoadVariable(rE, lptNormal, [],
    vtByte, amStatic, '',
    False, vtUnknown,
    'ld a,(_v__Global_Test1):ld e,a', 'a:!%_v__Global_Test1#10 e:!%_v__Global_Test1#10 de:!?');
  DoTestLoadVariable(rH, lptNormal, [],
    vtByte, amStatic, '',
    False, vtUnknown,
    'ld a,(_v__Global_Test1):ld h,a', 'a:!%_v__Global_Test1#10 h:!%_v__Global_Test1#10 hl:!?');
  DoTestLoadVariable(rL, lptNormal, [],
    vtByte, amStatic, '',
    False, vtUnknown,
    'ld a,(_v__Global_Test1):ld l,a', 'a:!%_v__Global_Test1#10 l:!%_v__Global_Test1#10 hl:!?');

  //And Int8s
  DoTestLoadVariable(rB, lptNormal, [],
    vtInt8, amStatic, '',
    False, vtUnknown,
    'ld a,(_v__Global_Test1):ld b,a', 'a:!%_v__Global_Test1#10 b:!%_v__Global_Test1#10 bc:!?');
  DoTestLoadVariable(rB, lptNormal, [],
    vtInt8, amStatic, '',
    False, vtUnknown,
    'ld a,(_v__Global_Test1):ld b,a', 'a:!%_v__Global_Test1#10 b:!%_v__Global_Test1#10 bc:!?');
end;

procedure TTestZ80LoadVariable.TestLoadRegStaticExtend;
begin
  //Unsigned extending
  DoTestLoadVariable(rBC, lptNormal, [],
    vtByte, amStatic, '',
    False, vtUnknown,
    'ld bc,(_v__Global_Test1):ld b,$00', 'b:$00 c:%_v__Global_Test1#10 bc:?');
  DoTestLoadVariable(rDE, lptNormal, [],
    vtByte, amStatic, '',
    False, vtUnknown,
    'ld de,(_v__Global_Test1):ld d,$00', 'd:$00 e:%_v__Global_Test1#10 de:?');
  DoTestLoadVariable(rHL, lptNormal, [],
    vtByte, amStatic, '',
    False, vtUnknown,
    'ld hl,(_v__Global_Test1):ld h,$00', 'h:$00 l:%_v__Global_Test1#10 hl:?');

  //Signed extending
  DoTestLoadVariable(rBC, lptNormal, [],
    vtInt8, amStatic, '',
    False, vtUnknown,
    'ld bc,(_v__Global_Test1):ld a,c:rla:sbc a,a:ld b,a',
    'a:!? cf:!? flags:!? bc:!? b:!? c:%_v__Global_Test1#10');
end;

procedure TTestZ80LoadVariable.TestLoadRegStaticHighOrLow;
begin
  //Loading high or low byte of 16-bit value
  DoTestLoadVariable(rB, lptLow, [],
    vtWord, amStatic, '',
    False, vtUnknown,
    'ld a,(_v__Global_Test1):ld b,a', 'a:!\_v__Global_Test1#10 b:!\_v__Global_Test1#10 bc:!?');
  DoTestLoadVariable(rC, lptHigh, [],
    vtInteger, amStatic, '',
    False, vtUnknown,
    'ld a,(_v__Global_Test1 +1):ld c,a', 'a:!/_v__Global_Test1#10 c:!/_v__Global_Test1#10 bc:!?');

  //Loading high or low byte of 16-bit value **and extend**. We must zero extend!
  DoTestLoadVariable(rBC, lptLow, [],
    vtInteger, amStatic, '',
    False, vtUnknown,
    'ld bc,(_v__Global_Test1):ld b,$00', 'c:!\_v__Global_Test1#10 b:!$00 bc:!?');
  DoTestLoadVariable(rDE, lptHigh, [],
    vtInteger, amStatic, '',
    False, vtUnknown,
    'ld de,(_v__Global_Test1 +1):ld d,$00', 'e:!/_v__Global_Test1#10 d:!$00 de:!?');
  //Scavenge the high byte
  DoTestLoadVariable(rBC, lptLow, [],
    vtInteger, amStatic, 'a:$00',
    False, vtUnknown,
    'ld bc,(_v__Global_Test1):ld b,a', 'c:!\_v__Global_Test1#10 b:!$00 bc:!?');
  //The high byte can be scavenged. But verify we don't scavenge from an overwritten register
  DoTestLoadVariable(rBC, lptLow, [],
    vtInteger, amStatic, 'b:$00',
    False, vtUnknown,
    'ld bc,(_v__Global_Test1):ld b,$00', 'c:!\_v__Global_Test1#10 b:!$00 bc:!?');
  DoTestLoadVariable(rBC, lptLow, [],
    vtInteger, amStatic, 'c:$00',
    False, vtUnknown,
    'ld bc,(_v__Global_Test1):ld b,$00', 'c:!\_v__Global_Test1#10 b:!$00 bc:!?');
end;

procedure TTestZ80LoadVariable.TestLoadRegStaticConvertTo16Bit;
begin //Note: We'll also test Range checks when FromType and ToType are the same
  //Integer to Integer
  DoTestLoadVariable(rHL, lptNormal, [],
    vtInteger, amStatic, '',
    True, vtInteger,
    'ld hl,(_v__Global_Test1)', 'hl:!%_v__Global_Test1#10 h:!/_v__Global_Test1#10 l:!\_v__Global_Test1#10');
  //Word/Pointer to Integer
  DoTestLoadVariable(rDE, lptNormal, [],
    vtPointer, amStatic, '',
    True, vtInteger,
    'ld de,(_v__Global_Test1):bit 7,d:jp nz,raise_range',
    'd:? e:\_v__Global_Test1#10 de:?');
  //Byte to Integer
  DoTestLoadVariable(rDE, lptNormal, [],
    vtByte, amStatic, '',
    True, vtInteger,
    'ld de,(_v__Global_Test1):ld d,$00',
    'd:$00 e:%_v__Global_Test1#10 de:?');
  //Int8 to Integer
  DoTestLoadVariable(rHL, lptNormal, [],
    vtInt8, amStatic, '',
    True, vtInteger,
    'ld hl,(_v__Global_Test1):ld a,l:rla:sbc a,a:ld h,a',
    'a:? h:? l:%_v__Global_Test1#10 hl:?');

  //Integer to Word/Pointer
  DoTestLoadVariable(rDE, lptNormal, [],
    vtInteger, amStatic, '',
    True, vtWord,
    'ld de,(_v__Global_Test1):bit 7,d:jp nz,raise_range',
    'd:? e:\_v__Global_Test1#10 de:?');
  //Word/Pointer to Word/Pointer
  DoTestLoadVariable(rBC, lptNormal, [],
    vtWord, amStatic, '',
    True, vtPointer,
    'ld bc,(_v__Global_Test1)',
    'b:/_v__Global_Test1#10 c:\_v__Global_Test1#10 bc:%_v__Global_Test1#10');
  //Byte to Word/Pointer
  DoTestLoadVariable(rHL, lptNormal, [],
    vtByte, amStatic, '',
    True, vtWord,
    'ld hl,(_v__Global_Test1):ld h,$00',
    'h:$00 l:%_v__Global_Test1#10 hl:?');
  //Int8 to Word/Pointer
  DoTestLoadVariable(rBC, lptNormal, [],
    vtInt8, amStatic, '',
    True, vtWord,
    'ld bc,(_v__Global_Test1):bit 7,c:jp nz,raise_range:ld b,$00',
    'a:? b:$00 c:? bc:?');
end;

procedure TTestZ80LoadVariable.TestLoadRegStaticConvertTo8Bit;
begin //Note: We'll also test Range checks when FromType and ToType are the same
  //Integer to Int8
  DoTestLoadVariable(rH, lptNormal, [],
    vtInteger, amStatic, '',
    True, vtInt8,
    'ld a,(_v__Global_Test1):ld h,a:rla:ld a,(_v__Global_Test1 +1):adc a,$00:jp nz,raise_range',
    'a:!$00 h:\_v__Global_Test1#10');
  //Word/Pointer to Int8
  DoTestLoadVariable(rH, lptNormal, [],
    vtWord, amStatic, '',
    True, vtInt8,
    'ld a,(_v__Global_Test1 +1):and a:jp nz,raise_range:ld a,(_v__Global_Test1):ld h,a:and a:jp m,raise_range',
    'a:!? h:!?');
  //Byte to Int8
  DoTestLoadVariable(rH, lptNormal, [],
    vtByte, amStatic, '',
    True, vtInt8,
    'ld a,(_v__Global_Test1):ld h,a:and a:jp m,raise_range',
    'a:!? h:!?');
  //Int8 to Int8
  DoTestLoadVariable(rH, lptNormal, [],
    vtInt8, amStatic, '',
    True, vtInt8,
    'ld a,(_v__Global_Test1):ld h,a',
    'a:%_v__Global_Test1#10 h:%_v__Global_Test1#10 hl:?');

  //Integer to Byte
  DoTestLoadVariable(rH, lptNormal, [],
    vtInteger, amStatic, '',
    True, vtByte,
    'ld a,(_v__Global_Test1 +1):and a:jp nz,raise_range:ld a,(_v__Global_Test1):ld h,a',
    'a:!? h:!?');
  //Word/Pointer to Byte
  DoTestLoadVariable(rH, lptNormal, [],
    vtWord, amStatic, '',
    True, vtByte,
    'ld a,(_v__Global_Test1 +1):and a:jp nz,raise_range:ld a,(_v__Global_Test1):ld h,a',
    'a:\_v__Global_Test1#10 h:\_v__Global_Test1#10 hl:?');
  //Byte to Byte
  DoTestLoadVariable(rH, lptNormal, [],
    vtByte, amStatic, '',
    True, vtByte,
    'ld a,(_v__Global_Test1):ld h,a',
    'a:%_v__Global_Test1#10 h:%_v__Global_Test1#10 hl:!?');
  //Int8 to Byte
  DoTestLoadVariable(rE, lptNormal, [],
    vtInt8, amStatic, '',
    True, vtByte,
    'ld a,(_v__Global_Test1):ld e,a:and a:jp m,raise_range',
    'a:? e:? de:?');
end;


procedure TTestZ80LoadVariable.TestLoadRegStackConvertTo16Bit;
begin
  //Integer to Integer
  DoTestLoadVariable(rHL, lptNormal, [],
    vtInteger, amStack, '',
    True, vtInteger,
    'ld l,(ix+_v__Global_Test1):ld h,(ix+_v__Global_Test1 +1)',
    'hl:!%_v__Global_Test1#10 h:!/_v__Global_Test1#10 l:!\_v__Global_Test1#10');
  //Word/Pointer to Integer
  DoTestLoadVariable(rDE, lptNormal, [],
    vtPointer, amStack, '',
    True, vtInteger,
    'ld e,(ix+_v__Global_Test1):ld d,(ix+_v__Global_Test1 +1):bit 7,d:jp nz,raise_range',
    'd:? e:\_v__Global_Test1#10 de:?');
  //Byte to Integer
  DoTestLoadVariable(rDE, lptNormal, [],
    vtByte, amStack, '',
    True, vtInteger,
    'ld e,(ix+_v__Global_Test1):ld d,$00',
    'd:$00 e:%_v__Global_Test1#10 de:?');
  //Int8 to Integer
  DoTestLoadVariable(rHL, lptNormal, [],
    vtInt8, amStack, '',
    True, vtInteger,
    'ld l,(ix+_v__Global_Test1):ld a,l:rla:sbc a,a:ld h,a',
    'a:? h:? l:%_v__Global_Test1#10 hl:?');

  //Integer to Word/Pointer
  DoTestLoadVariable(rDE, lptNormal, [],
    vtInteger, amStack, '',
    True, vtWord,
    'ld e,(ix+_v__Global_Test1):ld d,(ix+_v__Global_Test1 +1):bit 7,d:jp nz,raise_range',
    'd:? e:\_v__Global_Test1#10 de:?');
  //Word/Pointer to Word/Pointer
  DoTestLoadVariable(rBC, lptNormal, [],
    vtWord, amStack, '',
    True, vtPointer,
    'ld c,(ix+_v__Global_Test1):ld b,(ix+_v__Global_Test1 +1)',
    'b:/_v__Global_Test1#10 c:\_v__Global_Test1#10 bc:%_v__Global_Test1#10');
  //Byte to Word/Pointer
  DoTestLoadVariable(rHL, lptNormal, [],
    vtByte, amStack, '',
    True, vtWord,
    'ld l,(ix+_v__Global_Test1):ld h,$00',
    'h:$00 l:%_v__Global_Test1#10 hl:?');
  //Int8 to Word/Pointer
  DoTestLoadVariable(rBC, lptNormal, [],
    vtInt8, amStack, '',
    True, vtWord,
    'ld c,(ix+_v__Global_Test1):bit 7,c:jp nz,raise_range:ld b,$00',
    'a:? b:$00 c:? bc:?');
end;

procedure TTestZ80LoadVariable.TestLoadRegStackConvertTo8Bit;
begin
  //Integer to Int8
  DoTestLoadVariable(rH, lptNormal, [],
    vtInteger, amStack, '',
    True, vtInt8,
    'ld h,(ix+_v__Global_Test1):ld a,h:rla:ld a,(ix+_v__Global_Test1 +1):adc a,$00:jp nz,raise_range',
    'a:!$00 h:!\_v__Global_Test1#10');
  //Word/Pointer to Int8
  DoTestLoadVariable(rH, lptNormal, [],
    vtWord, amStack, '',
    True, vtInt8,
    'ld a,(ix+_v__Global_Test1 +1):and a:jp nz,raise_range:ld h,(ix+_v__Global_Test1):bit 7,h:jp nz,raise_range',
    'a:!$00 h:!?');
  //Byte to Int8
  DoTestLoadVariable(rH, lptNormal, [],
    vtByte, amStack, '',
    True, vtInt8,
    'ld h,(ix+_v__Global_Test1):bit 7,h:jp nz,raise_range',
    'h:!? hl:!?');
  //Int8 to Int8
  DoTestLoadVariable(rH, lptNormal, [],
    vtInt8, amStack, '',
    True, vtInt8,
    'ld h,(ix+_v__Global_Test1)',
    'h:%_v__Global_Test1#10 hl:?');

  //Integer to Byte
  DoTestLoadVariable(rH, lptNormal, [],
    vtInteger, amStack, '',
    True, vtByte,
    'ld a,(ix+_v__Global_Test1 +1):and a:jp nz,raise_range:ld h,(ix+_v__Global_Test1)',
    'a:!$00 h:!?');
  //Word/Pointer to Byte
  DoTestLoadVariable(rH, lptNormal, [],
    vtWord, amStack, '',
    True, vtByte,
    'ld a,(ix+_v__Global_Test1 +1):and a:jp nz,raise_range:ld h,(ix+_v__Global_Test1)',
    'a:$00 h:!\_v__Global_Test1#10 hl:?');
  //Byte to Byte
  DoTestLoadVariable(rH, lptNormal, [],
    vtByte, amStack, '',
    True, vtByte,
    'ld h,(ix+_v__Global_Test1)',
    'h:!%_v__Global_Test1#10 hl:!?');
  //Int8 to Byte
  DoTestLoadVariable(rE, lptNormal, [],
    vtInt8, amStack, '',
    True, vtByte,
    'ld e,(ix+_v__Global_Test1):bit 7,e:jp nz,raise_range',
    'a:? e:? de:?');
end;

procedure TTestZ80LoadVariable.TestLoadRegScavengeConvertTo16Bit;
begin
  //Int8 to Integer
  DoTestLoadVariable(rHL, lptNormal, [],
    vtInt8, amStack, 'd:%Test1#10',
    True, vtInteger,
    'ld l,d:ld a,l:rla:sbc a,a:ld h,a',
    'a:!? d:%_v__Global_Test1#10 h:? l:%_v__Global_Test1#10');
  //Int8 in target low to Word/Pointer
  DoTestLoadVariable(rHL, lptNormal, [],
    vtInt8, amStack, 'l:%Test1#10',
    True, vtWord,
    'bit 7,l:jp nz,raise_range:ld h,$00',
    'h:!$00 l:%_v__Global_Test1#10');
  //Int8 in target high to Word/Pointer
  DoTestLoadVariable(rHL, lptNormal, [],
    vtInt8, amStack, 'h:%Test1#10',
    True, vtWord,
    'ld l,h:bit 7,l:jp nz,raise_range:ld h,$00',
    'h:!$00 l:%_v__Global_Test1#10');
  //Int8 in A to Integer
  DoTestLoadVariable(rDE, lptNormal, [],
    vtInt8, amStack, 'a:%Test1#10',
    True, vtInteger,
    'ld e,a:rla:sbc a,a:ld d,a',
    'a:!? d:!? e:%_v__Global_Test1#10');
  //Int8 in A to Word/Pointer
  DoTestLoadVariable(rHL, lptNormal, [],
    vtInt8, amStack, 'a:%Test1#10',
    True, vtPointer,
    'ld l,a:and a:jp m,raise_range:ld h,$00',
    'a:%_v__Global_Test1#10 h:!$00 l:%_v__Global_Test1#10');

  //Byte to Integer
  DoTestLoadVariable(rHL, lptNormal, [],
    vtByte, amStack, 'd:%Test1#10',
    True, vtInteger,
    'ld l,d:ld h,$00',
    'h:$00 l:%_v__Global_Test1#10 hl:!?');
  //Byte to Word/Pointer
  DoTestLoadVariable(rHL, lptNormal, [],
    vtByte, amStack, 'e:%Test1#10',
    True, vtWord,
    'ld l,e:ld h,$00',
    'h:$00 l:%_v__Global_Test1#10 hl:!?');
  //Byte in target low to Word/Pointer
  DoTestLoadVariable(rHL, lptNormal, [],
    vtByte, amStack, 'l:%Test1#10',
    True, vtWord,
    'ld h,$00',
    'h:$00 l:%_v__Global_Test1#10 hl:!?');
  //Byte in target high to Word/Pointer
  DoTestLoadVariable(rHL, lptNormal, [],
    vtByte, amStack, 'h:%Test1#10',
    True, vtWord,
    'ld l,h:ld h,$00',
    'h:$00 l:%_v__Global_Test1#10 hl:!?');
end;

procedure TTestZ80LoadVariable.TestLoadRegScavengeConvertTo8Bit;
begin
  //Integer to Int8
  DoTestLoadVariable(rH, lptNormal, [],
    vtInteger, amStack, 'de:%Test1#10',
    True, vtInt8,
    'ld a,e:rla:ld a,d:adc a,$00:jp nz,raise_range:ld h,e',
    'a:!$00 h:!\_v__Global_Test1#10');
  //Word/Pointer to Int8
  DoTestLoadVariable(rH, lptNormal, [],
    vtWord, amStack, 'hl:%Test1#10',
    True, vtInt8,
    'ld a,h:and a:jp nz,raise_range:ld h,l:bit 7,h:jp nz,raise_range',
    'a:!$00 H:\_v__Global_Test1#10');
  //Integer to Int8 in A
  DoTestLoadVariable(rA, lptNormal, [],
    vtInteger, amStack, 'de:%Test1#10',
    True, vtInt8,
    'ld a,e:rla:ld a,d:adc a,$00:jp nz,raise_range:ld a,e',
    'a:\_v__Global_Test1#10');
  //Pointer to Int8 in A
  DoTestLoadVariable(rA, lptNormal, [],
    vtPointer, amStack, 'de:%Test1#10',
    True, vtInt8,
    'ld a,d:and a:jp nz,raise_range:ld a,e:and a:jp m,raise_range',
    'A:\_v__Global_Test1#10');

  //Integer to Byte
  DoTestLoadVariable(rH, lptNormal, [],
    vtInteger, amStack, 'de:%Test1#10',
    True, vtByte,
    'ld a,d:and a:jp nz,raise_range:ld h,e',
    'a:$00 h:\_v__Global_Test1#10 HL:?');
  //Word/Pointer to Byte
  DoTestLoadVariable(rH, lptNormal, [],
    vtWord, amStack, 'de:%Test1#10',
    True, vtByte,
    'ld a,d:and a:jp nz,raise_range:ld h,e',
    'a:$00 h:!\_v__Global_Test1#10 hl:?');
end;

{ TTestZ80StoreVariable }

procedure TTestZ80StoreVariable.DoTestStoreVariable(Reg: TCPUReg;
  Options: TMoveOptionSet; RangeCheck: Boolean; FromType, VarType: TVarType;
  VarAddrMode: TAddrMode; InState, OutCode, OutState: String);
var Param: TILParam;
  V: PVariable;
  Code: String;
  State: String;
  Compare: String;
begin
  Setup;
  InitialiseScopes;
  RegStateInitialise;

  V := VarCreate('Test1', GetSystemType(VarType));
  V.AddrMode := VarAddrMode;
  V.Offset := 0;  //Needed??
  V.Version := 10; //VarVersion Needed??
  Param.Reg := Reg;
  Param.Kind := pkVarDest;
  Param.Variable := V;
  Param.VarVersion := 10;
  Param.Flags := [];
  if RangeCheck then
    Param.Flags := Param.Flags + [cgRangeCheck];

  CPUStringToState(InState);
  RegStateClearModified;

  GenDestParam(Param, GetSystemType(FromType), RangeCheck, nil, Options);  //<--- This is the big call

  Code := PeekAssembly;
  State := CPUStateToString;
  Check(Code = OutCode, 'Expected ' + OutCode + ', got ' + Code + ' with ' + State);
  Compare := CPUStateCompare(OutState);
  Check(Compare = '', Compare + 'Expected ' + OutState + ' got ' + State + ' with ' + Code);
end;

procedure TTestZ80StoreVariable.TestStoreRegStack16;
begin
  //Basic loads 16-bit variable to 16-bit registers
  DoTestStoreVariable(rHL, [], False, vtUnknown,
    vtInteger, amStack, '',
    'ld (ix+_v__Global_Test1),l:ld (ix+_v__Global_Test1 +1),h', '');
  DoTestStoreVariable(rDE, [], False, vtUnknown,
    vtInteger, amStack, '',
    'ld (ix+_v__Global_Test1),e:ld (ix+_v__Global_Test1 +1),d', '');
  DoTestStoreVariable(rBC, [], False, vtUnknown,
    vtInteger, amStack, '',
    'ld (ix+_v__Global_Test1),c:ld (ix+_v__Global_Test1 +1),b', '');
  //Can't store index registers to stack variables
{  DoTestStoreVariable(rIX, [], False, vtUnknown,
    vtInteger, amStack, '',
    'ld (ix+_v__Global_Test1),l:ld (ix+_v__Global_Test1 +1),h', '');
  DoTestStoreVariable(rIY, [], False, vtUnknown,
    vtInteger, amStack, '',
    'ld (ix+_v__Global_Test1),l:ld (ix+_v__Global_Test1 +1),h', '');
}
  //Words and pointers - no troubles expected here
  DoTestStoreVariable(rHL, [], False, vtUnknown,
    vtWord, amStack, '',
    'ld (ix+_v__Global_Test1),l:ld (ix+_v__Global_Test1 +1),h', '');
  DoTestStoreVariable(rHL, [], False, vtUnknown,
    vtPointer, amStack, '',
    'ld (ix+_v__Global_Test1),l:ld (ix+_v__Global_Test1 +1),h', '');
end;

procedure TTestZ80StoreVariable.TestStoreRegStack8;
begin
  //Basic loads 8-bit variable to 8-bit registers
  DoTestStoreVariable(rA, [], False, vtUnknown,
    vtByte, amStack, '',
    'ld (ix+_v__Global_Test1),a', '');
  DoTestStoreVariable(rB, [], False, vtUnknown,
    vtByte, amStack, '',
    'ld (ix+_v__Global_Test1),b', '');
  DoTestStoreVariable(rC, [], False, vtUnknown,
    vtInt8, amStack, '',
    'ld (ix+_v__Global_Test1),c', '');
  DoTestStoreVariable(rD, [], False, vtUnknown,
    vtInt8, amStack, '',
    'ld (ix+_v__Global_Test1),d', '');
  DoTestStoreVariable(rE, [], False, vtUnknown,
    vtByte, amStack, '',
    'ld (ix+_v__Global_Test1),e', '');
  DoTestStoreVariable(rH, [], False, vtUnknown,
    vtInt8, amStack, '',
    'ld (ix+_v__Global_Test1),h', '');
  DoTestStoreVariable(rL, [], False, vtUnknown,
    vtByte, amStack, '',
    'ld (ix+_v__Global_Test1),l', '');
end;

procedure TTestZ80StoreVariable.TestStoreRegStackConvertTo16Bit;
begin
  //Basic loads 8-bit variable to 16-bit registers with range checking
  //Int8 to Integer
  DoTestStoreVariable(rA, [], True, vtInt8,
    vtInteger, amStack, '',
    'ld (ix+_v__Global_Test1),a:rla:sbc a,a:ld (ix+_v__Global_Test1 +1),a',
    'a:/_v__Global_Test1#10');
//    'a:!?');
  DoTestStoreVariable(rH, [], True, vtInt8,
    vtInteger, amStack, '',
    'ld (ix+_v__Global_Test1),h:ld a,h:rla:sbc a,a:ld (ix+_v__Global_Test1 +1),a',
    'a:/_v__Global_Test1#10 h:\_v__Global_Test1#10');
//    'a:!?');
  //Byte to Integer
  DoTestStoreVariable(rA, [], True, vtByte,
    vtInteger, amStack, '',
    'ld (ix+_v__Global_Test1),a:xor a:ld (ix+_v__Global_Test1 +1),a',
    'a:!$00');
  DoTestStoreVariable(rH, [], True, vtByte,
    vtInteger, amStack, '',
    'ld (ix+_v__Global_Test1),h:xor a:ld (ix+_v__Global_Test1 +1),a',
    'a:!$00');
  //Int8 to Word
  DoTestStoreVariable(rA, [], True, vtInt8,
    vtWord, amStack, '',
    'and a:jp m,raise_range:ld (ix+_v__Global_Test1),a:xor a:ld (ix+_v__Global_Test1 +1),a',
    'a:$00');
  DoTestStoreVariable(rL, [], True, vtInt8,
    vtWord, amStack, '',
    'bit 7,l:jp nz,raise_range:ld (ix+_v__Global_Test1),l:xor a:ld (ix+_v__Global_Test1 +1),a',
    'a:$00');
  //Byte to Word
  DoTestStoreVariable(rA, [], True, vtByte,
    vtWord, amStack, '',
    'ld (ix+_v__Global_Test1),a:xor a:ld (ix+_v__Global_Test1 +1),a',
    'a:!$00');
  DoTestStoreVariable(rH, [], True, vtByte,
    vtWord, amStack, '',
    'ld (ix+_v__Global_Test1),h:xor a:ld (ix+_v__Global_Test1 +1),a',
    'a:!$00');
  //Byte to Pointer
  DoTestStoreVariable(rA, [], True, vtByte,
    vtPointer, amStack, '',
    'ld (ix+_v__Global_Test1),a:xor a:ld (ix+_v__Global_Test1 +1),a',
    'a:!$00');
  DoTestStoreVariable(rL, [], True, vtByte,
    vtPointer, amStack, '',
    'ld (ix+_v__Global_Test1),l:xor a:ld (ix+_v__Global_Test1 +1),a',
    'a:!$00');
end;

procedure TTestZ80StoreVariable.TestStoreRegStackConvertTo8Bit;
begin
  //Basic loads 16-bit variable to 8-bit registers
  //Integer to Int8
  DoTestStoreVariable(rHL, [], True, vtInteger,
    vtInt8, amStack, '',
    'ld a,l:rla:ld a,h:adc a,$00:jp nz,raise_range:ld (ix+_v__Global_Test1),l',
    'a:!$00');
  //Integer to Byte
  DoTestStoreVariable(rDE, [], True, vtInteger,
    vtByte, amStack, '',
    'ld a,d:and a:jp nz,raise_range:ld (ix+_v__Global_Test1),e',
    'a:!$00');
  //Word to Int8
  DoTestStoreVariable(rBC, [], True, vtWord,
    vtInt8, amStack, '',
    'ld a,c:and $80:or b:jp nz,raise_range:ld (ix+_v__Global_Test1),c',
    'a:!?$00');
  //Word to Byte
  DoTestStoreVariable(rHL, [], True, vtWord,
    vtByte, amStack, '',
    'ld a,h:and a:jp nz,raise_range:ld (ix+_v__Global_Test1),l',
    'a:!$00');
  //Pointer to Byte
  DoTestStoreVariable(rDE, [], True, vtPointer,
    vtByte, amStack, '',
    'ld a,d:and a:jp nz,raise_range:ld (ix+_v__Global_Test1),e',
    'a:!$00');
end;

procedure TTestZ80StoreVariable.TestStoreRegStackExtend;
begin
  //Unsigned extend
  DoTestStoreVariable(rA, [], False, vtByte,
    vtWord, amStack, '',
    'ld (ix+_v__Global_Test1),a:xor a:ld (ix+_v__Global_Test1 +1),a', 'a:!$00');
  DoTestStoreVariable(rB, [], False, vtByte,
    vtPointer, amStack, '',
    'ld (ix+_v__Global_Test1),b:xor a:ld (ix+_v__Global_Test1 +1),a', 'a:!$00');
  DoTestStoreVariable(rC, [], False, vtByte,
    vtWord, amStack, '',
    'ld (ix+_v__Global_Test1),c:xor a:ld (ix+_v__Global_Test1 +1),a', 'a:!$00');
  DoTestStoreVariable(rD, [], False, vtByte,
    vtInteger, amStack, '',
    'ld (ix+_v__Global_Test1),d:xor a:ld (ix+_v__Global_Test1 +1),a', 'a:!$00');

  //Signed extend
  DoTestStoreVariable(rA, [], False, vtInt8,
    vtInteger, amStack, '',
    'ld (ix+_v__Global_Test1),a:rla:sbc a,a:ld (ix+_v__Global_Test1 +1),a', 'A:/_v__Global_Test1#10');
  DoTestStoreVariable(rE, [], False, vtInt8,
    vtInteger, amStack, '',
    'ld (ix+_v__Global_Test1),e:ld a,e:rla:sbc a,a:ld (ix+_v__Global_Test1 +1),a', 'a:/_v__Global_Test1#10 e:\_v__Global_Test1#10');
  DoTestStoreVariable(rH, [], False, vtInt8,
    vtInteger, amStack, '',
    'ld (ix+_v__Global_Test1),h:ld a,h:rla:sbc a,a:ld (ix+_v__Global_Test1 +1),a', 'a:/_v__Global_Test1#10 h:\_v__Global_Test1#10');
end;

procedure TTestZ80StoreVariable.TestStoreRegStatic16;
begin
  //Basic loads 16-bit variable to 16-bit registers
  DoTestStoreVariable(rHL, [], False, vtUnknown,
    vtInteger, amStatic, '',
    'ld (_v__Global_Test1),hl', '');
  DoTestStoreVariable(rDE, [], False, vtUnknown,
    vtInteger, amStatic, '',
    'ld (_v__Global_Test1),de', '');
  DoTestStoreVariable(rBC, [], False, vtUnknown,
    vtInteger, amStatic, '',
    'ld (_v__Global_Test1),bc', '');
  DoTestStoreVariable(rIX, [], False, vtUnknown,
    vtInteger, amStatic, '',
    'ld (_v__Global_Test1),ix', '');
  DoTestStoreVariable(rIY, [], False, vtUnknown,
    vtInteger, amStatic, '',
    'ld (_v__Global_Test1),iy', '');

  //Words and pointers - no troubles expected here
  DoTestStoreVariable(rHL, [], False, vtUnknown,
    vtWord, amStatic, '',
    'ld (_v__Global_Test1),hl', '');
  DoTestStoreVariable(rHL, [], False, vtUnknown,
    vtPointer, amStatic, '',
    'ld (_v__Global_Test1),hl', '');
end;

procedure TTestZ80StoreVariable.TestStoreRegStatic8;
begin
  //8-bit loads to 8-bit registers
  DoTestStoreVariable(rA, [], False, vtUnknown,
    vtByte, amStatic, '',
    'ld (_v__Global_Test1),a', '');
  //8-bit loads need to go via the A register
  DoTestStoreVariable(rB, [], False, vtUnknown,
    vtByte, amStatic, '',
    'ld a,b:ld (_v__Global_Test1),a', 'a:!%_v__Global_Test1#10');
  DoTestStoreVariable(rC, [], False, vtUnknown,
    vtByte, amStatic, '',
    'ld a,c:ld (_v__Global_Test1),a', 'a:!%_v__Global_Test1#10');
  DoTestStoreVariable(rD, [], False, vtUnknown,
    vtByte, amStatic, '',
    'ld a,d:ld (_v__Global_Test1),a', 'a:!%_v__Global_Test1#10');
  DoTestStoreVariable(rE, [], False, vtUnknown,
    vtByte, amStatic, '',
    'ld a,e:ld (_v__Global_Test1),a', 'a:!%_v__Global_Test1#10');
  DoTestStoreVariable(rH, [], False, vtUnknown,
    vtByte, amStatic, '',
    'ld a,h:ld (_v__Global_Test1),a', 'a:!%_v__Global_Test1#10');
  DoTestStoreVariable(rL, [], False, vtUnknown,
    vtByte, amStatic, '',
    'ld a,l:ld (_v__Global_Test1),a', 'a:!%_v__Global_Test1#10');

  //And Int8s
  DoTestStoreVariable(rB, [], False, vtUnknown,
    vtInt8, amStatic, '',
    'ld a,b:ld (_v__Global_Test1),a', 'a:!%_v__Global_Test1#10');
  DoTestStoreVariable(rB, [], False, vtUnknown,
    vtInt8, amStatic, '',
    'ld a,b:ld (_v__Global_Test1),a', 'a:!%_v__Global_Test1#10');
end;

procedure TTestZ80StoreVariable.TestStoreRegStaticConvertTo16Bit;
begin
  //Basic loads 8-bit variable to 16-bit registers with range checking
  //Int8 to Integer
  DoTestStoreVariable(rA, [], True, vtInt8,
    vtInteger, amStatic, '',
    'ld (_v__Global_Test1),a:rla:sbc a,a:ld (_v__Global_Test1 +1),a',
    'a:/_v__Global_Test1#10');
  DoTestStoreVariable(rH, [], True, vtInt8,
    vtInteger, amStatic, '',
    'ld a,h:ld (_v__Global_Test1),a:rla:sbc a,a:ld (_v__Global_Test1 +1),a',
    'a:/_v__Global_Test1#10 h:\_v__Global_Test1#10');
  //Byte to Integer
  DoTestStoreVariable(rA, [], True, vtByte,
    vtInteger, amStatic, '',
    'ld (_v__Global_Test1),a:xor a:ld (_v__Global_Test1 +1),a',
//    'a:/_v__Global_Test1#10');
    'a:!$00');
  DoTestStoreVariable(rH, [], True, vtByte,
    vtInteger, amStatic, '',
    'ld a,h:ld (_v__Global_Test1),a:xor a:ld (_v__Global_Test1 +1),a',
//    'a:/_v__Global_Test1#10 h:\_v__Global_Test1#10');
    'a:!$00');
  //Int8 to Word
  DoTestStoreVariable(rA, [], True, vtInt8,
    vtWord, amStatic, '',
    'and a:jp m,raise_range:ld (_v__Global_Test1),a:xor a:ld (_v__Global_Test1 +1),a',
//    'a:/_v__Global_Test1#10');
    'a:$00');
  DoTestStoreVariable(rL, [], True, vtInt8,
    vtWord, amStatic, '',
    'bit 7,l:jp nz,raise_range:ld a,l:ld (_v__Global_Test1),a:xor a:ld (_v__Global_Test1 +1),a',
//    'a:/_v__Global_Test1#10 l:\_v__Global_Test1#10');
    'a:!$00');
  //Byte to Word
  DoTestStoreVariable(rA, [], True, vtByte,
    vtWord, amStatic, '',
    'ld (_v__Global_Test1),a:xor a:ld (_v__Global_Test1 +1),a',
//    'a:/_v__Global_Test1#10');
    'a:$00');
  DoTestStoreVariable(rH, [], True, vtByte,
    vtWord, amStatic, '',
    'ld a,h:ld (_v__Global_Test1),a:xor a:ld (_v__Global_Test1 +1),a',
//    'a:/_v__Global_Test1#10 h:\_v__Global_Test1#10');
    'a:$00');
  //Byte to Pointer
  DoTestStoreVariable(rA, [], True, vtByte,
    vtPointer, amStatic, '',
    'ld (_v__Global_Test1),a:xor a:ld (_v__Global_Test1 +1),a',
//    'a:/_v__Global_Test1#10');
    'a:$00');
  DoTestStoreVariable(rL, [], True, vtByte,
    vtPointer, amStatic, '',
    'ld a,l:ld (_v__Global_Test1),a:xor a:ld (_v__Global_Test1 +1),a',
//    'a:/_v__Global_Test1#10 l:\_v__Global_Test1#10');
    'a:$00');
end;

procedure TTestZ80StoreVariable.TestStoreRegStaticConvertTo8Bit;
begin
  //Basic loads 16-bit variable to 8-bit registers
  //Integer to Int8
  DoTestStoreVariable(rHL, [], True, vtInteger,
    vtInt8, amStatic, '',
    'ld a,l:rla:ld a,h:adc a,$00:jp nz,raise_range:ld a,l:ld (_v__Global_Test1),a',
//IDEAL:    'ld a,l:ld (_v__Global_Test1),a:rla:ld a,h:adc a,$00:jp nz,raise_range',
    'a:%_v__Global_Test1#10');
  //Integer to Byte
  DoTestStoreVariable(rDE, [], True, vtInteger,
    vtByte, amStatic, '',
    'ld a,d:and a:jp nz,raise_range:ld a,e:ld (_v__Global_Test1),a',
    'a:%_v__Global_Test1#10');
  //Word to Int8
  DoTestStoreVariable(rBC, [], True, vtWord,
    vtInt8, amStatic, '',
    'ld a,c:and $80:or b:jp nz,raise_range:ld a,c:ld (_v__Global_Test1),a',
    'a:%_v__Global_Test1#10');
  //Word to Byte
  DoTestStoreVariable(rHL, [], True, vtWord,
    vtByte, amStatic, '',
    'ld a,h:and a:jp nz,raise_range:ld a,l:ld (_v__Global_Test1),a',
    'a:%_v__Global_Test1#10');
  //Pointer to Byte
  DoTestStoreVariable(rDE, [], True, vtPointer,
    vtByte, amStatic, '',
    'ld a,d:and a:jp nz,raise_range:ld a,e:ld (_v__Global_Test1),a',
    'a:%_v__Global_Test1#10');
end;

procedure TTestZ80StoreVariable.TestStoreRegStaticExtend;
begin
  //Unsigned extending
  DoTestStoreVariable(rA, [], False, vtByte,
    vtWord, amStatic, '',
    'ld (_v__Global_Test1),a:xor a:ld (_v__Global_Test1 +1),a', 'a:!$00');
  DoTestStoreVariable(rB, [], False, vtByte,
    vtWord, amStatic, '',
    'ld a,b:ld (_v__Global_Test1),a:xor a:ld (_v__Global_Test1 +1),a', 'a:!$00');
  DoTestStoreVariable(rD, [], False, vtByte,
    vtPointer, amStatic, '',
    'ld a,d:ld (_v__Global_Test1),a:xor a:ld (_v__Global_Test1 +1),a', 'a:!$00');
  DoTestStoreVariable(rL, [], False, vtByte,
    vtWord, amStatic, '',
    'ld a,l:ld (_v__Global_Test1),a:xor a:ld (_v__Global_Test1 +1),a', 'a:!$00');

  //Signed extending
  DoTestStoreVariable(rA, [], False, vtInt8,
    vtInteger, amStatic, '',
    'ld (_v__Global_Test1),a:rla:sbc a,a:ld (_v__Global_Test1 +1),a', 'a:/_v__Global_Test1#10');
  DoTestStoreVariable(rH, [], False, vtInt8,
    vtInteger, amStatic, '',
    'ld a,h:ld (_v__Global_Test1),a:rla:sbc a,a:ld (_v__Global_Test1 +1),a', 'a:/_v__Global_Test1#10 h:\_v__Global_Test1#10');
end;

initialization
	TestFrameWork.RegisterTest(TTestZ80LoadLiteral.Suite);
	TestFrameWork.RegisterTest(TTestZ80LoadVariable.Suite);
	TestFrameWork.RegisterTest(TTestZ80StoreVariable.Suite);
end.

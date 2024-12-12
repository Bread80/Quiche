{
This file should only code which is generic to any CPU
}
unit CodeGen;

interface
uses Classes,
  Def.Globals, Def.Scopes, Def.IL;

//------------- UTILITIES

//Constants to hex strings
//Values are masked as needed to the given size
function ByteToStr(Value: Integer): String;
function WordToStr(Value: Integer): String;
function WordLoToStr(Value: Integer): String;

//Write an entire instruction line
procedure Instr(S: String);

procedure Line(S: String);

procedure Lines(S: String);

procedure Error(Msg: String);

procedure Opcode(const Op: String;const P1: String = '';const P2: String = '');

//Returns state variables
function GetCodeGenScope: PScope;
function GetCurrBlockID: Integer;
function GetCurrProcName: String;

//Insert code from a fragment or a library call
procedure GenLibraryProc(ProcName: String;ILItem: PILItem);

procedure GenCode(ProcName: String;ILItem: PILItem);


procedure InitialiseCodeGen(PlatformFile, QuicheLibrary: String);

//BlockType can be used to specify whether the root/global block should
//use stack vars or static vars.
//For functions (Scope.Func <> nil) BlockType MUST be btDefault
function CodeGenBlock(Scope: PScope;BlockType: TBlockType): Boolean;

procedure SaveAssemblyFile(FileName: String);

//Striclty for TESTING ONLY
function PeekAssembly: String;

var
  CodeGenErrorString: String;
  CurrErrorCount: Integer;  //In current routine
  TotalErrorCount: Integer; //In current build


//Option for testing
//If True every primitive used will be logged
var LogPrimitives: Boolean;
  PrimitiveLog: TStringList;

  function UsesPrimitive(const Name: String): Boolean;

implementation
uses SysUtils,
  Def.Functions, Def.Operators, Def.Primitives, Def.QTypes, Def.Variables,
  Parse.Base,
  CG.Fragments,
  Z80.CPU, Z80.Optimise, Z80.CPUState, Z80.CodeGen, Z80.LoadStoreMove, Z80.Load, Z80.Store,
  Z80.RegAlloc,
  IDE.Compiler; //<-- Allowed here ONLY so we can access assembler output meta data settings

function ByteToStr(Value: Integer): String;
begin
  Result := '$' + IntToHex(Value and $ff, 2).ToLower
end;

function WordToStr(Value: Integer): String;
begin
  Result := '$' + IntToHex(Value and $ffff, 4).ToLower
end;

function WordLoToStr(Value: Integer): String;
begin
  Result := '$' + IntToHex(lo(Value) and $ff, 2).ToLower
end;

//======================================= UTILITIES
var
  CodeGenScope: PScope;
  AsmCodeFull: TStringList;
  AsmCodeScope: TStringList;
  AsmDataFull: TStringList;
  AsmDataScope: TStringList;

function GetCodeGenScope: PScope;
begin
  Result := CodeGenScope;
end;


procedure Line(S: String);
begin
  AsmCodeFull.Add(S);
  if Assigned(AsmCodeScope) then
    AsmCodeScope.Add(S);
end;

procedure Lines(S: String);
begin
  Line(S);
end;

procedure SaveAssemblyFile(FileName: String);
begin
  AsmCodeFull.Append(AsmDataFull.Text);
  AsmCodeFull.Add('__quiche_end:');

  AsmCodeFull.SaveToFile(Filename);

end;


var CurrProcName: String;
var CurrBlockID: Integer;
  CurrSourceLineNo: Integer;
  LabelIndex: Integer;  //Used to generate unique labels. Cleared at the start of each routine

function GetCurrBlockID: Integer;
begin
  Result := CurrBlockID;
end;

function GetCurrProcName: String;
begin
  Result := CurrProcName;
end;

procedure Error(Msg: String);
begin
  inc(CurrErrorCount);
  inc(TotalErrorCount);
  Line('ERROR: ' + Msg + ' in ' + CurrProcName);
end;

function GetUniqueLabel: String;
begin
  Result := '.x'+IntToStr(LabelIndex);
  inc(LabelIndex);
end;

//Write an entire instruction line
procedure Instr(S: String);
begin
  Line('  '+S);

end;

procedure Opcode(const Op: String;const P1: String = '';const P2: String = '');
var S: String;
begin
  S := '  ' + Op;
  if P1 <> '' then
  begin
    S := S + ' ' + P1;
    if P2 <> '' then
      S := S + ',' + P2;
  end;
  Line(S);
end;

procedure GenLabel(Name: String);
begin
  Line(Name + ':');
end;


//=======================DATA SECTIONS

//Convert the string into a sanitised string for a data directive
function AsmSanitiseString(const S: String): String;
var InString: Boolean;
  I: Integer;
begin
  InString := False;
  for I := 0 to length(S)-1 do
    if (S.Chars[I] >= #32) and (S.Chars[I] < #128) then
    begin
      if not InString then
      begin
        if Result <> '' then
          Result := Result + ',';
        Result := Result + '''';
        InString := True;
      end;
      Result := Result + S.Chars[I];
    end
    else
    begin
      if InString then
      begin
        Result := Result + '''';
        InString := False;
      end;
      if Result <> '' then
        Result := Result + ',';
      Result := Result + ord(S.Chars[I]).ToString;
    end;

  if InString then
    Result := Result + '''';
end;

procedure GenLiterals(const Param: TILParam);
var Code: String;
  S: String;
begin
  if Param.Kind <> pkImmediate then
    EXIT;
  if not IsPointeredType(Param.Imm.VarType) then
    EXIT;
  case Param.Imm.VarType of
    vtString:
    begin
      S := Param.Imm.StringValue;
      Code := Param.Imm.ToLabel + ':'#13 +
        'db ' + length(S).ToString + ',' + AsmSanitiseString(S);
    end;
  else
    Assert(False);
  end;

  AsmDataScope.Append(Code);
  AsmDataFull.Append(Code);
end;

procedure LiteralsGen;
var I: Integer;
  ILItem: PILItem;
begin
    //Codegen for each item
  for I := 0 to ILGetCount-1 do
  begin
    ILItem := ILIndexToData(I);
    GenLiterals(ILItem.Param1);
    GenLiterals(ILItem.Param2);
    GenLiterals(ILItem.Param3);
  end;
end;

//For the current scope:
//Allocates global memory for static vars.
//Generates constants for offsets for stack vars
procedure DataGen;
var I: Integer;
  V: PVariable;
  S: String;
  C: Integer;
begin
  for I := 0 to VarGetCount-1 do
  begin
    V := VarIndexToData(I);
    case V.Storage of
      vsStatic:
      begin
        S := V.GetAsmName + ': ';
        case GetTypeSize(V.VarType) of
          1: S := S + 'db 0';
          2: S := S + '  dw 0';
        else
          S := 'db ';
          for C := 1 to GetTypeSize(V.VarType) do
          begin
            if C <> 1 then
              S := S + ',';
            S := S + '0';
          end;
        end;
      end;
      vsStack:
      begin
        S := V.GetAsmName + ' equ ' + abs(V.Offset).ToString;
      end;
    else
      Assert(False);
    end;

    AsmDataScope.Append(S);
    AsmDataFull.Append(S);
  end;
end;

//================================== LIBRARY CODE


//Insert code from a fragment or a library call
procedure GenLibraryProc(ProcName: String;ILItem: PILItem);
begin
  if ProcName.Chars[0] = ':' then
    Instr('call ' + ProcName.SubString(1) + ' ;Call')
  else
    CG.Fragments.GenFragmentItemName(ProcName, ILItem, GetCodeGenScope);
end;

procedure GenCode(ProcName: String;ILItem: PILItem);
var
  Prim: PPrimitive;
  Proc: TCodeGenProc;
begin
  if ProcName = 'empty' then
    EXIT;
  if ProcName = '' then
    EXIT;

  if ProcName.Chars[0] = ':' then
    GenLibraryProc(ProcName, ILItem)
  else
  begin
    Prim := PrimFindByProcName(ProcName);
    if Assigned(Prim) then
    begin
    if IDE.Compiler.Config.CodeGen.PrimitiveNames then
        Line('                     ;Prim: ' + ProcName);
      Prim.Proc(ILItem);
    end
    else
    begin
      Proc := FindCodeGenProc(ProcName);
      if Assigned(Proc) then
        Proc(ILItem)
      else
        GenLibraryProc(ProcName, ILItem);
    end;
  end;
end;

//If overflow checks are enabled, applies such overflow checks.
//This routine is called after the primitive has executed.
//The validation routine (if there is one) is specifiied in the 'Validate' column
//of the Primitives table (spreadsheet)
procedure OverflowCheckAfterPrim(ILItem: PILItem;Prim: PPrimitive);
begin
  if Prim <> nil then
    if cgOverflowCheck in ILItem.Flags then
      //Validation for the operation itself
      GenCode(Prim.OverflowCheckProcName, ILItem);
end;

procedure GenPrimitive(var ILItem: TILItem;ILIndex: Integer);
var Prim: PPrimitive;
  SwapParams: Boolean;
begin
  //Find the Prim based on the operation and parameter data type(s)
  Prim := ILItemToPrimitive(ILItem, SwapParams);
  if not assigned(Prim) then
    Error('No primitiveNG found:'#13#10 + ILItem.ToString);
  if SwapParams then
    ILItem.SwapParams;
  if LogPrimitives then
    PrimitiveLog.Add(Prim.ProcName);

  //Temp
  TEMPRegAllocPrim(ILItem, Prim);

  //TODO: This should be done at Primitive selection time (or other)
  Assert(ILItem.Param1.LoadType = lptNormal);
  ILItem.Param1.LoadType := Prim.LLoadType;

  //Load parameters
  GenRegLoad(ILIndex, Prim);
//  LoadBeforePrim(ILItem, Prim);

  if Assigned(Prim.Fragment) then
    GenFragmentItem(Prim.Fragment, @ILItem, GetCurrentScope)
  else if Assigned(Prim.Proc) then
    Prim.Proc(@ILItem)
  else
    GenLibraryProc(Prim.ProcName, @ILItem);

  if not Prim.ProcMeta.HaveCorrupts then
    //Fallback default -  clear all state
    RegStateInitialise;

  OverflowCheckAfterPrim(@ILItem, Prim);
  //Also generates branches and updates CPU state for Dest
  StoreAfterPrim(@ILItem, Prim);
end;

//========================= CODE GENERATOR

procedure DoCodeGenItem(ILIndex: Integer);
var ILItem: PILItem;
  Comments: String;
begin
  ILItem := ILIndexToData(ILIndex);

  if (ILItem.SourceLineNo <> -1) and (ILItem.SourceLineNo <> CurrSourceLineNo) then
  begin //Output source code line
    CurrSourceLineNo := ILItem.SourceLineNo;
    if IDE.Compiler.Config.CodeGen.SourceCode then
      Line(';' + IntToStr(CurrSourceLineNo) + ': ' + Parser.Source[CurrSourceLineNo].Trim);
  end;  //Output block ID

  if IDE.Compiler.Config.CodeGen.ILCode then
    Line(';IL-' + ILIndex.ToString +': ' + ILItem.ToString);

  if ILItem.BlockID >= 0 then
  begin
    CurrBlockID := ILItem.BlockID;
    if IDE.Compiler.Config.CodeGen.BlockInfo then
      Comments := ' ;' + ILItem.Comments
    else
      Comments := '';

    Line(CurrProcName + IntToStr(ILItem.BlockID) + ':' + Comments);

    //TEMP - clear all state
    RegStateInitialise;
  end;


  case ILItem.Op of
    //Operations which don't use the parameter load-store mechanism below
    opPhi: RegStateInitialise; //Branches merge here. Clear cached data - we don't know what might be there
    opStoreImm: //Store a literal into a variable
      GenStoreImm(ILItem, []);
    opMove: //Move a single value between variables or to/from the stack etc.
    begin //TODO: Rework this to be neater
      //When loading: load to type specified in Dest (extend, range check etc).
      //GenOpMove(ILItem);
      TEMPRegAllocMove(ILItem); //(??)
      GenLoadParam(ILItem.Param1, ILItem.Dest.GetVarType, []);
      //Range checking and extending (if required) is done while loading
      GenDestParam(ILItem.Dest, vtUnknown, False, nil, [])
     end;
    opBranch: GenUncondBranch(ILItem);
    opBoolVarBranch:  //Branch where condition is a boolean variable (which could
                      //be in a CPU flag)
    begin //TODO: Rework this to be neater
      //GenOpMove(ILItem);
      TEMPRegAllocBoolVarBranch(ILItem); //(??)
      GenLoadParam(ILItem.Param1, vtFlag, []);
      GenDestParam(ILItem.Dest, vtUnknown, False, nil, [])
     end;
    opRegLoad, opRegLoadExtended: //Load multiple values to registers
      ILIndex := GenRegLoad(ILIndex, nil);
    opRegStore, opRegStoreExtended: //Store multiple values from registers
      ILIndex := GenRegStore(ILIndex);
    opFuncCall, opFuncCallExtended:
    begin
      GenRegLoad(ILIndex, nil);
      GenFuncCall(ILIndex);
      ILIndex := GenRegStore(ILIndex);
    end;

  else    //Operations which do use the parameter load-store mechanism
    GenPrimitive(ILItem^, ILIndex);
  end;

  if IDE.Compiler.Config.CodeGen.CPUState then
    Line(';'+CPUStateToString);
end;

function CodeGenBlock(Scope: PScope;BlockType: TBlockType): Boolean;
var I: Integer;
begin
  RegStateInitialise;
  //TODO: For Register calling convention, set CPUState any parameter

  CodeGenScope := Scope;
  try
    CodeGenErrorString := '';

    CurrErrorCount := 0;
    AsmCodeScope := Scope.AsmCode;
    AsmDataScope := Scope.AsmData;

    CreateVarMap;

    //Calc variables size/offsets
    VarSetOffsets;

    LiteralsGen;

    //Generate any global data
    DataGen;

    Line(';=========='+Scope.Name);
    Line('');

    CurrProcName := '_'+Scope.Name.ToLower;
    CurrBlockID := -1;
    CurrSourceLineNo := -1;
    LabelIndex := 1;

    GenLabel(CurrProcName);

    GenFunctionPreamble(Scope, BlockType);

    //Codegen for each item
    I := 0;
    while I < ILGetCount do
    begin
      DoCodeGenItem(I);
      //These op types extend into following IL items. Skip them
      while ILIndexToData(I).Op in [opRegLoadExtended, opRegStoreExtended, opFuncCallExtended] do
        inc(I);
      inc(I);
    end;


    GenLabel(CurrProcName+IntToStr(CurrBlockID+1));

    GenFunctionPostamble(Scope, BlockType);
    if IDE.Compiler.Config.CodeGen.VarMap then
    begin
      Line('');
      Line(VarMapToString);
    end;
    Line(';----------'+Scope.Name);
    Line('');

    Result := True;
  except
    on E:Exception do
    begin
      CodeGenErrorString := E.Message;
      Line('CODEGEN ERROR: ' + E.Message);
      Result := False;
    end;
  end;
end;

procedure InsertPreamble(PlatformFile, QuicheLibrary: String);
begin
  AsmCodeFull.Add(';Quiche object code');
  AsmCodeFull.Add(';Auto-created. Will be overwritten!');
  AsmCodeFull.Add(';Designed for RASM assembler');
  AsmCodeFull.Add('');
  AsmCodeFull.Add(';Insert platform specific code');
  AsmCodeFull.Add('include "' + PlatformFile + '"');
  AsmCodeFull.Add('');
  AsmCodeFull.Add(';Insert Quiche libraries');
  AsmCodeFull.Add('include "' + QuicheLibrary + '"');
  AsmCodeFull.Add('');
  AsmCodeFull.Add(';Generated code starts here');
  AsmCodeFull.Add('quiche:');
end;

procedure InitialiseCodeGen(PlatformFile, QuicheLibrary: String);
begin
  if PrimitiveLog = nil then
    PrimitiveLog := TStringList.Create
  else
    PrimitiveLog.Clear;

  InitPrimitives;
  CurrErrorCount := 0;
  TotalErrorCount := 0;

  if AsmCodeFull = nil then
    AsmCodeFull := TStringList.Create
  else
    AsmCodeFull.Clear;
  AsmCodeScope := nil;
  if AsmDataFull = nil then
    AsmDataFull := TStringList.Create
  else
    AsmDataFull.Clear;
  AsmDataScope := nil;
  if (PlatformFile <> '') or (QuicheLibrary <> '') then
    InsertPreAmble(PlatformFile, QuicheLibrary);
end;

//Testing: Returns true if the name primitive have been used
function UsesPrimitive(const Name: String): Boolean;
var S: String;
begin
  for S in PrimitiveLog do
    if CompareText(S, Name) = 0 then
      EXIT(True);
  Result := False;
end;

function PeekAssembly: String;
var Line: String;
begin
  Result := '';
  for Line in AsmCodeFull do
  begin
    if Result <> '' then
      Result := Result + ':';
    Result := Result + Trim(Line);
  end;
end;

initialization
  LogPrimitives := False;

  PrimitiveLog := nil;
  AsmCodeFull := nil;
  AsmDataFull := nil;
finalization
  PrimitiveLog.Free;
  AsmCodeFull.Free;
  AsmDataFull.Free;
end.

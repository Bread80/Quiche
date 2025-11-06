{
This file should only code which is generic to any CPU
}
unit CodeGen;

interface
uses Classes,
  Def.Globals, Def.Scopes, Def.IL;

//------------- UTILITIES

procedure InitialiseCodeGen(PlatformFile, QuicheLibrary: String);

//Generates code for a code section (ie a function)
//BlockType can be used to specify whether the root/global block should
//use stack vars or static vars.
//For functions (Scope.Func <> nil) BlockType MUST be btDefault
function CodeGenSection(Scope: PScope;BlockType: TBlockType): Boolean;

//Returns the ID of the Block we're currently generating
function GetCGBlockID: Integer;

//Option for testing
//If True every primitive used will be logged
var LogPrimitives: Boolean;
  PrimitiveLog: TStringList;

function UsesPrimitive(const Name: String): Boolean;

implementation
uses SysUtils,
  Def.Functions, Def.Operators, Def.VarTypes, Def.Variables, Def.UserTypes, Def.Consts,
  Parse.Base,
  Lib.GenFragments, Lib.Data, Lib.Primitives,
  CG.Data, CG.VarMap,
  Z80.CG, Z80.LoadStoreMove, Z80.Load, Z80.Store, Z80.RegAlloc, Z80.CPUState, Z80.GenProcs,
  IDE.Compiler; //<-- Allowed here ONLY so we can access assembler output meta data settings


//======================================= UTILITIES

var CurrCGBlockID: Integer;
  CurrSourceLineNo: Integer;

function GetCGBlockID: Integer;
begin
  Result := CurrCGBlockID;
end;

//=======================DATA SECTIONS

//Convert the string into a sanitised string for a data directive
function AsmSanitiseString(const S: String): String;
var InString: Boolean;
  I: Integer;
begin
  InString := False;
  Result := '';
  for I := 0 to length(S)-1 do
    //Printable chars except single quotes
    if CharInSet(S.Chars[I], [#32..#128] - ['''']) then
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

procedure GenPointeredLiteral(C: PConst);
var S: String;
  Code: String;
begin
  case UTToVT(C.UserType) of
    vtString:
    begin
      S := C.Value.StringValue;
      Code := C.Value.ToLabel + ':'#13 +
        'db ' + length(S).ToString + ',' + AsmSanitiseString(S);
    end;
  else
    raise Exception.Create('Unknown VarType');
  end;

  AsmDataLine(Code);
end;


//Generates any constant data for pointered types in current scope
procedure GenPointeredLiterals;
var Scope: PScope;
  List: PConstList;
  I: Integer;
begin
  Scope := GetCurrentScope;
  List := Scope.ConstList;
  for I := 0 to List.Count-1 do
    if IsPointeredType(UTToVT(List.Items[I].UserType)) then
      GenPointeredLiteral(List.Items[I]);
end;

//For the current scope:
//Allocates global memory for static vars.
//Generates constants for offsets for stack vars
procedure DataGen;
var I: Integer;
  V: PVariable;
  S: String;
  Bytes: Integer;
  C: Integer;
begin
  for I := 0 to Vars.GetCount-1 do
  begin
    V := Vars.IndexToData(I);
    if V.RequiresStorage then
    begin
      case V.AddrMode of
        amStatic:
        begin
          S := V.GetAsmName + ': ';
          Bytes := GetTypeSize(V.UserType);
          case Bytes of
            0: ;  //TODO: ??
            1: S := S + 'db 0';
            2: S := S + '  dw 0';
          else
            S := S + 'db ';
            for C := 1 to Bytes do
            begin
              if C <> 1 then
                S := S + ',';
              S := S + '0';
            end;
          end;
        end;
        amStaticRef:
        begin
          Assert(GetVarTypeSize(vtPointer) = 2);
          S := V.GetAsmName + ': dw 0';
        end;
        amStack, amStackRef:
        begin
          S := V.GetAsmName + ' equ ' + abs(V.Offset).ToString;
        end;
      else
        Assert(False);
      end;

      AsmDataLine(S);
    end;
  end;
end;

//================================== LIBRARY CODE

//If overflow checks are enabled, applies such overflow checks.
//This routine is called after the primitive has executed.
//The validation routine (if there is one) is specifiied in the 'Validate' column
//of the Primitives table (spreadsheet)
procedure OverflowCheckAfterPrim(ILItem: PILItem;Prim: PPrimitive);
begin
  if Prim <> nil then
    if cgOverflowCheck in ILItem.Flags then
      if Prim.OverflowCheckProcName <> '' then
        GenLibraryProc(Prim.OverflowCheckProcName, ILItem)
      else
        Assert(False);  //All primitives which allow overflow checking must specify
                        //an overflow check routine (Primitives.csv)
end;

procedure GenPrimitiveOLD(var ILItem: TILItem;ILIndex: Integer);
var Prim: PPrimitive;
  SwapParams: Boolean;
begin
  begin //OLD METHOD (Without CleverPuppy)
    //Find the Prim based on the operation and parameter data type(s)
    Prim := ILItemToPrimitive(ILItem, SwapParams);
    if not assigned(Prim) then
      AsmError('No primitiveNG found:'#13#10 + ILItem.ToString);
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
//    LoadBeforePrim(ILItem, Prim);

    if Assigned(Prim.Fragment) then
      GenFragmentItem(Prim.Fragment, @ILItem)
    else
      GenLibraryProc(Prim.ProcName, @ILItem);

    if not Prim.ProcMeta.HaveCorrupts then
      //Fallback default -  clear all state
      RegStateInitialise;

    OverflowCheckAfterPrim(@ILItem, Prim);
    //Also generates branches and updates CPU state for Dest
    StoreAfterPrim(@ILItem, Prim);
  end;
end;


procedure GenPrimitive(var ILItem: TILItem;ILIndex: Integer);
begin
  if Assigned(ILItem.Prim) then
  begin
    //NEW METHOD (With CleverPuppy):
    //Primitive has been pre-assigned, which implies all other register allocation
    //etc data must also have been applied

    if LogPrimitives then
      PrimitiveLog.Add(ILItem.Prim.ProcName);

    //TODO: Param Load Type (must have been done by allocator)

    //Generate any code for source algos
    GenSourceParams(ILIndex);

    if Assigned(ILItem.Prim.Fragment) then
      GenFragmentItem(ILItem.Prim.Fragment, @ILItem)
    else
      GenLibraryProc(ILItem.Prim.ProcName, @ILItem);

    //Genate code for overflow algo (if there is one)
//TODO    GenOverflowCheck(@ILItem, Prim);

    //Generate code for Dest algos.
    //Also generates branches and updates CPU state for Dest
    GenDestParams(ILIndex);
  end
  else
    GenPrimitiveOLD(ILItem, ILIndex);
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
    if IDE.Compiler.GetConfig.CodeGen.SourceCode then
      AsmLine(';' + IntToStr(CurrSourceLineNo) + ': ' + Parser.Source[CurrSourceLineNo].Trim);
  end;  //Output block ID

  if IDE.Compiler.GetConfig.CodeGen.ILCode then
    AsmLine(';IL-' + ILIndex.ToString +': ' + ILItem.ToString);

  if ILItem.BlockID >= 0 then
  begin //The start of a new block
    CurrCGBlockID := ILItem.BlockID;
    if IDE.Compiler.GetConfig.CodeGen.BlockInfo then
      Comments := ' ;' + ILItem.Comments
    else
      Comments := '';

    AsmLine(GetCurrProcName + IntToStr(ILItem.BlockID) + ':' + Comments);

    //TEMP - clear all state (we could have multiple predecessors and, therefore,
    //state is ambiguous).
    RegStateInitialise;
  end;

  case ILItem.Op of
    //Operations which don't use the parameter load-store and primitive mechanisms below
    opPhi: RegStateInitialise; //Branches merge here. Clear cached data - we don't know what might be there
    opStoreImm: //Store a literal into a variable
      GenStoreImm(ILItem, []);
    opTypecast:
    begin
      TEMPRegAllocTypecast(ILItem);
      GenRegLoad(ILIndex, nil);
      GenTypecast(ILItem);
      GenRegStore(ILIndex);
    end;
    opMove: //Move a single value between variables or to/from the stack etc.
    begin //TODO: Rework this to be neater
      //When loading: load to type specified in Dest (extend, range check etc).
      //GenOpMove(ILItem);
      TEMPRegAllocMove(ILItem); //(??)
      GenLoadParam(ILItem.Param1, ILItem.Dest.GetUserType, []);
      //Range checking and extending (if required) is done while loading
      GenDestParam(ILItem.Dest, nil, False, nil, [])
    end;
    opBlockCopy:
    begin
      TEMPRegAllocBlockCopy(ILItem);
      GenRegLoad(ILIndex, nil);
      GenBlockCopy(ILItem);
    end;
    opBranch: GenUncondBranch(ILItem);
    opBoolVarBranch:  //Branch where condition is a boolean variable (which could
                      //be in a CPU flag)
    begin //TODO: Rework this to be neater
      //GenOpMove(ILItem);
      TEMPRegAllocBoolVarBranch(ILItem); //(??)
      GenLoadParam(ILItem.Param1, GetSystemType(vtFlag), []);
      GenDestParam(ILItem.Dest, nil, False, nil, [])
     end;
    opPtrLoad:
    begin
      TEMPRegAllocPtrLoad(ILItem);
      GenPtrLoad(ILItem^, []);
      GenDestParam(ILItem.Dest, nil, False, nil, [])
    end;
    opPtrStore:
    begin
      TEMPRegAllocPtrStore(ILItem);
      GenPtrStore(ILItem^, []);
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

  if IDE.Compiler.GetConfig.CodeGen.CPUState then
    AsmLine(';'+CPUStateToString);
end;

function CodeGenSection(Scope: PScope;BlockType: TBlockType): Boolean;
var I: Integer;
begin
  RegStateInitialise;
  //TODO: For Register calling convention, set CPUState any parameter

  try
    CGDataBlockInit(Scope);

    CreateVarMap;

    //Calc variables size/offsets
    Vars.SetOffsets;

    GenPointeredLiterals;
//    LiteralsGen;

    //Generate any global data
    DataGen;

    AsmLine(';=========='+Scope.Name);
    if Assigned(Scope.Func) then
      AsmLine(';'+Scope.Func.ToString);
    AsmLine('');

    CurrCGBlockID := -1;
    CurrSourceLineNo := -1;

    AsmLabel(GetCurrProcName);

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


    AsmLabel(GetCurrProcName+IntToStr(CurrCGBlockID+1));

    GenFunctionPostamble(Scope, BlockType);
    if IDE.Compiler.GetConfig.CodeGen.VarMap then
    begin
      AsmLine('');
      AsmLine(VarMapToString);
    end;
    AsmLine(';----------'+Scope.Name);
    AsmLine('');

    Result := True;
  except
    on E:Exception do
    begin
      CodeGenErrorString := E.Message;
      AsmLine('CODEGEN ERROR: ' + E.Message);
      Result := False;
    end;
  end;
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

  CGDataCompileInit;

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

initialization
  LogPrimitives := False;

  PrimitiveLog := nil;
finalization
  PrimitiveLog.Free;
end.

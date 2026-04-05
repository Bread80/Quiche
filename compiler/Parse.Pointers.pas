(*
******THIS TEXT IS OUT OF DATE*********

Instances of pointer variables (addressing modes)

a) SIMPLE TYPES

1. Direct: var d: Integer     //Stack/static
Syntax:  x := b + d
IL:   x add varsource b, varsource d
Asm:  LD HL,(var_b)   //Stack: (IX+offset)*
      LD DE,(var_d)   //  "
      ADD HL,DE
      LD (var_x),HL

2. Indirect: var pd: ^Integer //Stack/static
Syntax:  x := b + pd^
IL:   temp1 move varsource pd           //temp1 takes the value of pd (the address of the data)
      x add varsource b, varptr temp1   //VarPtr derefs temp1
Vars: temp1 is addressing mode StaticPtr/StackPtr - a pointer to the data
Asm:  LD HL,(var_pd)  //IL-1 Value of pd (addr of data). HL is temp1

      LD DE,(HL)      //IL-2 Load data (deref pd)
      EX HL,DE        //Move to HL
      LD DE,(var_d)   //  "
      ADD HL,DE
      LD (var_x),HL
Notes:  Code needs to disambiguate the meaning of pd^
        Thus we need to generate the extra step of IL to show that we eventually
        need the value pointed at by pd^, not the value of pd itself.
        If we used VarSource pd we'd get the value of pd
        If we used VarPtr pd we'd ???
Note 2: IL step 1 assists the parser - it is the IL representation of the ^ symbol
        and updates the state of both the parser and IL after consuming the ^
        This is not needed for paramaters by ref because no such symbol is present.
Note 2b:Without optimisation temp1 will be a duplicate of pd. This is an regretful
        necessity. However, the opimiser will easily be able to resolve the issue.
        Also, if the same variable is dereffed more than once in close succession
        (ie and both source and dest) the use of a discreet variable for the value
        should be easier to cache/optimise away.

3. By Reference: proc f(var br: Integer)  //Stack(?)
(depending on addressing mode)
Syntax:  x := b + br
IL:   x add varsource b, varsource br   //VarSource loads value of br
Vars: br is addressing mode StaticPtr/StackPtr
Asm:  LD HL,(var_br)  //IL-1 (addr of data)
      LD DE,(HL)      //IL-2 Load data
      EX HL,DE        //Move to HL
      LD DE,(var_d)   //  "
      ADD HL,DE
      LD (var_x),HL
Notes:  The meaning here is clear: we need to value of br
        VarSource always loads the value of a variable.
        VarPtr always loads the value pointed at by a variable if the variable is
        a pointer.


b) 'POINTERED' TYPES

1. Direct: var da: array ...  //Stack/Static
Syntax: x := x + da[n]
IL:   temp1 addrof da, n
      x add varsource x, varptr temp1
Asm:  LD HL,var_da    //IL-1
      LD DE,var_n
      LD DE,<multiply><offset> of var_n
      ADD HL,DE       //HL is temp1

      LD DE,(HL)      //IL-2 deref temp1
      LD HL,(var_x)   //Load x
      ADD HL,DE
      LD (var_x),HL

2. Indirect: var pda: ^array ...  //Stack/Static
Syntax: x := x + da^[n]
IL:   temp1 move varsource da     //temp1 becomes base of array - as for simple types
      temp2 addrof varsource temp1, n
      x add varsource x, varptr temp2
Vars: temp1 and temp2 are both of type StaticPtr/StackPtr (depending on array storage)
Asm:  LD HL,(var_da)  //IL-1 HL is addr of data

      LD DE,var_n     //IL-2
      LD DE,<multiply><offset> of var_n
      ADD HL,DE       //HL is temp1

      LD DE,(HL)      //IL-3 deref temp1
      LD HL,(var_x)   //Load x
      ADD HL,DE
      LD (var_x),HL

3. By Reference: proc f(var bra: array ...) //Stack/Static
Syntax: x := x + bra[n]
IL:   temp1 addrof varsource bra, n
      x add varsource x, varptr temp1
Asm:  LD HL,(var_bra)  //IL-1 HL is addr of data
      LD DE,var_n
      LD DE,<multiply><offset> of var_n
      ADD HL,DE       //HL is temp1

      LD DE,(HL)      //IL-2 deref temp1
      LD HL,(var_x)   //Load x
      ADD HL,DE
      LD (var_x),HL


Primitive lookup
----------------
If parameter is VarPtr we need to lookup routines for the type pointed to by the parameter.



Addressing Modes and Parameter Kinds
------------------------------------
An addressing mode specifies how data for a variable is stored
A parameter kind specifies how a variable will be used within an expression item
either as a source or setination side of the expression

Addressing Mode
amStatic    //Stored at fixed address
amStack     //Stored on stack (offset from IX)
amStaticPtr //Address stored at fixed address
amStackPtr  //Address stored on stack (offset from IX)

Param Kinds

Simple Types
pkVarSource - Load value of the variable
  amStatic:     LD r,(v)
  amStack:      LD r,(IX+o)
  amStaticPtr:  LD HL,(v)
                LD r,(HL)
  amStaticPtr:  LD L,(IX+o)
                LD H,(IX+o+1)
                LD r,(HL)
pkVarDest - Store value of variable
  amStatic:     LD (v),r
  amStack:      LD (IX+o),r
  amStaticPtr:  LD HL,(v)
                LD (HL),r
  amStaticPtr:  LD L,(IX+o)
                LD H,(IX+o+1)
                LD (HL),r

Pointer Variables (To Simple Types)
  - Pointer dereferencing is seperate, discrete, IL step
  IL first derefs the pointer - gets the /value/ of the pointer variable
  then reads or writes the pointed to data
  To encode x^ := x^ + y
  IL: {vardest temp1} move {varsource x}
      {varptr temp1} add {varptr temp1}, y
  temp1 will be Static or Stack, as required.

pkVarPtr    - Load the value pointed to
  amStatic:     LD HL,(v)     //Get pointer
                LD r,(HL)     //Get data
  amStack:      LD L,(IX+o)   Get pointer
                LD H,(IX+o+1)
                LD r,(HL)     Get data
??(pkVarPtr)- Store the value pointed to
  amStatic:     LD HL,(v)     //Get pointer
                LD (HL),r     //Get data
  amStack:      LD L,(IX+o)   Get pointer
                LD H,(IX+o+1)
                LD (HL),r     Get data
(Note that the codes generated for a pkVarPtr for Static/Stack are the same as
those for amStaticPtr/amStackPtr

Pointered Types - types which are always referenced via pointers
amStaticRef    //Data stored at fixed address
amStackRef     //Data stored on stack (offset from IX)
amStaticRefPtr //Address of data is stored at fixed address
amStackRefPtr  //Address of data is stored on stack (offset from IX)

pkRef - Address of item
  amStaticRef:  LD HL,v
  amStackRef:   PUSH IX       ;Stack base
                POP HL
                LD DE,offset  ;Offset from stack base
                ADD HL,DE
  smStaticPtr:  LD HL,(v)     ;v stores address of data
  smStackPtr:   LD L,(IX+o)   ;v (on stack) stores address of data
                LD H,(IX+o+1)


*)

unit Parse.Pointers;

interface
uses Parse.Literals, Parse.Errors,
  Def.Consts, Def.Scopes, Def.ScopesEX, Def.Variables, Def.UserTypes, Def.IL;

const PtrSuffix = ['^','[','.'];

//Is the next char/chars a pointer suffix?
function TestForPtrSuffix: Boolean;


//Parses one or more suffixes which derefence pointers
//  x^  - Explicit pointer deref
//  X[] - Array index
//  X.  - Dotted suffix (a record field)
//Multiple suffixes can be parsed in one go:
//  X^[12]^.Field

//Parses pointered references on the right hand side of an expression
//Deref is the item to be derefenced (variable, const, function etc)
//Creates a hidden pointer variable which contains the resultant address and
//assigns that variable to a Slug.
//The slug's Param Kind will reflect the type of the created variable,
//pkVarRef for Pointered types
//pkVarPtr for non Pointered types
//opPtrLoad is used to assign the value to the slug's variable
function ParsePtrSuffixLoad(var Slug: TExprSlug;const Deref: TIdentData): TQuicheError;

//Parses a dereference on the assignment side of an expression.
//Uses ParsePointedSuffix to parse the code to dereference the item, then
//parses an assignment and expression. Then generates IL code to assign the expression
//to the dereferenced item.
//AddrVar is the variable being dereferenced.
//opPtrStore is used to store the expression's result to the parsed address
//TODO: Update to use a TIdentData so we can also dereference functions
function ParseAssignPtrSuffixStore(AddrVar: PVariable): TQuicheError;

implementation
uses
  SysUtils,
  Parse.Expr, Parse.Base, Parse.Source,
  Def.VarTypes, Def.Operators;

function TestForPtrSuffix: Boolean;
var Ch: Char;
  Cursor: TParseCursor;
begin
  Ch := Parser.TestChar;
  if CharInSet(Ch, ['^','[']) then
    EXIT(True);
  if Ch <> '.' then
    EXIT(False);

  //We're testing for '..' range operator here. Anything else will be considered
  //valid '.' dot syntax (at least for now...)
  Cursor := Parser.GetCursor;
  Parser.SkipChar;
  Result := Parser.TestChar <> '.';
  Parser.SetCursor(Cursor);
end;

//Parse array array reference.
//Generates an opAddrElement <array-var> <index>
//<index> may be an immediate or a variable. If the index is a run-time expression
//then <index> will be a temporary variable to which the index expression is
//assigned
//Parse must be positioned at the opening [
//Deref is the item being dereferenced, which can be a variable, constant or function (for the result)
//  If the expression is not a constant one it's value will be validated against the
//  type of Deref
//AddrVar returns tha variable to which the data pointer is assigned.
function ParseArrayIndexEX(const Deref: TIdentData;out AddrVar: PVariable): TQuicheError;
var IndexSlug: TExprSlug; //Slug for the index expresssion
  ArrayType: TArrayType;
  ILItem: PILItem;
  BoundsType: TOrdinalType;
  Index: Integer;
begin
  Assert(CharInSet(Parser.TestChar, ['[',',']));
  Parser.SkipChar;

  if Deref.GetUserType is TTypedPointer then
    ArrayType := (Deref.GetUserType as TTypedPointer).OfType as TArrayType
  else
    ArrayType := Deref.GetUserType as TArrayType;
  Assert(ArrayType <> nil);
  Assert(ArrayType.VarType = vtArrayType);

  case ArrayType.ArrayKind of
    atArray: BoundsType := (ArrayType as TPascalArrayType).BoundsType;
    atVector, atList: BoundsType := ArrayType.IndexMetaType;
  else
    raise EVarType.Create;
  end;
  //Index expression must be of BoundsType
  Result := ParseExprToSlugWithTypeCheck(IndexSlug, BoundsType);
  if Result <> qeNone then
    EXIT;

  //If we have a literal expression validate against length (vector) or capacity (list)
  //(Array types bounds will have been trapped by ParseExprTo due to BoundsType range)
  if IndexSlug.ILItem = nil then
    case ArrayType.ArrayKind of
      atArray: ;
      atVector:
      begin
        Assert(IsIntegerType(IndexSlug.ResultType));
        Index := IndexSlug.Operand.Imm.IntValue;
        if (Index < 0) or (Index >= (ArrayType as TVectorType).Length) then
          EXIT(Err(qeConstantOutOfRange));
      end;
      atList:
      begin
        Assert(IsIntegerType(IndexSlug.ResultType));
        Index := IndexSlug.Operand.Imm.IntValue;
        if (Index < 0) or (Index >= (ArrayType as TListType).Capacity) then
          EXIT(Err(qeConstantOutOfRange));
      end;
    else
      raise EVarType.Create;
    end;

  if IndexSlug.ILItem <> nil then
    IndexSlug.AssignToHiddenVar;

  //Get the address of the element into AddrVar as a typed pointer
  //opAddrElement will give us @V + MetaSize + ElementSize * Param2 where V is Param1
  ILItem := ILAppend(opAddrElement);
  //First parameter is the address of the array
  case Deref.IdentType of
    itVariable: ILItem.Param1.SetVarRef(Deref.V);
    itConst: ILItem.Param1.SetImmediate(Deref.C.Value);
  else
    raise Exception.Create('Unknown IdentType');
  end;

  ILItem.Param2 := IndexSlug.Operand;
  ILItem.ResultType := TTypes.SearchScopesForAnonTypedPointer(ArrayType.OfType);
  AddrVar := ILItem.AssignToHiddenVar(ILItem.ResultType);

  Result := Parser.SkipWhite;
  if Result <> qeNone then
    EXIT;

(*  if Parser.TestChar = ',' then
    if IsArrayType(ArrayType.VarType) then
      //TODO: comma separated multi-dimensional array references - recurse back to ourselves
      //(references using multiple square brace syntax will be parsed at a higher level)
      Assert(False, 'TODO')
    else
      EXIT(Err(qeArrayTooManyDimensions))
  else*) if Parser.TestChar <> ']' then
    EXIT(Err(qeCloseSquareBraceExpected));

  Parser.SkipChar;
end;

//Parses ^ suffix (pointer dereference)
//Deref is the item being derefenced (variable, constant or (function result))
//AddrVar returns the variable holding the data pointer
function ParsePtrSuffix(const Deref: TIdentData;out AddrVar: PVariable): TQuicheError;
var ILItem: PILItem;
  DerefType: TUserType;
begin
  Assert(Parser.TestChar = '^');
  Parser.SkipChar;
  DerefType := Deref.GetUserType;

  if not (DerefType.VarType in [vtTypedPointer, vtPointer]) then
    EXIT(ErrSub(qePointerDerefError, DerefType.Description));
  Assert(Deref.GetAddrMode in [amStatic, amStack], 'TODO');

  //Gen IL code to assign var to a temp var with addressing mode StaticPtr/StackPtr
  //and Type which is the OfType of V
  ILItem := ILAppend(opMove);    //A Sugar op - no data actually changes...
  begin
    case Deref.IdentType of
      itVariable: ILItem.Param1.SetVarSource(Deref.V);
    else
      raise exception.Create('TODO: IdentType');
    end;

    if DerefType.VarType = vtPointer then
      ILItem.ResultType := TTypes.SearchScopesForAnonTypedPointer(GetSystemType(vtByte))
    else
      ILItem.ResultType := DerefType;
  end;

  AddrVar := ILItem.AssignToHiddenVar(ILItem.ResultType);
  case Deref.GetAddrMode of              //...but we need to update the addressing mode
    amStatic, amStaticRef: AddrVar.AddrMode := amStatic;//Ptr;
    amStack{, amStackPtr}: AddrVar.AddrMode := amStack;//Ptr;
    //?? What if already StackPtr.StaticPtr?
  else
    Assert(False);
  end;
  Result := qeNone;
end;

//Parses pointer suffixes.
//Generates IL code to load the pointers *value* into a temp var.
//IL generated may be somewhat redundant depending on the addressing mode of the
//variable, but will be needed where the address of V itself also has to be retrieved
//(ie. for StaticPtr and StackPtr variables).
//Suffuxes:
//  ^           to dereference a pointer
//  [<index>]   to address an array element
//  .<field>    to access a record field
//Deref is the item to be dereferenced (a variable, constant or function (result))
//AddrVar returns a variable holding the address of the data (ie a pointer)
function ParsePointedSuffix(const Deref: TIdentData;out AddrVar: PVariable): TQuicheError;
var Ch: Char;
  CurrIdent: TIdentData;
begin
  Assert(CharInSet(Parser.TestChar, PtrSuffix));
  CurrIdent := Deref;

  while True do
  begin
    Ch := Parser.TestChar;
    case Ch of
      '^': Result := ParsePtrSuffix(CurrIdent, AddrVar);
      '[': Result := ParseArrayIndexEX(CurrIdent, AddrVar);
      '.':  //Dotted syntax for record fields
        Assert(False, 'TODO');
    else
      EXIT(qeNone);
    end;
    if Result <> qeNone then
      EXIT;

    CurrIdent.IdentType := itVariable;
    CurrIdent.V := AddrVar;
  end;
end;

//As ParsePointedSuffix but also generates IL to load the value being dereferenced.
//Returns a Slug with the code.
function ParsePtrSuffixLoad(var Slug: TExprSlug;const Deref: TIdentData): TQuicheError;
var ILItem: PILItem;
  AddrVar: PVariable;
  DestVar: PVariable;
  OfType: TUserType;
begin
  //Call PassPointedSuffix
  Result := ParsePointedSuffix(Deref, AddrVar);
  if Result <> qeNone then
    EXIT;

  //Generate the Slug with appropriate Kind (eq pkVarPtr)
  if IsPointeredType(AddrVar.UserType) then
  begin //Return a VarRef to the data. Caller will have to work out what to do with it
    Slug.Operand.SetVarRef(AddrVar);
    Slug.ResultType := AddrVar.UserType;
    Slug.ImplicitType := Slug.ResultType;
  end
  else
  begin
    Assert(AddrVar.UserType.VarType in [vtPointer, vtTypedPointer]);
    OfType := (AddrVar.UserType as TTypedPointer).OfType;
    if IsPointeredType(OfType) then
    begin //Return a VarRef to the data. Caller will have to work out what to do with it
      Slug.Operand.SetVarRef(AddrVar);
      Slug.ResultType := OfType;
      Slug.ImplicitType := OfType;
    end
    else
    begin
      //Load the actual data with a PtrLoad operation
      ILItem := ILAppend(opPtrLoad);
      ILItem.Param1.SetVarPtr(AddrVar);
      ILItem.ResultType := OfType;
      DestVar := ILItem.AssignToHiddenVar(OfType);

      Slug.Operand.SetVarSource(DestVar);
      Slug.ResultType := DestVar.UserType;
      Slug.ImplicitType := Slug.ResultType;
    end;
  end;
end;

//Where AddrVar is a variable containing the address to store data to,
//parses the assignment operator then the expression to be assigned and
//generates the IL code for the PtrStore
function DoAssignPtrStore(AddrVar: PVariable;ExprType: TUserType): TQuicheError;
var ExprSlug: TExprSlug;
  ILItem: PILItem;
begin
  Parser.SkipWhite;

  if not TestAssignment then
    EXIT(Err(qeAssignmentExpected));

  //Parse the assignment expression
  Result := ParseExprToSlugWithTypeCheck(ExprSlug, ExprType);
  if Result <> qeNone then
    EXIT;

  ILItem := ILAppend(opPtrStore);
  if IsPointeredType(AddrVar.UserType) then
    Assert(False, 'TODO')
(*    ILItem.Param1.SetVarRef(AddrVar);*)(*TODO*)
  else  //Param is the value of the pointer (the address to read from/write to)
        //So we need a VarSource, NOT a VarPtr (which would load the value pointed to)
    ILItem.Param1.SetVarSource(AddrVar);

  if ExprSlug.ILItem <> nil then
  begin
    ExprSlug.AssignToHiddenVar;
    ILItem.Param2.SetVarSource(ExprSlug.ILItem.Dest.Variable);
  end
  else
    ILItem.Param2 := ExprSlug.Operand;

  ILItem.ResultType := nil;
end;

function ParseAssignPtrSuffixStore(AddrVar: PVariable): TQuicheError;
var WriteType: TUserType;
  Deref: TIdentData;
  DestVar: PVariable;
begin
  Deref.IdentType := itVariable;
  Deref.V := AddrVar;
  Result := ParsePointedSuffix(Deref, DestVar);
  if Result <> qeNone then
    EXIT;

  //Get the type we're actually writing
  Assert(DestVar.UserType is TTypedPointer);
  WriteType := (DestVar.UserType as TTypedPointer).OfType;
  //Parse Assignment expression
  Result := DoAssignPtrStore(DestVar, WriteType);
end;

end.

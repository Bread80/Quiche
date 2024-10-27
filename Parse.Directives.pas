unit Parse.Directives;

(*
$O, $OVERFLOW  ;(boolean) Overflow checking
$R, $RANGE     ;(boolean) Range checking (boolean)

$VARAUTOCREATE ;(boolean) Assignments to unknown variables auto-created the
               ;variable. (Ie. No need to use VAR). Indended for use in interactive
               ;mode. Not recommended for file-based compilations.

$CALLING       ;Set the default calling convention for functions. Can be overridden
               ;on a per function basis with appropriate directives.
               ;Valid values: REGISTER, STACK

Not yet implemented:
$PLATFORM     ;Specify the target platform - Only valid before the first line of code
*)

interface
uses Parse.Errors, Parse.Base;

//Parse a directive (or list of comma separated directives)
function ParseDirective: TQuicheError;

implementation
uses SysUtils,
  Def.Globals, Def.Functions;

function ParseBoolean(out Value: Boolean): TQuicheError;
var Ch: Char;
begin
  Ch := Parser.TestChar;
  if Ch = '+' then
  begin
    Value := True;
    Parser.SkipChar;
    EXIT(qeNone);
  end
  else if Ch = '-' then
  begin
    Value := False;
    Parser.SkipChar;
    EXIT(qeNone);
  end;

  EXIT(Err(qeDirectiveSyntax));
end;

function ParseCallingConvention: TQuicheError;
var Ident: String;
  Directive: TFuncDirective;
  Value: TCallingConvention;
begin
  Result := Parser.SkipWhite;
  if Result <> qeNone then
    EXIT;

  Result := ParseIdentifier(#0, Ident);
  if Result <> qeNone then
    EXIT;

  Directive := IdentToFuncDirective(Ident);
  case Directive of
    dirREGISTER: optDefaultCallingConvention := ccRegister;
    dirSTACK:    optDefaultCallingConvention := ccStack;
  else
    Result := ErrSub(qeInvalidDirectiveValue, Ident);
  end;
end;

function ParseDirective: TQuicheError;
var Ident: String;
begin
  while True do
  begin
    Result := ParseIdentifier(#0, Ident);
    if Result <> qeNone then
      EXIT;

    //Boolean directives - Code generation
    if (CompareText(Ident, 'O') = 0) or (CompareText(Ident, 'OVERFLOW') = 0) then
      Result := ParseBoolean(optOverflowChecks)
    else if (CompareText(Ident, 'R') = 0) or (CompareText(Ident, 'RANGE') = 0) then
      Result := ParseBoolean(optRangeChecks)

    //Parameter directives - Code generation
    else if CompareText(Ident, 'CALLING') = 0 then
      Result := ParseCallingConvention

    //Boolean directives - Syntax
    else if CompareText(Ident, 'VARAUTOCREATE') = 0 then
      Result := ParseBoolean(optVarAutoCreate)

    else
      Result := ErrSub(qeUnknownDirective, Ident);

    Result := Parser.SkipWhite;
    if Result <> qeNone then
      EXIT;
    if Parser.TestChar <> ',' then
      EXIT;
    Parser.SkipChar;
    Result := Parser.SkipWhite;
    if Result <> qeNone then
      EXIT;
  end;
end;

end.

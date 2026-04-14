(*
A scope holds data about variables, types, constants, functions etc that have been
created by the current code 'segment'. Segment here usually refers to a single function
or the global scope.

A scope also has a pointer to the 'parent' scope. The parent scope is the next higher
level function or, ultimately, the global scope.

The parser will create a new scope at the start of every new function. The parser
will end that scope once it reaches the end of the function.

Scopes are mostly transparent to the rest of the compiler. Other than the above use
case at the start and end of parsing a function, scopes are also use by Variables,
Function, Types and Consts when searching and, occasionally, when adding, items.
In such uses the CurrentScope must be obtained and restored after setting and searching
parent scopes.

The parser also stores ILData and Assembly output for each scope for debugging etc
purposes.
*)

unit Def.Scopes;

interface
uses Classes,
  Def.Compiler, Def.Functions, Def.IL, Def.Variables, Def.Consts, Def.UserTypes, Def.ScopesEX,
  CleverPuppy;

implementation
uses Generics.Collections, SysUtils,
  Def.Globals;
end.

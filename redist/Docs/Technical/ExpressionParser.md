# The Quiche Expression Parser

This document explains how the Quiche expression parser functions.

One of the design goals for Quiche is that it will be able to be self hosted on the target device. Ie. it will, eventually, be able to run on the Z80 processor. That has resulted in an expression parser which works differently to that of the majority of compilers.

Firstly, the parser directly outputs Intermediate Language (IL) and does not tokenise the input or generate a parse tree. Secondly the parser uses a simple recursive algorithm consisting, essentially, of two relatively short functions. Since this is different to common practice it's probably worth a detailed explanation of how things work.

## Expressions are Everywhere

```A := 1
A := B
A := B + 1
A := False
A := -B
A := B + (X * Y)
A := f(B, C, D/2) + 5```

Above are several examples of expressions. := is the assignment operator in Quiche. It probably won't surprise you to learn the everything to the right of the assignment operator in these examples is an expression. Note though that some expression contain a single term (and no operators). Others contain only constant values.

What you might not realise is that the parameter list of a function call consists of a series of expression separated by commas. In the last example ```B```, ```C```, and ```D/2``` are all separate expressions. Each is parsed in turn to build up the arguments to pass to the function.

## Parsing basics

Here's a fairly typical looking expression:

```b + c * d / e - f```

As I'm sure you're aware different operators have different precedences and therefor evaluation is not strictly left to right. In the example expression the multiplication must be performed first, then the division. After that the addition and then the subtraction must be carried out.

The Quiche expression parser divides the expression into 'slugs' (for want of a better term). Each stug consists of an operand (here b,c,d,e,f) and an operator (here +, *, /, -). 

An operand could be any of:
* A constant value (number, character, string)
* An identifier (a variable, symbolic constant)
* A function (I'll deal with this later)
* A sub-expression in parentheses (another one best left for later)
* A unary operator (-, +, NOT etc - and also to be left for later)

An operator could be:
* a symbol (+, -, =, etc)
* an identifier (and, shl, in etc)

Divided up the above expression appears as:

```b + | c * | d / | e - | f   |```

The parser begins at the start of the expression (the left hand side) and reads the first slug. Reading a slug involves attempting to parse an operand, adn then attempting to parse an operator. If the parser fails to parse an operand this generate an error. If the parser fails to parse an operator this is considered the end of the expression and the slug is given an opUnknown value for the operator. (I'll return late to explain how the compiler decides if an expression is syntactically complete).

Thus the parser acquires the first slug | b + |, which is called 'Left'.

If this first slug has the opUnknown operator, denoting the end of the expression, the parser simply returns the operand and the parsing of that expression is complete.

If the parser did read an operand it then reads the next slug, in this can | c * |, calling if 'Right'.

It now compares the precedences of the operators in the two slugs, Left and Right.

If Right's operand is of equal of lower precedence, or if Right's operand is opUnknown, the expression can be evaluated from left to right. The parser generates an ILItem ***EXPLAIN*** for the two operands and Left's operator. The result of the ILItem is assigned to a temporary variable (%1).

if Right's operand is opUnknown then the expression is finished and the code can exit.
Otherwise Left is then updated to have operand %1 and the operator from Right and the code loops back to read another slug into Right.

---END

where | denotes each breaks between slugs. A special operator called opUnknown is used to denote the end of the expression - ie. 






***************************************************

Version 2

## Goals

For low end systems (Z80)
Efficient code
Simple data structures (array vs AST)

## IL data and TAC (Three Address Code)

## Slugs

## The Parser Itself

### Simple expression (no precedence)

### Precedence and Recursion

### Sub-expressions (Unary operators, Parentheses)

## Function Calls
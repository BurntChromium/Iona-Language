# Iona Language Compiler

## Language Features

## Compiler Features

### Good error messages 

Iona strives to make debugging as easy as possible by providing useful error messages, code context, cross-references to other parts of the codebase, and hints (where appropriate).

Here's an example of a very basic error (no hinting or references) that I manufactured (don't worry, parenthesis are properly recognized in normal operation).

```sh
Error: unrecognized symbol: ')' at line 13, column 44
  12 |     let x_dist :: float = pow (minus p.x q.x) 2;
  13 |     let y_dist :: float = pow (minus p.y q.y) 2;
  14 |     return sqrt add x_dist y_dist;
```

### Working with Source

Run the compiler against an example program

```sh
cabal run iona-compiler -- ./examples/tc1.iona
```
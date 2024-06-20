# Iona Language Compiler

Iona is a hobby programming language to research advanced programming language features. It is a work in progress, and is not currently usable.

Iona is somewhere between a functional and an imperative paradigm (like Rust).

### Current Status

A(n incomplete) list of things done and things that need doing.

**Top level**

Bottom line
- ❌ Ready for personal use and experimentation
- ❌ Ready for professional or production use

Major milestones
- ❌ Functions
- ❌ Container types (list, vec, etc.)
- ❌ Custom types (structs, enums, etc.)
- ❌ Tests (testing within Iona programs, not testing of the compiler)

**Internals**

- ✅ Lexing
- ❌ First-pass parsing (a fixed set of grammars)
- ❌ Expression parsing
- ❌ Post-parsing processing: scope computation
- ❌ Post-parsing processing: function declaration
- ❌ Static analysis: function requirements 
- ❌ Static analysis: type checking
- ❌ Code generation: function declarations
- ❌ Code generation: function bodies/execution logic
- ❌ Code generation: custom and container types

## Language Features

### Contracts & Refinement Types

Iona supports contracts: runtime checks to prevent a program from entering an invalid state. There are three types of supported contract:

1. Preconditions: checks before a function is executed
2. Postconditions: checks on the result of a function, before it's returned
3. Invariants: checks during function execution

The goal of contracts is to try and catch potential runtime errors at compile time. Suppose you have a division function `fn div :: numerator -> denominator -> quotient`. You could always manually check in the body that `denominator != 0`, but if you make it a contract the compiler can warn you ahead of time about runtime problems based on the inputs you provide. For instance, when composing functions we can check that the post conditions of the inner function are at least as strict as the pre conditions of outer function.

At least with pre- and post- conditions this is the same idea as [refinement types](https://en.wikipedia.org/wiki/Refinement_type), like Liquid Haskell.

```rs
// Function with a (precondition) contract
// If a contract does not evaluate to True, it errors (think of a contract like a "whitelist" of allowed inputs)
fn div :: a int -> b int -> int {
    Properties: Pure Export
    In: b != 0 -> "b must not be 0"
    return a / b
}
```

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
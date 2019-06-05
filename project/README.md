# Scheme Interpreter in Haskell
Morgan Grant (mlgrant@ucsc.edu), UCSC CS 203 Final Project

This is my final project for CS 203. It's a REPL for Scheme, written in Haskell.

It consists of:

- A tokenizer written using alex, contained in Lexer.x
- A parser written using happy, contained in Parser.y
- Functions for evaluating Scheme ASTs, contained in Eval.hs.
- Data structures for representing Scheme states, contained in State.hs
- The REPL implementation, contained in REPL.hs

I implemented most of the core builtins, including `lambda`, `define`, `if`, `let`, and a number of arithmetic and boolean operations.

There is a quote implementation, but it can only be used to make lists of atomic values. I also implemented the higher-order functions `map`, `apply` and `filter`.

Build the REPL with
```
alex Lexer.x
happy Parser.y
ghc --make REPL.hs -o repl
```

rlwrap is recommended to add history and editing. Run the REPL with
```
rlwrap ./repl
```

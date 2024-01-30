# tilox.hs: toy interpreter for LOX in Haskell 

## license

GPLv3

## Scanner Design

The scanning process is a stateful computation.

A scanner is a function that

- receive a character or an EOF marker also the code location
- output 
  - (continue scanning) a scanner which can further receive an EOF marker or input characters, or 
  - (terminating) a list of tokens

A common approach to build compostable stateful computation is the state monad.
Our design is inspired by state monad.

Combing scanners

- sequential combination of two scanners
- read until the input meet a specific condition, transform the consumed inputs into a token, continue scanning with another scanner
- peek the next input character and determine which scanner to handle the rest of the input

## references

- [craftinginterpreters.com](https://craftinginterpreters.com/contents.html)
- [github repo munificent/craftinginterpreters: Repository for the book "Crafting Interpreters"](https://github.com/munificent/craftinginterpreters)

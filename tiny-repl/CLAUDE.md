# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

tiny-repl is a minimal Read-Eval-Print Loop (REPL) implementation in Haskell. The project demonstrates basic interactive input/output with a simple interpreter that echoes user input with "Interpreted: " prefix.

## Build System

This project uses Cabal as its build system (Cabal 3.0 specification, Haskell2010 standard, GHC 9.6.7).

### Common Commands

Build the project:
```bash
cabal build
```

Run the executable:
```bash
cabal run tiny-repl
```

Run tests:
```bash
cabal test
```

Clean build artifacts:
```bash
cabal clean
```

Build with warnings enabled (already configured in cabal file):
```bash
cabal build --ghc-options=-Wall
```

## Architecture

### Module Structure

- **app/Main.hs**: Entry point that initializes the REPL with a welcome message and calls `mainLoop`
- **src/MyLib.hs**: Core REPL logic
  - `mainLoop`: Implements the read-eval-print loop, handling user input recursively
  - `interpret`: Dispatcher function that processes input commands (currently returns `Nothing` for "exit", otherwise wraps input with "Interpreted: " prefix)
- **test/Main.hs**: Test suite placeholder (not yet implemented)

### REPL Flow

1. `Main.main` displays welcome message and invokes `MyLib.mainLoop`
2. `mainLoop` prompts for input and calls `interpret`
3. `interpret` returns `Maybe String`:
   - `Nothing` terminates the REPL
   - `Just output` prints the result and recursively calls `mainLoop`

### Extension Points

To add new REPL commands, modify the `interpret` function in src/MyLib.hs:17. The pattern matching structure makes it straightforward to add new command cases.

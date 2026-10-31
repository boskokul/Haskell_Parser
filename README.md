# Haskell_Parser

This repository implements the frontend half of a custom compiler pipeline:

Frontend (Haskell parser + type checker) -> Canonical Typed AST -> JSON Contract -> Backend (C++ LLVM, upcoming)

The goal of this branch is to stabilize the Haskell-side compiler frontend before moving to the native backend. The parser reads a small custom language, performs type inference, normalizes the AST via a conservative desugaring pass, and emits a JSON file that a downstream C++ LLVM backend can consume.

## Repository architecture

### 1. Frontend parser
- `hsParser.hs` contains the raw parser AST and grammar for the custom source language.
- It handles literals, arithmetic expressions, function-like syntax, and control-flow constructs such as `if` and `case`.

### 2. Canonical typed AST
- `AstTypes.hs` defines the compiler-facing typed representation.
- It models primitive types (`Int32`, `Float`, `Bool`, `String`), typed literals, typed expressions, function signatures, and the top-level program representation used by the backend.

### 3. Type inference and checking
- `TypeInfer.hs` validates the typed expressions and ensures that operations are type-correct.
- It infers result types for arithmetic, logical operators, list literals, and function calls.

### 4. Desugaring / normalization
- `Desugar.hs` normalizes the typed expressions into a canonical form that is easier to consume in later backend stages.
- This is intentionally conservative: it preserves semantics while simplifying the structure.

### 5. JSON contract for the backend
- `AstTypes.hs` includes `ToJSON`/`FromJSON` support for the typed AST.
- `Main.hs` writes `typed_ast.json`, which is the contract used by the future C++ LLVM backend.

## Local build options

The repository is currently a compact Haskell project built with GHC directly, but it can also be wrapped by Cabal or Stack if preferred.

### Build with GHC directly

```bash
ghc -O0 -o compiler Main.hs HsParser.hs AstTypes.hs TypeInfer.hs Desugar.hs
```

### Optional Cabal / Stack workflow

If you prefer package-driven builds, create a simple `cabal.project` or `stack.yaml` in the repo root and then run:

```bash
cabal build
```

or

```bash
stack build
```

## Running the frontend

The CLI entry point is `Main.hs`.

Usage:

```bash
./compiler <input-file> [output-json]
```

Example:

```bash
./compiler test.prog typed_ast.json
```

This will:
- parse the custom language source file
- lower it into a typed AST
- infer and validate types
- desugar the AST to canonical form
- emit `typed_ast.json`

## Example input

Create a sample program like this:

```text
add a b = a + b
x = 3 + 4
flag = if x > 5 then True else False
```

Then run:

```bash
./compiler test.prog typed_ast.json
```

If successful, the output file `typed_ast.json` should exist and be non-empty.

## What is generated

The JSON output is a typed, machine-readable AST with the following shape:

```json
{
  "tpFunctions": [
    {
      "tfSig": {
        "fsName": "add",
        "fsParams": [["a", "Int32"], ["b", "Int32"]],
        "fsReturnType": "Int32"
      },
      "tfBody": {
        "kind": "Binary",
        "op": "Add",
        "left": {"kind": "Variable", "name": "a"},
        "right": {"kind": "Variable", "name": "b"}
      }
    }
  ],
  "tpBindings": []
}
```

## Next steps

The typed AST frontend is complete enough to hand off to the C++ LLVM backend. The next phase is:

1. Read `typed_ast.json` in C++.
2. Reconstruct the typed AST in C++ memory.
3. Lower the typed IR into LLVM IR using `llvm::IRBuilder`.
4. Add optimization passes and a JIT / codegen pipeline.

## Local verification script

A convenience script is included at the repo root:

```bash
./verify.sh
```

This script compiles the Haskell frontend, writes a sample source file if needed, runs the CLI, and validates that `typed_ast.json` is created successfully.

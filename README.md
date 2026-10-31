# Haskell_Parser

This repository now includes a typed AST audit layer to support LLVM lowering work.

## What is included

- `AstTypes.hs`: explicit primitive type system and canonical AST model
- `TypeInfer.hs`: type checking / inference pass for typed expressions
- `Desugar.hs`: canonicalization pass for LLVM-friendly lowering
- `SerializeAst.hs`: serializer for downstream C++ compiler tooling

## Why this matters

The original parser is a useful front-end prototype, but it mixes parsing, expression structure, and type metadata in a way that is not ideal for LLVM lowering. The new modules deliberately keep the legacy parser intact while introducing a typed canonical layer suitable for code generation.

## How to verify

1. Build the legacy parser:

   ghc --make hsParser.hs

2. Run the parser against the sample language file:

   ./hsParser testLanguage.hs

3. Confirm the parser still emits output to `parsed.txt`.

4. Build the new typed pipeline:

   ghc --make AstTypes.hs TypeInfer.hs Desugar.hs SerializeAst.hs

5. Smoke-test the type inference pipeline in GHCi or a small script:

   Prelude> let expr = TBinary Add (TLiteral (TLInt 3)) (TLiteral (TLInt 4)) TInt32
   Prelude> inferExpr [] expr
   Right (TBinary Add (TLiteral (TLInt 3)) (TLiteral (TLInt 4)) TInt32)

6. Confirm a mismatch is rejected:

   Prelude> inferExpr [] (TBinary Add (TLiteral (TLInt 3)) (TLiteral (TLBool True)) TInt32)
   Left (TypeMismatch TInt32 TBool ...)

## Design notes

- The original parser remains usable without a large rewrite.
- The typed pipeline is intentionally additive and conservative.
- The canonical AST is structured to make the next compiler stages easier:
  - type validation
  - desugaring
  - lowering to LLVM IR
  - backend serialization

## Recommended next step

The next increment should be to connect this canonical type-checked AST to a concrete C++ AST reader and then generate LLVM IR with `llvm::IRBuilder`.

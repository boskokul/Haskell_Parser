module Desugar
  ( desugarExpr
  , desugarProgram
  ) where

import AstTypes

-- A conservative canonicalization pass that keeps semantics explicit for LLVM lowering.
desugarExpr :: TypedExpr -> TypedExpr
desugarExpr expr =
  case expr of
    TNeg inner typ -> TNeg (desugarExpr inner) typ
    TBinary op left right typ -> TBinary op (desugarExpr left) (desugarExpr right) typ
    TIf cond thenBranch elseBranch typ ->
      TIf (desugarExpr cond) (desugarExpr thenBranch) (desugarExpr elseBranch) typ
    TListLit elems typ -> TListLit (map desugarExpr elems) typ
    TLet name value body typ -> TLet name (desugarExpr value) (desugarExpr body) typ
    TCall name args typ -> TCall name (map desugarExpr args) typ
    _ -> expr

desugarProgram :: CanonicalProgram -> CanonicalProgram
desugarProgram (CanonicalProgram funcs binds) =
  CanonicalProgram
    { cpFunctions = map desugarFunction funcs
    , cpBindings = map desugarBinding binds
    }

desugarFunction :: CanonicalFunction -> CanonicalFunction
desugarFunction (CanonicalFunction name params ret body) =
  CanonicalFunction name params ret (desugarExpr body)

desugarBinding :: CanonicalBinding -> CanonicalBinding
desugarBinding (CanonicalBinding name ty value) =
  CanonicalBinding name ty (desugarExpr value)

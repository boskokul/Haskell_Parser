-- Desugar.hs
-- Stage 3: canonicalization pass.
-- This pass normalizes ancestors like conditionals and calls into a more backend-friendly form.
module Desugar
  ( desugarExpr
  , desugarProgram
  ) where

import AstTypes

-- Conservative desugaring pass that normalizes typed expressions into a backend-friendly form.
desugarExpr :: TypedExpr -> TypedExpr
desugarExpr expr =
  case expr of
    TUnary op inner ty -> TUnary op (desugarExpr inner) ty
    TBinary op left right ty -> TBinary op (desugarExpr left) (desugarExpr right) ty
    TCall name args ty -> TCall name (map desugarExpr args) ty
    TIf cond thenBranch elseBranch ty ->
      TIf (desugarExpr cond) (desugarExpr thenBranch) (desugarExpr elseBranch) ty
    TLet name value body ty -> TLet name (desugarExpr value) (desugarExpr body) ty
    TListLit elems ty -> TListLit (map desugarExpr elems) ty
    _ -> expr

desugarProgram :: TypedProgram -> TypedProgram
desugarProgram prog =
  TypedProgram
    { tpFunctions = map desugarFunction (tpFunctions prog)
    , tpBindings = map desugarBinding (tpBindings prog)
    }

desugarFunction :: TypedFunction -> TypedFunction
desugarFunction (TypedFunction sig body) =
  TypedFunction sig (desugarExpr body)

desugarBinding :: (String, PrimitiveType, TypedExpr) -> (String, PrimitiveType, TypedExpr)
desugarBinding (name, ty, value) = (name, ty, desugarExpr value)

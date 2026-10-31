module TypeInfer
  ( lowerProgram
  , lowerStmt
  , lowerExpr
  , inferProgram
  , inferExpr
  ) where

import AstTypes
import qualified HsParser
import Data.List (nub)

-- Stage 2: type inference and validation.
-- This pass walks the typed expression tree and validates type correctness before lowering.
type Scope = [(String, PrimitiveType)]

lowerProgram :: HsParser.Stmt -> Either TypeError TypedProgram
lowerProgram stmt = lowerStmt stmt []

lowerStmt :: HsParser.Stmt -> Scope -> Either TypeError TypedProgram
lowerStmt (HsParser.Sequence stmts) scope = do
  results <- mapM (\s -> lowerStmt s scope) stmts
  let funcs = concatMap tpFunctions results
  let binds = concatMap tpBindings results
  pure (TypedProgram funcs binds)

lowerStmt (HsParser.FunctionDeclaration name params body) scope = do
  let paramScope = map (\p -> (p, TUnknown)) params
  let extendedScope = paramScope ++ scope
  funcBody <- lowerStmtBody body extendedScope
  let retType = exprType funcBody
  let sig = FunctionSig { fsName = name, fsParams = paramScope, fsReturnType = retType }
  pure (TypedProgram [TypedFunction sig funcBody] [])

lowerStmt (HsParser.AssignNew (HsParser.VarLI name) expr) scope = do
  lowered <- lowerExpr expr scope
  let inferred = lowerTypeExpr scope lowered
  pure (TypedProgram [] [(name, exprType inferred, inferred)])

lowerStmt (HsParser.AssignRegular expr _) scope = do
  lowered <- lowerExpr expr scope
  pure (TypedProgram [] [])

lowerStmt (HsParser.TypeDeclaration _ _) _ = pure (TypedProgram [] [])

lowerStmt (HsParser.If cond thenStmt elseStmt) scope = do
  _ <- lowerLogicalExpr cond scope
  _ <- lowerStmt thenStmt scope
  _ <- lowerStmt elseStmt scope
  pure (TypedProgram [] [])

lowerStmt (HsParser.LetIn bind body) scope = do
  _ <- lowerStmt bind scope
  _ <- lowerStmt body scope
  pure (TypedProgram [] [])

lowerStmt (HsParser.CaseOfStm expr _) scope = do
  _ <- lowerExpr expr scope
  pure (TypedProgram [] [])

lowerStmt (HsParser.EmbeddedExpression expr) scope = do
  _ <- lowerExpr expr scope
  pure (TypedProgram [] [])

lowerStmt _ _ = pure (TypedProgram [] [])

lowerStmtBody :: HsParser.Stmt -> Scope -> Either TypeError TypedExpr
lowerStmtBody (HsParser.AssignRegular expr _) scope = lowerExpr expr scope
lowerStmtBody (HsParser.EmbeddedExpression expr) scope = lowerExpr expr scope
lowerStmtBody (HsParser.If cond thenStmt elseStmt) scope = do
  condExpr <- lowerLogicalExpr cond scope
  lhs <- lowerStmtBody thenStmt scope
  rhs <- lowerStmtBody elseStmt scope
  pure (TIf condExpr lhs rhs (if exprType lhs == exprType rhs then exprType lhs else TUnknown))
lowerStmtBody other scope = do
  _ <- lowerStmt other scope
  pure (TVariable "__unit__" TUnknown)

lowerExpr :: HsParser.Expression -> Scope -> Either TypeError TypedExpr
lowerExpr (HsParser.VarExpr (HsParser.VarLI name)) scope =
  case lookup name scope of
    Just ty -> Right (TVariable name ty)
    Nothing -> Right (TVariable name TUnknown)

lowerExpr (HsParser.LiteralExpr lit) _ = Right (lowerLiteralValue lit)

lowerExpr (HsParser.Neg expr) scope = do
  inner <- lowerExpr expr scope
  pure (TUnary Neg inner (exprType inner))

lowerExpr (HsParser.ArithmeticBinaryExpr op left right) scope = do
  lhs <- lowerExpr left scope
  rhs <- lowerExpr right scope
  let resultType = combineNumericType (exprType lhs) (exprType rhs)
  pure (TBinary (mapArithmeticOp op) lhs rhs resultType)

lowerExpr (HsParser.List items) scope = do
  values <- mapM (\item -> lowerLiteralIdentifier item scope) items
  let itemTypes = map exprType values
  case nub itemTypes of
    [t] -> pure (TListLit values (TList t))
    [] -> pure (TListLit [] (TList TUnknown))
    _ -> Left (InvalidOperation "list elements must share a single type")

lowerExpr (HsParser.FunctionCallNew name args) scope = do
  vals <- mapM (\arg -> lowerLiteralIdentifier arg scope) args
  pure (TCall name vals TUnknown)

lowerExpr (HsParser.EExpr expr) scope = lowerExpr expr scope

lowerExpr (HsParser.CaseOf expr _) _ = do
  lowerExpr expr []

lowerLiteralIdentifier :: HsParser.LiteralIdentifier -> Scope -> Either TypeError TypedExpr
lowerLiteralIdentifier (HsParser.VarLI name) scope =
  case lookup name scope of
    Just ty -> Right (TVariable name ty)
    Nothing -> Right (TVariable name TUnknown)
lowerLiteralIdentifier (HsParser.IntConstLI n) _ = Right (TLiteral (TLInt n))
lowerLiteralIdentifier (HsParser.FloatConstLI f) _ = Right (TLiteral (TLFloat f))
lowerLiteralIdentifier (HsParser.BoolConstLI b) _ = Right (TLiteral (TLBool b))
lowerLiteralIdentifier (HsParser.String s) _ = Right (TLiteral (TLString s))

lowerLiteralValue :: HsParser.LiteralIdentifier -> TypedExpr
lowerLiteralValue (HsParser.IntConstLI n) = TLiteral (TLInt n)
lowerLiteralValue (HsParser.FloatConstLI f) = TLiteral (TLFloat f)
lowerLiteralValue (HsParser.BoolConstLI b) = TLiteral (TLBool b)
lowerLiteralValue (HsParser.String s) = TLiteral (TLString s)
lowerLiteralValue (HsParser.VarLI name) = TVariable name TUnknown

lowerLogicalExpr :: HsParser.LogicalExpr -> Scope -> Either TypeError TypedExpr
lowerLogicalExpr (HsParser.LogicalVar lit) scope =
  case lit of
    HsParser.BoolConstLI b -> Right (TLiteral (TLBool b))
    HsParser.VarLI name ->
      case lookup name scope of
        Just ty -> Right (TVariable name ty)
        Nothing -> Right (TVariable name TUnknown)
    _ -> Left (InvalidOperation "logical values must be booleans")

lowerLogicalExpr (HsParser.Not expr) scope = do
  inner <- lowerLogicalExpr expr scope
  pure (TUnary Not inner TBool)

lowerLogicalExpr (HsParser.LogicalBinary op left right) scope = do
  lhs <- lowerLogicalExpr left scope
  rhs <- lowerLogicalExpr right scope
  pure (TBinary (mapLogicalOp op) lhs rhs TBool)

lowerLogicalExpr (HsParser.RelationalBinary op left right) scope = do
  lhs <- lowerExpr left scope
  rhs <- lowerExpr right scope
  pure (TBinary (mapRelationalOp op) lhs rhs TBool)

mapArithmeticOp :: HsParser.ArithmBinOpNEW -> BinaryOp
mapArithmeticOp HsParser.Plus = Add
mapArithmeticOp HsParser.Minus = Sub
mapArithmeticOp HsParser.Times = Mul
mapArithmeticOp HsParser.Divided = Div

mapLogicalOp :: HsParser.LogicalBinOp -> BinaryOp
mapLogicalOp HsParser.And = And
mapLogicalOp HsParser.Or = Or

mapRelationalOp :: HsParser.RelationalBinOp -> BinaryOp
mapRelationalOp HsParser.Greater = Gt
mapRelationalOp HsParser.Less = Lt
mapRelationalOp HsParser.GreaterEqual = Ge
mapRelationalOp HsParser.LessEqual = Le
mapRelationalOp HsParser.Equal = Eq
mapRelationalOp HsParser.NotEqual = Neq

combineNumericType :: PrimitiveType -> PrimitiveType -> PrimitiveType
combineNumericType TInt32 TInt32 = TInt32
combineNumericType TFloat TFloat = TFloat
combineNumericType TInt32 TFloat = TFloat
combineNumericType TFloat TInt32 = TFloat
combineNumericType TUnknown _ = TUnknown
combineNumericType _ TUnknown = TUnknown
combineNumericType _ _ = TUnknown

exprType :: TypedExpr -> PrimitiveType
exprType (TLiteral lit) =
  case lit of
    TLInt _ -> TInt32
    TLFloat _ -> TFloat
    TLBool _ -> TBool
    TLString _ -> TString
exprType (TVariable _ ty) = ty
exprType (TUnary _ _ ty) = ty
exprType (TBinary _ _ _ ty) = ty
exprType (TCall _ _ ty) = ty
exprType (TIf _ _ _ ty) = ty
exprType (TLet _ _ _ ty) = ty
exprType (TListLit _ ty) = ty

lowerTypeExpr :: Scope -> TypedExpr -> TypedExpr
lowerTypeExpr _ expr = expr

inferExpr :: Scope -> TypedExpr -> Either TypeError TypedExpr
inferExpr scope expr =
  case expr of
    TLiteral lit -> Right (TLiteral lit)
    TVariable name ty ->
      case lookup name scope of
        Just actualType ->
          if actualType == ty || ty == TUnknown
            then Right (TVariable name actualType)
            else Left (TypeMismatch actualType ty ("variable " ++ name))
        Nothing -> Right (TVariable name ty)
    TUnary Neg inner ty -> do
      inferred <- inferExpr scope inner
      let it = exprType inferred
      if isNumeric it
        then Right (TUnary Neg inferred ty)
        else Left (InvalidOperation "negation requires a numeric operand")
    TUnary Not inner ty -> do
      inferred <- inferExpr scope inner
      let it = exprType inferred
      if it == TBool
        then Right (TUnary Not inferred ty)
        else Left (InvalidOperation "logical not requires bool operand")
    TBinary op left right ty -> do
      lhs <- inferExpr scope left
      rhs <- inferExpr scope right
      let l = exprType lhs
      let r = exprType rhs
      case op of
        Add -> ensureNumeric l r "addition"
        Sub -> ensureNumeric l r "subtraction"
        Mul -> ensureNumeric l r "multiplication"
        Div -> ensureNumeric l r "division"
        Eq -> ensureComparable l r "equality"
        Neq -> ensureComparable l r "inequality"
        Lt -> ensureComparable l r "less-than"
        Gt -> ensureComparable l r "greater-than"
        Le -> ensureComparable l r "less-or-equal"
        Ge -> ensureComparable l r "greater-or-equal"
        And -> ensureBool l r "logical and"
        Or -> ensureBool l r "logical or"
      Right (TBinary op lhs rhs ty)
    TCall name args ty -> do
      inferredArgs <- mapM (inferExpr scope) args
      Right (TCall name inferredArgs ty)
    TIf cond thenBranch elseBranch ty -> do
      condInferred <- inferExpr scope cond
      if exprType condInferred /= TBool
        then Left (TypeMismatch TBool (exprType condInferred) "if condition")
        else do
          th <- inferExpr scope thenBranch
          el <- inferExpr scope elseBranch
          if exprType th == exprType el
            then Right (TIf condInferred th el ty)
            else Left (TypeMismatch (exprType th) (exprType el) "if branches")
    TLet name value body ty -> do
      v <- inferExpr scope value
      let newScope = (name, exprType v) : scope
      b <- inferExpr newScope body
      Right (TLet name v b ty)
    TListLit items ty -> do
      inferred <- mapM (inferExpr scope) items
      let itemTypes = map exprType inferred
      case nub itemTypes of
        [t] -> Right (TListLit inferred (TList t))
        [] -> Right (TListLit inferred (TList TUnknown))
        _ -> Left (InvalidOperation "list elements must share a single type")

ensureNumeric :: PrimitiveType -> PrimitiveType -> String -> Either TypeError ()
ensureNumeric left right context =
  if isNumeric left && isNumeric right then Right () else Left (InvalidOperation ("numeric operation required for " ++ context))

ensureComparable :: PrimitiveType -> PrimitiveType -> String -> Either TypeError ()
ensureComparable left right context =
  if left == right then Right () else Left (TypeMismatch left right ("comparison required for " ++ context))

ensureBool :: PrimitiveType -> PrimitiveType -> String -> Either TypeError ()
ensureBool left right context =
  if left == TBool && right == TBool then Right () else Left (TypeMismatch left right ("boolean operation required for " ++ context))

isNumeric :: PrimitiveType -> Bool
isNumeric TInt32 = True
isNumeric TFloat = True
isNumeric _ = False

inferProgram :: TypedProgram -> Either TypeError TypedProgram
inferProgram prog = do
  bindings <- mapM inferBinding (tpBindings prog)
  functions <- mapM inferFunction (tpFunctions prog)
  pure (TypedProgram functions bindings)

inferBinding :: (String, PrimitiveType, TypedExpr) -> Either TypeError (String, PrimitiveType, TypedExpr)
inferBinding (name, ty, expr) = do
  inferred <- inferExpr [] expr
  let actual = exprType inferred
  if actual == ty || ty == TUnknown
    then pure (name, actual, inferred)
    else Left (TypeMismatch ty actual ("binding " ++ name))

inferFunction :: TypedFunction -> Either TypeError TypedFunction
inferFunction (TypedFunction sig body) = do
  let env = fsParams sig
  inferredBody <- inferExpr env body
  let actual = exprType inferredBody
  if actual == fsReturnType sig || fsReturnType sig == TUnknown
    then pure (TypedFunction sig { fsReturnType = actual } inferredBody)
    else Left (TypeMismatch (fsReturnType sig) actual ("function " ++ fsName sig))

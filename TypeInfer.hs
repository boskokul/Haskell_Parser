module TypeInfer
  ( lowerProgram
  , inferProgram
  , inferExpr
  , lowerExpr
  ) where

import AstTypes
import qualified HsParser
import Data.List (nub)

type Scope = [(String, PrimitiveType)]

-- Lower a legacy parsed statement tree into a typed program.
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
  loweredBody <- lowerExpr body extendedScope
  inferredBody <- inferExpr extendedScope loweredBody
  let retType = exprType inferredBody
  let sig = FunctionSig { fsName = name, fsParams = paramScope, fsReturnType = retType }
  pure (TypedProgram [TypedFunction sig inferredBody] [])

lowerStmt (HsParser.AssignNew (HsParser.VarLI name) expr) scope = do
  loweredExpr <- lowerExpr expr scope
  inferred <- inferExpr scope loweredExpr
  pure (TypedProgram [] [(name, exprType inferred, inferred)])

lowerStmt (HsParser.AssignRegular expr _) scope = do
  _ <- lowerExpr expr scope
  pure (TypedProgram [] [])

lowerStmt (HsParser.TypeDeclaration _ _) _ = pure (TypedProgram [] [])
lowerStmt (HsParser.If cond stmt1 stmt2) scope = do
  _ <- lowerLogicalExpr cond scope
  _ <- lowerStmt stmt1 scope
  _ <- lowerStmt stmt2 scope
  pure (TypedProgram [] [])
lowerStmt (HsParser.LetIn stmt1 stmt2) scope = do
  _ <- lowerStmt stmt1 scope
  _ <- lowerStmt stmt2 scope
  pure (TypedProgram [] [])
lowerStmt (HsParser.CaseOfStm expr _) scope = do
  _ <- lowerExpr expr scope
  pure (TypedProgram [] [])
lowerStmt (HsParser.EmbeddedExpression expr) scope = do
  _ <- lowerExpr expr scope
  pure (TypedProgram [] [])
lowerStmt _ _ = pure (TypedProgram [] [])

lowerExpr :: HsParser.Expression -> Scope -> Either TypeError TypedExpr
lowerExpr (HsParser.Neg expr) scope = do
  inner <- lowerExpr expr scope
  inferred <- inferExpr scope inner
  pure (TUnary Neg inferred (exprType inferred))

lowerExpr (HsParser.VarExpr (HsParser.VarLI name)) scope =
  case lookup name scope of
    Just ty -> Right (TVariable name ty)
    Nothing -> Right (TVariable name TUnknown)

lowerExpr (HsParser.LiteralExpr lit) scope =
  pure (lowerLiteralValue lit)

lowerExpr (HsParser.ArithmeticBinaryExpr op left right) scope = do
  lhs <- lowerExpr left scope
  rhs <- lowerExpr right scope
  let lhsType = exprType lhs
  let rhsType = exprType rhs
  let resultType = combineNumericType lhsType rhsType
  let mapped = mapArithmeticOp op
  pure (TBinary mapped lhs rhs resultType)

lowerExpr (HsParser.List items) scope = do
  exprs <- mapM (\item -> lowerLiteralIdentifier item scope) items
  let types = map exprType exprs
  case nub types of
    [t] -> pure (TListLit exprs (TList t))
    [] -> pure (TListLit [] (TList TUnknown))
    _ -> Left (InvalidOperation "list elements must share a single type")

lowerExpr (HsParser.FunctionCallNew name args) scope = do
  argExprs <- mapM (\arg -> lowerLiteralIdentifier arg scope) args
  let retType = lookupFunctionType name scope
  pure (TCall name argExprs retType)

lowerExpr (HsParser.EExpr expr) scope = lowerExpr expr scope
lowerExpr (HsParser.CaseOf _ _) _ = Left (InvalidOperation "case expressions are lowered via a desugaring step")

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
  lowered <- lowerLogicalExpr expr scope
  pure (TUnary Not lowered TBool)

lowerLogicalExpr (HsParser.LogicalBinary op left right) scope = do
  lhs <- lowerLogicalExpr left scope
  rhs <- lowerLogicalExpr right scope
  let mapped = mapLogicalOp op
  pure (TBinary mapped lhs rhs TBool)

lowerLogicalExpr (HsParser.RelationalBinary op left right) scope = do
  lhs <- lowerExpr left scope
  rhs <- lowerExpr right scope
  let mapped = mapRelationalOp op
  pure (TBinary mapped lhs rhs TBool)

lowerType :: HsParser.Type -> PrimitiveType
lowerType (HsParser.RegularType "Integer") = TInt32
lowerType (HsParser.RegularType "Int32") = TInt32
lowerType (HsParser.RegularType "Float") = TFloat
lowerType (HsParser.RegularType "Bool") = TBool
lowerType (HsParser.RegularType "String") = TString
lowerType (HsParser.ListType inner) = TList (lowerType (HsParser.RegularType inner))
lowerType (HsParser.FunctionType []) = TUnknown
lowerType (HsParser.FunctionType xs) =
  let tys = map lowerType xs
  in case reverse tys of
       [] -> TUnknown
       ret : _ -> TFunction (init tys) ret

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

lookupFunctionType :: String -> Scope -> PrimitiveType
lookupFunctionType name scope =
  case lookup name scope of
    Just ty -> ty
    Nothing -> TUnknown

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

inferExpr :: Scope -> TypedExpr -> Either TypeError TypedExpr
inferExpr scope expr =
  case expr of
    TLiteral lit -> Right (TLiteral lit)

    TVariable name expectedType ->
      case lookup name scope of
        Just actualType ->
          if actualType == expectedType || expectedType == TUnknown
            then Right (TVariable name actualType)
            else Left (TypeMismatch actualType expectedType ("variable " ++ name))
        Nothing -> Left (UnknownVariable name)

    TUnary Neg inner _ -> do
      inferred <- inferExpr scope inner
      let innerType = exprType inferred
      if innerType == TInt32 || innerType == TFloat
        then Right (TUnary Neg inferred innerType)
        else Left (InvalidOperation "negation requires a numeric operand")

    TUnary Not inner _ -> do
      inferred <- inferExpr scope inner
      let innerType = exprType inferred
      if innerType == TBool
        then Right (TUnary Not inferred TBool)
        else Left (InvalidOperation "logical not requires bool operand")

    TBinary op left right expectedType -> do
      lhs <- inferExpr scope left
      rhs <- inferExpr scope right
      let leftType = exprType lhs
      let rightType = exprType rhs
      case op of
        Add -> ensureNumeric leftType rightType "addition"
        Sub -> ensureNumeric leftType rightType "subtraction"
        Mul -> ensureNumeric leftType rightType "multiplication"
        Div -> ensureNumeric leftType rightType "division"
        Eq -> ensureComparable leftType rightType "equality"
        Neq -> ensureComparable leftType rightType "inequality"
        Lt -> ensureComparable leftType rightType "less-than"
        Gt -> ensureComparable leftType rightType "greater-than"
        Le -> ensureComparable leftType rightType "less-or-equal"
        Ge -> ensureComparable leftType rightType "greater-or-equal"
        And -> ensureBool leftType rightType "logical and"
        Or -> ensureBool leftType rightType "logical or"
      let resolvedType = if expectedType == TUnknown then combineNumericType leftType rightType else expectedType
      Right (TBinary op lhs rhs resolvedType)

    TCall name args _ -> do
      args' <- mapM (inferExpr scope) args
      pure (TCall name args' (inferCallResultType name args'))

    TIf cond thenBranch elseBranch expectedType -> do
      condInferred <- inferExpr scope cond
      if exprType condInferred /= TBool
        then Left (TypeMismatch TBool (exprType condInferred) "if condition")
        else do
          th <- inferExpr scope thenBranch
          el <- inferExpr scope elseBranch
          let thType = exprType th
          let elType = exprType el
          if thType == elType
            then Right (TIf condInferred th el (if expectedType == TUnknown then thType else expectedType))
            else Left (TypeMismatch thType elType "if branches")

    TLet name value body expectedType -> do
      v <- inferExpr scope value
      let vType = exprType v
      let newScope = (name, vType) : scope
      b <- inferExpr newScope body
      Right (TLet name v b (if expectedType == TUnknown then exprType b else expectedType))

    TListLit items expectedType -> do
      inferred <- mapM (inferExpr scope) items
      let types = map exprType inferred
      case nub types of
        [t] -> Right (TListLit inferred (if expectedType == TUnknown then TList t else expectedType))
        [] -> Right (TListLit inferred (if expectedType == TUnknown then TList TUnknown else expectedType))
        _ -> Left (InvalidOperation "all elements in a list must have the same type")

ensureNumeric :: PrimitiveType -> PrimitiveType -> String -> Either TypeError ()
ensureNumeric left right context =
  if isNumeric left && isNumeric right then Right () else Left (InvalidOperation ("numeric arithmetic required for " ++ context))

ensureComparable :: PrimitiveType -> PrimitiveType -> String -> Either TypeError ()
ensureComparable left right context =
  if left == right then Right () else Left (TypeMismatch left right ("comparison in " ++ context))

ensureBool :: PrimitiveType -> PrimitiveType -> String -> Either TypeError ()
ensureBool left right context =
  if left == TBool && right == TBool then Right () else Left (TypeMismatch left right ("boolean operation in " ++ context))

isNumeric :: PrimitiveType -> Bool
isNumeric TInt32 = True
isNumeric TFloat = True
isNumeric _ = False

inferCallResultType :: String -> [TypedExpr] -> PrimitiveType
inferCallResultType _ _ = TUnknown

inferProgram :: TypedProgram -> Either TypeError TypedProgram
inferProgram prog = do
  bindings <- mapM inferBinding (tpBindings prog)
  functions <- mapM inferFunction (tpFunctions prog)
  pure (TypedProgram functions bindings)

inferBinding :: (String, PrimitiveType, TypedExpr) -> Either TypeError (String, PrimitiveType, TypedExpr)
inferBinding (name, ty, value) = do
  inferred <- inferExpr [] value
  let actual = exprType inferred
  if actual == ty || ty == TUnknown
    then pure (name, actual, inferred)
    else Left (TypeMismatch ty actual ("binding " ++ name))

inferFunction :: TypedFunction -> Either TypeError TypedFunction
inferFunction (TypedFunction sig body) = do
  let paramScope = fsParams sig
  inferredBody <- inferExpr paramScope body
  let retType = exprType inferredBody
  if retType == fsReturnType sig || fsReturnType sig == TUnknown
    then pure (TypedFunction (sig { fsReturnType = retType }) inferredBody)
    else Left (TypeMismatch (fsReturnType sig) retType ("function " ++ fsName sig))

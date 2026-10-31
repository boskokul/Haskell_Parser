module TypeInfer
  ( inferLiteral
  , inferExpr
  , inferProgram
  , primitiveFromString
  , exprType
  ) where

import AstTypes
import Data.List (nub)

inferLiteral :: TypedLiteral -> PrimitiveType
inferLiteral (TLInt _) = TInt32
inferLiteral (TLFloat _) = TFloat
inferLiteral (TLBool _) = TBool
inferLiteral (TLString _) = TString

primitiveFromString :: String -> Maybe PrimitiveType
primitiveFromString "Integer" = Just TInt32
primitiveFromString "Int32" = Just TInt32
primitiveFromString "Float" = Just TFloat
primitiveFromString "Bool" = Just TBool
primitiveFromString "String" = Just TString
primitiveFromString _ = Nothing

exprType :: TypedExpr -> PrimitiveType
exprType (TLiteral lit) = inferLiteral lit
exprType (TVariable _ typ) = typ
exprType (TNeg _ typ) = typ
exprType (TBinary _ _ _ typ) = typ
exprType (TCall _ _ typ) = typ
exprType (TIf _ _ _ typ) = typ
exprType (TListLit _ typ) = typ
exprType (TLet _ _ _ typ) = typ

checkArithmetic :: PrimitiveType -> PrimitiveType -> String -> Either TypeError ()
checkArithmetic left right context =
  if isNumeric left && isNumeric right
    then Right ()
    else Left (InvalidOperation ("numeric arithmetic required for " ++ context))

checkComparable :: PrimitiveType -> PrimitiveType -> String -> Either TypeError ()
checkComparable left right context =
  if left == right
    then Right ()
    else Left (TypeMismatch left right ("comparison in " ++ context))

checkBool :: PrimitiveType -> PrimitiveType -> String -> Either TypeError ()
checkBool left right context =
  if left == TBool && right == TBool
    then Right ()
    else Left (TypeMismatch left right ("boolean operation in " ++ context))

isNumeric :: PrimitiveType -> Bool
isNumeric TInt32 = True
isNumeric TFloat = True
isNumeric _ = False

inferExpr :: [(String, PrimitiveType)] -> TypedExpr -> Either TypeError TypedExpr
inferExpr env expr =
  case expr of
    TLiteral lit -> Right (TLiteral lit)

    TVariable name expectedType ->
      case lookup name env of
        Just actualType ->
          if actualType == expectedType
            then Right (TVariable name expectedType)
            else Left (TypeMismatch actualType expectedType ("variable " ++ name))
        Nothing -> Left (UnknownVariable name)

    TNeg inner typ ->
      do
        inferred <- inferExpr env inner
        let innerType = exprType inferred
        if isNumeric innerType
          then Right (TNeg inferred typ)
          else Left (InvalidOperation "negation requires a numeric operand")

    TBinary op left right typ ->
      do
        leftInferred <- inferExpr env left
        rightInferred <- inferExpr env right
        let leftType = exprType leftInferred
        let rightType = exprType rightInferred

        case op of
          Add -> checkArithmetic leftType rightType "addition"
          Sub -> checkArithmetic leftType rightType "subtraction"
          Mul -> checkArithmetic leftType rightType "multiplication"
          Div -> checkArithmetic leftType rightType "division"
          Eq -> checkComparable leftType rightType "equality"
          Neq -> checkComparable leftType rightType "inequality"
          Lt -> checkComparable leftType rightType "less-than"
          Gt -> checkComparable leftType rightType "greater-than"
          Le -> checkComparable leftType rightType "less-or-equal"
          Ge -> checkComparable leftType rightType "greater-or-equal"
          And -> checkBool leftType rightType "logical and"
          Or -> checkBool leftType rightType "logical or"

        case typ of
          TInt32 -> Right (TBinary op leftInferred rightInferred TInt32)
          TFloat -> Right (TBinary op leftInferred rightInferred TFloat)
          TBool -> Right (TBinary op leftInferred rightInferred TBool)
          _ -> Left (InvalidOperation ("unsupported result type " ++ show typ))

    TCall name args typ ->
      do
        inferredArgs <- mapM (inferExpr env) args
        Right (TCall name inferredArgs typ)

    TIf cond thenBranch elseBranch ty ->
      do
        condInferred <- inferExpr env cond
        case exprType condInferred of
          TBool -> do
            lhs <- inferExpr env thenBranch
            rhs <- inferExpr env elseBranch
            let lhsType = exprType lhs
            let rhsType = exprType rhs
            if lhsType == rhsType
              then Right (TIf condInferred lhs rhs ty)
              else Left (TypeMismatch lhsType rhsType "if branches")
          condType -> Left (TypeMismatch TBool condType "condition")

    TListLit elems typ ->
      case elems of
        [] -> Right (TListLit [] (TList typ))
        _ -> do
          inferred <- mapM (inferExpr env) elems
          let elementTypes = map exprType inferred
          case nub elementTypes of
            [singleType] -> Right (TListLit inferred (TList singleType))
            _ -> Left (InvalidOperation "list elements must all share the same type")

    TLet name value body typ ->
      do
        inferredValue <- inferExpr env value
        let valueType = exprType inferredValue
        let newEnv = (name, valueType) : env
        inferredBody <- inferExpr newEnv body
        Right (TLet name inferredValue inferredBody typ)

inferProgram :: CanonicalProgram -> Either TypeError CanonicalProgram
inferProgram prog =
  do
    bindings <- mapM inferBinding (cpBindings prog)
    functions <- mapM inferFunction (cpFunctions prog)
    Right (CanonicalProgram functions bindings)

inferBinding :: CanonicalBinding -> Either TypeError CanonicalBinding
inferBinding (CanonicalBinding name ty value) =
  do
    inferred <- inferExpr [] value
    let actualType = exprType inferred
    if actualType == ty
      then Right (CanonicalBinding name ty inferred)
      else Left (TypeMismatch ty actualType ("binding " ++ name))

inferFunction :: CanonicalFunction -> Either TypeError CanonicalFunction
inferFunction (CanonicalFunction name params ret body) =
  do
    inferred <- inferExpr params body
    let actualType = exprType inferred
    if actualType == ret
      then Right (CanonicalFunction name params ret inferred)
      else Left (TypeMismatch ret actualType ("function " ++ name))

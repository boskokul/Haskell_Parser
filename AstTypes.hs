{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module AstTypes
  ( PrimitiveType(..)
  , TypedLiteral(..)
  , UnaryOp(..)
  , BinaryOp(..)
  , TypedExpr(..)
  , FunctionSig(..)
  , TypedFunction(..)
  , TypedProgram(..)
  , TypeError(..)
  , encodeProgram
  , writeProgramJson
  ) where

import GHC.Generics
import Data.List (intercalate)

-- Primitive types used by the typed frontend and LLVM backend.
data PrimitiveType
  = TInt32
  | TFloat
  | TBool
  | TString
  | TList PrimitiveType
  | TFunction [PrimitiveType] PrimitiveType
  | TUnknown
  deriving (Eq, Show, Generic)

-- Concrete literal values emitted by the parser and then typed.
data TypedLiteral
  = TLInt Integer
  | TLFloat Double
  | TLBool Bool
  | TLString String
  deriving (Eq, Show, Generic)

data UnaryOp = Neg | Not deriving (Eq, Show, Generic)

data BinaryOp
  = Add
  | Sub
  | Mul
  | Div
  | Eq
  | Neq
  | Lt
  | Gt
  | Le
  | Ge
  | And
  | Or
  deriving (Eq, Show, Generic)

-- Typed IR expressions: every node carries a result type.
data TypedExpr
  = TLiteral TypedLiteral
  | TVariable String PrimitiveType
  | TUnary UnaryOp TypedExpr PrimitiveType
  | TBinary BinaryOp TypedExpr TypedExpr PrimitiveType
  | TCall String [TypedExpr] PrimitiveType
  | TIf TypedExpr TypedExpr TypedExpr PrimitiveType
  | TLet String TypedExpr TypedExpr PrimitiveType
  | TListLit [TypedExpr] PrimitiveType
  deriving (Eq, Show, Generic)

data TypeError
  = UnknownVariable String
  | TypeMismatch PrimitiveType PrimitiveType String
  | InvalidOperation String
  deriving (Eq, Show, Generic)

-- Function signatures and typed functions.
data FunctionSig = FunctionSig
  { fsName :: String
  , fsParams :: [(String, PrimitiveType)]
  , fsReturnType :: PrimitiveType
  } deriving (Eq, Show, Generic)

data TypedFunction = TypedFunction
  { tfSig :: FunctionSig
  , tfBody :: TypedExpr
  } deriving (Eq, Show, Generic)

-- Top-level typed program used by the downstream C++ LLVM backend.
data TypedProgram = TypedProgram
  { tpFunctions :: [TypedFunction]
  , tpBindings :: [(String, PrimitiveType, TypedExpr)]
  } deriving (Eq, Show, Generic)

-- Minimal JSON encoding for the typed AST without external package dependencies.
jsonString :: String -> String
jsonString s = '"' : concatMap escape s ++ "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape c = [c]

encodePrimitiveType :: PrimitiveType -> String
encodePrimitiveType TInt32 = "\"Int32\""
encodePrimitiveType TFloat = "\"Float\""
encodePrimitiveType TBool = "\"Bool\""
encodePrimitiveType TString = "\"String\""
encodePrimitiveType (TList t) = "{\"kind\":\"List\",\"elementType\":" ++ encodePrimitiveType t ++ "}"
encodePrimitiveType (TFunction args ret) =
  "{\"kind\":\"Function\",\"args\":[" ++ intercalate "," (map encodePrimitiveType args) ++ "],\"return\":" ++ encodePrimitiveType ret ++ "}"
encodePrimitiveType TUnknown = "\"Unknown\""

encodeLiteral :: TypedLiteral -> String
encodeLiteral (TLInt n) = "{\"kind\":\"Int\",\"value\":" ++ show n ++ "}"
encodeLiteral (TLFloat f) = "{\"kind\":\"Float\",\"value\":" ++ show f ++ "}"
encodeLiteral (TLBool b) = "{\"kind\":\"Bool\",\"value\":" ++ if b then "true" else "false" ++ "}"
encodeLiteral (TLString s) = "{\"kind\":\"String\",\"value\":" ++ jsonString s ++ "}"

encodeTypedExpr :: TypedExpr -> String
encodeTypedExpr expr =
  case expr of
    TLiteral lit -> "{\"kind\":\"Literal\",\"value\":" ++ encodeLiteral lit ++ "}"
    TVariable name ty -> "{\"kind\":\"Variable\",\"name\":" ++ jsonString name ++ ",\"type\":" ++ encodePrimitiveType ty ++ "}"
    TUnary op inner ty ->
      "{\"kind\":\"Unary\",\"op\":" ++ jsonString (show op) ++ ",\"value\":" ++ encodeTypedExpr inner ++ ",\"type\":" ++ encodePrimitiveType ty ++ "}"
    TBinary op left right ty ->
      "{\"kind\":\"Binary\",\"op\":" ++ jsonString (show op) ++ ",\"left\":" ++ encodeTypedExpr left ++ ",\"right\":" ++ encodeTypedExpr right ++ ",\"type\":" ++ encodePrimitiveType ty ++ "}"
    TCall name args ty ->
      "{\"kind\":\"Call\",\"name\":" ++ jsonString name ++ ",\"args\":[" ++ intercalate "," (map encodeTypedExpr args) ++ "],\"type\":" ++ encodePrimitiveType ty ++ "}"
    TIf cond thenBranch elseBranch ty ->
      "{\"kind\":\"If\",\"cond\":" ++ encodeTypedExpr cond ++ ",\"then\":" ++ encodeTypedExpr thenBranch ++ ",\"else\":" ++ encodeTypedExpr elseBranch ++ ",\"type\":" ++ encodePrimitiveType ty ++ "}"
    TLet name value body ty ->
      "{\"kind\":\"Let\",\"name\":" ++ jsonString name ++ ",\"value\":" ++ encodeTypedExpr value ++ ",\"body\":" ++ encodeTypedExpr body ++ ",\"type\":" ++ encodePrimitiveType ty ++ "}"
    TListLit elems ty ->
      "{\"kind\":\"List\",\"items\":[" ++ intercalate "," (map encodeTypedExpr elems) ++ "],\"type\":" ++ encodePrimitiveType ty ++ "}"

encodeFunctionSig :: FunctionSig -> String
encodeFunctionSig sig =
  "{\"name\":" ++ jsonString (fsName sig) ++
  ",\"params\":[" ++ intercalate "," (map encodeParam (fsParams sig)) ++ "]\n,\"returnType\":" ++ encodePrimitiveType (fsReturnType sig) ++ "}"
  where
    encodeParam (name, ty) = "[" ++ jsonString name ++ "," ++ encodePrimitiveType ty ++ "]"

encodeTypedFunction :: TypedFunction -> String
encodeTypedFunction fn =
  "{\"tfSig\":" ++ encodeFunctionSig (tfSig fn) ++ ",\"tfBody\":" ++ encodeTypedExpr (tfBody fn) ++ "}"

encodeBinding :: (String, PrimitiveType, TypedExpr) -> String
encodeBinding (name, ty, expr) =
  "[" ++ jsonString name ++ "," ++ encodePrimitiveType ty ++ "," ++ encodeTypedExpr expr ++ "]"

encodeProgram :: TypedProgram -> String
encodeProgram prog =
  "{\n  \"tpFunctions\": [" ++ intercalate ",\n    " (map encodeTypedFunction (tpFunctions prog)) ++ "],\n  \"tpBindings\": [" ++ intercalate ",\n    " (map encodeBinding (tpBindings prog)) ++ "]\n}"

writeProgramJson :: FilePath -> TypedProgram -> IO ()
writeProgramJson path prog = writeFile path (encodeProgram prog)

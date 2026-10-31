module AstTypes
  ( PrimitiveType(..)
  , TypedLiteral(..)
  , TypedBinaryOp(..)
  , TypedExpr(..)
  , TypeError(..)
  , CanonicalFunction(..)
  , CanonicalBinding(..)
  , CanonicalProgram(..)
  ) where

data PrimitiveType
  = TInt32
  | TFloat
  | TBool
  | TString
  | TList PrimitiveType
  | TUnknown
  deriving (Eq, Show)

data TypedLiteral
  = TLInt Integer
  | TLFloat Double
  | TLBool Bool
  | TLString String
  deriving (Eq, Show)

data TypedBinaryOp
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
  deriving (Eq, Show)

data TypedExpr
  = TLiteral TypedLiteral
  | TVariable String PrimitiveType
  | TNeg TypedExpr PrimitiveType
  | TBinary TypedBinaryOp TypedExpr TypedExpr PrimitiveType
  | TCall String [TypedExpr] PrimitiveType
  | TIf TypedExpr TypedExpr TypedExpr PrimitiveType
  | TListLit [TypedExpr] PrimitiveType
  | TLet String TypedExpr TypedExpr PrimitiveType
  deriving (Eq, Show)

data TypeError
  = UnknownVariable String
  | TypeMismatch PrimitiveType PrimitiveType String
  | InvalidOperation String
  deriving (Eq, Show)

data CanonicalFunction = CanonicalFunction
  { cfName :: String
  , cfParams :: [(String, PrimitiveType)]
  , cfReturnType :: PrimitiveType
  , cfBody :: TypedExpr
  } deriving (Eq, Show)

data CanonicalBinding = CanonicalBinding
  { cbName :: String
  , cbType :: PrimitiveType
  , cbValue :: TypedExpr
  } deriving (Eq, Show)

data CanonicalProgram = CanonicalProgram
  { cpFunctions :: [CanonicalFunction]
  , cpBindings :: [CanonicalBinding]
  } deriving (Eq, Show)

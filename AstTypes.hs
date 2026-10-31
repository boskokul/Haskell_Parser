{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- AstTypes.hs
-- This module defines the canonical typed AST used by the compiler frontend.
-- It is the contract between the Haskell frontend and the downstream C++ LLVM backend.
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
import Data.Aeson
import qualified Data.ByteString.Lazy as BL

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

instance ToJSON PrimitiveType
instance FromJSON PrimitiveType

-- Concrete literal values emitted by the parser and then typed.
data TypedLiteral
  = TLInt Integer
  | TLFloat Double
  | TLBool Bool
  | TLString String
  deriving (Eq, Show, Generic)

instance ToJSON TypedLiteral
instance FromJSON TypedLiteral

data UnaryOp = Neg | Not deriving (Eq, Show, Generic)
instance ToJSON UnaryOp
instance FromJSON UnaryOp

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

instance ToJSON BinaryOp
instance FromJSON BinaryOp

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

instance ToJSON TypedExpr
instance FromJSON TypedExpr

data TypeError
  = UnknownVariable String
  | TypeMismatch PrimitiveType PrimitiveType String
  | InvalidOperation String
  deriving (Eq, Show, Generic)

instance ToJSON TypeError
instance FromJSON TypeError

-- Function signatures and typed functions.
data FunctionSig = FunctionSig
  { fsName :: String
  , fsParams :: [(String, PrimitiveType)]
  , fsReturnType :: PrimitiveType
  } deriving (Eq, Show, Generic)

instance ToJSON FunctionSig
instance FromJSON FunctionSig

data TypedFunction = TypedFunction
  { tfSig :: FunctionSig
  , tfBody :: TypedExpr
  } deriving (Eq, Show, Generic)

instance ToJSON TypedFunction
instance FromJSON TypedFunction

-- Top-level typed program used by the downstream C++ LLVM backend.
data TypedProgram = TypedProgram
  { tpFunctions :: [TypedFunction]
  , tpBindings :: [(String, PrimitiveType, TypedExpr)]
  } deriving (Eq, Show, Generic)

instance ToJSON TypedProgram
instance FromJSON TypedProgram

-- JSON export helpers for the C++ backend.
encodeProgram :: TypedProgram -> BL.ByteString
encodeProgram = Data.Aeson.encode

writeProgramJson :: FilePath -> TypedProgram -> IO ()
writeProgramJson path prog = BL.writeFile path (encodeProgram prog)

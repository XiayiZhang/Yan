module AST
  ( Program(..)
  , Stmt(..)
  , FuncDef(..)
  , Expr(..)
  , TypeName(..)
  , BinOp(..)
  , UnaryOp(..)
  , Name
  ) where

import Data.Text (Text)

type Name = Text

data Program = Program [Stmt]
  deriving (Eq, Show)

data Stmt
  = BindStmt Name Expr
  | TypeStmt Name TypeName
  | FuncStmt FuncDef
  | ReturnStmt Expr
  | IfStmt Expr [Stmt] [Stmt]
  | ExprStmt Expr
  | Block [Stmt]
  deriving (Eq, Show)

data FuncDef = FuncDef
  { funcName :: Name
  , funcParams :: [(Name, TypeName)]
  , funcReturnType :: TypeName
  , funcBody :: [Stmt]
  }
  deriving (Eq, Show)

data Expr
  = IntLit Integer
  | BoolLit Bool
  | Var Name
  | Call Name [Expr]
  | UnaryOp UnaryOp Expr
  | BinaryOp BinOp Expr Expr
  | IfExpr Expr Expr Expr
  deriving (Eq, Show)

data UnaryOp = Neg | Not
  deriving (Eq, Show)

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Eq
  | Neq
  | Lt
  | Gt
  | And
  | Or
  deriving (Eq, Show)

data TypeName
  = TypeAlias Name
  | TypeInt
  | TypeBool
  | TypeFunc TypeName TypeName
  deriving (Eq, Show)

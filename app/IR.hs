module IR where

import Data.Map

data Type
  = TVoid
  | TI32
  | TStr
  deriving (Show)

data Val
  = VI32 Integer
  | VStr String
  deriving (Show)

data Exp
  = Var String
  | Val Val
  | Call String [Exp]
  deriving (Show)

data Stmt
  = Decl Type String Exp
  | Exp Exp
  deriving (Show)

data Top
  = Global Type String Val
  | Function Type String [(Type, String)] [Stmt]
  deriving (Show)

type Prog = [Top]
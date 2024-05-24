module Exp where

data Exp
  = Symbol String
  | Int Integer
  | Str String
  | Bind Exp Exp -- Name, Value
  | Call Exp [Exp] -- Name, [Args]
  | Function [(Exp, Exp)] Exp Exp -- [(Param, Type)] ReturnType Body
  | Struct [(Exp, Exp)] -- Name, Type
  | Block [Exp]
  deriving (Show)

type Prog = [Exp]
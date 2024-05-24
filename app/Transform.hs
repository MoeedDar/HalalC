module Transform where

import Data.Map as Map
import Exp
import IR

getTypeOfVal :: Val -> Type
getTypeOfVal (VI32 _) = TI32
getTypeOfVal (VStr _) = TStr

-- Naive af, we will replace this soon enough

inferType :: Exp.Exp -> Type
inferType (Int _) = TI32
inferType (Str _) = TStr
inferType _ = TVoid

transformName :: Exp.Exp -> Either String String
transformName (Symbol s) = Right s

transformVal :: Exp.Exp -> Either String Val
transformVal (Int i) = Right $ VI32 i
transformVal (Str s) = Right $ VStr s

transformStmt :: Exp.Exp -> Either String IR.Stmt
transformStmt (Bind name exp) = do
  name' <- transformName name
  exp' <- transformExp exp
  let typ = inferType exp
  return $ IR.Decl typ name' exp'
transformStmt exp = IR.Exp <$> transformExp exp

transformBlock :: Exp.Exp -> Either String [IR.Stmt]
transformBlock (Exp.Block exps) = mapM transformStmt exps
transformBlock exp = transformBlock $ Exp.Block [exp] 

transformExp :: Exp.Exp -> Either String IR.Exp
transformExp (Symbol s) = Right $ Var s
transformExp (Int i) = Right $ Val $ VI32 i
transformExp (Str s) = Right $ Val $ VStr s
transformExp (Exp.Call name args) = do
  name' <- transformName name
  args' <- mapM transformExp args
  return $ IR.Call name' args'
transformExp exp = Left $ "Invalid expression " ++ show exp

transformType :: Exp.Exp -> Either String Type
transformType (Symbol "i32") = Right TI32
transformType (Symbol "str") = Right TStr
transformType (Symbol "void") = Right TVoid
transformType _ = Left "Invalid type"

transformTop :: Exp.Exp -> Either String IR.Top
transformTop (Exp.Bind var (Exp.Function args typ body)) = do
  typ' <- transformType typ
  var' <- transformName var
  args' <-
    mapM
      ( \(name, argType) -> do
          name' <- transformName name
          argType' <- transformType argType
          return (argType', name')
      )
      args
  body' <- transformBlock body
  return $ IR.Function typ' var' args' body'
transformTop (Exp.Bind var val) = do
  var' <- transformName var
  val' <- transformVal val
  let typ = getTypeOfVal val'
  return $ Global typ var' val'

transformProg :: Exp.Prog -> Either String IR.Prog
transformProg = mapM transformTop
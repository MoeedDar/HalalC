module Codegen where

import Data.List
import IR

codegenType :: Type -> String
codegenType TVoid = "void"
codegenType TI32 = "const int"
codegenType TStr = "const char *"

codegenVal :: Val -> String
codegenVal (VI32 n) = show n
codegenVal (VStr s) = "\"" ++ s ++ "\""

codegenExp :: Exp -> String
codegenExp (Var v) = v
codegenExp (Val val) = codegenVal val
codegenExp (Call fn args) = fn ++ "(" ++ unwords (map codegenExp args) ++ ")"

codegenStmt :: Stmt -> String
codegenStmt (Decl typ var exp) = codegenType typ ++ " " ++ var ++ " = " ++ codegenExp exp ++ ";"
codegenStmt (Exp exp) = codegenExp exp ++ ";"

codegenTop :: Top -> String
codegenTop (Global typ var val) = codegenType typ ++ " " ++ var ++ " = " ++ codegenVal val ++ ";"
codegenTop (Function typ name params stmts) =
  codegenType typ
    ++ " "
    ++ name
    ++ " ("
    ++ intercalate ", " (map (\(paramType, paramName) -> codegenType paramType ++ " " ++ paramName) params)
    ++ "){\n"
    ++ unlines (map (\stmt -> "  " ++ codegenStmt stmt) stmts)
    ++ "}\n"

codegenProg :: Prog -> String
codegenProg = unlines . map codegenTop
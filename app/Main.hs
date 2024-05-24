module Main where

import Codegen
import Parse
import Transform

input =
  "\
  \msg := \"hello world\"\
  \main := fn () void {\
  \  println msg\
  \}"

main :: IO ()
main = do
  case parse input of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right exp -> do
      case transformProg exp of
        Left err -> putStrLn $ "Transform error: " ++ err
        Right ir -> putStrLn $ codegenProg ir
module Parse where

import Data.Char (isSpace)
import Exp
import Lex
import Text.Parsec
import Text.Parsec.String (Parser)

simpleSpace =
  skipMany1 (satisfy (\c -> (c /= '\n') && isSpace c))

parse :: String -> Either ParseError Prog
parse = Text.Parsec.parse (Lex.whitespace *> many parseTop <* eof)  ""
  where
    parseTop = choice [try parseBind, try parseFunction]

parseAtom :: Parser Exp.Exp
parseAtom =
  choice
    [ try parseSymbol,
      try parseInt,
      try parseStr,
      try parseBlock,
      try $ Lex.parens parseExp
    ]

parseExp :: Parser Exp.Exp
parseExp =
  choice
    [ try parseBind,
      try parseFunction,
      try parseCall,
      try parseAtom
    ]

parseBlock :: Parser Exp.Exp
parseBlock = do
  Exp.Block <$> Lex.braces (many parseExp)

parseSymbol :: Parser Exp.Exp
parseSymbol = do
  Exp.Symbol <$> Lex.symbol

parseInt :: Parser Exp.Exp
parseInt = do
  Exp.Int <$> Lex.int

parseStr :: Parser Exp.Exp
parseStr = do
  Exp.Str <$> Lex.str

parseBind :: Parser Exp.Exp
parseBind = do
  name <- parseAtom
  Lex.operator ":="
  Exp.Bind name <$> parseExp

parseCall :: Parser Exp.Exp
parseCall = do
  name <- parseSymbol
  args <- sepBy parseAtom simpleSpace
  optional newline
  return $ Exp.Call name args

parseFunction :: Parser Exp.Exp
parseFunction = do
  Lex.keyword "fn"
  params <- Lex.parens parseParams
  returnType <- parseAtom
  Exp.Function params returnType <$> parseExp

parseParams :: Parser [(Exp.Exp, Exp.Exp)]
parseParams = Lex.commaSep parseParam

parseParam :: Parser (Exp.Exp, Exp.Exp)
parseParam = do
  name <- parseSymbol
  Lex.whitespace
  exp <- parseAtom
  return (name, exp)
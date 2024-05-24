module Lex where

import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token as Token

langDef =
  emptyDef
    { Token.commentStart = "/*",
      Token.commentEnd = "*/",
      Token.commentLine = "//",
      Token.identStart = letter,
      Token.identLetter = alphaNum <|> oneOf "_",
      Token.reservedNames = ["fn", "struct"],
      Token.reservedOpNames = [":="]
    }

lexer = Token.makeTokenParser langDef

whitespace = Token.whiteSpace lexer

comma = Token.comma lexer

symbol = Token.identifier lexer

int = Token.integer lexer

str = Token.stringLiteral lexer

keyword = Token.reserved lexer

operator = Token.reservedOp lexer

parens = Token.parens lexer

braces = Token.braces lexer

commaSep = Token.commaSep lexer
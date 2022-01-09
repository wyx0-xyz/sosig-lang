{-# LANGUAGE FlexibleContexts #-}

module Language.Sosig.Lexer where

import Text.Parsec (ParsecT, satisfy, (<|>))
import qualified Text.Parsec.Char as C
import Text.Parsec.Prim (Stream, (<?>))
import qualified Text.Parsec.Token as P

lexer :: Stream s m Char => P.GenTokenParser s u m
lexer =
  P.makeTokenParser $
    P.LanguageDef
      { P.commentStart = [],
        P.commentEnd = [],
        P.commentLine = "//",
        P.nestedComments = True,
        P.identStart = C.letter <|> C.oneOf "+-*/%",
        P.identLetter = C.letter <|> C.char '"' <|> C.oneOf "+-*/%",
        P.opStart = C.oneOf [],
        P.opLetter = C.oneOf [],
        P.reservedNames = [],
        P.reservedOpNames = [],
        P.caseSensitive = True
      }

char :: Stream s m Char => ParsecT s u m Char
char = P.charLiteral lexer

brackets :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
brackets = P.brackets lexer

double :: Stream s m Char => ParsecT s u m Double
double = P.float lexer

identifier :: Stream s m Char => ParsecT s u m String
identifier = P.identifier lexer

integer :: Stream s m Char => ParsecT s u m Integer
integer = P.integer lexer

lexeme :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
lexeme = P.lexeme lexer

operator :: Stream s m Char => ParsecT s u m String
operator = P.operator lexer

parens :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens = P.parens lexer

string :: Stream s m Char => ParsecT s u m String
string = P.stringLiteral lexer

symbol :: Stream s m Char => String -> ParsecT s u m String
symbol = P.symbol lexer

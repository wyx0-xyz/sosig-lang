module Language.Sosig.Parser where

import qualified Language.Sosig.Lexer as Lexer
import Text.Parsec (between, many, string, try, (<|>))
import Text.Parsec.String (Parser)

data Expr
  = Integer Integer
  | Double Double
  | Char Char
  | String String
  | Boolean Bool
  | Identifier String
  | List [Expr]
  deriving (Show)

parseInteger :: Parser Expr
parseInteger = Integer <$> Lexer.integer

parseDouble :: Parser Expr
parseDouble = Double <$> Lexer.double

parseChar :: Parser Expr
parseChar = Char <$> Lexer.char

parseString :: Parser Expr
parseString = String <$> Lexer.string

parseBoolean :: Parser Expr
parseBoolean = do
  boolean <- string "True" <|> string "False"
  return $ Boolean $ boolean == "True"

parseIdentifier :: Parser Expr
parseIdentifier = Identifier <$> Lexer.identifier

parseList :: Parser Expr
parseList = Lexer.brackets $ List <$> many parseExpr

parseExpr :: Parser Expr
parseExpr = try parseDouble <|> parseInteger <|> parseChar <|> parseString <|> parseBoolean <|> parseIdentifier <|> parseList

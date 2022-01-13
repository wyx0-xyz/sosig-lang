module Language.Sosig.Parser where

import Control.Monad (replicateM_)
import qualified Language.Sosig.Lexer as Lexer
import Text.Parsec (between, char, many, string, try, (<|>))
import Text.Parsec.String (Parser)

data Statement
  = Def String [Type] Type Expr
  deriving (Eq, Show)

data Expr
  = Integer Integer
  | Double Double
  | Char Char
  | String String
  | Boolean Bool
  | Identifier String
  | List [Expr]
  | Void
  deriving (Eq, Show)

data Type
  = TInteger
  | TDouble
  | TChar
  | TString
  | TBoolean
  | TList Type
  | TVoid
  deriving Eq

instance Show Type where
  show TInteger = "Integer"
  show TDouble = "Double"
  show TChar = "Char"
  show TString = "String"
  show TBoolean = "Boolean"
  show (TList t) = "[" ++ show t ++ "]"
  show (TVar name) = name
  show TVoid = "Void"

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

parseVoid :: Parser Expr
parseVoid = string "()" >> return Void

parseExpr :: Parser Expr
parseExpr = try parseDouble <|> parseInteger <|> parseChar <|> parseString <|> parseBoolean <|> parseIdentifier <|> parseList <|> parseVoid

parseTInteger :: Parser Type
parseTInteger = string "Integer" >> return TInteger

parseTDouble :: Parser Type
parseTDouble = string "Double" >> return TDouble

parseTChar :: Parser Type
parseTChar = string "Char" >> return TChar

parseTString :: Parser Type
parseTString = string "String" >> return TString

parseTBoolean :: Parser Type
parseTBoolean = string "Boolean" >> return TBoolean

parseTList :: Parser Type
parseTList = TList <$> between (char '[') (char ']') parseType

parseTVoid :: Parser Type
parseTVoid = string "Void" >> return TVoid

parseType :: Parser Type
parseType = parseTInteger <|> parseTDouble <|> parseTChar <|> parseTString <|> parseTBoolean <|> parseTList <|> parseTVoid

parseSignature :: Parser ([Type], Type)
parseSignature = do
  parameters <- between (Lexer.symbol ":<") (Lexer.symbol ">:") (many $ Lexer.lexeme parseType)
  output <- parseType
  return (parameters, output)

parseDef :: Parser Statement
parseDef = do
  name <- Lexer.lexeme Lexer.identifier
  (parameters, output) <- parseSignature
  char '\n'
  Lexer.symbol name
  replicateM_ (length parameters) (Lexer.lexeme Lexer.identifier)
  Lexer.symbol "="
  Def name parameters output <$> parseExpr

parseStatement :: Parser Statement
parseStatement = parseDef

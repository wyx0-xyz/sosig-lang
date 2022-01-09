module Main where

import Language.Sosig.Parser (parseStatement)
import System.Environment (getArgs)
import Text.Parsec (parse)

main :: IO ()
main = do
  (filePath : _) <- getArgs
  content <- readFile filePath

  print $ parse parseStatement filePath content

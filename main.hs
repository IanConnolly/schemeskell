module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse (space >> symbol) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"


spaces = Parser ()
spaces = skipMany1 space


main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr $ args !! 0)


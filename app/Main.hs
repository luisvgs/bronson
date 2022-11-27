module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data Ast = Var String | Int Integer | Bool Bool | String String

parseString :: Parser Ast
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "bronson" input of
    Left err -> "Whooops " ++ show err
    Right val -> "Found val"

main :: IO ()
main = do
    (expr : _) <- getArgs
    putStrLn (readExpr expr)

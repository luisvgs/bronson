module Main where

import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data AstNode = Var String | Int Integer | Bool Bool | Binary AstNode AstNode | String String deriving (Eq)

eval :: AstNode -> AstNode
eval value@(String _) = value
eval value@(Int _) = value
eval value@(Bool _) = value
eval value@(Binary a b) =
    let (Int x) = eval a
     in let (Int y) = eval b
         in Int (x + y)

parseString :: Parser AstNode
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

parseBinary :: Parser AstNode
parseBinary = do
    first <- parseNumber
    op <- symbol
    rest <- parseNumber
    return (Binary first rest)

parseVariable :: Parser AstNode
parseVariable = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let name = first : rest
    return $ case name of
        "true" -> Bool True
        "false" -> Bool False
        _ -> Var name

parseNumber :: Parser AstNode
parseNumber = fmap (Int . read) $ many1 digit

parseExpr :: Parser AstNode
parseExpr = parseVariable <|> parseNumber <|> parseString

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> AstNode
readExpr input = case parse parseExpr "bronson" input of
    Left err -> String $ "Whooops " ++ show err
    Right val -> val

toString :: AstNode -> String
toString (Var name) = name
toString (String literal) = literal
toString (Int x) = show x
toString (Bool True) = "True"
toString (Bool False) = "False"

instance Show AstNode where show = toString

main :: IO ()
main = getArgs >>= print . eval . readExpr . head

module Main where

import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data AstNode = Var String | Int Integer | Bool Bool | Assign String AstNode | Binary AstNode AstNode | String String deriving (Eq)

data Value = VInt Integer | VBool Bool | VStr String
instance Show Value where show = showValue
showValue :: Value -> String
showValue (VInt x) = show x
showValue (VBool False) = "false"
showValue (VBool True) = "true"
showValue (VStr s) = s

type Env = Map.Map String Value

eval :: AstNode -> Value
eval (String s) = VStr s
eval (Int x) = VInt x
eval (Bool b) = VBool b
-- eval (Assign s e) env =
--     let val = eval e env
--      in Map.insert s val env
eval (Binary a b) =
    let (VInt x) = eval a
     in let (VInt y) = eval b
         in VInt (x + y)

parseAssign :: Parser AstNode
parseAssign = do
    name <- parseString
    char '='
    val <- parseExpr
    return (Assign (toString name) val)

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
toString (Binary x y) = "Binary: " ++ show x ++ " " ++ show y

instance Show AstNode where show = toString

main :: IO ()
main = putStrLn "Hola"

-- main = getArgs >>= print . eval . readExpr . head

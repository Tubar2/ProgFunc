module Parser (parseString) where

import Syntax
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef =
  emptyDef { Token.identStart = letter
           , Token.identLetter = alphaNum
           , Token.reservedOpNames = ["+", "-", "*", "/"]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reservedOp = Token.reservedOp lexer
integer = Token.integer lexer
parens = Token.parens lexer
whiteSpace = Token.whiteSpace lexer

program :: Parser Expr
program = whiteSpace >> expr

expr :: Parser Expr
expr = buildExpressionParser operators term

operators = [ [Infix  (reservedOp "*" >> return Mult) AssocLeft,
               Infix  (reservedOp "/" >> return Div) AssocLeft]
            , [Infix  (reservedOp "+" >> return Add) AssocLeft,
               Infix  (reservedOp "-" >> return Sub) AssocLeft]
            ]

term = parens expr
    <|> Num . fromInteger <$> integer

parseString :: String -> Maybe Expr
parseString s =
  case parse (program <* eof) "" s of
    Left _ -> Nothing
    Right p -> Just p

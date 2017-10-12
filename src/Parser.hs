{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Syntax

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Data.Functor.Identity

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { Tok.commentStart    = "/*"
  , Tok.commentEnd      = "*/"
  , Tok.commentLine     = "//"
  , Tok.nestedComments  = False
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedNames   = []
  , Tok.reservedOpNames = []
  , Tok.caseSensitive   = True
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

expr :: Parser Expr
expr = Ex.buildExpressionParser table term

table :: Ex.OperatorTable String () Identity Expr
table = [
    [ prefix "-" Neg ]
  , [ binary "*" Mul Ex.AssocLeft, binary "/" Div Ex.AssocLeft ]
  , [ binary "+" Add Ex.AssocLeft, binary "-" Sub Ex.AssocLeft ]
  ]

plus :: Parser String
plus = string "+"

minus :: Parser String
minus = string "-"

number :: Parser String
number = many1 digit

unsignedDouble :: Parser String
unsignedDouble =
      try (do {char '.'; n <- number; return ("0." ++ n)}) -- .123 -> 0.123
  <|> try (do {n1 <- number; char '.'; n2 <- number; return (n1 ++ "." ++ n2)}) -- 1.23 -> 1.23
  <|> try number -- 123 -> 123

double :: Parser Double
double = (do
  option "" plus
  m <- option "" minus
  n <- unsignedDouble
  return (read $ m ++ n)) <?> "number"

numExpr :: Parser Expr
numExpr = do
  n <- double
  Tok.whiteSpace lexer
  return (LNum n)

term :: Parser Expr
term =
      numExpr
  <|> parens expr

binary :: String -> (a -> a -> a) -> Ex.Assoc -> Ex.Operator String () Identity a
binary name f = Ex.Infix (reservedOp name >> return f)

prefix :: String -> (a -> a) -> Ex.Operator String () Identity a
prefix name f = Ex.Prefix (reservedOp name >> return f)

postfix :: String -> (a -> a) -> Ex.Operator String () Identity a
postfix name f = Ex.Postfix (reservedOp name >> return f)

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"
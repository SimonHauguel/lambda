module Parser.LambdaParser where

import           Parser.ProgramTypes
import           LambdaData.Data      ( Expr(..) )
import           Text.Parsec
import           Text.Parsec.String
import           Parser.ParseTools


available :: [Char]
available =    ['a'..'z']
            ++ ['A'..'Z']
            ++ ['1'..'9']
            ++ [ '!' , '@' , '$' , '%'
               , '^' , '&' , '*' , '\''
               , '[' , '[' , '/' , '>'
               , '<' , '\'', '\\', ','
               , '.' , '=' , '+' , '-'
               , '?', '_', '|'
               ]


-- Tools
parens :: Parser a -> Parser a
parens = between (char '(' *> spas) (spas *> char ')')

num :: Parser String
num = many1 digit

spas :: Parser String
spas = many space

betSpas :: Parser a -> Parser a
betSpas = between spas spas

named :: Parser String
named = (++) <$> many digit <*> many1 (oneOf available)


-- Parser lambda expr
lambdaExpr :: Parser Expr
lambdaExpr =  lambdaFun
          <|> try lambdaCall
          <|> lambdaVar
          <|> try lnamed
          <|> lambdaNum
          <|> parens lambdaExpr

lambdaVar :: Parser Expr
lambdaVar = char '#' >> Var . read <$> num

lnamed :: Parser Expr
lnamed = Named <$> named

lambdaFun :: Parser Expr
lambdaFun = do
  _         <- oneOf ['λ', '\\']
  numberFun <- option "1" num
  _         <- between spas spas (char '.')
  val       <- lambdaExpr
  return $ recurInt val (const Abs) $ read numberFun

lambdaCall :: Parser Expr
lambdaCall = do
  foldl1 App <$> parens ((:) <$> parspas <*> many1 parspas)
    where parspas = betSpas lambdaExpr

lambdaNum :: Parser Expr
lambdaNum = recurInt (Abs $ Abs $ Var 1) add1Comp . read <$> num



-- Parser
program :: Parser Program
program = foundMain <$> many (betSpas fnDec)


fnDec :: Parser FunctionDec
fnDec = do
  name <- named
  _    <- betSpas (string "::=")
  FunctionDec name <$> between (betSpas (char '{')) (betSpas (char '}')) lambdaExpr



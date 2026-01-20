module Parser where

import           Data.Char                  (isAlphaNum)
import           Data.Void                  (Void)
import           Named
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- TODOs:
-- [ ] Handle multiple arguments syntax sugar (`\x  y. -> ...`)
-- [ ] Use Text
type Parser = Parsec Void String

var :: Parser Var
var =  do
  first <- letterChar <|> char '_'
  rest <- many alphaNumCharExtra
  return $ MkVar $ first : rest
    where alphaNumCharExtra = alphaNumChar <|> char '_'

lamVar :: Parser Var
lamVar = var <* symbol "."

lam :: Parser Expr
lam = do
  _ <- char '\\' <|> char 'λ'
  v <- lamVar
  body <- expr
  return $ Lam v body

app :: Parser Expr
app = do
  e1 <- expr
  space1
  e2 <- expr
  return $ App e1 e2

expr :: Parser Expr
expr = lexeme $ parens $ lam <|> fmap Var var <|> app

parseExpr :: Parser Expr
parseExpr = expr <* eof

parseStr :: String -> Either (ParseErrorBundle String Void) Expr
parseStr = parse parseExpr ""

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockCommentNested "/*" "*/")

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens    = between (symbol "(") (symbol ")")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

module Parser where

import           Data.Void                  (Void)
import           Named
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Megaparsec uses a topdown approach so the solution can not
-- rely on left-recursive parsing. Thus, the strategy of parsing
-- will be:
--
-- Atoms: Var, Lam, parenthesed expression
-- Application: a chain of atoms
--
-- Lexemes: handle spaces by mainly making tokens lexemes.
-- Yes, the tokens, not the high level abstractions.
-- So we're talking about making things like `.`, `(`, `\` as
-- lexemes, not higher level abstractions like `atom`.

-- TODOs:
-- [ ] eleminate left-recursive parsing as it's incompatible with
-- the megaparsec topdown approach. Use atoms: var, lam and parenthesed
-- expr. And then an application is a chain of atoms
-- [ ] Handle multiple arguments syntax sugar (`\x  y. -> ...`)
-- [ ] Use Text
type Parser = Parsec Void String

var :: Parser Var
var =  label "variable" $ lexeme $ do
  first <- letterChar <|> char '_'
  rest <- many alphaNumCharExtra
  return $ MkVar $ first : rest
    where alphaNumCharExtra = alphaNumChar <|> char '_'

lamVar :: Parser Var
lamVar = label "lamVar" $ var <* symbol "."

lam :: Parser Expr
lam = label "lambda" $ do
  _ <- lambdaToken
  v <- lamVar
  body <- expr
  return $ Lam v body
  where
    lambdaToken = lexeme $ char '\\' <|> char 'λ'

app :: Parser Expr
app = label "application" $ do
  exprs <- some atom
  case exprs of
    []     -> return (Var (MkVar "id"))
    (e:xs) -> return $ foldl App e xs

atom :: Parser Expr
atom = fmap Var var <|> parens expr

expr :: Parser Expr
expr = try lam <|> try app

parseExpr :: Parser Expr
parseExpr = expr <* eof

parseStr :: String -> Either (ParseErrorBundle String Void) Expr
parseStr = parse parseExpr ""

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockCommentNested "/*" "*/")

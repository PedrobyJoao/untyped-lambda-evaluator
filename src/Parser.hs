module Parser (parseExprStr) where

import           Control.Monad              (void)
import qualified Data.Map.Strict            as M
import qualified Data.Set                   as S
import           Data.Void                  (Void)
import           Named
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

reserved :: [String]
reserved = ["let"]

-- ====================
-- Entry level
-- ====================

-- TODO: error pretty
parseExprStr :: String -> Either (ParseErrorBundle String Void) Expr
parseExprStr = parse program ""

-- ====================
-- Let bindings + Final Lambda expression
-- ====================

program :: Parser Expr
program = do
  binds <- many (try letBinding)
  scn
  final <- expr
  scn
  eof
  case resolveLets binds final of
    Left msg -> fail msg
    Right e  -> pure e

-- Parse a top-level let binding:
--   let x = <expr>\n
--   let bindings are newline based
letBinding :: Parser (Var, Expr)
letBinding = label "let binding" $ do
  scn
  _ <- letKw
  v <- var
  _ <- symbol "="
  rhs <- expr
  -- require end-of-line (or EOF) so you can't write: let a=...let b=...
  (void eol <|> eof)
  pure (v, rhs)
  where
    letKw :: Parser ()
    letKw = lexeme $ do
      _ <- string "let"
      notFollowedBy allowedVarChars
      pure ()

-- ====================
-- Let resolution (preprocessing, no trace pollution)
-- ====================

-- Inline all bindings from env into an expression (capture-avoiding).
inlineWith :: M.Map Var Expr -> Expr -> Expr
inlineWith env expression =
  M.foldlWithKey'
    (\e v rhs -> fst (substitute v e rhs))
    expression
    env

resolveLets :: [(Var, Expr)] -> Expr -> Either String Expr
resolveLets binds finalExpr = do
  let names = S.fromList (map fst binds)
  if S.size names /= length binds
    then Left $ "Duplicate let-binding name found; Names: " ++ show (map fst binds)
    else pure ()

  let pending0 = M.fromList binds
      depsOk :: S.Set Var -> Expr -> Bool
      depsOk known rhs =
        let deps = freeVars rhs `S.intersection` names
        in deps `S.isSubsetOf` known

      go :: M.Map Var Expr -> M.Map Var Expr -> Either String (M.Map Var Expr)
      go env pending
        | M.null pending = Right env
        | otherwise =
            let known = M.keysSet env
                (ready, later) = M.partition (\rhs -> depsOk known rhs) pending
            in if M.null ready
                 then Left ("Cyclic (recursive) let-bindings detected involving: " ++ show (M.keys pending))
                 else
                   let env' =
                         M.foldlWithKey'
                           (\e v rhs -> M.insert v (inlineWith e rhs) e)
                           env
                           ready
                   in go env' later

  env <- go M.empty pending0
  pure (inlineWith env finalExpr)

-- ====================
-- Parsing Lambda expressions
-- ====================
-- Megaparsec uses a topdown approach so the solution can not
-- rely on left-recursive parsing. Thus, the strategy of parsing
-- will be:
--
-- Atoms: Var, parenthesed expression
-- Application: a chain of atoms (or just one atom)
-- Lambda: it's not an atom, nor an application
--
-- Therefore, an application can be a single Var
--
-- Lexemes: handle spaces by mainly making tokens lexemes.
-- Yes, the tokens, not the high level abstractions.
-- So we're talking about making things like `.`, `(`, `\` as
-- lexemes, not higher level abstractions like `atom`.

var :: Parser Var
var = label "variable" $ lexeme $ do
  first <- letterChar <|> char '_'
  rest <- many allowedVarChars
  let name = first : rest
  if name `elem` reserved
    then fail $ "Reserved name: " ++ name
    else pure (MkVar name)

lamVar :: Parser Var
lamVar = label "lamVar" $ var <* symbol "."

lam :: Parser Expr
lam = label "lambda" $ do
  _ <- lambdaToken
  v <- lamVar
  body <- expr
  pure (Lam v body)
  where
    lambdaToken = lexeme $ char '\\' <|> char 'λ'

-- app can be a single atom (aka: a Var)
app :: Parser Expr
app = label "application" $ do
  exprs <- some atom
  pure (foldl1 App exprs)

atom :: Parser Expr
atom = fmap Var var <|> parens expr

expr :: Parser Expr
expr = try lam <|> app

-- ====================
-- Lexemes
-- ====================

-- Horizontal space consumer (NO newlines)
sc :: Parser ()
sc = L.space
  hspace1
  (L.skipLineComment "--")
  (L.skipBlockCommentNested "/*" "*/")

-- Space consumer that DOES include newlines (for top-level program structure).
scn :: Parser ()
scn = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockCommentNested "/*" "*/")

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- ====================
-- Helpers
-- ====================
allowedVarChars :: Parser Char
allowedVarChars = alphaNumChar <|> char '_'

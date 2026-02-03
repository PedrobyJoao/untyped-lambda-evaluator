module Parser (ParsedProgram(..), parseWithPrelude, parseNoPrelude) where

import           Control.Monad              (void)
import qualified Data.Map.Strict            as M
import qualified Data.Set                   as S
import           Data.Void                  (Void)
import           Named
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data ParsedProgram = ParsedProgram
  { parsedExpr :: Expr
  , namings    :: M.Map Var Expr
  }

reserved :: [String]
reserved = ["let"]

-- ====================
-- Entry level
-- ====================

-- TESTS
parseWithPrelude :: String -> Either (ParseErrorBundle String Void) ParsedProgram
parseWithPrelude = parseProgramWithEnv preludeEnv

parseNoPrelude:: String -> Either (ParseErrorBundle String Void) ParsedProgram
parseNoPrelude = parseProgramWithEnv M.empty

-- env is a map of names to expressions (for equational referencing)
parseProgramWithEnv :: M.Map Var Expr -> String -> Either (ParseErrorBundle String Void) ParsedProgram
parseProgramWithEnv env0 = parse (program env0) ""

-- ====================
-- Let bindings + Final Lambda expression
-- ====================

program :: M.Map Var Expr -> Parser ParsedProgram
program env0 = do
  binds <- many (try letBinding)
  scn
  final <- expr
  scn
  eof
  case resolveLets env0 binds final of
    Left msg          -> fail msg
    Right (e, envOut) -> pure (ParsedProgram e envOut)

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

resolveLets :: M.Map Var Expr -> [(Var, Expr)] -> Expr -> Either String (Expr, M.Map Var Expr)
resolveLets env0 binds finalExpr = do
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

  env <- go env0 pending0
  pure (inlineWith env finalExpr, env)

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

-- ====================
-- Prelude
-- ====================

preludeEnv :: M.Map Var Expr
preludeEnv =
  M.fromList
    [ (v "I", iComb)
    , (v "K", kComb)
    , (v "S", sComb)
    , (v "B", bComb)
    , (v "C", cComb)
    , (v "W", wComb)
    , (v "Y", yComb)
    ]
  where
    v :: String -> Var
    v = MkVar

    varE :: String -> Expr
    varE = Var . v

    lam1 :: String -> Expr -> Expr
    lam1 x body = Lam (v x) body

    app2 :: Expr -> Expr -> Expr
    app2 = App

    app3 :: Expr -> Expr -> Expr -> Expr
    app3 f a b = app2 (app2 f a) b

    iComb :: Expr
    iComb = lam1 "x" (varE "x")

    kComb :: Expr
    kComb = lam1 "x" (lam1 "y" (varE "x"))

    sComb :: Expr
    sComb =
      lam1 "x" $
        lam1 "y" $
          lam1 "z" $
            app2
              (app2 (varE "x") (varE "z"))
              (app2 (varE "y") (varE "z"))

    bComb :: Expr
    bComb =
      lam1 "f" $
        lam1 "g" $
          lam1 "x" $
            app2 (varE "f") (app2 (varE "g") (varE "x"))

    cComb :: Expr
    cComb =
      lam1 "f" $
        lam1 "x" $
          lam1 "y" $
            app3 (varE "f") (varE "y") (varE "x")

    wComb :: Expr
    wComb =
      lam1 "f" $
        lam1 "x" $
          app3 (varE "f") (varE "x") (varE "x")

    yComb :: Expr
    yComb =
      lam1 "f" $
        app2
          (lam1 "x" (app2 (varE "f") (app2 (varE "x") (varE "x"))))
          (lam1 "x" (app2 (varE "f") (app2 (varE "x") (varE "x"))))

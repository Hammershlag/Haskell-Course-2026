module Parser
  ( parseContract
  ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import AST
import Data.Void

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

rwords :: [String]
rwords =
  [ "contract"
  , "state"
  , "transaction"
  , "require"
  , "if"
  , "else"
  , "sender"
  , "empty"
  , "map"
  , "int"
  , "bool"
  , "address"
  , "true"
  , "false"
  ]

identifier :: Parser String
identifier = lexeme $ do
  c <- letterChar
  cs <- many alphaNumChar
  let name = c : cs
  if name `elem` rwords
    then fail $ "Reserved word " ++ show name ++ " cannot be used as an identifier"
    else return name

typeParser :: Parser Type
typeParser = choice
  [ TInt <$ symbol "int"
  , TBool <$ symbol "bool"
  , TAddress <$ symbol "address"
  , do
      _ <- symbol "map"
      _ <- symbol "<"
      t1 <- typeParser
      _ <- symbol ","
      t2 <- typeParser
      _ <- symbol ">"
      return $ TMap t1 t2
  ]

addressLiteral :: Parser String
addressLiteral = choice
  [ try hexAddr
  , quotedString
  ]
  where
    hexAddr = lexeme $ do
      _ <- string "0x"
      rest <- some hexDigitChar
      return $ "0x" ++ rest
    quotedString = lexeme $ do
      _ <- char '"'
      s <- many (anySingleBut '"')
      _ <- char '"'
      return s

integerParser :: Parser Integer
integerParser = L.signed sc (lexeme L.decimal)

valueParser :: Parser Value
valueParser = choice
  [ VBool True <$ symbol "true"
  , VBool False <$ symbol "false"
  , VAddress <$> addressLiteral
  , VInt <$> integerParser
  ]

postfixes :: Expr -> Parser Expr
postfixes base = choice
  [ do
      _ <- symbol "["
      idx <- exprParser
      _ <- symbol "]"
      postfixes (Index base idx)
  , return base
  ]

termParser :: Parser Expr
termParser = do
  base <- choice
    [ Sender <$ symbol "sender"
    , EmptyMap <$ symbol "empty"
    , Lit <$> valueParser
    , Var <$> identifier
    , parens exprParser
    ]
  postfixes base

opsTable :: [[Operator Parser Expr]]
opsTable =
  [ [ InfixL (BinOp Mul <$ symbol "*")
    , InfixL (BinOp Div <$ symbol "/")
    ]
  , [ InfixL (BinOp Add <$ symbol "+")
    , InfixL (BinOp Sub <$ symbol "-")
    ]
  , [ InfixN (BinOp LessEqual <$ symbol "<=")
    , InfixN (BinOp LessThan <$ symbol "<")
    , InfixN (BinOp GreaterEqual <$ symbol ">=")
    , InfixN (BinOp GreaterThan <$ symbol ">")
    , InfixN (BinOp Equal <$ symbol "==")
    , InfixN (BinOp NotEqual <$ symbol "!=")
    ]
  , [ InfixL (BinOp And <$ symbol "&&") ]
  , [ InfixL (BinOp Or <$ symbol "||") ]
  ]

exprParser :: Parser Expr
exprParser = makeExprParser termParser opsTable

statementParser :: Parser Statement
statementParser = choice
  [ requireStmt
  , ifStmt
  , assignStmt
  ]
  where
    requireStmt = do
      _ <- symbol "require"
      cond <- exprParser
      _ <- symbol ";"
      return $ Require cond
    ifStmt = do
      _ <- symbol "if"
      cond <- parens exprParser
      thenBranch <- blockParser
      elseBranch <- option [] (symbol "else" >> (blockParser <|> (pure <$> statementParser)))
      return $ If cond thenBranch elseBranch
    assignStmt = do
      lhs <- exprParser
      _ <- symbol ":="
      rhs <- exprParser
      _ <- symbol ";"
      return $ Assign lhs rhs

blockParser :: Parser [Statement]
blockParser = between (symbol "{") (symbol "}") (many statementParser)

stateVarParser :: Parser StateVar
stateVarParser = do
  name <- identifier
  _ <- symbol ":"
  t <- typeParser
  _ <- symbol "="
  initExpr <- exprParser
  _ <- symbol ";"
  return $ StateVar name t initExpr

stateBlockParser :: Parser [StateVar]
stateBlockParser = do
  _ <- symbol "state"
  _ <- symbol "{"
  vars <- many stateVarParser
  _ <- symbol "}"
  return vars

paramParser :: Parser (String, Type)
paramParser = do
  name <- identifier
  _ <- symbol ":"
  t <- typeParser
  return (name, t)

paramListParser :: Parser [(String, Type)]
paramListParser = paramParser `sepBy` symbol ","

transactionDefParser :: Parser TransactionDef
transactionDefParser = do
  _ <- symbol "transaction"
  name <- identifier
  params <- parens paramListParser
  body <- blockParser
  return $ TransactionDef name params body

contractParser :: Parser Contract
contractParser = do
  _ <- symbol "contract"
  name <- identifier
  _ <- symbol "{"
  stateVars <- option [] stateBlockParser
  txs <- many transactionDefParser
  _ <- symbol "}"
  return $ Contract name stateVars txs

parseContract :: String -> String -> Either String Contract
parseContract srcName input =
  case parse (sc *> contractParser <* eof) srcName input of
    Left err -> Left (errorBundlePretty err)
    Right c  -> Right c

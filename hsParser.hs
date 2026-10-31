module HsParser
  ( LiteralIdentifier(..)
  , ArithmBinOpNEW(..)
  , LogicalExpr(..)
  , LogicalBinOp(..)
  , RelationalBinOp(..)
  , Type(..)
  , Branch(..)
  , Expression(..)
  , Stmt(..)
  , parseFile
  , writeFileOutput
  , haskellParser
  , statement
  ) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.List (intercalate)

-- The raw parser AST reflects the syntax that is actually supported by the project.
data LiteralIdentifier = VarLI String
                      | IntConstLI Integer
                      | FloatConstLI Double
                      | BoolConstLI Bool
                      | String String
                      deriving (Eq, Show)

data ArithmBinOpNEW = Plus
                    | Minus
                    | Times
                    | Divided
                    deriving (Eq, Show)

data LogicalExpr = LogicalVar LiteralIdentifier
                 | Not LogicalExpr
                 | LogicalBinary LogicalBinOp LogicalExpr LogicalExpr
                 | RelationalBinary RelationalBinOp Expression Expression
                 deriving (Eq, Show)

data LogicalBinOp = And | Or deriving (Eq, Show)

data RelationalBinOp = Greater
                    | Less
                    | GreaterEqual
                    | LessEqual
                    | Equal
                    | NotEqual
                    deriving (Eq, Show)

data Type = RegularType String
          | ListType String
          | FunctionType [Type]
          deriving (Eq, Show)

data Branch = Branch LiteralIdentifier LiteralIdentifier deriving (Eq, Show)

data Expression = Neg Expression
                | VarExpr LiteralIdentifier
                | LiteralExpr LiteralIdentifier
                | ArithmeticBinaryExpr ArithmBinOpNEW Expression Expression
                | List [LiteralIdentifier]
                | FunctionCallNew String [LiteralIdentifier]
                | EExpr Expression
                | CaseOf Expression [Branch]
                deriving (Eq, Show)

data Stmt = Sequence [Stmt]
          | LetIn Stmt Stmt
          | TypeDeclaration String Type
          | If LogicalExpr Stmt Stmt
          | FunctionDeclaration String [String] Stmt
          | AssignNew LiteralIdentifier Expression
          | NoWhere
          | AssignRegular Expression Stmt
          | CaseOfStm Expression [Branch]
          | EmbeddedExpression Expression
          deriving (Eq)

instance Show Stmt where
  show = showStmt 0
    where
      showStmt indentLevel stmt =
        case stmt of
          Sequence stmts ->
            "Sequence [\n" ++ intercalate ",\n" (map (showStmt (indentLevel + 1)) stmts) ++ "\n" ++ replicate (indentLevel * 4) ' ' ++ "]"
          AssignNew li expr ->
            replicate (indentLevel * 4) ' ' ++ "AssignNew " ++ show li ++ " = " ++ show expr
          LetIn s1 s2 ->
            replicate (indentLevel * 4) ' ' ++ "LetIn " ++ showStmt (indentLevel + 1) s1 ++ " " ++ showStmt (indentLevel + 1) s2
          TypeDeclaration name ty ->
            replicate (indentLevel * 4) ' ' ++ "TypeDeclaration " ++ name ++ " :: " ++ show ty
          If cond thenBranch elseBranch ->
            replicate (indentLevel * 4) ' ' ++ "If (" ++ show cond ++ ") then " ++ showStmt (indentLevel + 1) thenBranch ++ " else " ++ showStmt (indentLevel + 1) elseBranch
          FunctionDeclaration name params body ->
            replicate (indentLevel * 4) ' ' ++ "FunctionDeclaration " ++ name ++ " " ++ show params ++ " = " ++ show body
          NoWhere -> "NoWhere"
          AssignRegular expr _ -> "AssignRegular " ++ show expr
          CaseOfStm expr branches -> "CaseOfStm " ++ show expr ++ " " ++ show branches
          EmbeddedExpression expr -> "EmbeddedExpression " ++ show expr

languageDef = emptyDef
  { Token.commentStart = "{-"
  , Token.commentEnd = "-}"
  , Token.commentLine = "--"
  , Token.identStart = letter
  , Token.identLetter = alphaNum
  , Token.reservedNames = ["let", "in", "where", "True", "False", "if", "then", "else", "case", "of"]
  }

lexer = Token.makeTokenParser languageDef
identifierParser = Token.identifier lexer
reservedParser = Token.reserved lexer
reservedOpParser = Token.reservedOp lexer
parensParser = Token.parens lexer
integerParser = Token.integer lexer
floatParser = Token.float lexer
commaParser = Token.comma lexer
semiParser = Token.semi lexer
whitespaceParser = Token.whiteSpace lexer

boolParser :: Parser Bool
boolParser = (reservedParser "True" >> return True) <|> (reservedParser "False" >> return False)

variableParser :: Parser LiteralIdentifier
variableParser = VarLI <$> identifierParser

literalParser :: Parser LiteralIdentifier
literalParser = choice
  [ FloatConstLI <$> try floatParser
  , IntConstLI <$> integerParser
  , BoolConstLI <$> boolParser
  ]

parseType :: Parser Type
parseType = do
  name <- many1 letter
  case name of
    "Integer" -> return (RegularType name)
    "Int32" -> return (RegularType "Int32")
    "Float" -> return (RegularType "Float")
    "Bool" -> return (RegularType "Bool")
    "String" -> return (RegularType "String")
    _ -> fail ("Unsupported type: " ++ name)

parseFunctionType :: Parser Type
parseFunctionType = do
  argType <- parseType
  _ <- reservedOpParser "->"
  retType <- parseType
  return (FunctionType [argType, retType])

parseListItem :: Parser LiteralIdentifier
parseListItem = choice
  [ BoolConstLI <$> boolParser
  , FloatConstLI <$> try floatParser
  , IntConstLI <$> integerParser
  , VarLI <$> identifierParser
  ]

parseListLiteral :: Parser Expression
parseListLiteral = do
  _ <- reservedOpParser "["
  items <- parseListItem `sepBy` commaParser
  _ <- reservedOpParser "]"
  return (List items)

functionCallParser :: Parser Expression
functionCallParser = do
  name <- identifierParser
  _ <- reservedOpParser "("
  args <- many (parseListItem <|> VarLI <$> identifierParser) `sepBy` commaParser
  _ <- reservedOpParser ")"
  return (FunctionCallNew name args)

parseAtom :: Parser Expression
parseAtom = choice
  [ try parseListLiteral
  , try functionCallParser
  , parensParser arithmeticExprParser
  , LiteralExpr <$> literalParser
  , VarExpr <$> variableParser
  ]

arithmeticExprParser :: Parser Expression
arithmeticExprParser = buildExpressionParser operatorTable parseTerm
  where
    parseTerm = choice
      [ Neg <$> (reservedOpParser "-" >> parseTerm)
      , parseAtom
      ]
    operatorTable =
      [ [Prefix (reservedOpParser "-" >> return Neg)]
      , [binary "*" Times AssocLeft, binary "/" Divided AssocLeft]
      , [binary "+" Plus AssocLeft, binary "-" Minus AssocLeft]
      ]
    binary name fun = Infix (reservedOpParser name >> return (ArithmeticBinaryExpr fun))

relationalOperator :: Parser RelationalBinOp
relationalOperator =
      (reservedOpParser ">" >> return Greater)
  <|> (reservedOpParser "<" >> return Less)
  <|> (reservedOpParser ">=" >> return GreaterEqual)
  <|> (reservedOpParser "<=" >> return LessEqual)
  <|> (reservedOpParser "==" >> return Equal)
  <|> (reservedOpParser "!=" >> return NotEqual)

logicalExprParser :: Parser LogicalExpr
logicalExprParser = do
  left <- choice
    [ try (do lhs <- arithmeticExprParser; op <- relationalOperator; rhs <- arithmeticExprParser; return (RelationalBinary op lhs rhs))
    , try (LogicalVar <$> (boolParser >>= return . BoolConstLI))
    , try (LogicalVar <$> variableParser)
    , parensParser logicalExprParser
    ]
  rest <- optionMaybe ((reservedOpParser "&&" >> return And) <|> (reservedOpParser "||" >> return Or))
  case rest of
    Nothing -> return left
    Just op -> do
      right <- logicalExprParser
      return (LogicalBinary op left right)

ifStmtParser :: Parser Stmt
ifStmtParser = do
  reservedParser "if"
  cond <- logicalExprParser
  reservedParser "then"
  thenStmt <- statement
  reservedParser "else"
  elseStmt <- statement
  return (If cond thenStmt elseStmt)

caseStmtParser :: Parser Stmt
caseStmtParser = do
  reservedParser "case"
  expr <- arithmeticExprParser
  reservedParser "of"
  branches <- many1 branchParser
  return (CaseOfStm expr branches)

branchParser :: Parser Branch
branchParser = do
  pat <- choice [VarLI <$> identifierParser, IntConstLI <$> integerParser, BoolConstLI <$> boolParser]
  _ <- reservedOpParser "->"
  value <- choice [VarLI <$> identifierParser, IntConstLI <$> integerParser, FloatConstLI <$> floatParser, BoolConstLI <$> boolParser, String <$> many1 letter]
  return (Branch pat value)

assignmentParser :: Parser Stmt
assignmentParser = do
  lhs <- variableParser
  _ <- reservedOpParser "="
  rhs <- finalExprParser
  return (AssignNew lhs rhs)

functionDeclarationParser :: Parser Stmt
functionDeclarationParser = do
  name <- identifierParser
  params <- many identifierParser
  _ <- reservedOpParser "="
  body <- finalExprParser
  return (FunctionDeclaration name params (AssignRegular body NoWhere))

whereClauseParser :: Parser Stmt
whereClauseParser = do
  reservedParser "where"
  stmt <- statement
  return stmt

exprBodyParser :: Parser Stmt
exprBodyParser = do
  expr <- finalExprParser
  optional (reservedParser "where")
  return (AssignRegular expr NoWhere)

finalExprParser :: Parser Expression
finalExprParser = choice
  [ try parseListLiteral
  , try functionCallParser
  , try arithmeticExprParser
  , try (LiteralExpr <$> literalParser)
  , try (VarExpr <$> variableParser)
  ]

statement :: Parser Stmt
statement = choice
  [ try typeDeclarationParser
  , try ifStmtParser
  , try caseStmtParser
  , try functionDeclarationParser
  , try assignmentParser
  , try letInStmtParser
  ]

letInStmtParser :: Parser Stmt
letInStmtParser = do
  reservedParser "let"
  bind <- assignmentParser
  reservedParser "in"
  body <- statement
  return (LetIn bind body)

typeDeclarationParser :: Parser Stmt
typeDeclarationParser = do
  name <- identifierParser
  _ <- reservedOpParser "::"
  ty <- parseType
  return (TypeDeclaration name ty)

parseFile :: FilePath -> IO Stmt
parseFile file = do
  src <- readFile file
  case parse (whitespaceParser >> many statement <* eof) file src of
    Left err -> fail (show err)
    Right stmts -> return (if length stmts == 1 then head stmts else Sequence stmts)

writeFileOutput :: FilePath -> IO ()
writeFileOutput file = do
  result <- parseFile file
  writeFile "parsed.txt" (show result)

haskellParser :: Parser Stmt
haskellParser = do
  stmts <- many statement
  eof
  return (if length stmts == 1 then head stmts else Sequence stmts)

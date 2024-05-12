import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token as Token

data ArithmeticExpr = Var String
           | IntConst Integer
           | Negative ArithmeticExpr
           | ABinary ABinOp ArithmeticExpr ArithmeticExpr
              deriving (Show)

data ABinOp = Add
            | Subtract
            | Multiply
            | Divide
                deriving (Show)

data Stmt = Sequence [Stmt]
          | Assign String ArithmeticExpr
            deriving (Show)

languageDef = emptyDef  { Token.identStart      = letter
                        , Token.identLetter     = alphaNum
                        , Token.reservedOpNames = ["+", "-", "*", "/", "=", "<", ">" ]
                        }

lexer = Token.makeTokenParser languageDef

identifierParser = Token.identifier lexer

reservedOpParser = Token.reservedOp lexer

parensParser     = Token.parens     lexer

integerParser    = Token.integer    lexer

haskellParser :: Parser Stmt
haskellParser = statement

statement :: Parser Stmt
statement =   parensParser statement
          <|> sequenceOfStmt

sequenceOfStmt =
  do list <- many subStatement
     return $ if length list == 1 then head list else Sequence list

subStatement :: Parser Stmt
subStatement =   assignStmt

assignStmt :: Parser Stmt
assignStmt =
  do var  <- identifierParser
     reservedOpParser "="
     Assign var <$> aExpression


aExpression :: Parser ArithmeticExpr
aExpression = buildExpressionParser aOperators aTerm

aOperators = [ [Prefix (reservedOpParser "-"   >> return Negative)          ]
             , [Infix  (reservedOpParser "*"   >> return (ABinary Multiply)) AssocLeft,
                Infix  (reservedOpParser "/"   >> return (ABinary Divide  )) AssocLeft,
                Infix  (reservedOpParser "+"   >> return (ABinary Add     )) AssocLeft,
                Infix  (reservedOpParser "-"   >> return (ABinary Subtract)) AssocLeft]
              ]

aTerm =  parensParser aExpression
     <|> fmap Var identifierParser
     <|> fmap IntConst integerParser


parseFile :: FilePath -> IO Stmt
parseFile file =
  do haskellCode  <- readFile file
     case parse haskellParser "" haskellCode of
       Left e  -> print e >> fail "parse error"
       Right r -> return r
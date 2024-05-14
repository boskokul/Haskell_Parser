import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token as Token

data ArithmeticExpr = Var String
           | IntConst Integer
           | Negative ArithmeticExpr
           | ArithmeticBinary ABinOp ArithmeticExpr ArithmeticExpr
           | FunctionCall String String
              deriving (Show)

data ABinOp = Add
            | Subtract
            | Multiply
            | Divide
                deriving (Show)

data LogicalExpr = BoolConst Bool
                | Not LogicalExpr
                | LogicalBinary LogicalBinOp LogicalExpr LogicalExpr
                deriving (Show)

data LogicalBinOp = And 
                  | Or 
                  deriving (Show)

data Type = RegularType String
            | ListType String
            | FunctionType [Type]
              deriving (Show)
              
data Stmt = Sequence [Stmt]
          | Assign String ArithmeticExpr Stmt
          | LetIn Stmt Stmt
          | TypeDeclaration String Type
          | If LogicalExpr Stmt Stmt
          | NoWhere
            deriving (Show)


acceptableTypes :: [String]
acceptableTypes = ["Integer", "String", "Bool"]

languageDef = emptyDef  { Token.identStart      = letter
                        , Token.identLetter     = alphaNum
                        , Token.reservedOpNames = ["+", "-", "*", "/", "=", "<", ">", "::", "->" ]
                        , Token.reservedNames = ["let", "in", "where", "True", "False", "if", "then", "else"]
                        }

lexer = Token.makeTokenParser languageDef

identifierParser = Token.identifier lexer

reservedOpParser = Token.reservedOp lexer

parensParser = Token.parens lexer

integerParser = Token.integer lexer

reservedParser = Token.reserved lexer

semiParser = Token.semi lexer

haskellParser :: Parser Stmt
haskellParser = statement

statement :: Parser Stmt
statement =   parensParser statement
          <|> sequenceOfStmt

sequenceOfStmt =
  do list <- many subStatement
     return $ if length list == 1 then head list else Sequence list

subStatement :: Parser Stmt
subStatement = do
  try typeStmt <|> letInStmt <|> assignStmt <|> ifStmt

embeddedStmt =
  do list <- (subStatement <* spaces) `sepBy1` semiParser
     return $ if length list == 1 then head list else Sequence list

letInStmt :: Parser Stmt
letInStmt =
  do reservedParser "let"
     stmt1 <- embeddedStmt
     mIn <- optionMaybe (try (reservedParser "in"))
     pairsIn <- case mIn of
        Just _ -> embeddedStmt
        Nothing -> fail "missing in clause"
     return $ LetIn stmt1 pairsIn

parseRegularType :: Parser Type
parseRegularType = do
    typ <- many1 letter
    if typ `elem` acceptableTypes
        then return $ RegularType typ
        else fail $ "Invalid type: " ++ typ

parseListType :: Parser Type
parseListType = do
    _ <- char '['
    typ <- many1 letter
    _ <- char ']'
    if typ `elem` acceptableTypes
        then return $ ListType typ
        else fail $ "Invalid type: " ++ typ

parseArgType :: Parser Type
parseArgType = parseRegularType  <|> parseListType

parseFunctionType :: Parser Type
parseFunctionType = do
    argTypes <- (parseArgType <* spaces) `sepBy1` reservedOpParser "->"
    return $ if length argTypes == 1 then head argTypes else FunctionType argTypes

typeStmt :: Parser Stmt
typeStmt =
  do var  <- identifierParser
     reservedOpParser "::"
     typeName <- try parseFunctionType <|> parseRegularType <|> parseListType
     return $ TypeDeclaration var typeName

--popravi ovo
embeddedFunctionCallArgs =
  do (identifierParser <* spaces) `sepBy1` semiParser

functionCall :: Parser ArithmeticExpr
functionCall =
  do f1  <- identifierParser
     FunctionCall f1 <$> identifierParser

ifStmt :: Parser Stmt
ifStmt =
  do reservedParser "if"
     cond  <- logicalExpression
     mThen <- optionMaybe (try (reservedParser "then"))
     stmt1 <- case mThen of
        Just _ -> embeddedStmt
        Nothing -> fail "missing then clause"
     mElse <- optionMaybe (try (reservedParser "else"))
     stmt2 <- case mElse of
        Just _ -> embeddedStmt
        Nothing -> fail "missing else clause"
     return $ If cond stmt1 stmt2

logicalExpression :: Parser LogicalExpr
logicalExpression = buildExpressionParser bOperators bTerm

bOperators = [ [Infix  (reservedOpParser "and" >> return (LogicalBinary And     )) AssocLeft,
                Infix  (reservedOpParser "or"  >> return (LogicalBinary Or      )) AssocLeft]
             ]

bTerm =  parensParser logicalExpression
     <|> (reservedParser "True"  >> return (BoolConst True ))
     <|> (reservedParser "False" >> return (BoolConst False))



assignStmt :: Parser Stmt
assignStmt =
  do var  <- identifierParser
     reservedOpParser "="
     expr <- try functionCall <|> aExpression
     mWhere <- optionMaybe (try (reservedParser "where"))
     pairsWhere <- case mWhere of
        Just _ -> embeddedStmt
        Nothing -> return NoWhere
     return $ Assign var expr pairsWhere

aExpression :: Parser ArithmeticExpr
aExpression = buildExpressionParser aOperators aTerm

aOperators = [ [Prefix (reservedOpParser "-"   >> return Negative)          ]
             , [Infix  (reservedOpParser "*"   >> return (ArithmeticBinary Multiply)) AssocLeft,
                Infix  (reservedOpParser "/"   >> return (ArithmeticBinary Divide  )) AssocLeft,
                Infix  (reservedOpParser "+"   >> return (ArithmeticBinary Add     )) AssocLeft,
                Infix  (reservedOpParser "-"   >> return (ArithmeticBinary Subtract)) AssocLeft]
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
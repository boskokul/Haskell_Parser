import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token as Token
import Data.List



data LiteralIdentifier  = VarLI String
                        | IntConstLI Integer
                        | FloatConstLI Double
                        | BoolConstLI Bool
                        | String String
                        deriving (Show)

data ArithmBinOpNEW = Plus
                 | Minus
                 | Times
                 | Divided
                 deriving (Show)

data LogicalExpr = LogicalVar LiteralIdentifier
               | Not LogicalExpr
               | LogicalBinary LogicalBinOp LogicalExpr LogicalExpr
               | RelationalBinary RelationalBinOp Expression Expression
                deriving (Show)

data LogicalBinOp = And
                  | Or
                  deriving (Show)

data RelationalBinOp = Greater
                     | Less
                     | GreaterEqual
                     | LessEqual
                     | Equal
                     | NotEqual
                     deriving (Show)

data Type = RegularType String
            | ListType String
            | FunctionType [Type]
            deriving Show

data Branch = Branch LiteralIdentifier LiteralIdentifier
            deriving Show

showIndented indentLevel (Branch s a) =
                replicate (indentLevel * 4) ' ' ++ "Branch " ++ show s ++ " (" ++ show a ++ ")"

showBranch indentLevel branch = showIndented indentLevel branch

data Expression = Neg Expression
                | VarExpr LiteralIdentifier
                | LiteralExpr LiteralIdentifier
                | ArithmeticBinaryExpr ArithmBinOpNEW Expression Expression
                | List [LiteralIdentifier]
                | FunctionCallNew String [LiteralIdentifier]
                | EExpr Expression
                | CaseOf Expression [Branch]
                    deriving (Show)
            
data Stmt = Sequence [Stmt]
          | LetIn Stmt Stmt
          | TypeDeclaration String Type
          | If LogicalExpr Stmt Stmt
          | FunctionDeclaration String [String] Stmt
          | AssignNew LiteralIdentifier Stmt
          | NoWhere
          | AssignRegular Expression Stmt
          | CaseOfStm Expression [Branch]
          | EmbeddedExpression Expression
            -- deriving (Show)

instance Show Stmt where
    show = showIndented 0
        where
            showIndented indentLevel (Sequence stmts) =
                "Sequence [\n" ++ intercalate ",\n" (map (showStmt (indentLevel + 1)) stmts) ++ "\n" ++ replicate (indentLevel * 4) ' ' ++ "]"
            showIndented indentLevel (AssignNew li stmt) =
                replicate (indentLevel * 4) ' ' ++ "AssignNew " ++ show li ++ showStmt (indentLevel + 1) stmt
            showIndented indentLevel (LetIn stmt1 stmt2) =
                replicate (indentLevel * 4) ' ' ++ "LetIn " ++ showStmt (indentLevel + 1) stmt1 ++ " " ++ showStmt (indentLevel + 1) stmt2
            showIndented indentLevel (TypeDeclaration s t) =
                replicate (indentLevel * 4) ' ' ++ "TypeDeclaration " ++ s ++ " " ++ show t
            showIndented indentLevel (If l stmt1 stmt2) =
                replicate (indentLevel * 4) ' ' ++ "If (" ++ show l ++ ") \n" ++ showStmt (indentLevel + 1) stmt1 ++ " \n" ++ showStmt (indentLevel + 1) stmt2
            showIndented indentLevel (FunctionDeclaration s params a) =
                replicate (indentLevel * 4) ' ' ++ "FunctionDeclaration " ++ s ++ " " ++ show params ++ " (" ++ showStmt (indentLevel + 1) a ++ ")"
            showIndented indentLevel (CaseOfStm a branches) =
                replicate (indentLevel * 4) ' ' ++ "CaseOf" ++ " (" ++ show a ++ ") " ++ "[\n" ++ intercalate ",\n" (map (showBranch (indentLevel + 1)) branches)  ++ "\n" ++ replicate (indentLevel * 4) ' ' ++ "]"
            showIndented _ NoWhere = "NoWhere"
            showIndented indentLevel (AssignRegular expr stmt) =
                " " ++ " (" ++ show expr ++ ") " ++ showStmt (indentLevel + 1) stmt
            showIndented indentLevel (EmbeddedExpression a) =
                replicate (indentLevel * 4) ' ' ++ " ( " ++ show a ++ " ) "

            showStmt indentLevel stmt = showIndented indentLevel stmt


languageDef = emptyDef  {  Token.commentStart    = "{-",
                           Token.commentEnd      = "-}",
                           Token.commentLine     = "--",
                           Token.identStart      = letter,
                           Token.identLetter     = alphaNum,
                           Token.reservedNames = ["let", "in", "where", "True", "False", "if", "then", "else", "case", "of"]
                        }

lexer = Token.makeTokenParser languageDef

identifierParser = Token.identifier lexer

reservedOpParser = Token.reservedOp lexer

parensParser = Token.parens lexer

integerParser = Token.integer lexer

floatParser = Token.float lexer

reservedParser = Token.reserved lexer

semiParser = Token.semi lexer

commaParser = Token.comma lexer


boolParser :: Parser Bool
boolParser = (reserved lexer "True" >> return True) 
            <|> (reservedParser "False" >> return False)

variableParser :: Parser LiteralIdentifier
variableParser = VarLI <$> identifierParser

literalParser :: Parser LiteralIdentifier
literalParser = choice
    [   FloatConstLI <$> try floatParser,
        IntConstLI <$> integerParser,  
        BoolConstLI <$> boolParser
    ]

arithmeticExprNewParser :: Parser Expression
arithmeticExprNewParser = buildExpressionParser operatorTable term
    where
        term = choice
            [ Neg <$> (reservedOp lexer "-" >> term)
            , parensParser arithmeticExprNewParser
            , LiteralExpr <$> literalParser
            , VarExpr <$> variableParser
            ]

        operatorTable = [ [Prefix (reservedOp lexer "-" >> return Neg)]
                        , [binary "*" Times AssocLeft, binary "/" Divided AssocLeft]
                        , [binary "+" Plus AssocLeft, binary "-" Minus AssocLeft]
                        ]
        binary name fun = Infix (reservedOp lexer name >> return (ArithmeticBinaryExpr fun))
       
       
        -- prefix name fun = Prefix (reservedOp lexer name >> return fun)


assignmentStmParser :: Parser Stmt
assignmentStmParser = do
    var <- variableParser
    _ <- reservedOpParser "="
    expr <- parseExpr                                                               
    return $ AssignNew var expr

parseExpr :: Parser Stmt
parseExpr = do 
    expr <- try functionCall <|> parseListVar <|> arithmeticExprNewParser
    mWhere <- optionMaybe (reservedParser "where")
    pairsWhere <- case mWhere of
        Just _ -> embeddedStmt
        Nothing -> return NoWhere
    return $ AssignRegular expr pairsWhere

statement :: Parser Stmt
statement =   parensParser statement
          <|> sequenceOfStmt

sequenceOfStmt :: Parser Stmt
sequenceOfStmt =
  do list <- many subStatement
     return $ if length list == 1 then head list else Sequence list

subStatement :: Parser Stmt
subStatement = do
  try assignmentStmParser <|> try typeStmt <|> functionDeclaration      -- <|> letInStmt <|> ifStmt     -- <|> caseOfStmt 

embeddedStmt :: Parser Stmt
embeddedStmt =
  do list <- (subStatement <* spaces) `sepBy1` semiParser
     return $ if length list == 1 then head list else Sequence list


parseExpr2 = do 
    expr <- try functionCall <|> arithmeticExprNewParser
    return $ EExpr expr 

embeddedExpression :: Parser Stmt
embeddedExpression = do
    expr <- parseExpr2
    return $ EmbeddedExpression expr

subStatement2 :: Parser Stmt
subStatement2 = do
  try embeddedExpression

embeddedStmtExpr =
  do list <- (subStatement2 <* spaces) `sepBy1` semiParser
     return $ if length list == 1 then head list else Sequence list

letInStmt :: Parser Stmt
letInStmt =
  do reservedParser "let"
     stmt1 <- embeddedStmt
     mIn <- optionMaybe (reservedParser "in")
     pairsIn <- case mIn of
        Just _ -> embeddedStmtExpr
        Nothing -> fail "missing in clause"
     return $ LetIn stmt1 pairsIn

acceptableTypes :: [String]
acceptableTypes = ["Integer", "String", "Bool", "Float"]

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
     _ <- reservedOpParser "::"
     typeName <- try parseFunctionType <|> parseRegularType <|> parseListType
     return $ TypeDeclaration var typeName


-- identifierParserF = do try identifierParser

parseArgsFC :: Parser [LiteralIdentifier]
parseArgsFC = manyTill (many space *> literalIdentifierTerm <* many space) (reservedOpParser ")")

functionCall :: Parser Expression
functionCall = do  
      f1  <- identifierParser
      _ <- reservedOpParser "("
      pars <- parseArgsFC
      return $ FunctionCallNew f1 pars

ifStmt :: Parser Stmt
ifStmt =
  do reservedParser "if"
     cond  <- logicalExpression
     mThen <- optionMaybe (reservedParser "then")
     stmt1 <- case mThen of
        Just _ -> embeddedStmtExpr
        Nothing -> fail "missing then clause"
     mElse <- optionMaybe (reservedParser "else")
     stmt2 <- case mElse of
        Just _ -> embeddedStmtExpr
        Nothing -> fail "missing else clause"
     return $ If cond stmt1 stmt2

logicalExpression :: Parser LogicalExpr
logicalExpression = buildExpressionParser lOperators lTerm

lOperators = [ [Prefix (reservedOpParser "not" >> return Not)],
               [Infix  (reservedOpParser "&&" >> return (LogicalBinary And)) AssocLeft,
                Infix  (reservedOpParser "||"  >> return (LogicalBinary Or)) AssocLeft]
             ]

lTerm =  parensParser logicalExpression
     <|> LogicalVar <$> (reservedParser "True"  >> return (BoolConstLI True ))
     <|> LogicalVar <$> (reservedParser "False" >> return (BoolConstLI False))
     <|> rExpression

rExpression =
  do a1 <- arithmeticExprNewParser
     op <- rOperator
     RelationalBinary op a1 <$> arithmeticExprNewParser

rOperator =  (reservedOpParser ">" >> return Greater)
         <|> (reservedOpParser "<" >> return Less)
         <|> (reservedOpParser ">=" >> return GreaterEqual)
         <|> (reservedOpParser "<=" >> return LessEqual)
         <|> (reservedOpParser "==" >> return Equal)
         <|> (reservedOpParser "!=" >> return NotEqual)

parseParams :: Parser [String]
parseParams= many (many space *> identifierParser <* many space)

functionDeclaration :: Parser Stmt
functionDeclaration = do
    name <- identifierParser
    args <- parseParams
    _ <- reservedOpParser "="
    body <- try caseOfStmt <|> try parseExpr <|> letInStmt <|> ifStmt
    return (FunctionDeclaration name args body)

caseOfStmt :: Parser Stmt
caseOfStmt =
  do reservedParser "case"
     expr  <- arithmeticExprNewParser
     mOf <- optionMaybe (reservedParser "of")
     branches <- case mOf of
        Just _ -> try parseBranches
        Nothing -> fail "missing of clause"
     return $ CaseOfStm expr branches

parseBranches = do 
      list <- many1 embeddedBranches
      return $ list

embeddedBranches = do try parseBranche

parseBranche :: Parser Branch
parseBranche = do
     name <- try parseId <|> parseInt
     _  <- reservedOpParser "->"
     expr <- whiteSpaceParser *> try quotedIdentifier <|> parseId <|> parseInt
     return $ Branch name expr


quotedIdentifier = do
    _ <- char '"'
    identifier <- many1 letter
    _ <- char '"'
    whiteSpaceParser
    return $ String identifier

parseId = do
    identifier <- identifierParser
    return $ VarLI identifier

parseInt = do
    identifier <- integerParser
    return $ IntConstLI identifier


caseOfStmtNew :: Parser Expression
caseOfStmtNew =
  do reservedParser "case"
     expr  <- arithmeticExprNewParser
     mOf <- optionMaybe (reservedParser "of")
     branches <- case mOf of
        Just _ -> try parseBranches
        Nothing -> fail "missing of clause"
     return $ CaseOf expr branches


literalIdentifierTerm = (reservedParser "True"  >> return (BoolConstLI True ))
                    <|> (reservedParser "False" >> return (BoolConstLI False))
                    <|> VarLI <$> identifierParser
                    <|> FloatConstLI <$> try floatParser
                    <|> IntConstLI <$> integerParser

parseListVar :: Parser Expression
parseListVar = do
    _ <- reservedOpParser "["
    vars <- literalIdentifierTerm `sepBy1` commaParser
    _ <- reservedOpParser "]"
    return $ List vars

whiteSpaceParser = Token.whiteSpace lexer

haskellParser :: Parser Stmt
haskellParser = whiteSpaceParser *> statement


parseFile :: FilePath -> IO Stmt
parseFile file =
  do haskellCode  <- readFile file
     case parse haskellParser "" haskellCode of
       Left e  -> print e >> fail "parse error"
       Right r -> return r

writeFileOutput :: FilePath -> IO ()
writeFileOutput file =
  do haskellCode  <- readFile file
     case parse haskellParser "" haskellCode of
       Left e  -> print e >> fail "parse error"
       Right r -> writeFile "parsed.txt" $ show r
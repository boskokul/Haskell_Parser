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

data ArithmeticExprNEW = Neg ArithmeticExprNEW
                        | VarExpr LiteralIdentifier
                        | LiteralExpr LiteralIdentifier
                        | ArithmeticBinaryExpr ArithmBinOpNEW ArithmeticExprNEW ArithmeticExprNEW
                        | List [LiteralIdentifier]
                        | FunctionCallNew String [LiteralIdentifier]
                        | EExpr ArithmeticExprNEW
                            deriving (Show)

data ArithmBinOpNEW = Plus
                 | Minus
                 | Times
                 | Divided
                 deriving (Show)

-- data ArithmeticExpr = Var String
--                     | IntConst Integer
--                     | FloatConst Double
--                     | BoolConstL Bool
--                     | String String
--                     | ListVar [LiteralIdentifier]
--                     | Negative ArithmeticExpr
--                     | ArithmeticBinary ArithmBinOp ArithmeticExpr ArithmeticExpr
--                     | FunctionCall String [LiteralIdentifier]
--                         deriving (Show)

-- data ListParExpr = LVar String
--                 | LIntConst Integer
--                 | LBoolConst Bool
--                 | LFloatConst Double
--                 deriving (Show)

-- data ArithmBinOp = Add
--                 | Subtract
--                 | Multiply
--                 | Divide
--                     deriving (Show)

data LogicalExpr = LogicalVar LiteralIdentifier
               | Not LogicalExpr
               | LogicalBinary LogicalBinOp LogicalExpr LogicalExpr
               | RelationalBinary RelationalBinOp ArithmeticExprNEW ArithmeticExprNEW
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

-- instance Show Type where
--    show (RegularType x) = "Regular type " ++ show x ++ "\n"
--    show (ListType s) = "ListType " ++ show s ++ "\n"
--    show (FunctionType ts) = "FunctionType " ++ show ts ++ "\n"

data Branch = Branch String LiteralIdentifier
            -- deriving Show

showIndented indentLevel (Branch s a) =
                replicate (indentLevel * 4) ' ' ++ "Branch " ++ s ++ " (" ++ show a ++ ")"

showBranch indentLevel branch = showIndented indentLevel branch
            

data Stmt = Sequence [Stmt]
          | LetIn Stmt Stmt
          | TypeDeclaration String Type
          | If LogicalExpr Stmt Stmt
          | FunctionDeclaration String [String] ArithmeticExprNEW
          | CaseOf ArithmeticExprNEW [Branch]
          | AssignNew LiteralIdentifier Stmt
          | NoWhere
          | AssigRegular ArithmeticExprNEW Stmt
          | EmbeddedExpression ArithmeticExprNEW
            -- deriving (Show)

instance Show Stmt where
    show = showIndented 0
        where
            showIndented indentLevel (Sequence stmts) =
                "Sequence [\n" ++ intercalate ",\n" (map (showStmt (indentLevel + 1)) stmts) ++ "\n" ++ replicate (indentLevel * 4) ' ' ++ "]"
            -- showIndented indentLevel (Assign a b stmt) =
            --     replicate (indentLevel * 4) ' ' ++ "Assign " ++ a ++ " (" ++ show b ++ ") " ++ showStmt (indentLevel + 1) stmt
            showIndented indentLevel (AssignNew li stmt) =
                replicate (indentLevel * 4) ' ' ++ "AssignNew " ++ show li ++ showStmt (indentLevel + 1) stmt
            showIndented indentLevel (LetIn stmt1 stmt2) =
                replicate (indentLevel * 4) ' ' ++ "LetIn " ++ showStmt (indentLevel + 1) stmt1 ++ " " ++ showStmt (indentLevel + 1) stmt2
            showIndented indentLevel (TypeDeclaration s t) =
                replicate (indentLevel * 4) ' ' ++ "TypeDeclaration " ++ s ++ " " ++ show t
            showIndented indentLevel (If l stmt1 stmt2) =
                replicate (indentLevel * 4) ' ' ++ "If (" ++ show l ++ ") \n" ++ showStmt (indentLevel + 1) stmt1 ++ " \n" ++ showStmt (indentLevel + 1) stmt2
            showIndented indentLevel (FunctionDeclaration s params a) =
                replicate (indentLevel * 4) ' ' ++ "FunctionDeclaration " ++ s ++ " " ++ show params ++ " (" ++ show a ++ ")"
            showIndented indentLevel (CaseOf a branches) =
                replicate (indentLevel * 4) ' ' ++ "CaseOf" ++ " (" ++ show a ++ ") " ++ "[\n" ++ intercalate ",\n" (map (showBranch (indentLevel + 1)) branches)  ++ "\n" ++ replicate (indentLevel * 4) ' ' ++ "]"
            showIndented _ NoWhere = "NoWhere"
            showIndented indentLevel (AssigRegular expr stmt) =
                " " ++ " (" ++ show expr ++ ") " ++ showStmt (indentLevel + 1) stmt
            showIndented indentLevel (EmbeddedExpression a) =
                replicate (indentLevel * 4) ' ' ++ " ( " ++ show a ++ " ) "

            showStmt indentLevel stmt = showIndented indentLevel stmt

acceptableTypes :: [String]
acceptableTypes = ["Integer", "String", "Bool"]

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
boolParser = (reserved lexer "True" >> return True) <|> (reservedParser "False" >> return False)

variableParser :: Parser LiteralIdentifier
variableParser = VarLI <$> identifierParser

literalParser :: Parser LiteralIdentifier
literalParser = choice
    [   FloatConstLI <$> try floatParser,
        IntConstLI <$> integerParser,  
        BoolConstLI <$> boolParser
    ]

arithmeticExprNewParser :: Parser ArithmeticExprNEW
arithmeticExprNewParser = buildExpressionParser operatorTable term <?> "expression"
    where
        term = choice
            [ Neg <$> (reservedOp lexer "-" >> term)
            , parens lexer arithmeticExprNewParser
            , LiteralExpr <$> literalParser
            , VarExpr <$> variableParser
            ]

        operatorTable = [ [prefix "-" Neg]
                        , [binary "*" Times AssocLeft, binary "/" Divided AssocLeft]
                        , [binary "+" Plus AssocLeft, binary "-" Minus AssocLeft]
                        ]
        binary name fun = Infix (reservedOp lexer name >> return (ArithmeticBinaryExpr fun))
        prefix name fun = Prefix (reservedOp lexer name >> return fun)


assignmentStmParser :: Parser Stmt
assignmentStmParser = do
    var <- variableParser
    _ <- reservedOpParser "="
    expr <- parseExpr <|> letInStmt <|> ifStmt <|> caseOfStmt
    return $ AssignNew var expr

parseExpr = do 
    expr <- try functionCall <|> parseListVar <|> arithmeticExprNewParser
    mWhere <- optionMaybe (try (reservedParser "where"))
    pairsWhere <- case mWhere of
        Just _ -> embeddedStmt
        Nothing -> return NoWhere
    return $ AssigRegular expr pairsWhere

statement :: Parser Stmt
statement =   parensParser statement
          <|> sequenceOfStmt

sequenceOfStmt =
  do list <- many subStatement
     return $ if length list == 1 then head list else Sequence list

subStatement :: Parser Stmt
subStatement = do
  try assignmentStmParser <|> try typeStmt <|> functionDeclaration <|> letInStmt <|> ifStmt <|> caseOfStmt 

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

-- ovo je za izraze unutar letIn i ifElseThen iskljucivo, inace subStatement a ne subStatement2 
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
     mIn <- optionMaybe (try (reservedParser "in"))
     pairsIn <- case mIn of
        Just _ -> embeddedStmtExpr
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
     _ <- reservedOpParser "::"
     typeName <- try parseFunctionType <|> parseRegularType <|> parseListType
     return $ TypeDeclaration var typeName


identifierParserF = do try identifierParser

-- parseParameters = manyTill (many space *> aExpression <* many space) (reservedOpParser ")")

parseParametersFC = manyTill (many space *> literalIdentifierTerm <* many space) (reservedOpParser ")")

functionCall :: Parser ArithmeticExprNEW
functionCall = do  
      f1  <- identifierParser
      _ <- reservedOpParser "("
      pars <- parseParametersFC
      return $ FunctionCallNew f1 pars

ifStmt :: Parser Stmt
ifStmt =
  do reservedParser "if"
     cond  <- logicalExpression
     mThen <- optionMaybe (try (reservedParser "then"))
     stmt1 <- case mThen of
        Just _ -> embeddedStmtExpr
        Nothing -> fail "missing then clause"
     mElse <- optionMaybe (try (reservedParser "else"))
     stmt2 <- case mElse of
        Just _ -> embeddedStmtExpr
        Nothing -> fail "missing else clause"
     return $ If cond stmt1 stmt2

parseBranches = do 
      list <- many1 embeddedBranches
      return $ list

embeddedBranches = do try parseBranche

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

parseBranche :: Parser Branch
parseBranche = do
     name <- identifierParser
     _  <- reservedOpParser "->"
     expr <- whiteSpaceParser *> try quotedIdentifier <|> parseId <|> parseInt
     return $ Branch name expr

caseOfStmt :: Parser Stmt
caseOfStmt =
  do reservedParser "case"
     expr  <- arithmeticExprNewParser
     mOf <- optionMaybe (try (reservedParser "of"))
     branches <- case mOf of
        Just _ -> try parseBranches
        Nothing -> fail "missing of clause"
     return $ CaseOf expr branches

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


parseArguments = many (many space *> identifierParser <* many space)

functionDeclaration :: Parser Stmt
functionDeclaration = do
    name <- identifierParser
    args <- parseArguments
    _ <- reservedOpParser "="
    body <- try arithmeticExprNewParser
    return (FunctionDeclaration name args body)

-- assignStmt :: Parser Stmt
-- assignStmt =
--   do var  <- identifierParser
--      _ <- reservedOpParser "="
--      expr <- try functionCall <|> parseListVar <|> aExpression
--      mWhere <- optionMaybe (try (reservedParser "where"))
--      pairsWhere <- case mWhere of
--         Just _ -> embeddedStmt
--         Nothing -> return NoWhere
--      return $ Assign var expr pairsWhere

-- aExpression :: Parser ArithmeticExpr
-- aExpression = buildExpressionParser aOperators aTerm

-- aOperators = [ [Prefix (reservedOpParser "-"   >> return Negative)          ]
--              , [Infix  (reservedOpParser "*"   >> return (ArithmeticBinary Multiply)) AssocLeft,
--                 Infix  (reservedOpParser "/"   >> return (ArithmeticBinary Divide  )) AssocLeft,
--                 Infix  (reservedOpParser "+"   >> return (ArithmeticBinary Add     )) AssocLeft,
--                 Infix  (reservedOpParser "-"   >> return (ArithmeticBinary Subtract)) AssocLeft]
--               ]

-- aOperators = [ [prefix "-" Negative]
--              , [binary "*" Multiply, binary "/" Divide]
--              , [binary "+" Add, binary "-" Subtract]
--              ]
--   where
--     binary opName op = Infix (reservedOpParser opName >> return (ArithmeticBinary op)) AssocLeft
--     prefix  opName op = Prefix (do
--                                 reservedOpParser opName
--                                 return op )

-- aTerm =  parensParser aExpression
--         <|> (reservedParser "True"  >> return (BoolConstL True ))
--         <|> (reservedParser "False" >> return (BoolConstL False))
--         <|> Var <$> identifierParser
--         <|> FloatConst <$> try floatParser
--         <|> IntConst <$> integerParser

literalIndentifierExpression :: Parser LiteralIdentifier
literalIndentifierExpression = buildExpressionParser literalIdentifierOperators literalIdentifierTerm

literalIdentifierOperators = []

literalIdentifierTerm =  parensParser literalIndentifierExpression
        <|> (reservedParser "True"  >> return (BoolConstLI True ))
        <|> (reservedParser "False" >> return (BoolConstLI False))
        <|> VarLI <$> identifierParser
        <|> FloatConstLI <$> try floatParser
        <|> IntConstLI <$> integerParser

listParExpression :: Parser LiteralIdentifier
listParExpression = buildExpressionParser listOperators literalIdentifierTerm

listOperators = [ ]

-- listTerm =  parensParser listParExpression
--      <|> (reservedParser "True"  >> return (LBoolConst True ))
--      <|> (reservedParser "False" >> return (LBoolConst False))
--      <|> LVar <$> identifierParser
--      <|> LFloatConst <$> try floatParser
--      <|> LIntConst <$> integerParser

-- varExpr = try aExpression <|> try logicalExpression

parseListVar :: Parser ArithmeticExprNEW
parseListVar = do
    _ <- reservedOpParser "["
    vars <- listParExpression `sepBy` commaParser
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

writeFileOutput file =
  do haskellCode  <- readFile file
     case parse haskellParser "" haskellCode of
       Left e  -> print e >> fail "parse error"
       Right r -> writeFile "parsed.txt" $ show r
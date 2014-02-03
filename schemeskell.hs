module Main where
import System.Environment
import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.String


data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVariable String String
               | Default String

instance Show LispError where show = showError
instance Error LispError where
     noMsg = Default "An unknown error has occurred"
     strMsg = Default

type ThrowsError = Either LispError

showError :: LispError -> String
showError (UnboundVariable message name) = message ++ ": " ++ name
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (Parser parseErr) = "Parser error at: " ++ show parseErr
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected_num list_of_found) = "Expected "
                                               ++ show expected_num
                                               ++ " args but found these: "
                                               ++ unwordsList list_of_found
showError (TypeMismatch expected found) = "Invalid type; Expected: "
                                        ++ expected ++ ", but found: "
                                        ++ show found


trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val



data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (Atom name) = name
showVal (Number n) = show n
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h ++ "." ++ showVal t ++ ")"


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal


primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = do x <- mapM unpackNum params
                            (return . Number . foldl1 op) $ x

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                          if null parsed
                            then throwError $ TypeMismatch "number" $ String n
                            else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum


eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = do x <- mapM eval args
                                    apply func x
eval otherwiseBadForm = throwError $ BadSpecialForm
                      "Unrecognised special form" otherwiseBadForm


apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Uncrecognised func" func)
                        ($ args)
                        (lookup func primitives)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"


spaces :: Parser ()
spaces = skipMany1 space


parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x


parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first : rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    h <- endBy parseExpr spaces
    t <- char '.' >> spaces >> parseExpr
    return $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]


parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- (try parseList) <|> parseDottedList
               char ')'
               return x

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val



main :: IO ()
main = do
    args <- getArgs
    let arg = args !! 0
    evaled <- return $ liftM show $ readExpr arg >>= eval
    putStrLn $ extractValue $ trapError evaled

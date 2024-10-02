module Parser where 


import AST 
import Data.Void ( Void )
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Monad.Combinators.Expr 


-- Lexing/Parsing:
-------------------------------------------------------------------------------
type Parser = Parsec Void String

lineComment :: Parser ()
lineComment = L.skipLineComment "//"

blockComment :: Parser ()
blockComment = L.skipBlockComment "/**" "*/"

scn :: Parser ()
scn = L.space space1 lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme scn

symbol :: String -> Parser String
symbol = L.symbol scn

int :: Parser Int
int = lexeme L.decimal

reserved :: String -> Parser ()
reserved w = (lexeme . try) $ string w *> notFollowedBy alphaNumChar

reservedOp :: String -> Parser ()
reservedOp s = (lexeme . try) $ string s *> notFollowedBy (oneOf opChar)

opChar :: [Char]
opChar = ".,;+-*/&|<>~="

underscore :: Parser Char
underscore = lexeme $ char '_'

eq, semi, comma, colon, dot, pipe, hash :: Parser String
eq          = symbol "="
semi        = symbol ";"
comma       = symbol ","
colon       = symbol ":"
dot         = symbol "."
pipe        = symbol "|"
hash        = symbol "#"

parens, brackets, curly :: Parser a -> Parser a
parens   = between (symbol "(") (symbol ")")
brackets = between (symbol "[") (symbol "]")
curly    = between (symbol "{") (symbol "}")


identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
   where
      p       = (:) <$> (letterChar <|> underscore) <*> many (alphaNumChar <|> underscore)
      check x =
         if x `elem` reservedWords
         then fail $ "keyword " ++ show x ++ " cannot be an identifier."
         else return x

reservedWords :: [String]
reservedWords =
   [ "class", "constructor", "function", "method", "field", "static"
   , "var", "int", "char", "boolean", "void", "true", "false", "null"
   , "this", "let", "do", "if", "else", "while", "return"
   ]


-- Parsing Jack:
runParse :: String -> Either (ParseErrorBundle String Void) [Class]
runParse = parse (between scn eof parseClasses) ""

-- Parsing classes:
parseClasses :: Parser [Class]
parseClasses = some parseClass

parseClass :: Parser Class
parseClass = Class <$ reserved "class"
   <*> identifier
   <*> curly parseClassDecl
   <?> "class"

parseClassDecl :: Parser ClassDecl
parseClassDecl = ClassDecl <$> many parseVarDecl <*> many parseSubroutine

parseSubroutine :: Parser Subroutine 
parseSubroutine = (  Constructor <$ reserved "constructor" 
      <|> Function <$ reserved "function"
      <|> Method <$ reserved "method"
   ) 
   <*> parseType 
   <*> identifier 
   <*> parens parseParameterList 
   <*> curly (SBody <$> many parseVarDec <*> many parseStmt)
   <?> "subroutine"

parseVarDecl :: Parser VarDecl
parseVarDecl = Static <$ reserved "static" <*> parseType <*> parseVarNames <* semi
   <|> Field <$ reserved "field" <*> parseType <*> parseVarNames <* semi
   <?> "global variable declaration"

parseType :: Parser Type
parseType = TInt <$ reserved "int"
   <|> TChar <$ reserved "char"
   <|> TBool <$ reserved "boolean"
   <|> TVoid <$ reserved "void"
   <|> TClass <$> identifier
   <?> "type"

parseVarNames :: Parser [String]
parseVarNames = sepBy identifier comma
   <?> "identifier list"

parseParameterList :: Parser (Maybe ParameterList)
parseParameterList = try (Just . ParameterList <$> sepBy parseParameterType comma)
   <|> return Nothing
   <?> "parameter list"

parseParameterType :: Parser ParameterType
parseParameterType = ParameterType <$> parseType <*> identifier
   <?> "parameter type"

parseVarDec :: Parser VarDec 
parseVarDec = VarDec <$ reserved "var" <*> parseType <*> sepBy identifier comma <* semi

-- Parsing statements:
parseStmt :: Parser Stmt
parseStmt = Let <$ reserved "let" 
      <*> identifier 
      <*> optional (brackets parseExpr) 
      <* eq 
      <*> parseExpr 
      <* semi
   <|> If <$ reserved "if" 
      <*> parens parseExpr 
      <*> curly (many parseStmt) 
      <*> optional (reserved "else" *> curly (many parseStmt))
   <|> While <$ reserved "while" <*> parens parseExpr <*> curly (some parseStmt) 
   <|> Do <$ reserved "do" 
      <*> parseSubCall
      <* semi 
   <|> Return <$ reserved "return" <*> optional parseExpr <* semi
   <?> "statement"


-- Parsing expressions:
parseExprAtom :: Parser Expr
parseExprAtom = Term <$> parseTerm 
   <|> parens parseExpr

parseExpr :: Parser Expr 
parseExpr = makeExprParser parseExprAtom operators 
   <?> "expression"

operators :: [[Operator Parser Expr]]
operators = 
   [  [ Prefix (Un Neg <$ symbol "-")
      , Prefix (Un Not <$ symbol "~") ]

   ,  [ InfixL (Bin Mul <$ symbol "*")
      , InfixL (Bin Div <$ symbol "/") ]

   ,  [ InfixL (Bin Add <$ symbol "+")
      , InfixL (Bin Sub <$ symbol "-") ]

   ,  [ InfixL (Bin Lt <$ symbol "<") 
      , InfixL (Bin Gt <$ symbol ">")  ]

   ,  [ InfixL (Bin Eq <$ symbol "=") ]

   ,  [ InfixL (Bin And <$ symbol "&") ]

   ,  [ InfixL (Bin Or <$ symbol "|") ]

   ]

parseTerm :: Parser Term 
parseTerm = try (Literal <$> parseLiteral)
   <|> try (TermSubCall <$> parseSubCall)
   <|> try (TVar <$> identifier <*> optional (brackets parseExpr))
   <|> Expr <$> parens parseExpr
   <?> "term"

parseLiteral :: Parser Literal 
parseLiteral = Int <$> int 
   <|> String <$ char '\"' <*> manyTill L.charLiteral (char '\"')
   <|> Bool True <$ reserved "true"
   <|> Bool False <$ reserved "false"
   <|> Null <$ reserved "null"
   <|> This <$ reserved "this"
   <?> "term-literal"

parseSubCall :: Parser SubCall 
parseSubCall = SubCall <$> optional (try (identifier <* dot))
   <*> identifier
   <*> parens (sepBy parseExpr comma)
   <?> "subroutine call"

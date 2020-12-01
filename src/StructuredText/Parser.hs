{-# LANGUAGE OverloadedStrings #-}
module StructuredText.Parser
    ( top, Parser
    ) where

import Control.Monad.Combinators.Expr ( makeExprParser, Operator (..) )
import Data.Char ( isAscii, isAlpha, isAlphaNum, isDigit )
import Data.Text ( Text, singleton, pack )
import Data.Functor ( ($>) )
import Data.Void ( Void )
import Text.Megaparsec
      ( try, Parsec, takeWhileP, manyTill, anySingle
      , many, (<|>), (<?>), between, sepBy, satisfy
      , lookAhead, some, notFollowedBy
      )
import StructuredText.Syntax

import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

top :: Parser STxt
top = STxt <$> (space *> some global)

global :: Parser Global
global = try functionBlock
      <|> try function
      <|> try program
      <|> try typeDef
      <|> try globalVars

functionBlock :: Parser Global
functionBlock = do
      skipSymbol' "FUNCTION_BLOCK"
      n <- ident
      vs <- many functionBlockVarDecl
      sts <- many stmt
      skipSymbol' "END_FUNCTION_BLOCK"
      pure $ FunctionBlock n vs sts

function :: Parser Global
function = do
      skipSymbol' "FUNCTION"
      n <- ident
      t <- declType
      vs <- many functionVarDecl
      sts <- many stmt
      skipSymbol' "END_FUNCTION"
      pure $ Function n t vs sts

program :: Parser Global
program = do
      skipSymbol' "PROGRAM"
      n <- ident
      vs <- many programVarDecl
      sts <- many stmt
      skipSymbol' "END_PROGRAM"
      pure $ Program n vs sts

typeDef :: Parser Global
typeDef = symbol' "TYPE" *> manyTill anySingle (symbol' "END_TYPE") $> TypeDef ""

globalVars :: Parser Global
globalVars = GlobalVars <$> fileVarDecl

var :: Parser VarDecl
var = symbol' "VAR"
      *> (Var <$> qualifier <*> (concat <$> some (try typedNames)))
      <* symbol' "END_VAR"

varInput :: Parser VarDecl
varInput = symbol' "VAR_INPUT"
      *> (VarInput <$> qualifier <*> (concat <$> some (try typedNames)))
      <* symbol' "END_VAR"

varOutput :: Parser VarDecl
varOutput = symbol' "VAR_OUTPUT"
      *> (VarOutput <$> qualifier <*> (concat <$> some (try typedNames)))
      <* symbol' "END_VAR"

varInOut :: Parser VarDecl
varInOut = symbol' "VAR_IN_OUT"
      *> (VarInOut <$> qualifier <*> (concat <$> some (try typedNames)))
      <* symbol' "END_VAR"

varExternal :: Parser VarDecl
varExternal = symbol' "VAR_EXTERNAL"
      *> (VarExternal <$> qualifier <*> (concat <$> some (try typedNames)))
      <* symbol' "END_VAR"

varGlobal :: Parser VarDecl
varGlobal = symbol' "VAR_GLOBAL"
      *> (VarGlobal <$> qualifier <*> (concat <$> some (try typedNames)))
      <* symbol' "END_VAR"

varAccess :: Parser VarDecl
varAccess = symbol' "VAR_ACCESS"
      *> (VarAccess <$> qualifier <*> (concat <$> some (try typedNames)))
      <* symbol' "END_VAR"

fileVarDecl :: Parser VarDecl
fileVarDecl = try varGlobal <|> try varAccess

functionVarDecl :: Parser VarDecl
functionVarDecl = try varInput  <|> try var

functionBlockVarDecl :: Parser VarDecl
functionBlockVarDecl = try varOutput <|> try varInOut <|> try varExternal
      <|> try functionVarDecl

programVarDecl :: Parser VarDecl
programVarDecl = try fileVarDecl <|> try functionBlockVarDecl

typedNames :: Parser [TypedName]
typedNames = try noLocNames
      <|> try (pure <$> singleLocName)
      <|> try (pure <$> typedLocation)

noLocNames :: Parser [TypedName]
noLocNames = do
      xs <- commas ident
      t <- declType
      i <- maybeInit
      semi
      pure $ map (\ x -> TypedName x Nothing t i) xs

singleLocName :: Parser TypedName
singleLocName = do
      x <- ident
      symbol' "AT" $> ()
      loc <- location
      t <- declType
      i <- maybeInit
      semi
      pure $ TypedName x (Just loc) t i

typedLocation :: Parser TypedName
typedLocation = do
      symbol' "AT" $> ()
      loc <- location
      t <- declType
      i <- maybeInit
      semi
      pure $ TypedLocation loc t i

maybeInit :: Parser (Maybe Init)
maybeInit = try (Just <$> initializer) <|> pure Nothing

initializer :: Parser Init
initializer = try simpleInit <|> try compoundInit

simpleInit :: Parser Init
simpleInit = colonEq *> (SimpleInit <$> lit)

compoundInit :: Parser Init
compoundInit = colonEq *> (CompoundInit <$> parens (commas fieldInit))

fieldInit :: Parser FieldInit
fieldInit = FieldInit <$> ident <*> initializer

qualifier :: Parser Qualifier
qualifier = try (symbol' "RETAIN" $> Retain)
      <|> try (symbol' "NON_RETAIN" $> NonRetain)
      <|> try (symbol' "CONSTANT" $> Constant)
      <|> pure None

-- e.g., " : INT"
declType :: Parser Type
declType = symbol ":" *> typ

stmt :: Parser Stmt
stmt = try assignStmt <|> try invokeStmt
   <|> try returnStmt <|> try ifStmt
   <|> try caseStmt   <|> try forStmt
   <|> try whileStmt  <|> try repeatStmt
   <|> try exitStmt   <|> try emptyStmt

assignStmt :: Parser Stmt
assignStmt = Assign <$> lval <*> (colonEq *> expr <* semi)

invokeStmt :: Parser Stmt
invokeStmt = Invoke <$> ident <*> parens (commas arg) <* semi

-- TODO(chathhorn): suspect either all arguments must be named or none.
arg :: Parser Arg
arg = try (ArgOutNeg <$> (symbol' "NOT" *> ident <* symbol "=>") <*> ident)
      <|> try (ArgOut <$> (ident <* symbol "=>") <*> ident)
      <|> try (ArgIn <$> ident <*> (colonEq *> expr))
      <|> try (Arg <$> expr)

returnStmt :: Parser Stmt
returnStmt = symbol' "RETURN" *> semi $> Return

elsif :: Parser Elsif
elsif = Elsif
      <$> (symbol' "ELSIF" *> expr)
      <*> (symbol' "THEN" *> many stmt)
      <?> "ELSIF block"

ifStmt :: Parser Stmt
ifStmt = If
      <$> (symbol' "IF" *> expr)
      <*> (symbol' "THEN" *> many stmt)
      <*> many (try elsif)
      <*> ( ([] <$ lookAhead (symbol' "END_IF"))
            <|> (symbol' "ELSE" *> many stmt))
      <* symbol' "END_IF"
      <* semi
      <?> "IF statement"

-- TODO(chathhorn): comma-seperated label lists
labeled :: Parser Labeled
labeled = try (LabelRange <$> expr <*> (symbol ".." *> expr) <*> (symbol ":" *> many stmt))
      <|> try (Label <$> expr <*> (symbol ":" *> many stmt))

caseStmt :: Parser Stmt
caseStmt = Case
      <$> (symbol' "CASE" *> expr <* symbol' "OF")
      <*> many labeled
      <*> (try (symbol' "ELSE" *> many stmt) <|> pure [])
      <* symbol' "END_CASE"
      <* semi
      <?> "CASE statement"

forStmt :: Parser Stmt
forStmt = For
      <$> (symbol' "FOR" *> ident)
      <*> (colonEq *> expr)
      <*> (symbol' "TO" *> expr)
      <*> (try (Just <$> (symbol' "BY" *> expr)) <|> pure Nothing)
      <*> (symbol' "DO" *> many stmt)
      <* symbol' "END_FOR"
      <* semi
      <?> "FOR statement"

whileStmt :: Parser Stmt
whileStmt = While
      <$> (symbol' "WHILE" *> expr)
      <*> (symbol' "DO" *> many stmt)
      <* symbol' "END_WHILE"
      <* semi
      <?> "WHILE statement"

repeatStmt :: Parser Stmt
repeatStmt = Repeat
      <$> (symbol' "REPEAT" *> many stmt)
      <*> (symbol' "UNTIL" *> expr)
      <* symbol' "END_REPEAT"
      <* semi
      <?> "REPEAT statement"

exitStmt :: Parser Stmt
exitStmt = symbol' "EXIT" *> semi $> Exit <?> "EXIT statement"

emptyStmt :: Parser Stmt
emptyStmt = symbol ";" $> Empty <?> "empty statement"

lval :: Parser LVal
lval = try (Index <$> ident <*> (symbol "[" *> expr <* symbol "]"))
      <|> try qualIdent
      <|> try (Id <$> ident)

expr :: Parser Expr
expr = makeExprParser term opTable <?> "expression"

lit :: Parser Lit
lit = try (Float <$> float)
      <|> try (Int <$> integer)
      <|> try (Bool <$> bool)
      <|> try duration
      <|> try (WString <$> wstring)
      <|> try (String <$> string)

-- TODO(chathhorn): escape chars.
wstring :: Parser Text
wstring = pack <$> (symbol "\"" *> manyTill anySingle (symbol "\""))

string :: Parser Text
string = pack <$> (symbol "\'" *> manyTill anySingle (symbol "\'"))

duration :: Parser Lit
duration =  (symbol' "TIME" <|> symbol' "T") *> hash *> (Duration
      <$> (try (decimal <* symbol' "D")  <|> pure 0)
      <*> (try (decimal <* symbol' "H")  <|> pure 0)
      <*> (try (decimal <* symbol' "M")  <|> pure 0)
      <*> (try (decimal <* symbol' "S")  <|> pure 0)
      <*> (try (decimal <* symbol' "MS") <|> pure 0))

term :: Parser Expr
term = try (parens expr)
      <|> try (Call <$> ident <*> parens (commas arg))
      <|> try (Lit <$> lit)
      <|> try (LV <$> lval)
      <?> "term"

opTable :: [[Operator Parser Expr]]
opTable =
      [ [ prefix "-"   Negate
        , prefix "NOT" Not
        , prefix "@"   AddrOf
        ]
      , [ binop "**"  $ BinOp Exp ]
      , [ binop "*"   $ BinOp Mult
        , binop "/"   $ BinOp Div
        , binop "MOD" $ BinOp Mod
        ]
      , [ binop "+"   $ BinOp Plus
        , binop "-"   $ BinOp Minus
        ]
      , [ binop "<="  $ BinOp Lte
        , binop ">="  $ BinOp Gte
        , binop' "<"   $ BinOp Lt
        , binop ">"   $ BinOp Gt
        ]
      , [ binop "="   $ BinOp Eq
        , binop "<>"  $ BinOp Neq
        ]
      , [ binop "&"   $ BinOp And
        , binop "AND" $ BinOp And
        ]
      , [ binop "XOR" $ BinOp Xor ]
      , [ binop "OR"  $ BinOp Or ]
      ]

binop :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binop name f = InfixL  (f <$ symbol' name)

binop' :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binop' name f = InfixL  (f <$ try (symbol' name *> notFollowedBy (symbol ">")))

prefix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix  (f <$ symbol' name)

qualIdent :: Parser LVal
qualIdent = QualId <$> ident <*> (symbol "." *> ident)

notIdent' :: Text -> Parser ()
notIdent' s = symbol' s *> notFollowedBy ident

typ :: Parser Type
typ = try ( (symbol' "BOOL"  *> symbol' "R_EDGE") $> TBoolREdge )
  <|> try ( (symbol' "BOOL" *> symbol' "F_EDGE")  $> TBoolFEdge )
  <|> try ( notIdent' "BOOL"   $> TBool )
  <|> try ( notIdent' "REAL"   $> TReal  ) <|> try ( notIdent' "LREAL" $> TLReal )
  <|> try ( notIdent' "INT"    $> TInt   ) <|> try ( notIdent' "UINT"  $> TUInt  )
  <|> try ( notIdent' "SINT"   $> TSInt  ) <|> try ( notIdent' "USINT" $> TUSInt )
  <|> try ( notIdent' "DINT"   $> TDInt  ) <|> try ( notIdent' "UDINT" $> TUDInt )
  <|> try ( notIdent' "LINT"   $> TLInt  ) <|> try ( notIdent' "ULINT" $> TULInt )
  <|> try ( notIdent' "BYTE"   $> TByte  ) <|> try ( notIdent' "WORD"  $> TWord  )
  <|> try ( notIdent' "DWORD"  $> TDWord ) <|> try ( notIdent' "LWORD" $> TLWord )
  <|> try ( notIdent' "TIME"   $> TTime  ) <|> try ( notIdent' "DATE"  $> TDate  )
  <|> try ( notIdent' "STRING" $> TTime  ) <|> try ( notIdent' "WSTRING"  $> TDate  )
  <|> try ( ( notIdent' "TIME_OF_DAY"      <|>   notIdent' "TOD")  $> TTimeOfDay )
  <|> try ( ( notIdent' "DATE_AND_TYPE"    <|>   notIdent' "DT" )  $> TDateTime  )
  <|> ( TId <$> ident )

-----------
-- Lexer --
-----------

skipLineComment :: Parser ()
skipLineComment = L.skipLineComment "//"

skipBlockComment :: Parser ()
skipBlockComment = try (L.skipBlockComment "(*" "*)") <|> try (L.skipBlockComment "/*" "*/")

space :: Parser ()
space = L.space C.space1 skipLineComment skipBlockComment

symbol :: Text -> Parser Text
symbol = L.symbol space

symbol' :: Text -> Parser Text
symbol' = L.symbol' space

skipSymbol' :: Text -> Parser ()
skipSymbol' s = symbol' s $> ()

semi :: Parser ()
semi = symbol ";" $> ()

colonEq :: Parser ()
colonEq = symbol ":=" $> ()

hash :: Parser ()
hash = symbol "#" $> ()

sixteen :: Parser ()
sixteen = symbol "16" $> ()

eight :: Parser ()
eight = symbol "8" $> ()

two :: Parser ()
two = symbol "2" $> ()

one :: Parser ()
one = symbol "1" $> ()

zero :: Parser ()
zero = symbol "0" $> ()

parens :: Parser a -> Parser a
parens = between (symbol "(") $ symbol ")"

commas :: Parser a -> Parser [a]
commas p = p `sepBy` symbol ","

-- TODO(chathhorn): underscores are allowed in numeric literals.
integer :: Parser Int
integer = try hexadecimal <|> try binary <|> try octal <|> try decimal

binary :: Parser Int
binary = two *> hash *> (L.signed space $ L.lexeme space L.binary)

octal :: Parser Int
octal = eight *> hash *> (L.signed space $ L.lexeme space L.octal)

decimal :: Parser Int
decimal = L.signed space $ L.lexeme space L.decimal

hexadecimal :: Parser Int
hexadecimal = sixteen *> hash *> (L.signed space $ L.lexeme space L.hexadecimal)

float :: Parser Double
float = L.signed space $ L.lexeme space L.float

true :: Parser Bool
true = True <$
      ( symbol' "TRUE"
      <|> (symbol' "BOOL" <* hash <* symbol' "TRUE")
      <|> (symbol' "BOOL" <* hash <* one)
      )

false :: Parser Bool
false = False <$
      ( symbol' "FALSE"
      <|> (symbol' "BOOL" <* hash <* symbol' "FALSE")
      <|> (symbol' "BOOL" <* hash <* zero)
      )

bool :: Parser Bool
bool = try true <|> try false

-- Note: identifiers aren't supposed to end with an underscore.
ident :: Parser Text
ident = do
      x <- satisfy isInitIdChar
      xs <- takeWhileP (Just "legal identifier character") isIdChar
      space
      pure $ singleton x <> xs

location :: Parser Location
location =
      (   try (InputLoc  <$> (symbol "%I" *> locAddr <* space))
      <|> try (OutputLoc <$> (symbol "%Q" *> locAddr <* space))
      <|> try (MemoryLoc <$> (symbol "%M" *> locAddr <* space))
      )

locAddr :: Parser Text
locAddr = takeWhileP (Just "legal physical or logical variable location address character") isLocAddrChar

isLocAddrChar :: Char -> Bool
isLocAddrChar c = c `elem` ['X', 'B', 'W', 'D', 'L', '*', '.'] || isDigit c

isIdChar :: Char -> Bool
isIdChar c = c == '_' || isAscii c && isAlphaNum c

isInitIdChar :: Char -> Bool
isInitIdChar c = isAscii c && isAlpha c


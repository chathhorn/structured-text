{-# LANGUAGE OverloadedStrings #-}
module StructuredText.Parser
    ( top, Parser
    ) where

import Control.Monad.Combinators.Expr ( makeExprParser, Operator (..) )
import Data.Char ( isAscii, isAlpha, isAlphaNum )
import Data.Text ( Text, singleton )
import Data.Functor ( ($>) )
import Data.Void ( Void )
import Text.Megaparsec
      ( try, Parsec, takeWhileP, manyTill, anySingle
      , many, (<|>), (<?>), between, sepBy, satisfy
      )
import StructuredText.Syntax

import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

top :: Parser STxt
top = STxt <$> (space *> many global)

-- Note: identifiers aren't supposed to end with an underscore.
ident :: Parser Text
ident = do
      x <- satisfy isInitIdChar
      xs <- takeWhileP (Just "legal identifier character") isIdChar
      space
      pure $ singleton x <> xs

isIdChar :: Char -> Bool
isIdChar c = c == '_' || isAscii c && isAlphaNum c

isInitIdChar :: Char -> Bool
isInitIdChar c = isAscii c && isAlpha c

global :: Parser Global
global = try functionBlock
      <|> try function
      <|> try program
      <|> try typeDef

functionBlock :: Parser Global
functionBlock = do
      skipSymbol' "FUNCTION_BLOCK"
      n <- ident
      vs <- many $ try varDecl
      sts <- many stmt
      skipSymbol' "END_FUNCTION_BLOCK"
      pure $ FunctionBlock n vs sts

function :: Parser Global
function = do
      skipSymbol' "FUNCTION"
      n <- ident
      t <- parseDeclType
      vs <- many $ try varDecl
      sts <- many stmt
      skipSymbol' "END_FUNCTION"
      pure $ Function n t vs sts

program :: Parser Global
program = do
      skipSymbol' "PROGRAM"
      n <- ident
      vs <- many $ try varDecl
      sts <- many stmt
      skipSymbol' "END_PROGRAM"
      pure $ Program n vs sts

typeDef :: Parser Global
typeDef = symbol' "TYPE" *> manyTill anySingle (symbol' "END_TYPE") $> TypeDef ""

varDecl :: Parser VarDecl
varDecl = (   symbol' "VAR_INPUT"    $> VarInput -- Order matters.
          <|> symbol' "VAR_IN_OUT"   $> VarInOut
          <|> symbol' "VAR_OUTPUT"   $> VarOutput
          -- Not sure if "VAR_OUT" allowed or not.
          <|> symbol' "VAR_OUT"      $> VarOutput
          <|> symbol' "VAR_EXTERNAL" $> VarExternal
          <|> symbol' "VAR"          $> Var)
      <* manyTill anySingle (symbol' "END_VAR")

-- e.g., " : INT"
parseDeclType :: Parser Type
parseDeclType = symbol ":" *> typ

stmt :: Parser Stmt
stmt = try assignStmt <|> try invokeStmt
   <|> try returnStmt <|> try ifStmt
   <|> try caseStmt   <|> try forStmt
   <|> try whileStmt  <|> try repeatStmt
   <|> try exitStmt   <|> try emptyStmt

assignStmt :: Parser Stmt
assignStmt = Assign <$> ident <*> (colonEq *> expr <* semi)

-- TODO(chathhorn): (also other stmts)
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

ifStmt :: Parser Stmt
ifStmt = symbol' "IF"
      *> manyTill anySingle (symbol' "END_IF")
      *> semi
      $> If
      <?> "IF statement"

caseStmt :: Parser Stmt
caseStmt = symbol' "CASE"
      *> manyTill anySingle (symbol' "OF")
      *> manyTill anySingle (symbol' "END_CASE")
      *> semi
      $> Case
      <?> "CASE statement"

forStmt :: Parser Stmt
forStmt = symbol' "FOR"
      *> manyTill anySingle (symbol' "DO")
      *> manyTill anySingle (symbol' "END_FOR")
      *> semi
      $> For
      <?> "FOR statement"

whileStmt :: Parser Stmt
whileStmt = symbol' "WHILE"
      *> manyTill anySingle (symbol' "DO")
      *> manyTill anySingle (symbol' "END_WHILE")
      *> semi
      $> While
      <?> "WHILE statement"

repeatStmt :: Parser Stmt
repeatStmt = symbol' "REPEAT"
      *> manyTill anySingle (symbol' "END_REPEAT")
      *> semi
      $> Repeat
      <?> "REPEAT statement"

exitStmt :: Parser Stmt
exitStmt = symbol' "EXIT" *> semi $> Exit <?> "EXIT statement"

emptyStmt :: Parser Stmt
emptyStmt = symbol ";" $> Empty <?> "empty statement"

expr :: Parser Expr
expr = makeExprParser term opTable <?> "expression"

term :: Parser Expr
term = try (parens expr)
      <|> try (Call <$> ident <*> parens (commas arg))
      <|> try qualIdent
      <|> try (Id <$> ident)
      <|> try (IntLit <$> integer)
      <?> "term"

opTable :: [[Operator Parser Expr]]
opTable =
      [ [ Prefix $ symbol "-"    $> Negate
        , Prefix $ symbol' "NOT" $> Not
        ]
      , [ InfixL $ symbol "**"   $> BinOp Exp ]
      , [ InfixL $ symbol "*"    $> BinOp Mult
        , InfixL $ symbol "/"    $> BinOp Div
        , InfixL $ symbol' "MOD" $> BinOp Mod
        ]
      , [ InfixL $ symbol "+"    $> BinOp Plus
        , InfixL $ symbol "-"    $> BinOp Minus
        ]
      , [ InfixL $ symbol "<"    $> BinOp Lt
        , InfixL $ symbol ">"    $> BinOp Gt
        , InfixL $ symbol "<="   $> BinOp Lte
        , InfixL $ symbol ">="   $> BinOp Gte
        ]
      , [ InfixL $ symbol "="    $> BinOp Eq
        , InfixL $ symbol "<>"   $> BinOp Neq
        ]
      , [ InfixL $ symbol "&"    $> BinOp And
        , InfixL $ symbol' "AND" $> BinOp And
        ]
      , [ InfixL $ symbol' "XOR" $> BinOp Xor ]
      , [ InfixL $ symbol' "OR"  $> BinOp Or ]
      ]

qualIdent :: Parser Expr
qualIdent = QualId <$> ident <*> (symbol "." *> ident)

typ :: Parser Type
typ = ( symbol' "BOOL"  $> TBool  )
        <|> ( symbol' "REAL"  $> TReal  ) <|> ( symbol' "LREAL" $> TLReal )
        <|> ( symbol' "INT"   $> TInt   ) <|> ( symbol' "UINT"  $> TUInt  )
        <|> ( symbol' "SINT"  $> TSInt  ) <|> ( symbol' "USINT" $> TUSInt )
        <|> ( symbol' "DINT"  $> TDInt  ) <|> ( symbol' "UDINT" $> TUDInt )
        <|> ( symbol' "LINT"  $> TLInt  ) <|> ( symbol' "ULINT" $> TULInt )
        <|> ( symbol' "BYTE"  $> TByte  ) <|> ( symbol' "WORD"  $> TWord  )
        <|> ( symbol' "DWORD" $> TDWord ) <|> ( symbol' "LWORD" $> TLWord )
        <|> ( symbol' "TIME"  $> TTime  ) <|> ( symbol' "DATE"  $> TDate  )
        <|> ( ( symbol' "TIME_OF_DAY"     <|>   symbol' "TOD")  $> TTimeOfDay )
        <|> ( ( symbol' "DATE_AND_TYPE"   <|>   symbol' "DT" )  $> TDateTime  )

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

parens :: Parser a -> Parser a
parens = between (symbol "(") $ symbol ")"

commas :: Parser a -> Parser [a]
commas p = p `sepBy` symbol ","

integer :: Parser Int
integer = L.signed space $ L.lexeme space L.decimal

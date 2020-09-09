{-# LANGUAGE OverloadedStrings #-}
module StructuredText.Parser
    ( parseTop, Parser
    ) where

import Data.Char (isAscii, isAlphaNum)
import Data.Text (Text)
import Data.Functor (($>))
import Data.Void (Void)
import Text.Megaparsec
      ( try, Token, Parsec, takeWhile1P, manyTill, skipSome, anySingle, many, (<|>))
import Text.Megaparsec.Char (char, string, string', space1, eol)
import StructuredText.Syntax

type Parser = Parsec Void Text

parseTop :: Parser STxt
parseTop = STxt <$> many parseGlobal

-- Note: identifiers aren't supposed to end with an underscore.
parseId :: Parser Text
parseId = interspace *> (takeWhile1P (Just "legal identifier character") $ isIdChar)

isIdChar :: Token Text -> Bool
isIdChar c   = c == '_' || isAscii c && isAlphaNum c

parseGlobal :: Parser Global
parseGlobal = try parseFunctionBlock
          <|> try parseFunction
          <|> try parseProgram
          <|> try parseTypeDef

parseFunctionBlock :: Parser Global
parseFunctionBlock = do
      interspace
      skipStr "FUNCTION_BLOCK"
      n <- parseId
      vs <- many $ try parseVarDecl
      sts <- many parseStmt
      interspace *> skipStr "END_FUNCTION_BLOCK" $> ()
      pure $ FunctionBlock n vs sts

parseFunction :: Parser Global
parseFunction = do
      interspace
      skipStr "FUNCTION"
      n <- parseId
      t <- parseDeclType
      vs <- many $ try parseVarDecl
      sts <- many parseStmt
      interspace *> skipStr "END_FUNCTION" $> ()
      pure $ Function n t vs sts

parseProgram :: Parser Global
parseProgram = do
      interspace
      skipStr "PROGRAM"
      n <- parseId
      vs <- many $ try parseVarDecl
      sts <- many parseStmt
      interspace *>  skipStr "END_PROGRAM" $> ()
      pure $ Program n vs sts

parseTypeDef :: Parser Global
parseTypeDef = do
      interspace
      skipStr "TYPE"
      interspace
      manyTill anySingle (skipStr "END_TYPE") $> ()
      pure $ TypeDef ""

parseVarDecl :: Parser VarDecl
parseVarDecl = interspace
      -- Order matters.
      *>    (   string' "VAR_INPUT"    $> VarInput
            <|> string' "VAR_IN_OUT"   $> VarInOut
            <|> string' "VAR_OUTPUT"   $> VarOutput
            -- Not sure if "VAR_OUT" allowed or not.
            <|> string' "VAR_OUT"      $> VarOutput
            <|> string' "VAR_EXTERNAL" $> VarExternal
            <|> string' "VAR"          $> Var)
      <* manyTill anySingle (skipStr "END_VAR")

-- e.g., " : INT"
parseDeclType :: Parser Type
parseDeclType = interspace *> char ':' *> interspace *> parseType

parseStmt :: Parser Stmt
parseStmt = try parseAssign <|> try parseCall
        <|> try parseReturn <|> try parseIf
        <|> try parseCase   <|> try parseFor
        <|> try parseWhile  <|> try parseRepeat
        <|> try parseExit   <|> try parseEmpty

parseAssign :: Parser Stmt
parseAssign = parseId
      *> colonEq
      *> parseExp
      *> semi
      $> Assign

-- TODO(chathhorn) (also other stmts)
parseCall :: Parser Stmt
parseCall = parseId
      -- TODO(chathhorn): space?
      *> interspace
      *> ( try (char '(' *> interspace *> char ')') -- c()
            <|> try (char '(' *> parseInit *> char ')') -- c(a:=b)
            <|> try (char '(' *> parseInit *> many (interspace *> char ',' *> parseInit)  *> interspace *> char ')')) -- c(a:=b, x:=y, ...)
      *> semi
      $> Call

-- TODO(chathhorn)
parseInit :: Parser ()
parseInit = parseId *> colonEq *> parseExp $> ()


parseReturn :: Parser Stmt
parseReturn = interspace
      *> skipStr "RETURN"
      *> semi
      $> Return

parseIf :: Parser Stmt
parseIf = interspace
      *> skipStr "IF"
      *> manyTill anySingle (skipStr "END_IF")
      *> semi
      $> If

parseCase :: Parser Stmt
parseCase = interspace
      *> skipStr "CASE"
      *> manyTill anySingle (skipStr "OF")
      *> manyTill anySingle (skipStr "END_CASE")
      *> semi
      $> Case

parseFor :: Parser Stmt
parseFor = interspace
      *> skipStr "FOR"
      *> manyTill anySingle (skipStr "DO")
      *> manyTill anySingle (skipStr "END_FOR")
      *> semi
      $> For

parseWhile :: Parser Stmt
parseWhile = interspace
      *> skipStr "WHILE"
      *> manyTill anySingle (skipStr "DO")
      *> manyTill anySingle (skipStr "END_WHILE")
      *> semi
      $> While

parseRepeat :: Parser Stmt
parseRepeat = interspace
      *> skipStr "REPEAT"
      *> manyTill anySingle (skipStr "END_REPEAT")
      *> semi
      $> Repeat

parseExit :: Parser Stmt
parseExit = interspace
      *> skipStr "EXIT"
      *> semi
      $> Exit

parseEmpty :: Parser Stmt
parseEmpty = interspace *> char ';' $> Empty

-- TODO(chathhorn): order matters!
parseExp :: Parser Exp
parseExp = try parseQualId <|> (Id <$> parseId)

parseQualId :: Parser Exp
parseQualId = do
      x <- parseId
      char '.' $> ()
      y <- parseId
      pure $ QualId x y

semi :: Parser ()
semi = interspace *> char ';' $> ()

interspace :: Parser ()
interspace = skipSome
      (   try space1
      <|> try lineComment
      <|> try inlineComment
      <|> try inlineCommentC
      )

colonEq :: Parser ()
colonEq = interspace *> string ":=" $> ()

skipStr :: Text -> Parser ()
skipStr s = string' s $> ()

lineComment :: Parser ()
lineComment = string "//" *> manyTill anySingle eol $> ()

inlineComment :: Parser ()
inlineComment = string "(*" *> manyTill anySingle (string "*)") $> ()

inlineCommentC :: Parser ()
inlineCommentC = string "/*" *> manyTill anySingle (string "*\\") $> ()

parseType :: Parser Type
parseType = ( string' "BOOL"  $> TBool  )
        <|> ( string' "REAL"  $> TReal  ) <|> ( string' "LREAL" $> TLReal )
        <|> ( string' "INT"   $> TInt   ) <|> ( string' "UINT"  $> TUInt  )
        <|> ( string' "SINT"  $> TSInt  ) <|> ( string' "USINT" $> TUSInt )
        <|> ( string' "DINT"  $> TDInt  ) <|> ( string' "UDINT" $> TUDInt )
        <|> ( string' "LINT"  $> TLInt  ) <|> ( string' "ULINT" $> TULInt )
        <|> ( string' "BYTE"  $> TByte  ) <|> ( string' "WORD"  $> TWord  )
        <|> ( string' "DWORD" $> TDWord ) <|> ( string' "LWORD" $> TLWord )
        <|> ( string' "TIME"  $> TTime  ) <|> ( string' "DATE"  $> TDate  )
        <|> ( ( string' "TIME_OF_DAY"     <|>   string' "TOD")  $> TTimeOfDay )
        <|> ( ( string' "DATE_AND_TYPE"   <|>   string' "DT" )  $> TDateTime  )


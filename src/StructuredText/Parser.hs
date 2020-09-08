{-# LANGUAGE OverloadedStrings #-}
module StructuredText.Parser
    ( parse, parseTop
    ) where

import Prelude hiding (takeWhile)
import Control.Applicative ((<|>), many)
import Data.Text (Text)
import Data.Functor (($>))
import Data.Attoparsec.Text
      ( Parser, string, parseOnly, manyTill, takeTill, isEndOfLine, endOfLine
      , anyChar, skipSpace, asciiCI, inClass, takeWhile, char, skipMany, skip, space, satisfy)
import StructuredText.Syntax

parse :: Text -> Either String STxt
parse = parseOnly parseTop

parseTop :: Parser STxt
parseTop = STxt <$> many parseGlobal

-- Note: identifiers aren't supposed to end with an underscore.
parseId :: Parser Text
parseId = interspace *> (takeWhile $ inClass "a-zA-Z0-9_")

parseGlobal :: Parser Global
parseGlobal = parseFunctionBlock
          <|> parseFunction
          <|> parseProgram
          <|> parseTypeDef

parseFunctionBlock :: Parser Global
parseFunctionBlock = do
      interspace *> skipStr "FUNCTION_BLOCK"
      n <- parseId
      vs <- many parseVarDecl
      sts <- many parseStmt
      interspace *> skipStr "END_FUNCTION_BLOCK"
      pure $ FunctionBlock n vs sts

parseFunction :: Parser Global
parseFunction = do
      interspace *> skipStr "FUNCTION"
      n <- parseId
      t <- parseDeclType
      vs <- many parseVarDecl
      sts <- many parseStmt
      interspace *> skipStr "END_FUNCTION"
      pure $ Function n t vs sts

parseProgram :: Parser Global
parseProgram = do
      interspace
      skipStr "PROGRAM"
      n <- parseId
      vs <- many parseVarDecl
      sts <- many parseStmt
      interspace *>  skipStr "END_PROGRAM"
      pure $ Program n vs sts

parseTypeDef :: Parser Global
parseTypeDef = do
      interspace
      skipStr "TYPE"
      interspace
      manyTill anyChar $ skipStr "END_TYPE"
      pure $ TypeDef ""

parseVarDecl :: Parser VarDecl
parseVarDecl = interspace
      -- Order matters.
      *> (      asciiCI "VAR_INPUT"    $> VarInput
            <|> asciiCI "VAR_IN_OUT"   $> VarInOut
            <|> asciiCI "VAR_OUTPUT"   $> VarOutput
            -- Not sure if "VAR_OUT" allowed or not.
            <|> asciiCI "VAR_OUT"      $> VarOutput
            <|> asciiCI "VAR_EXTERNAL" $> VarExternal
            <|> asciiCI "VAR"          $> Var)
      <* manyTill anyChar (skipStr "END_VAR")

-- e.g., " : INT"
parseDeclType :: Parser Type
parseDeclType = interspace *> char ':' *> interspace *> parseType

parseStmt :: Parser Stmt
parseStmt = parseAssign <|> parseCall
      <|> parseReturn <|> parseIf
      <|> parseCase <|> parseFor
      <|> parseWhile <|> parseRepeat
      <|> parseExit <|> parseEmpty

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
      *> ( (char '(' *> interspace *> char ')') -- c()
            <|> (char '(' *> parseInit *> char ')') -- c(a:=b)
            <|> (char '(' *> parseInit *> many (interspace *> char ',' *> parseInit)  *> interspace *> char ')')) -- c(a:=b, x:=y, ...)
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
      *> manyTill anyChar (skipStr "END_IF")
      *> semi
      $> If

parseCase :: Parser Stmt
parseCase = interspace
      *> skipStr "CASE"
      *> manyTill anyChar (skipStr "OF")
      *> manyTill anyChar (skipStr "END_CASE")
      *> semi
      $> Case

parseFor :: Parser Stmt
parseFor = interspace
      *> skipStr "FOR"
      *> manyTill anyChar (skipStr "DO")
      *> manyTill anyChar (skipStr "END_FOR")
      *> semi
      $> For

parseWhile :: Parser Stmt
parseWhile = interspace
      *> skipStr "WHILE"
      *> manyTill anyChar (skipStr "DO")
      *> manyTill anyChar (skipStr "END_WHILE")
      *> semi
      $> While

parseRepeat :: Parser Stmt
parseRepeat = interspace
      *> skipStr "REPEAT"
      *> manyTill anyChar (skipStr "END_REPEAT")
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
parseExp = parseQualId <|> (Id <$> parseId)

parseQualId :: Parser Exp
parseQualId = do
      x <- parseId
      char '.' $> ()
      y <- parseId
      pure $ QualId x y

semi :: Parser ()
semi = interspace *> char ';' $> ()

interspace :: Parser ()
interspace = skipMany
      (   ( space $> () )
      <|> lineComment
      <|> inlineComment
      <|> inlineCommentC
      )

colonEq :: Parser ()
colonEq = interspace *> string ":=" $> ()

skipStr :: Text -> Parser ()
skipStr s = asciiCI s $> ()

lineComment :: Parser ()
lineComment = string "//" *> (skipMany $ satisfy $ not . isEndOfLine)

inlineComment :: Parser ()
inlineComment = string "(*" *> manyTill anyChar (string "*)") $> ()

inlineCommentC :: Parser ()
inlineCommentC = string "/*" *> manyTill anyChar (string "*\\") $> ()

parseType :: Parser Type
parseType = ( asciiCI "BOOL"  $> TBool  )
        <|> ( asciiCI "REAL"  $> TReal  ) <|> ( asciiCI "LREAL" $> TLReal )
        <|> ( asciiCI "INT"   $> TInt   ) <|> ( asciiCI "UINT"  $> TUInt  )
        <|> ( asciiCI "SINT"  $> TSInt  ) <|> ( asciiCI "USINT" $> TUSInt )
        <|> ( asciiCI "DINT"  $> TDInt  ) <|> ( asciiCI "UDINT" $> TUDInt )
        <|> ( asciiCI "LINT"  $> TLInt  ) <|> ( asciiCI "ULINT" $> TULInt )
        <|> ( asciiCI "BYTE"  $> TByte  ) <|> ( asciiCI "WORD"  $> TWord  )
        <|> ( asciiCI "DWORD" $> TDWord ) <|> ( asciiCI "LWORD" $> TLWord )
        <|> ( asciiCI "TIME"  $> TTime  ) <|> ( asciiCI "DATE"  $> TDate  )
        <|> ( ( asciiCI "TIME_OF_DAY"     <|>   asciiCI "TOD")  $> TTimeOfDay )
        <|> ( ( asciiCI "DATE_AND_TYPE"   <|>   asciiCI "DT" )  $> TDateTime  )


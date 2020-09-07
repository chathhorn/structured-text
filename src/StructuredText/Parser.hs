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
      , anyChar, skipSpace, asciiCI, inClass, takeWhile, char, skipMany, skip, space)
import StructuredText.Syntax

parse :: Text -> Either String STxt
parse = parseOnly parseTop

parseTop :: Parser STxt
parseTop = STxt <$> many parseGlobal

parseId :: Parser Text
parseId = takeWhile $ inClass "a-zA-Z0-9_"

parseGlobal :: Parser Global
parseGlobal = parseFunctionBlock
          <|> parseFunction
          <|> parseProgram
          <|> parseTypeDef

parseFunctionBlock :: Parser Global
parseFunctionBlock = do
      skipVerticalSpace
      asciiCI "FUNCTION_BLOCK"
      skipSpace
      n <- parseId
      _ <- manyTill anyChar $ asciiCI "END_FUNCTION_BLOCK"
      pure $ FunctionBlock n []

parseFunction :: Parser Global
parseFunction = do
      skipVerticalSpace
      asciiCI "FUNCTION"
      skipSpace
      n <- parseId
      skipSpace
      char ':'
      skipSpace
      t <- parseType
      skipSpace
      _ <- manyTill anyChar $ asciiCI "END_FUNCTION"
      pure $ Function n t []

parseProgram :: Parser Global
parseProgram = do
      skipVerticalSpace
      asciiCI "PROGRAM"
      skipSpace
      n <- parseId
      skipSpace
      _ <- manyTill anyChar $ asciiCI "END_PROGRAM"
      pure $ Program n []

parseTypeDef :: Parser Global
parseTypeDef = do
      skipVerticalSpace
      asciiCI "TYPE"
      skipSpace
      _ <- manyTill anyChar $ asciiCI "END_TYPE"
      pure $ TypeDef ""

skipVerticalSpace :: Parser ()
skipVerticalSpace = skipMany ((space >> pure ()) <|> lineComment)

lineComment :: Parser ()
lineComment = do
      string "//"
      skip (not . isEndOfLine)

skipComment :: Parser ()
skipComment = (( string "(*" *> manyTill anyChar (string "*)")) <|> ( string "/*" *> manyTill anyChar (string "*\\"))) $> ()

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
        <|> ( (asciiCI "TIME_OF_DAY"    <|> asciiCI "TOD") $> TTimeOfDay  )
        <|> ( (asciiCI "DATE_AND_TYPE"  <|> asciiCI "DT" ) $> TDateTime   )


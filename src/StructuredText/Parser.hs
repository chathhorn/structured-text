{-# LANGUAGE OverloadedStrings #-}
module StructuredText.Parser
    ( parse, parseTop
    ) where

import Control.Applicative ((<|>), many)
import Data.Text (Text)
import Data.Attoparsec.Text
      ( Parser, string, parseOnly, manyTill, takeTill, isEndOfLine
      , anyChar, skipSpace)
import StructuredText.Syntax

parse :: Text -> Either String STxt
parse = parseOnly parseTop

parseTop :: Parser STxt
parseTop = STxt <$> many parseGlobal

parseGlobal :: Parser Global
parseGlobal = parseFunctionBlock
          <|> parseFunction
          <|> parseProgram
          <|> parseTypeDef

parseFunctionBlock :: Parser Global
parseFunctionBlock = do
      string "FUNCTION_BLOCK"
      skipSpace
      n <- takeTill isEndOfLine
      _ <- manyTill anyChar $ string "END_FUNCTION_BLOCK"
      pure $ FunctionBlock n []

parseFunction :: Parser Global
parseFunction = do
      string "FUNCTION"
      skipSpace
      n <- takeTill isEndOfLine
      _ <- manyTill anyChar $ string "END_FUNCTION"
      pure $ Function n NoType []

parseProgram :: Parser Global
parseProgram = do
      string "PROGRAM"
      skipSpace
      n <- takeTill isEndOfLine
      _ <- manyTill anyChar $ string "END_PROGRAM"
      pure $ Program n []

parseTypeDef :: Parser Global
parseTypeDef = do
      string "TYPE"
      skipSpace
      n <- takeTill isEndOfLine
      _ <- manyTill anyChar $ string "END_TYPE"
      pure $ TypeDef n

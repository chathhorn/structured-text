{-# LANGUAGE OverloadedStrings #-}
module StructuredText.Parser
    ( parse
    ) where

import Data.Text (Text)
import Data.Attoparsec.Text (Parser, string, parseOnly)
import StructuredText.Syntax

parseTop :: Parser STxt
parseTop = string "Hello World!" *> return (FunBlock "fun")

parse :: Text -> Either String STxt
parse = parseOnly parseTop

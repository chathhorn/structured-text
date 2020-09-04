{-# LANGUAGE OverloadedStrings #-}
module StructuredText.Parser
    ( parse
    ) where

import Data.Attoparsec.Text (Parser, string)
import StructuredText.Syntax

parse :: Parser STxt
parse = string "Hello World!" *> return (FunBlock "fun")


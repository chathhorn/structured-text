{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import StructuredText.Parser (parse)
import Prettyprinter.Util (putDocW)
import Prettyprinter (pretty)

main :: IO ()
main = case parse "Hello World!" of
      Left e  -> putStrLn $ "Error: " ++ e
      Right s -> putDocW 80 $ pretty s

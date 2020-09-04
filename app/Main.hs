module Main where

import StructuredText.Parser (parse)

main :: IO ()
main = putStrLn $ prettyPrint $ parse "Hello World!"

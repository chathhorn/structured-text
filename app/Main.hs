{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (readFile)
import Control.Monad (when, unless)
import System.Exit (exitFailure)
import Data.Text (Text)
import Data.Text.IO (readFile)
import System.Environment (getArgs)
import StructuredText.Parser (parse)
import Prettyprinter.Util (putDocW)
import Prettyprinter (pretty)
import System.Console.GetOpt (getOpt, usageInfo, OptDescr (..), ArgOrder (..), ArgDescr (..))
import System.IO (hPutStr, hPutStrLn, stderr)

data Flag = NoFlag
      deriving (Eq, Show)

options :: [OptDescr Flag]
options = []

exitUsage :: IO ()
exitUsage = hPutStr stderr (usageInfo "Usage: st-parse [OPTION...] <filename.rw>" options) >> exitFailure

parseFile :: FilePath -> IO ()
parseFile f = do
      txt <- readFile f
      case parse txt of
            Left e  -> putStrLn $ "Error: " ++ e
            Right s -> print s

main :: IO ()
main = do
      (flags, filenames, errs) <-  getOpt Permute options <$> getArgs

      unless (null errs) $ do
            mapM_ (hPutStrLn stderr) errs
            exitUsage

      mapM_ parseFile filenames


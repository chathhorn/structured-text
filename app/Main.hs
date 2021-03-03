module Main where

import Prelude hiding (readFile, putStr, putStrLn)
import Control.Monad (when, forM_)
import Data.Functor (($>))
import System.Exit (exitFailure, exitSuccess)
import Data.Text (unpack, pack)
import Data.Text.IO (readFile, putStrLn, putStr)
import System.Environment (getArgs)
import Text.Megaparsec (parse, errorBundlePretty)
import System.Console.GetOpt (getOpt, usageInfo, OptDescr (..), ArgOrder (..), ArgDescr (..))
import System.IO (stderr, hPutStrLn)

import StructuredText.Syntax (STxt)
import StructuredText.Parser (top)
import StructuredText.ToPython (toPython)

import Shell (shell)

import qualified Language.Python.Common.AST as Py
import qualified Language.Python.Common.Pretty as Py
import qualified Language.Python.Version3.Parser as Py

import Language.Python.Common.PrettyAST ()
import Language.Python.Common.PrettyParseError ()

data Flag = NoFlag | FlagP | FlagParsePy | FlagInteractive | FlagVersion | FlagHelp
      deriving (Eq, Show)

options :: [OptDescr Flag]
options =
      [ Option ['p'] ["parse"]        (NoArg FlagP)           "Print the parsed AST from the ST file arguments."
      , Option []    ["parse-python"] (NoArg FlagParsePy)     "Print the parsed AST from the python file arguments, then exit."
      , Option ['i'] ["interactive"]  (NoArg FlagInteractive) "Enter interactive mode instead of translating argument files."
      , Option [] ["version"]         (NoArg FlagVersion)     "Print version information."
      , Option ['h'] ["help"]         (NoArg FlagHelp)        "Print this information."
      ]

printVersion :: IO ()
printVersion = putStrLn "stxt: a python-to-st (IEC 61131-3 Structured Text) translator, version 1.0."

printUsage :: IO ()
printUsage = do
      printVersion
      putStr $ pack $ (usageInfo "Usage: stxt [OPTION...] <files>" options)

parsePy :: FilePath -> IO (Py.Module ())
parsePy f = do
      txt <- readFile f
      case Py.parseModule (unpack txt) f of
            Left e  -> do
                  putStrLn $ "Error: " <> pack (Py.prettyText e)
                  exitFailure
            Right s -> return $ fst s $> ()

parseST :: FilePath -> IO STxt
parseST f = do
      txt <- readFile f
      case parse top f txt of
            Left e  -> do
                  putStrLn $ "Error: " <> pack (errorBundlePretty e)
                  exitFailure
            Right s -> return s

main :: IO ()
main = do
      (flags, filenames, errs) <-  getOpt Permute options <$> getArgs

      when (not $ null errs) $ do
            mapM_ (hPutStrLn stderr) errs
            printUsage
            exitFailure

      when (FlagHelp `elem` flags) $ do
            printUsage
            exitSuccess

      when (FlagParsePy `elem` flags) $ do
            forM_ filenames $ \ f -> do
                  py <- parsePy f
                  print py
            exitSuccess

      when (FlagVersion `elem` flags) $ do
            printVersion
            exitSuccess

      forM_ filenames $ \ f -> do
            st <- parseST f
            when (FlagP `elem` flags) $
                  print st

            case toPython st of
                  Right py -> putStrLn $ pack (Py.prettyText py)
                  Left err -> putStrLn $ "Error: " <> err


      when (FlagInteractive `elem` flags) shell


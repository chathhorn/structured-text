module Main where

import Prelude hiding (readFile, putStr, putStrLn)
import Control.Monad (when, forM_)
import Data.Functor (($>))
import Data.Text.IO (readFile, putStrLn, hPutStrLn)
import Data.Text (unpack, pack)
import System.Console.GetOpt (getOpt, usageInfo, OptDescr (..), ArgOrder (..), ArgDescr (..))
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((-<.>))
import Text.Megaparsec (parse, errorBundlePretty)

import StructuredText.Syntax (STxt)
import StructuredText.Parser (top)
import StructuredText.ToPython (toPython)

import Shell (shell)

import qualified Language.Python.Common.AST as Py
import qualified Language.Python.Common.Pretty as Py
import qualified Language.Python.Version3.Parser as Py
import qualified System.IO as SIO

import Language.Python.Common.PrettyAST ()
import Language.Python.Common.PrettyParseError ()

data Flag = NoFlag | FlagParse | FlagParsePy | FlagInteractive | FlagOutput FilePath | FlagVersion | FlagHelp | FlagVerbose
      deriving (Eq, Show)

options :: [OptDescr Flag]
options =
      [ Option ['p'] ["parse"]        (NoArg FlagParse)                 "Print the parsed AST from the ST file arguments."
      , Option []    ["parse-python"] (NoArg FlagParsePy)               "Print the parsed AST from the python file arguments, then exit."
      , Option ['i'] ["interactive"]  (NoArg FlagInteractive)           "Enter interactive mode instead of translating argument files."
      , Option ['o'] []               (ReqArg FlagOutput $ unpack "filename.py")
                                                                        "Name for Python output file."
      , Option ['v'] []               (NoArg FlagVerbose)               "More verbose output."
      , Option [] ["version"]         (NoArg FlagVersion)               "Print version information."
      , Option ['h'] ["help"]         (NoArg FlagHelp)                  "Print this information."
      ]

printVersion :: IO ()
printVersion = putStrLn "stxt: a python-to-st (IEC 61131-3 Structured Text) translator, version 1.0."

printUsage :: IO ()
printUsage = do
      printVersion
      SIO.putStr $ usageInfo "Usage: stxt [OPTION...] <files>" options

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

getOutfile :: [Flag] -> FilePath -> IO String
getOutfile flags fname = case filter (\ case { FlagOutput {} -> True; _ -> False }) flags of
      []             -> pure $ fname -<.> "py"
      [FlagOutput o] -> pure o
      _              -> hPutStrLn SIO.stderr "Multiple output files specified on the command line!" >> exitFailure

main :: IO ()
main = do
      (flags, filenames, errs) <-  getOpt Permute options <$> getArgs

      when (not $ null errs) $ do
            mapM_ (SIO.hPutStrLn SIO.stderr) errs
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
            when (FlagVerbose `elem` flags) $ putStrLn $ "Parsing: " <> pack f

            st <- parseST f
            when (FlagParse `elem` flags) $
                  print st

            when (FlagVerbose `elem` flags) $ putStrLn $ "Translating: " <> pack f

            case toPython st of
                  Left err -> hPutStrLn SIO.stderr $ "Error: " <> err
                  Right py -> do
                        fout <- getOutfile flags f
                        SIO.writeFile fout $ Py.prettyText py

      when (FlagInteractive `elem` flags) shell


module Main where

import Prelude hiding (readFile, putStr, putStrLn)
import Control.Monad (when, forM_)
import Control.Arrow (second)
import Data.Functor (($>))
import Data.Text.IO (readFile, putStrLn, hPutStrLn)
import Data.Text (unpack, pack, Text)
import System.Console.GetOpt (getOpt, usageInfo, OptDescr (..), ArgOrder (..), ArgDescr (..))
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((-<.>))
import Text.Megaparsec (parse, errorBundlePretty)

import StructuredText.Syntax (STxt, ltls, gvars)
import StructuredText.Parser (top)
import StructuredText.ToPython (toPython)
import StructuredText.ToABA (toABA)
import StructuredText.DFA (toDFA)
import StructuredText.ToSTxt (toSTxt)
import StructuredText.LTL (normalize)

import Shell (shell)

import qualified Language.Python.Common.AST as Py
import qualified Language.Python.Common.Pretty as Py
import qualified Language.Python.Version3.Parser as Py
import qualified System.IO as SIO
import qualified Data.Text.IO as T

import Language.Python.Common.PrettyAST ()
import Language.Python.Common.PrettyParseError ()

import qualified Prettyprinter.Render.Text as P
import qualified Prettyprinter as P
import Prettyprinter (Pretty (..))

data Flag = NoFlag
          | FlagParse
          | FlagParsePython
          | FlagInteractive
          | FlagOutput FilePath
          | FlagVersion
          | FlagHelp
          | FlagVerbose
          | FlagPython
      deriving (Eq, Show)

options :: [OptDescr Flag]
options =
      [ Option ['p'] ["parse"]        (NoArg FlagParse)                 "Print the parsed AST from the ST file arguments."
      , Option []    ["parse-python"] (NoArg FlagParsePython)           "Print the parsed AST from the Python file arguments, then exit."
      , Option []    ["python"]       (NoArg FlagPython)                "Translate the input STxt to Python."
      , Option ['i'] ["interactive"]  (NoArg FlagInteractive)           "Enter interactive mode instead of translating argument files."
      , Option ['o'] []               (ReqArg FlagOutput $ unpack "filename.py")
                                                                        "Name for Python output file."
      , Option ['v'] []               (NoArg FlagVerbose)               "More verbose output."
      , Option [] ["version"]         (NoArg FlagVersion)               "Print version information."
      , Option ['h'] ["help"]         (NoArg FlagHelp)                  "Print this information."
      ]

prettyPrint :: Pretty a => a -> Text
prettyPrint = P.renderStrict . P.layoutPretty P.defaultLayoutOptions . P.pretty

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
      []             -> pure $ "monitor_" ++ fname
      [FlagOutput o] -> pure o
      _              -> hPutStrLn SIO.stderr "Multiple output files specified on the command line!" >> exitFailure

main :: IO ()
main = do
      (flags, filenames, errs) <-  getOpt Permute options <$> getArgs

      let verbose :: Text -> IO ()
          verbose = when (FlagVerbose `elem` flags) . putStrLn

      when (not $ null errs) $ do
            mapM_ (SIO.hPutStrLn SIO.stderr) errs
            printUsage
            exitFailure

      when (FlagHelp `elem` flags) $ do
            printUsage
            exitSuccess

      when (FlagParsePython `elem` flags) $ do
            forM_ filenames $ \ f -> do
                  py <- parsePy f
                  print py
            exitSuccess

      when (FlagVersion `elem` flags) $ do
            printVersion
            exitSuccess

      forM_ filenames $ \ f -> do
            verbose $ pack f <> ": parsing."

            st <- parseST f
            when (FlagParse `elem` flags) $ do
                  print st
                  T.putStrLn $ prettyPrint st

            verbose $ pack f <> ": translating."

            verbose $ pack f <> ": generating STxt DFAs."
            let dfas = mconcat $ map (uncurry (toSTxt $ gvars st) . (second $ toDFA . toABA . normalize)) $ ltls st
            fout <- getOutfile flags f
            verbose $ "Writing STxt DFAs to " <> pack fout <> "."
            T.writeFile fout $ prettyPrint dfas

            when (FlagPython `elem` flags) $ do
                  verbose $ pack f <> ": generating Python output."
                  case toPython st of
                        Left err -> hPutStrLn SIO.stderr $ "Error: " <> err
                        Right py -> do
                              let fout' = f -<.> "py"
                              verbose $ "Writing Python to " <> pack fout' <> "."
                              SIO.writeFile fout' $ Py.prettyText py

      when (FlagInteractive `elem` flags) shell


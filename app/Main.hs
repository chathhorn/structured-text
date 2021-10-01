module Main where

import Prelude hiding (readFile, putStr, putStrLn)
import Control.Monad (unless, when, forM_)
import Data.Functor (($>))
import Data.Text.IO (readFile, putStrLn, hPutStrLn)
import Data.Text (unpack, pack, Text)
import Data.Set (Set)
import qualified Data.Set as S
import System.Console.GetOpt (getOpt, usageInfo, OptDescr (..), ArgOrder (..), ArgDescr (..))
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((-<.>))
import Text.Megaparsec (parse, errorBundlePretty)

import StructuredText.Syntax (STxt, ltls, gvars, Expr)
import StructuredText.Parser (top)
import StructuredText.ToPython (toPython)
import StructuredText.ToABA (toABA)
import StructuredText.DFA (toDFA, DFA (..), LDFA, monitors)
import StructuredText.ToSTxt (toSTxt)
import StructuredText.LTL (normalize, satisfies)
import qualified StructuredText.LTL as LTL
import StructuredText.Testing (sequences)
import System.Random.Shuffle (shuffleM)

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

ppSeq :: Pretty a => [Set a] -> Text
ppSeq = prettyPrint . map (map prettyPrint . S.toList)

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

            verbose $ pack f <> ": generating DFAs."
            let (names, ltls') = unzip $ ltls st
                dfas :: [LDFA Expr]
                dfas = map (toDFA . toABA . normalize) ltls'

            verbose $ pack f <> ": verifying DFAs."
            forM_ (zip3 names ltls' dfas) $ \ (name, ltl, dfa) -> do
                  let seqs = sequences dfa
                  seqs' <- shuffleM seqs
                  verbose $ pack f <> ": " <> name <> ": has " <> pack (show $ statesDFA dfa) <> " states, " <> pack (show $ S.size $ alphaDFA dfa) <> " alphabet size; " <> pack (show $ length seqs) <> " candidate sequences."
                  forM_ (take 100 seqs') $ \ model -> do
                        verbose $ pack f <> ": " <> name <> ": checking sequence: " <> ppSeq model
                        unless (dfa `monitors` model == (model `satisfies` normalize ltl) || not (model `satisfies` normalize (LTL.Not ltl))) $ do
                              putStrLn $ pack f <> ": " <> name <> ": verification failed for LTL => DFA translation."
                              putStrLn $ "LTL: " <> prettyPrint ltl
                              putStrLn $ "Normalized LTL: " <> prettyPrint (normalize ltl)
                              putStrLn $ "dfa monitors model: " <> pack (show $ dfa `monitors` model)
                              putStrLn $ "model satisfies LTL: " <> pack (show $ model `satisfies` normalize ltl)
                              putStrLn $ "model satisfies (not LTL): " <> pack (show $ model `satisfies` normalize (LTL.Not ltl))
                              putStrLn $ "Failing sequence: " <> ppSeq model

            verbose $ pack f <> ": generating STxt from DFAs."
            let st' :: STxt
                st' = mconcat $ map (uncurry $ toSTxt $ gvars st) $ zip names dfas
            fout <- getOutfile flags f
            verbose $ "Writing STxt DFAs to " <> pack fout <> "."
            T.writeFile fout $ prettyPrint st'

            when (FlagPython `elem` flags) $ do
                  verbose $ pack f <> ": generating Python output."
                  case toPython st of
                        Left err -> hPutStrLn SIO.stderr $ "Error: " <> err
                        Right py -> do
                              let fout' = f -<.> "py"
                              verbose $ "Writing Python to " <> pack fout' <> "."
                              SIO.writeFile fout' $ Py.prettyText py

      when (FlagInteractive `elem` flags) shell

{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Shell ( shell ) where

import Prelude hiding (readFile, concat)
import Byline
import Byline.Shell
import Data.Text (Text, pack, unpack)
import Data.Text.IO (readFile)
import Control.Monad (forever, void)
import Control.Monad.Trans.State.Strict (evalStateT)
import Control.Monad.State.Class (MonadState (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Exit (exitSuccess)
import Control.Exception (try, IOException)
import qualified Options.Applicative as O
import Text.Megaparsec (parse, parseMaybe, errorBundlePretty)

import StructuredText.Parser (top, expr)
import StructuredText.ToPython (toPython)
import StructuredText.Eval (eval, evalExpr)

import qualified Language.Python.Common.Pretty as Py
import qualified Language.Python.Version3.Parser as Py
import Language.Python.Common.PrettyAST ()
import Language.Python.Common.PrettyParseError ()

data Command = Help
             | Quit
             | Load FilePath
             | Parse FilePath
             | ParsePython FilePath
             | ToPython FilePath
             | STExpr Text

parser :: O.Parser Command
parser = O.hsubparser $ mconcat
      [ O.command "help"           (O.info (pure Help)  $ O.progDesc "This message")
      , O.command "load"           (O.info loadP        $ O.progDesc "Load and evaluate a structured text file")
      , O.command "parse"          (O.info parseP       $ O.progDesc "Parse a structured text file")
      , O.command "parse-python"   (O.info parsePythonP $ O.progDesc "Parse a python file")
      , O.command "to-python"      (O.info toPythonP    $ O.progDesc "Translate an ST file to Python")
      , O.command "quit"           (O.info (pure Quit)  $ O.progDesc "Quit")
      , O.command "eval"           (O.info exprP        $ O.progDesc "Evaluate ST expression")
      ]
      where loadP        = Load        <$> O.strArgument (mconcat [O.metavar "FILE", O.help "Structured text file to load" ])
            parseP       = Parse       <$> O.strArgument (mconcat [O.metavar "FILE", O.help "Structured text file to parse" ])
            parsePythonP = ParsePython <$> O.strArgument (mconcat [O.metavar "FILE", O.help "Python file to parse" ])
            toPythonP    = ToPython    <$> O.strArgument (mconcat [O.metavar "FILE", O.help "ST file to translate into Python" ])
            exprP        = STExpr      <$> O.strArgument (mconcat [O.metavar "EXPR", O.help "ST expression" ])

dispatch :: (MonadByline m, MonadState (Shell Command) m, MonadIO m) => Command -> m ()
dispatch = \ case
      Help -> get >>= shellHelp
      Load f -> (liftIO $ try $ readFile f) >>= \ case
            Right txt -> case parse top f txt of
                  Right s -> case eval s of
                        Just r  -> sayLn $ text $ pack $ show r
                        Nothing -> sayLn $ text "Eval failed"
                  Left e  -> sayLn $ text $ "Error: " <> (pack $ errorBundlePretty e)
            Left (e :: IOException) -> sayLn $ text $ "Error: " <> (pack $ show e)
      Parse f -> (liftIO $ try $ readFile f) >>= \ case
            Right txt -> case parse top f txt of
                  Right s -> sayLn $ text $ pack $ show s
                  Left e  -> sayLn $ text $ "Error: " <> (pack $ errorBundlePretty e)
            Left (e :: IOException) -> sayLn $ text $ "Error: " <> (pack $ show e)
      ParsePython f -> (liftIO $ try $ readFile f) >>= \ case
            Right txt -> case Py.parseModule (unpack txt) f of
                  Right s -> sayLn $ text $ pack $ show $ fst s
                  Left e  -> sayLn $ text $ "Error: " <> (pack $ Py.prettyText e)
            Left (e :: IOException) -> sayLn $ text $ "Error: " <> (pack $ show e)
      ToPython f -> (liftIO $ try $ readFile f) >>= \ case
            Right txt -> case parse top f txt of
                  Right s -> case toPython s of
                        Right py -> sayLn $ text $ pack $ Py.prettyText py
                        Left err -> sayLn $ text $ "Error: " <> err
                  Left e  -> sayLn $ text $ "Error: " <> (pack $ errorBundlePretty e)
            Left (e :: IOException) -> sayLn $ text $ "Error: " <> (pack $ show e)
      STExpr s -> case parseMaybe expr s of
            Just e  -> case evalExpr e of
                  Right e' -> sayLn $ text $ pack $ show e'
                  Left err -> sayLn $ text $ "Eval failed: " <> err
            Nothing -> sayLn $ text "Failed to parse expression."
      Quit -> liftIO exitSuccess

shell :: IO ()
shell = do
      let shell' = Shell
            { shellPrefs = O.defaultPrefs
            , shellInfo = O.info parser (O.progDesc "Interactive structured text interpreter")
            , shellPrompt = "stxt> "
            }
      void $ runBylineT (go shell')
      where go :: (MonadByline m, MonadIO m) => Shell Command -> m ()
            go shell' = do
                  sayLn (text "Starting interpreter, type \"help\" for usage")
                  pushCompletionFunction (shellCompletion shell')
                  (`evalStateT` shell') $ forever (get >>= runShell dispatch)


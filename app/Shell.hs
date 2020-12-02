{-# LANGUAGE FlexibleContexts #-}
module Shell ( shell ) where

import Byline
import Byline.Shell
import Data.Text (Text)
import Control.Monad (forever, void)
import Control.Monad.Trans.State.Strict (evalStateT)
import Control.Monad.State.Class (MonadState (..), modify')
import qualified Data.Text as Text
import qualified Options.Applicative as O

data Command = Help
  | Echo [Text]
  | SetPrompt Text

parser :: O.Parser Command
parser = O.hsubparser $ mconcat
      [ O.command "help" (O.info (pure Help) $ O.progDesc "This message")
      , O.command "echo" (O.info echoP $ O.progDesc "Print all arguments")
      , O.command "set-prompt" (O.info promptP $ O.progDesc "Change the prompt")
      ]
      where echoP = Echo <$> O.many ( O.strArgument $ mconcat [ O.metavar "STR", O.help "A string to print" ])
            promptP = SetPrompt <$> O.strArgument ( mconcat [ O.metavar "STR", O.help "Set the prompt to STR" ])

dispatch :: (MonadByline m, MonadState (Shell Command) m) => Command -> m ()
dispatch = \ case
      Help -> do
            shell' <- get
            shellHelp shell'
      Echo ts ->
            sayLn (text $ Text.intercalate " " ts)
      SetPrompt prompt ->
            modify' (\s -> s {shellPrompt = text prompt})

shell :: IO ()
shell = do
      let shell' = Shell
            { shellPrefs = O.defaultPrefs
            , shellInfo = O.info parser (O.progDesc "Simple shell")
            , shellPrompt = "byline> "
            }
      void $ runBylineT (go shell')
      where go :: MonadByline m => Shell Command -> m ()
            go shell' = do
                  sayLn (text "Starting shell, use ^D to exit")
                  pushCompletionFunction (shellCompletion shell')
                  (`evalStateT` shell') $ forever (get >>= runShell dispatch)


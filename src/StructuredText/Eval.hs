module StructuredText.Eval ( eval ) where

import Data.Text (Text)
import Control.Monad.Trans.State.Strict (State, evalState, modify, gets)
import Control.Monad.State.Strict (MonadState (..))
import Control.Monad.Error (MonadError (..))
import Data.Map.Strict (Map)

import StructuredText.Syntax

data GlobalDef = FunBlockDef Text [VarDecl] [Stmt]
               | FunDef Text [VarDecl] [Stmt]
               | ProgDef Text [VarDecl] [Stmt]
               | GlobalVarDef Qualifier TypedName
               | AccessVarDef Qualifier TypedName

data S = S (Map Text GlobalDef)

data FEnv = FEnv

type M = State S

type STError = String

eval :: STxt -> M ()
eval (STxt gs) = mapM_ evalGlobal gs

evalGlobal :: Global -> M ()
evalGlobal = \ case
      FunctionBlock {} -> undefined
      Function {}      -> undefined
      Program {}       -> undefined
      TypeDef {}       -> undefined
      GlobalVars {}    -> undefined

data Result = RId Text
            | RQualId Text Text
            | RIndex Text Int
            | RBool Bool
            | RInt Int
            | RFloat Double
            | RDuration Int Int Int Int Int
            | RString Text
            | RWString Text
      deriving (Eq, Show)

evalExpr :: (MonadState FEnv m, MonadError STError m) => Expr -> m Result
evalExpr = \ case
      LV lv        -> evalLVal lv
      BinOp op a b -> do
            a' <- evalExpr a
            b' <- evalExpr b
            rBinOp op a' b'
      Negate e     -> evalExpr e >>= rNegate
      Not e        -> evalExpr e >>= rNot
      AddrOf e     -> evalExpr e >>= rAddrOf
      Call f args  -> mapM evalArg args >>= rCall f
      Lit lit      -> evalLit lit

evalLVal :: MonadError STError m => LVal -> m Result
evalLVal = undefined

evalLit :: MonadError STError m => Lit -> m Result
evalLit = undefined

evalArg :: MonadError STError m => Arg -> m Result
evalArg = undefined

rCall :: (MonadState FEnv m, MonadError STError m) => Text -> [Result] -> m Result
rCall = undefined

rBinOp :: MonadError STError m => Op -> Result -> Result -> m Result
rBinOp Plus (RInt x) (RInt y) = pure $ RInt $ x + y

rNegate :: MonadError STError m => Result -> m Result
rNegate = \ case
      RInt x   -> pure $ RInt (-x)
      RFloat x -> pure $ RFloat (-x)
      _        -> undefined

rNot :: MonadError STError m => Result -> m Result
rNot = \ case
      (RBool x) -> pure $ RBool $ not x
      _         -> undefined

rAddrOf :: MonadError STError m => Result -> m Result
rAddrOf = undefined


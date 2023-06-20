module StructuredText.Eval ( eval , evalExpr ) where

import Data.Text (Text)
import Control.Monad.State.Strict (MonadState (..), modify, execState)
import Control.Monad.Except (MonadError (..))
import Data.Map.Strict (Map, insert)

import StructuredText.Syntax

type FEnv = Map Text Global

-- type EEnv = Map Text Lit

-- type M = State FEnv

type Err = Text

putFunDef :: MonadState FEnv m => Global -> m ()
putFunDef f@(Function x _ _ _) = modify (insert x f)
putFunDef _                      = undefined

eval :: STxt -> Maybe FEnv
eval (STxt gs) = Just $ execState (mapM_ evalGlobal gs) mempty

evalGlobal :: MonadState FEnv m => Global -> m ()
evalGlobal = \ case
      FunctionBlock {} -> undefined
      f@Function {}    -> putFunDef f
      Program {}       -> undefined
      TypeDef {}       -> undefined
      GlobalVars {}    -> undefined
      LTLStmt {}       -> undefined

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

evalExpr :: MonadError Err m => Expr -> m Result
evalExpr = \ case
      LV lv        -> evalLVal lv
      BinOp op a b -> do
            a' <- evalExpr a
            b' <- evalExpr b
            rBinOp op a' b'
      Negate e    -> evalExpr e >>= rNegate
      Not e       -> evalExpr e >>= rNot
      AddrOf e    -> evalExpr e >>= rAddrOf
      Call f args -> mapM evalArg args >>= rCall f
      Lit lit     -> evalLit lit
      Paren e     -> evalExpr e

evalLVal :: LVal -> m Result
evalLVal = undefined

evalLit :: Monad m => Lit -> m Result
evalLit = pure . \ case
      Bool b              -> RBool b
      Int i               -> RInt i
      Float f             -> RFloat f
      Duration d h m s ms -> RDuration d h m s ms
      String t            -> RString t
      WString t           -> RWString t

evalArg :: Arg -> m Result
evalArg = undefined

rCall :: Text -> [Result] -> m Result
rCall = undefined

-- TODO: meh
rBinOp :: MonadError Err m => Op -> Result -> Result -> m Result
rBinOp Plus  (RInt x) (RInt y) = pure $ RInt  $ x + y
rBinOp Minus (RInt x) (RInt y) = pure $ RInt  $ x - y
rBinOp Mult  (RInt x) (RInt y) = pure $ RInt  $ x * y
rBinOp Div   (RInt x) (RInt y) = pure $ RInt  $ x `div` y -- TODO

rBinOp Lt    (RInt x) (RInt y) = pure $ RBool $ x < y
rBinOp Gt    (RInt x) (RInt y) = pure $ RBool $ x > y
rBinOp Lte   (RInt x) (RInt y) = pure $ RBool $ x <= y
rBinOp Gte   (RInt x) (RInt y) = pure $ RBool $ x >= y

rBinOp Mod   (RInt x) (RInt y) = pure $ RInt $ x `mod` y

rBinOp Plus  (RFloat x) (RFloat y) = pure $ RFloat $ x + y
rBinOp Minus (RFloat x) (RFloat y) = pure $ RFloat $ x - y
rBinOp Mult  (RFloat x) (RFloat y) = pure $ RFloat $ x * y
rBinOp Div   (RFloat x) (RFloat y) = pure $ RFloat $ x / y

rBinOp Lt    (RFloat x) (RFloat y) = pure $ RBool $ x < y
rBinOp Gt    (RFloat x) (RFloat y) = pure $ RBool $ x > y
rBinOp Lte   (RFloat x) (RFloat y) = pure $ RBool $ x <= y
rBinOp Gte   (RFloat x) (RFloat y) = pure $ RBool $ x >= y

rBinOp Eq    x y = pure $ RBool $ x == y
rBinOp Neq   x y = pure $ RBool $ x /= y

rBinOp _ _ _ = throwError "evaluating binary operator"

rNegate :: Monad m => Result -> m Result
rNegate = \ case
      RInt x   -> pure $ RInt (-x)
      RFloat x -> pure $ RFloat (-x)
      _        -> undefined

rNot :: Monad m => Result -> m Result
rNot = \ case
      (RBool x) -> pure $ RBool $ not x
      _         -> undefined

rAddrOf :: Monad m => Result -> m Result
rAddrOf = undefined


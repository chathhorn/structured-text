{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module StructuredText.ToPython ( toPython ) where

import Data.Text (Text, unpack)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.State.Class (MonadState, gets, modify)
import Control.Monad.Trans.State.Strict (evalStateT)
import Control.Monad.Except (MonadError (..))
import StructuredText.Syntax
import StructuredText.LTL (AtomicProp (..), NormLTL (..), atoms)
import qualified StructuredText.LTL as LTL
import Text.Casing (quietSnake)
import qualified Language.Python.Common.AST as Py
import qualified Language.Python.Common.Pretty as Py
import Language.Python.Common.PrettyAST ()

import Prettyprinter (Pretty (..))

import StructuredText.DFA (DFA (..), LDFA, toDFA)
import StructuredText.Boolean (B (..))
import StructuredText.ToABA (toABA)

type Sto = [Text]

type Err = Text

putFId :: MonadState Sto m => Text -> m ()
putFId x = modify (x:)

isFId :: MonadState Sto m => Text -> m Bool
isFId x = gets $ elem x

toPython :: STxt -> Either Err (Py.Module ())
toPython = flip evalStateT [] . transSTxt

transSTxt :: (MonadState Sto m, MonadError Err m) => STxt -> m (Py.Module ())
transSTxt (STxt gs) = Py.Module <$> (((importDFA:) . concat) <$> mapM transGlobal gs)

importDFA :: Py.Statement ()
importDFA = Py.FromImport (Py.ImportRelative 0 (Just [Py.Ident "dfa" ()]) ()) (Py.ImportEverything ()) ()

transGlobal :: (MonadState Sto m, MonadError Err m) => Global -> m [Py.Statement ()]
transGlobal = \ case
      FunctionBlock x ds body         -> pure <$> (Py.Fun <$> transId x <*> (concat <$> mapM transParams ds) <*> pure Nothing <*> mapM transStmt body <*> pure ())
      Function x ltls _ ds body       -> do
            x' <- transId x
            ds' <- concat <$> mapM transParams ds
            let ltls' = map LTL.normalize ltls
            ltls'' <- mapM transLTLTerms ltls'
            let dfas = zip (map (toDFA . toABA) ltls'') [0..]
            ((concatMap (uncurry $ ltlGlob x') dfas
                  ++ map (uncurry $ ltlFun x' ds') dfas) ++) . pure <$> (Py.Fun x' ds' Nothing <$> (putFId x *> mapM transStmt body) <*> pure ())
      Program x ds body               -> pure <$> (Py.Fun <$> transId x <*> (concat <$> mapM transParams ds) <*> pure Nothing <*> mapM transStmt body <*> pure ())
      TypeDef {}                      -> throwError "transGlobal TypeDef: unimplemented"
      GlobalVars {}                   -> throwError "transGlobal GlobalVars: unimplemented"

dfaId :: Py.Ident () -> Int -> Py.Ident ()
dfaId (Py.Ident x _)  n = Py.Ident (x ++ "_ltl_dfa_" ++ show n) ()

ltlGlob :: Py.Ident () -> LDFA (Py.Expr ()) -> Int -> [Py.Statement ()]
ltlGlob x dfa n = [Py.Assign [pyVar (dfaId x n)] dfaInit ()]
      where dfaInit :: Py.Expr ()
            dfaInit = Py.Call dfaClassVar
                  [ Py.ArgExpr (pyB $ currDFA dfa) ()
                  , Py.ArgExpr (Py.Dictionary (map pyDelta $ Map.toList $ deltaDFA dfa) ()) ()
                  ] ()

            dfaClassVar :: Py.Expr ()
            dfaClassVar = pyVar' "DFA"

            pyPair :: Py.Expr () -> Py.Expr () -> Py.Expr ()
            pyPair a b = Py.Paren (Py.Tuple [a, b] ()) ()

            pySet :: [Py.Expr ()] -> Py.Expr ()
            pySet es = Py.Call (pyVar' "frozenset") [Py.ArgExpr (Py.Set es ()) ()] ()

            pyDelta :: ((Int, Set (Py.Expr ())), Int) -> Py.DictKeyDatumList ()
            pyDelta ((ltl, es), b) = Py.DictMappingPair (pyPair (pyB ltl) (pySet $ map pyStringify (Set.toList es))) (pyB b)

-- | Add parens to a python expression. Makes a half-hearted attempt to remove redundant parens in the resulting
--   expression
parens :: Py.Expr () -> Py.Expr ()
parens = dedup . flip Py.Paren ()
      where dedup  = dedupSwitched False
            dedup' = dedupSwitched True

            dedupArg :: Py.Argument () -> Py.Argument ()
            dedupArg = \ case
                  Py.ArgExpr e a           -> Py.ArgExpr (dedup e) a
                  Py.ArgVarArgsPos e a     -> Py.ArgVarArgsPos (dedup e) a
                  Py.ArgVarArgsKeyword e a -> Py.ArgVarArgsKeyword (dedup e) a
                  Py.ArgKeyword k e a      -> Py.ArgKeyword k (dedup e) a

            dedupSwitched :: Bool -> Py.Expr () -> Py.Expr ()
            dedupSwitched sawParen  = \ case
                  Py.Paren e a                          -> if sawParen then dedup e else Py.Paren (dedup' e) a
                  Py.Call e args a                      -> Py.Call (dedup e) (map dedupArg args) a
                  Py.Subscript e1 e2 a                  -> Py.Subscript (dedup e1) (dedup e2) a
                  -- Py.SlicedExpr e sls a                 -> Py.SlicedExpr (dedup e) (map dedupSlice sls) a
                  Py.CondExpr e1 e2 e3 a                -> Py.CondExpr (dedup e1) (dedup e2) (dedup e3) a
                  Py.BinaryOp op e1 e2 a                -> Py.BinaryOp op (dedup e1) (dedup e2) a
                  Py.UnaryOp op e a                     -> Py.UnaryOp op (dedup e) a
                  Py.Dot e x a                          -> Py.Dot (dedup e) x a
                  Py.Lambda ps e a                      -> Py.Lambda ps (dedup e) a
                  Py.Tuple es a                         -> Py.Tuple (map dedup' es) a
                  Py.Yield (Just (Py.YieldFrom e a)) a' -> Py.Yield (Just (Py.YieldFrom (dedup e) a)) a'
                  Py.Yield (Just (Py.YieldExpr e)) a'   -> Py.Yield (Just (Py.YieldExpr (dedup e))) a'
                  -- Py.Generator c a                   -> Py.Generator (dedupComp c) a
                  Py.Await e a                          -> Py.Await (dedup e) a
                  -- Py.ListComp c a                    -> Py.ListComp (dedupComp c) a
                  Py.List es a                          -> Py.List (map dedup es) a
                  -- Py.Dictionary ks a                 -> Py.Dictionary (map dedupDictKey ks) a
                  -- Py.DictComp c a                    -> Py.DictComp (dedupComp c) a
                  Py.Set es a                           -> Py.Set (map dedup es) a
                  -- Py.SetComp c a                     -> Py.SetComp (dedupComp c) a
                  Py.Starred e a                        -> Py.Starred (dedup e) a
                  e                                     -> e

pyStringify :: Py.Expr () -> Py.Expr ()
pyStringify e = Py.Strings [qq $ Py.prettyText e] ()

pyStringifyLTL :: NormLTL (Py.Expr ()) -> Py.Expr ()
pyStringifyLTL e = Py.Strings [qq $ show $ pretty $ fmap parens e] ()

pyB :: Int -> Py.Expr ()
pyB n = Py.Int (toInteger n) (show n) ()

-- pyB :: B (NormLTL (Py.Expr ())) -> Py.Expr ()
-- pyB = \ case
--       BTrue        -> Py.Call bnode [barg "TRUE"] ()
--       BFalse       -> Py.Call bnode [barg "FALSE"] ()
--       BTerm e      -> Py.Call bnode [barg "TERM", arg $ pyStringifyLTL e] ()
--       BAnd e1 e2   -> Py.Call bnode [barg "AND", arg $ pyB e1, arg $ pyB e2] ()
--       BOr e1 e2    -> Py.Call bnode [barg "OR", arg $ pyB e1, arg $ pyB e2] ()
--       where bnode  = pyVar (Py.Ident "BNode" ())
--             b      = pyVar (Py.Ident "B" ())
--             arg e  = Py.ArgExpr e ()
--             barg n = arg $ Py.Dot b (Py.Ident n ()) ()

pyVar :: Py.Ident () -> Py.Expr ()
pyVar x = Py.Var x ()

pyVar' :: Text -> Py.Expr ()
pyVar' x = pyVar (Py.Ident (unpack x) ())

ltlFun :: Py.Ident () -> [Py.Parameter ()] -> LDFA (Py.Expr ()) -> Int -> Py.Statement ()
ltlFun x ds dfa n = Py.Fun funId (ds ++ [Py.Param x Nothing Nothing ()]) Nothing
               [ Py.Global [dfaId x n] ()
               , Py.Assign [d] (Py.Dictionary (map keyVal atoms') ()) ()
               , Py.Return (Just $ Py.Call (Py.Dot (pyVar $ dfaId x n) (Py.Ident "accept_dict" ()) ()) [Py.ArgExpr d ()] ()) ()
               ]
               ()
      where d :: Py.Expr ()
            d = pyVar (Py.Ident "d" ())

            funId :: Py.Ident ()
            funId = Py.Ident (Py.prettyText x ++ "_ltl_" ++ show n) ()

            atoms' :: [Py.Expr ()]
            atoms' = Set.toList $ Set.unions (alphaDFA dfa)

            keyVal :: Py.Expr () -> Py.DictKeyDatumList ()
            keyVal e = Py.DictMappingPair (pyStringify e) e

transId :: Monad m => Text -> m (Py.Ident ())
transId x = pure $ Py.Ident (quietSnake $ unpack x) ()

transVar :: Monad m => Text -> m (Py.Expr ())
transVar = pure . pyVar'

transOneVar :: Monad m => Text -> m [Py.Expr ()]
transOneVar x = pure <$> transVar x

-- TODO
-- transVarDecl :: Monad m => VarDecl -> m [Py.Statement ()]
-- transVarDecl = (catMaybes <$>) . mapM transTypedName . \ case
--       Var _ vs         -> vs
--       VarInput _ vs    -> vs
--       VarOutput _ vs   -> vs
--       VarInOut _ vs    -> vs
--       VarExternal _ vs -> vs
--       VarGlobal _ vs   -> vs
--       VarAccess _ vs   -> vs

-- TODO
-- transTypedName :: Monad m => TypedName -> m (Maybe (Py.Statement ()))
-- transTypedName = \ case
--       TypedName x _ _ (Just e) -> Just <$> (Py.Assign <$> transOneVar x <*> transInit e <*> pure ())
--       _                        -> pure Nothing

transParams :: MonadError Err m => VarDecl -> m [Py.Parameter ()]
transParams = \ case
      VarInput _ vs -> mapM transParam vs
      VarInOut _ vs -> mapM transParam vs
      _             -> pure []

transParam :: MonadError Err m => TypedName -> m (Py.Parameter ())
transParam = \ case
      TypedName x _ _ e -> Py.Param <$> transId x <*> pure Nothing <*> mapM transInit e <*> pure ()
      TypedLocation {}  -> throwError "transParam TypedLocation: unimplemented"

pyRange :: Py.Expr () -> Py.Expr () -> Py.Expr ()
pyRange from to = Py.Call (pyVar' "range") [Py.ArgExpr from (), Py.ArgExpr to ()] ()

qq :: String -> String
qq s = "\"" ++ s ++ "\""

transLTLTerms :: MonadError Err m => NormLTL Expr -> m (NormLTL (Py.Expr ()))
transLTLTerms = traverse transExpr

transStmt :: (MonadState Sto m, MonadError Err m) => Stmt -> m (Py.Statement ())
transStmt = \ case
      Assign lhs rhs           -> do
            r <- isReturnLVal lhs
            if r then Py.Return <$> (pure <$> transExpr rhs) <*> pure ()
                 else Py.Assign <$> (pure <$> transLVal lhs) <*> transExpr rhs <*> pure ()
      Invoke {}                -> throwError "transStmt Invoke: unimplemented"
      Return                   -> Py.Return <$> pure Nothing <*> pure ()
      If e thn [] els          -> Py.Conditional <$> ((\ a b -> [(a,b)]) <$> transExpr e <*> mapM transStmt thn) <*> mapM transStmt els <*> pure ()
      If {}                    -> throwError "transStmt: If"
      Case {}                  -> throwError "transStmt Case: unimplemented"
                                 -- TODO: I think "to" in the range needs to be incremented.
      For x from to _step body -> Py.For <$> transOneVar x <*> (pyRange <$> transExpr from <*> transExpr to) <*> mapM transStmt body <*> pure [] <*> pure ()
      While c body             -> Py.While <$> transExpr c <*> mapM transStmt body <*> pure [] <*> pure ()
      Repeat body c            -> do
            c' <- transExpr c
            body' <- mapM transStmt body
            pure $ Py.While (Py.Bool True ()) (body' ++ [Py.Conditional [(c', [Py.Break ()])] [] ()]) [] ()
      LTL ltl                  -> pure $ Py.StmtExpr (Py.Strings ["# " , show ltl] ()) ()
      Exit                     -> Py.Return <$> pure Nothing <*> pure () -- TODO: how would it be different from return?
      Empty                    -> Py.Pass <$> pure ()

transInit :: MonadError Err m => Init -> m (Py.Expr ())
transInit = \ case
      SimpleInit e    -> transLit e
      CompoundInit {} -> throwError "transInit compoundInit: unimplemented"

transExpr :: MonadError Err m => Expr -> m (Py.Expr ())
transExpr = \ case
      LV lv         -> transLVal lv
      BinOp op a b  -> Py.BinaryOp <$> transOp op <*> transExpr a <*> transExpr b <*> pure ()
      Negate e      -> Py.UnaryOp (Py.Minus ()) <$> transExpr e <*> pure ()
      Not e         -> Py.UnaryOp (Py.Not ()) <$> transExpr e <*> pure ()
      AddrOf _      -> throwError "transExpr AddrOf: unimplemented"
      Call _f _args -> throwError "transExpr Call: unimplemented"
      Paren e       -> Py.Paren <$> transExpr e <*> pure ()
      Lit n         -> transLit n

isReturnLVal :: MonadState Sto m => LVal -> m Bool
isReturnLVal = \ case
      Id x -> isFId x
      _    -> pure False


transLVal :: MonadError Err m => LVal -> m (Py.Expr ())
transLVal = \ case
      Id x -> Py.Var <$> transId x <*> pure ()
      _    -> throwError "transLVal: unimplemented"

transOp :: Monad m => Op -> m (Py.Op ())
transOp = \ case
      Plus  -> pure $ Py.Plus ()
      Minus -> pure $ Py.Minus ()
      Mult  -> pure $ Py.Multiply ()
      Div   -> pure $ Py.Divide ()
      Mod   -> pure $ Py.Modulo ()
      Exp   -> pure $ Py.Exponent ()
      Lt    -> pure $ Py.LessThan ()
      Gt    -> pure $ Py.GreaterThan ()
      Lte   -> pure $ Py.LessThanEquals ()
      Gte   -> pure $ Py.GreaterThanEquals ()
      Eq    -> pure $ Py.Equality ()
      Neq   -> pure $ Py.NotEquals ()
      And   -> pure $ Py.And ()
      Xor   -> pure $ Py.Xor ()
      Or    -> pure $ Py.Or ()

transLit :: MonadError Err m => Lit -> m (Py.Expr ())
transLit = \ case
      Bool b      -> pure $ Py.Bool b ()
      Int n       -> pure $ Py.Int (toInteger n) (show n) ()
      Float f     -> pure $ Py.Float f (show f) ()
      Duration {} -> throwError "transLit Duration: unimplemented"
      String s    -> pure $ Py.ByteStrings [unpack s] ()
      WString s   -> pure $ Py.Strings [unpack s] ()

-- | Orphan instance.
instance Pretty (Py.Expr ()) where
      pretty = pretty . Py.prettyText

-- | Orphan instance.
instance AtomicProp (Py.Expr ()) where
      atTrue  = Py.Bool True ()
      atNot a = Py.UnaryOp (Py.Not ()) a ()
      atEval (Py.Bool True ())  = Just True
      atEval (Py.Bool False ()) = Just False
      atEval (Py.Paren a ())    = atEval a
      atEval (Py.UnaryOp (Py.Not ()) a ()) = not <$> atEval a
      atEval (Py.BinaryOp (Py.And ()) a b ()) = case (atEval a, atEval b) of
            (Just True, a')         -> a'
            (Just False, _)         -> Just False
            (_, Just False)         -> Just False
            _                       -> Nothing
      atEval (Py.BinaryOp (Py.Or ()) a b ()) = case (atEval a, atEval b) of
            (Just False, a')        -> a'
            (Just True, _)          -> Just True
            (_, Just True)          -> Just True
            _                       -> Nothing
      atEval (Py.BinaryOp (Py.Xor ()) a b ()) = case (atEval a, atEval b) of
            (Just True, Just False) -> Just True
            (Just False, Just True) -> Just True
            (Just _, Just _)        -> Just False
            _                       -> Nothing
      atEval _ = Nothing

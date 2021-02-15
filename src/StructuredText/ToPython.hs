{-# LANGUAGE TupleSections #-}
module StructuredText.ToPython ( toPython ) where

-- import Data.Maybe (catMaybes)
import Data.Text (Text, unpack)
import Control.Monad.State.Class (MonadState, gets, modify)
import Control.Monad.Trans.State.Strict (evalStateT)
import Control.Monad.Except (MonadError (..))
import StructuredText.Syntax
import StructuredText.LTL (NormLTL (..), atoms, depth)
import qualified StructuredText.LTL as LTL
import Text.Casing (quietSnake)
import qualified Language.Python.Common.AST as Py

type Sto = [Text]

type Err = Text

putFId :: MonadState Sto m => Text -> m ()
putFId x = modify (x:)

isFId :: MonadState Sto m => Text -> m Bool
isFId x = gets $ elem x

toPython :: STxt -> Either Err (Py.Module ())
toPython = flip evalStateT [] . transSTxt

transSTxt :: (MonadState Sto m, MonadError Err m) => STxt -> m (Py.Module ())
transSTxt (STxt gs) = Py.Module <$> (concat <$> mapM transGlobal gs)

transGlobal :: (MonadState Sto m, MonadError Err m) => Global -> m [Py.Statement ()]
transGlobal = \ case
      FunctionBlock x ds body         -> pure <$> (Py.Fun <$> transId x <*> (concat <$> mapM transParams ds) <*> pure Nothing <*> mapM transStmt body <*> pure ())
      Function x ltls _ ds body       -> do
            x' <- transId x
            ds' <- concat <$> mapM transParams ds
            let ltls' = map LTL.normalize ltls
            ltls'' <- mapM (uncurry transLTLTerms) (zip [0..] ltls')
            let ltls''' = zip ltls'' [0..]
            ((concatMap (uncurry ltlGlob) ltls''' ++ map (uncurry (ltlFun x' ds')) ltls''') ++) . pure <$> (Py.Fun x' ds' Nothing <$> (putFId x *> mapM transStmt body) <*> pure ())
      Program x ds body               -> pure <$> (Py.Fun <$> transId x <*> (concat <$> mapM transParams ds) <*> pure Nothing <*> mapM transStmt body <*> pure ())
      TypeDef {}                      -> throwError "transGlobal TypeDef: unimplemented"
      GlobalVars {}                   -> throwError "transGlobal GlobalVars: unimplemented"

iterId :: Int -> Py.Ident ()
iterId n = Py.Ident ("i_" ++ show n) ()

jId :: Py.Ident ()
jId = Py.Ident "j" ()

pif :: Py.Expr () -> [Py.Statement ()] -> Py.Statement ()
pif c thn = pifElse c thn []

litInt :: Int -> Py.Expr ()
litInt n = Py.Int (toInteger n) (show n) ()

lit1 :: Py.Expr ()
lit1 = litInt 1

lit0 :: Py.Expr ()
lit0 = litInt 0

pifElse :: Py.Expr () -> [Py.Statement ()] -> [Py.Statement ()] -> Py.Statement ()
pifElse c thn els = Py.Conditional [(c, thn)] els ()

ltlGlob :: NormLTL (Py.Ident (), Py.Expr ()) -> Int -> [Py.Statement ()]
ltlGlob ltl n = map ltlGlob' (atomIds ltl) ++ [Py.Assign [Py.Var (iterId n) ()] lit0 ()]
      where ltlGlob' x = Py.Assign [Py.Var x ()] (Py.List (replicate (depth ltl) (Py.None ())) ()) ()

-- TODO: big mess
ltlFun :: Py.Ident () -> [Py.Parameter ()] -> NormLTL (Py.Ident (), Py.Expr ()) -> Int -> Py.Statement ()
ltlFun (Py.Ident x ()) ds ltl n = Py.Fun (Py.Ident (x ++ "_ltl_" ++ show n) ()) ds Nothing
            (  inits
            ++ rots
            ++ (map (uncurry upAtom) (zip atoms' $ map (snd . fst) (atoms ltl)))
            ++ tsts
            ++ postfix
            ) ()
      where inits   = [Py.Global (atoms' ++ [i']) ()] ++ [Py.Assign [vj] vi ()]
            rots    = [pif (Py.BinaryOp (Py.Equality ()) vi depth' ()) (map rot atoms' ++ [setJ])]
            setJ    = Py.Assign [vj] depth1' ()
            rot a   = Py.Assign [var a] (Py.BinaryOp (Py.Plus ()) (slice a (Just lit1) Nothing) (slice a Nothing (Just lit1)) ()) ()
            slice a hd tl = Py.SlicedExpr (var a) [Py.SliceProper hd tl Nothing ()] ()
            upAtom a aexpr = Py.Assign [Py.Subscript (var a) vj ()] aexpr ()
            tsts     = if length atomsSub > 0
                       then [pifElse (foldr1 and' (map isNotNone atomsSub)) [pifElse money [casePass] [caseFail]] [caseBuf]]
                       else [casePass]
            money    = transLTL ltl
            postfix  = [pif (Py.BinaryOp (Py.LessThan ()) vi depth' ()) [Py.Assign [vi] (Py.BinaryOp (Py.Plus ()) vi lit1 ()) ()]]
            casePass = prnt "\"Looks good.\""
            caseFail = prnt "\"Monitor failed!\""
            caseBuf  = prnt "\"Buffering...\""
            prnt msg = Py.StmtExpr (Py.Call (Py.Var (Py.Ident "print" ()) ()) [Py.ArgExpr (Py.Strings [msg] ()) ()] ()) ()
            depth'   = litInt $ depth ltl
            depth1'  = litInt $ depth ltl - 1
            atoms'   = atomIds ltl
            var x'   = Py.Var x' ()
            i'       = iterId n
            vi       = var i'
            vj       = var jId
            and' a b = Py.BinaryOp (Py.And ()) a b ()
            atomsSub  = map (uncurry tosub) (zip atoms' (map snd $ atoms ltl))
            tosub a d = Py.Subscript (var a) (litInt d) ()
            isNotNone a = Py.Paren (Py.BinaryOp (Py.IsNot ()) a (Py.None ()) ()) ()

transId :: Monad m => Text -> m (Py.Ident ())
transId x = pure $ Py.Ident (quietSnake $ unpack x) ()

transVar :: Monad m => Text -> m (Py.Expr ())
transVar x = Py.Var <$> transId x <*> pure ()

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
      TypedName x _ _ e -> Py.Param <$> transId x <*> pure Nothing <*> (mapM transInit e) <*> pure ()
      TypedLocation {}  -> throwError "transParam TypedLocation: unimplemented"

pyRange :: Py.Expr () -> Py.Expr () -> Py.Expr ()
pyRange from to = Py.Call (Py.Var (Py.Ident "range" ()) ()) [Py.ArgExpr from (), Py.ArgExpr to ()] ()

atomIds :: NormLTL (Py.Ident (), a) -> [Py.Ident ()]
atomIds ltl = map (fst . fst) $ atoms ltl

-- TODO: should make LTL functor instance.
transLTLTerms :: MonadError Err m => Int -> NormLTL Expr -> m (NormLTL (Py.Ident (), Py.Expr ()))
transLTLTerms n = trans' "_"
      where trans' i = \ case
                  LTL.TermN  a       -> LTL.TermN . (Py.Ident ("atom" ++ show n ++ i) (),) <$> transExpr a
                  LTL.AndN   e1 e2   -> LTL.AndN <$> trans' (i ++ "a") e1 <*> trans' (i ++ "b") e2
                  LTL.OrN    e1 e2   -> LTL.OrN <$> trans' (i ++ "a") e1 <*> trans' (i ++ "b") e2
                  LTL.UntilN e1 e2   -> LTL.UntilN <$> trans' (i ++ "a") e1 <*> trans' (i ++ "b") e2
                  LTL.ReleaseN e1 e2 -> LTL.ReleaseN <$> trans' (i ++ "a") e1 <*> trans' (i ++ "b") e2
                  LTL.NextN  e       -> LTL.NextN <$> trans' (i ++ "x") e

transLTL :: NormLTL (Py.Ident (), a) -> Py.Expr ()
transLTL = trans' 0
      where trans' d = \ case
                  LTL.TermN  (x, _)   -> Py.Subscript (Py.Var x ()) (litInt d) ()
                  LTL.AndN   e1 e2    -> Py.BinaryOp (Py.And ()) (trans' d e1) (trans' d e2) ()
                  LTL.OrN    e1 e2    -> Py.BinaryOp (Py.Or ()) (trans' d e1) (trans' d e2) ()
                  LTL.UntilN e1 _     -> trans' d e1 -- TODO
                  LTL.ReleaseN _ e2   -> trans' d e2 -- TODO
                  LTL.NextN  e        -> trans' (d + 1) e


transStmt :: (MonadState Sto m, MonadError Err m) => Stmt -> m (Py.Statement ())
transStmt = \ case
      Assign lhs rhs           -> do
            r <- isReturnLVal lhs
            if r then Py.Return <$> (pure <$> transExpr rhs) <*> pure ()
                 else Py.Assign <$> (pure <$> transLVal lhs) <*> transExpr rhs <*> pure ()
      Invoke {}                -> throwError "transStmt Invoke: unimplemented"
      Return                   -> Py.Return <$> pure Nothing <*> pure ()
      If e thn [] els          -> Py.Conditional <$> ((\ a b -> [(a,b)]) <$> transExpr e <*> mapM transStmt thn) <*> mapM transStmt els <*> pure ()
      If _ _ _ _               -> throwError "transStmt: If"
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

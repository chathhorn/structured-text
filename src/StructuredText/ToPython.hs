module StructuredText.ToPython ( toPython ) where

import Data.Maybe (catMaybes)
import Data.Text (Text, unpack)
import Control.Monad.Trans.State.Strict (State, evalState, modify, gets)
import StructuredText.Syntax
import Text.Casing (quietSnake)
import qualified Language.Python.Common.AST as Py

type M = State [Text]

putFId :: Text -> M ()
putFId x = modify (x:)

isFId :: Text -> M Bool
isFId x = gets $ elem x

toPython :: STxt -> Py.Module ()
toPython = flip evalState [] . transSTxt

transSTxt :: STxt -> M (Py.Module ())
transSTxt (STxt gs) = Py.Module <$> mapM transGlobal gs

transGlobal :: Global -> M (Py.Statement ())
transGlobal = \ case
      FunctionBlock x ds body -> Py.Fun <$> transId x <*> (concat <$> mapM transParams ds) <*> pure Nothing <*> mapM transStmt body <*> pure ()
      Function x _ ds body    -> Py.Fun <$> transId x <*> (concat <$> mapM transParams ds) <*> pure Nothing <*> (putFId x *> mapM transStmt body) <*> pure ()
      Program x ds body       -> Py.Fun <$> transId x <*> (concat <$> mapM transParams ds) <*> pure Nothing <*> mapM transStmt body <*> pure ()
      TypeDef {}              -> error "transGlobal TypeDef: unimplemented"
      GlobalVars {}           -> error "transGlobal GlobalVars: unimplemented"

transId :: Text -> M (Py.Ident ())
transId x = pure $ Py.Ident (quietSnake $ unpack x) ()

transVar :: Text -> M (Py.Expr ())
transVar x = Py.Var <$> transId x <*> pure ()

transOneVar :: Text -> M [Py.Expr ()]
transOneVar x = pure <$> transVar x

transVarDecl :: VarDecl -> M [Py.Statement ()]
transVarDecl = (catMaybes <$>) . mapM transTypedName . \ case
      Var _ vs         -> vs
      VarInput _ vs    -> vs
      VarOutput _ vs   -> vs
      VarInOut _ vs    -> vs
      VarExternal _ vs -> vs
      VarGlobal _ vs   -> vs
      VarAccess _ vs   -> vs

transTypedName :: TypedName -> M (Maybe (Py.Statement ()))
transTypedName = \ case
      TypedName x _ _ (Just e) -> Just <$> (Py.Assign <$> transOneVar x <*> transInit e <*> pure ())
      _                        -> pure Nothing

transParams :: VarDecl -> M [Py.Parameter ()]
transParams = \ case
      VarInput _ vs -> mapM transParam vs
      VarInOut _ vs -> mapM transParam vs
      _             -> pure []

transParam :: TypedName -> M (Py.Parameter ())
transParam = \ case
      TypedName x _ _ e -> Py.Param <$> transId x <*> pure Nothing <*> (mapM transInit e) <*> pure ()
      TypedLocation {}  -> error "transParam TypedLocation: unimplemented"

pyRange :: Py.Expr () -> Py.Expr () -> Py.Expr ()
pyRange from to = Py.Call (Py.Var (Py.Ident "range" ()) ()) [Py.ArgExpr from (), Py.ArgExpr to ()] ()

transStmt :: Stmt -> M (Py.Statement ())
transStmt = \ case
      Assign lhs rhs     -> do
            r <- isReturnLVal lhs
            if r then Py.Return <$> (pure <$> transExpr rhs) <*> pure ()
                 else Py.Assign <$> (pure <$> transLVal lhs) <*> transExpr rhs <*> pure ()
      Invoke {}          -> error "transStmt Invoke: unimplemented"
      Return             -> Py.Return <$> pure Nothing <*> pure ()
      If e thn [] els    -> Py.Conditional <$> ((\ a b -> [(a,b)]) <$> transExpr e <*> mapM transStmt thn) <*> mapM transStmt els <*> pure ()
      Case {}            -> error "transStmt Case: unimplemented"
                                 -- TODO: I think "to" in the range needs to be incremented.
      For x from to step body -> Py.For <$> transOneVar x <*> (pyRange <$> transExpr from <*> transExpr to) <*> mapM transStmt body <*> pure [] <*> pure ()
      While c body       -> Py.While <$> transExpr c <*> mapM transStmt body <*> pure [] <*> pure ()
      Repeat body c      -> do
            c' <- transExpr c
            body' <- mapM transStmt body
            pure $ Py.While (Py.Bool True ()) (body' ++ [Py.Conditional [(c', [Py.Break ()])] [] ()]) [] ()
      Exit               -> Py.Return <$> pure Nothing <*> pure () -- TODO: how would it be different from return?
      Empty              -> Py.Pass <$> pure ()

transInit :: Init -> M (Py.Expr ())
transInit = \ case
      SimpleInit e    -> transLit e
      CompoundInit {} -> error "transInit compoundInit: unimplemented"

transExpr :: Expr -> M (Py.Expr ())
transExpr = \ case
      LV lv        -> transLVal lv
      BinOp op a b -> Py.BinaryOp <$> transOp op <*> transExpr a <*> transExpr b <*> pure ()
      Negate e     -> Py.UnaryOp (Py.Minus ()) <$> transExpr e <*> pure ()
      Not e        -> Py.UnaryOp (Py.Not ()) <$> transExpr e <*> pure ()
      AddrOf _     -> error "transExpr AddrOf: unimplemented"
      Call f args  -> error "transExpr Call: unimplemented"
      Lit n        -> transLit n

isReturnLVal :: LVal -> M Bool
isReturnLVal = \ case
      Id x -> isFId x
      _    -> pure False


transLVal :: LVal -> M (Py.Expr ())
transLVal = \ case
      Id x -> Py.Var <$> transId x <*> pure ()
      _    -> error "transLVal: unimplemented"

transOp :: Op -> M (Py.Op ())
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

transLit :: Lit -> M (Py.Expr ())
transLit = \ case
      Bool b      -> pure $ Py.Bool b ()
      Int n       -> pure $ Py.Int (toInteger n) (show n) ()
      Float f     -> pure $ Py.Float f (show f) ()
      Duration {} -> error "transLit Duration: unimplemented"
      String s    -> pure $ Py.ByteStrings [unpack s] ()
      WString s   -> pure $ Py.Strings [unpack s] ()

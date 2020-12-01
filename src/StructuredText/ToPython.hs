{-# LANGUAGE LambdaCase #-}
module StructuredText.ToPython ( toPython ) where

import Data.Text (Text, unpack)
import Control.Monad.Trans.State.Strict (State, evalState, modify, gets)
import StructuredText.Syntax
import Text.Casing (quietSnake)
import qualified Language.Python.Common.AST as Py

type M = State [Text]

if' :: Bool -> a -> a -> a
if' True x _ = x
if' _ _ y    = y

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
      FunctionBlock {}      -> error "transGlobal FunctionBlock: unimplemented"
      Function x _ ds stmts -> Py.Fun <$> transId x <*> (concat <$> mapM transVarDecl ds) <*> pure Nothing <*> (putFId x *> mapM transStmt stmts) <*> pure ()
      Program {}            -> error "transGlobal Program: unimplemented"
      TypeDef {}            -> error "transGlobal TypeDef: unimplemented"
      GlobalVars {}         -> error "transGlobal GlobalVars: unimplemented"

transId :: Text -> M (Py.Ident ())
transId x = pure $ Py.Ident (quietSnake $ unpack x) ()

transVarDecl :: VarDecl -> M [Py.Parameter ()]
transVarDecl = \ case
      Var {}         -> error "transVarDecl Var: unimplemented"
      VarInput _ vs  -> mapM transTypedName vs
      VarOutput {}   -> error "transVarDecl VarOutput: unimplemented"
      VarInOut {}    -> error "transVarDecl VarInOut: unimplemented"
      VarExternal {} -> error "transVarDecl VarExternal: unimplemented"
      VarGlobal {}   -> error "transVarDecl VarGlobal: unimplemented"
      VarAccess {}   -> error "transVarDecl VarAccess: unimplemented"

transTypedName :: TypedName -> M (Py.Parameter ())
transTypedName = \ case
      TypedName x _ _ e -> Py.Param <$> transId x <*> pure Nothing <*> (mapM transInit e) <*> pure ()
      TypedLocation {}  -> error "transTypedName TypedLocation: unimplemented"

transStmt :: Stmt -> M (Py.Statement ())
transStmt = \ case
      Assign lhs rhs     -> do
            r <- isReturnLVal lhs
            if r then Py.Return <$> (pure <$> transExpr rhs) <*> pure ()
                 else Py.Assign <$> (pure <$> transLVal lhs) <*> transExpr rhs <*> pure ()
      Invoke {}          -> error "transStmt Invoke: unimplemented"
      Return {}          -> error "transStmt Return: unimplemented"
      If e thn [] els    -> Py.Conditional <$> ((\ a b -> [(a,b)]) <$> transExpr e <*> mapM transStmt thn) <*> mapM transStmt els <*> pure ()
      Case {}            -> error "transStmt Case: unimplemented"
      For {}             -> error "transStmt For: unimplemented"
      While {}           -> error "transStmt While: unimplemented"
      Repeat {}          -> error "transStmt Repeat: unimplemented"
      Exit {}            -> error "transStmt Exit: unimplemented"
      Empty {}           -> error "transStmt Empty: unimplemented"

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
      AddrOf _     -> error "transExpr: unimplemented"
      Call f args  -> error "transExpr: unimplemented"
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
      Plus -> pure $ Py.Plus ()
      Minus -> pure $ Py.Minus ()
      Mult -> pure $ Py.Multiply ()
      Div -> pure $ Py.Divide ()
      Mod -> pure $ Py.Modulo ()
      Exp -> pure $ Py.Exponent ()
      Lt -> pure $ Py.LessThan ()
      Gt -> pure $ Py.GreaterThan ()
      Lte -> pure $ Py.LessThanEquals ()
      Gte -> pure $ Py.GreaterThanEquals ()
      Eq -> pure $ Py.Equality ()
      Neq -> pure $ Py.NotEquals ()
      And -> pure $ Py.And ()
      Xor -> pure $ Py.Xor ()
      Or -> pure $ Py.Or ()

transLit :: Lit -> M (Py.Expr ())
transLit = \ case
      Bool b      -> pure $ Py.Bool b ()
      Int n       -> pure $ Py.Int (toInteger n) (show n) ()
      Float f     -> pure $ Py.Float f (show f) ()
      Duration {} -> error "transLit Duration: unimplemented"
      String s    -> pure $ Py.ByteStrings [unpack s] ()
      WString s   -> pure $ Py.Strings [unpack s] ()

{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module StructuredText.Syntax
      ( STxt (..)
      , Global (..)
      , Expr (..)
      , Type (..)
      , VarDecl (..)
      , Stmt (..)
      , Op (..)
      , Arg (..)
      ) where

import Data.Text (Text)
import Prettyprinter ((<+>), Pretty (..))

data STxt = STxt [Global]
      deriving (Eq, Show)

instance Pretty STxt where
      pretty (STxt gs) = pretty gs

data Global = FunctionBlock Text [VarDecl] [Stmt]
            | Function Text Type [VarDecl] [Stmt]
            | Program Text [VarDecl] [Stmt]
            | TypeDef Text
      deriving (Eq, Show)

instance Pretty Global where
      pretty = \ case
            FunctionBlock f _ _   -> pretty ("FUNCTION_BLOCK" :: Text) <+> pretty f
            Function f _ _ _      -> pretty ("FUNCTION" :: Text) <+> pretty f
            Program f _ _         -> pretty ("PROGRAM" :: Text) <+> pretty f
            TypeDef f           -> pretty ("TYPE" :: Text) <+> pretty f

data VarDecl = Var | VarInput | VarOutput
             | VarInOut | VarExternal
      deriving (Eq, Show)

data Arg = Arg Expr
         | ArgIn Text Expr
         | ArgOut Text Text
         | ArgOutNeg Text Text
      deriving (Eq, Show)

data Stmt = Assign Text Expr | Invoke Text [Arg] | Return | If
          | Case | For | While | Repeat
          | Exit | Empty
      deriving (Eq, Show)

data Op = Plus | Minus | Mult | Div | Mod | Exp
        | Lt | Gt | Lte | Gte | Eq | Neq | And | Xor | Or
      deriving (Eq, Show)

data Expr = Id Text
         | QualId Text Text
         | BinOp Op Expr Expr
         | Negate Expr
         | Not Expr
         | Call Text [Arg]
         | IntLit Int
      deriving (Eq, Show)

data Type = TBool   | TId Text
          | TReal   | TLReal
          | TInt    | TUInt | TSInt | TUSInt | TDInt | TUDInt | TLInt | TULInt
          | TByte   | TWord | TDWord | TLWord
          | TTime   | TDate | TTimeOfDay | TDateTime
          | TString | TWString
      deriving (Eq, Show)


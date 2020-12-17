module StructuredText.Syntax
      ( STxt (..), Global (..), Expr (..)
      , Type (..), VarDecl (..), Stmt (..)
      , Op (..), Arg (..), Elsif (..)
      , LVal (..), Lit (..), Labeled (..)
      , TypedName (..), Qualifier (..)
      , Location (..), Init (..), FieldInit (..)
      ) where

import Data.Text (Text)
import Prettyprinter ((<+>), Pretty (..))
import qualified StructuredText.LTL as LTL

data STxt = STxt [Global]
      deriving (Eq, Show)

instance Pretty STxt where
      pretty (STxt gs) = pretty gs

data Global = FunctionBlock Text [VarDecl] [Stmt]
            | Function Text [LTL.LTL Expr] Type [VarDecl] [Stmt]
            | Program Text [VarDecl] [Stmt]
            | TypeDef Text
            | GlobalVars VarDecl
      deriving (Eq, Show)

instance Pretty Global where
      pretty = \ case
            FunctionBlock f _ _   -> pretty ("FUNCTION_BLOCK" :: Text) <+> pretty f
            Function f _ _ _ _    -> pretty ("FUNCTION" :: Text) <+> pretty f
            Program f _ _         -> pretty ("PROGRAM" :: Text) <+> pretty f
            TypeDef f             -> pretty ("TYPE" :: Text) <+> pretty f
            GlobalVars _          -> pretty ("VAR_GLOBAL" :: Text)

data FieldInit = FieldInit Text Init
      deriving (Eq, Show)

data Init = SimpleInit Lit | CompoundInit [FieldInit]
      deriving (Eq, Show)

data TypedName = TypedName Text (Maybe Location) Type (Maybe Init)
               | TypedLocation Location Type (Maybe Init)
      deriving (Eq, Show)

data Location = InputLoc Text | OutputLoc Text | MemoryLoc Text
      deriving (Eq, Show)

data Qualifier = None | Retain | NonRetain | Constant
      deriving (Eq, Show)

data VarDecl = Var         Qualifier [TypedName]
             | VarInput    Qualifier [TypedName]
             | VarOutput   Qualifier [TypedName]
             | VarInOut    Qualifier [TypedName]
             | VarExternal Qualifier [TypedName]
             | VarGlobal   Qualifier [TypedName]
             | VarAccess   Qualifier [TypedName]
      deriving (Eq, Show)

data Arg = Arg Expr
         | ArgIn Text Expr
         | ArgOut Text Text
         | ArgOutNeg Text Text
      deriving (Eq, Show)

data Elsif = Elsif Expr [Stmt]
      deriving (Eq, Show)

data Labeled = Label Expr [Stmt]
             | LabelRange Expr Expr [Stmt]
      deriving (Eq, Show)

data Stmt = Assign LVal Expr
          | Invoke Text [Arg]
          | Return
          | If Expr [Stmt] [Elsif] [Stmt]
          | Case Expr [Labeled] [Stmt]
          | For Text Expr Expr (Maybe Expr) [Stmt]
          | While Expr [Stmt]
          | Repeat [Stmt] Expr
          | Exit
          | Empty
          | LTL (LTL.LTL Expr)
      deriving (Eq, Show)

data Op = Plus | Minus | Mult | Div | Mod | Exp
        | Lt | Gt | Lte | Gte | Eq | Neq | And | Xor | Or
      deriving (Eq, Show)

data LVal = Id Text
          | QualId Text Text
          | Index Text Expr
      deriving (Eq, Show)

data Lit = Bool Bool
         | Int Int
         | Float Double
         | Duration Int Int Int Int Int -- days, hours, mins, secs, msecs
         | String Text
         | WString Text
      deriving (Eq, Show)

data Expr = LV LVal
          | BinOp Op Expr Expr
          | Negate Expr
          | Not Expr
          | AddrOf Expr
          | Call Text [Arg]
          | Lit Lit
          | Paren Expr
      deriving (Eq, Show)

data Type = TBool   | TBoolREdge | TBoolFEdge
          | TId Text
          | TReal   | TLReal
          | TInt    | TUInt | TSInt | TUSInt | TDInt | TUDInt | TLInt | TULInt
          | TByte   | TWord | TDWord | TLWord
          | TTime   | TDate | TTimeOfDay | TDateTime
          | TString | TWString
      deriving (Eq, Show)

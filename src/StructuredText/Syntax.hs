module StructuredText.Syntax
      ( STxt (..), Global (..), Expr (..)
      , Type (..), VarDecl (..), Stmt (..)
      , Op (..), Arg (..), Elsif (..)
      , LVal (..), Lit (..), Labeled (..)
      , TypedName (..), Qualifier (..)
      , Location (..), Init (..), FieldInit (..)
      , vars
      ) where

import Data.Text (Text)
import Prettyprinter
      ( (<+>)
      , Pretty (..), vsep, Doc
      , dot, lparen, rparen, tupled, lbracket, rbracket, semi
      , nest, vcat, colon, emptyDoc
      )
import qualified StructuredText.LTL as LTL

($$) :: Doc ann -> Doc ann -> Doc ann
a $$ b = vsep [a, b]

pp :: Text -> Doc ann
pp = pretty

newtype STxt = STxt [Global]
      deriving (Eq, Show)

instance Pretty STxt where
      pretty (STxt gs) = pretty gs

data Global = FunctionBlock Text [VarDecl] [Stmt]
            | LTLStmt Text (LTL.LTL Expr)
            | Function Text Type [VarDecl] [Stmt]
            | Program Text [VarDecl] [Stmt]
            | TypeDef Text
            | GlobalVars VarDecl
      deriving (Eq, Show)

instance Pretty Global where
      pretty = \ case
            FunctionBlock f vs ss -> pp "FUNCTION_BLOCK" <+> pretty f
                                          $$ nest 2 (vcat $ map pretty vs)
                                          $$ nest 2 (vcat $ map pretty ss)
                                          $$ pp "END_FUNCTION_BLOCK"
            Function f t vs ss    -> pp "FUNCTION" <+> pretty f <+> colon <+> pretty t
                                          $$ nest 2 (vcat $ map pretty vs)
                                          $$ nest 2 (vcat $ map pretty ss)
                                          $$ pp "END_FUNCTION"
            Program f vs ss       -> pp "PROGRAM" <+> pretty f
                                          $$ nest 2 (vcat $ map pretty vs)
                                          $$ nest 2 (vcat $ map pretty ss)
                                          $$ pp "END_PROGRAM"
            TypeDef f             -> pp "TYPE" <+> pretty f
            GlobalVars v          -> pretty v
            LTLStmt x e           -> pp "LTL" <+> pretty x <+> colon <+> pretty e

data FieldInit = FieldInit Text Init
      deriving (Eq, Show)

instance Pretty FieldInit where
      pretty (FieldInit x i) = pretty x <+> pretty i

data Init = SimpleInit Lit | CompoundInit [FieldInit]
      deriving (Eq, Show)

instance Pretty Init where
      pretty = \ case
            SimpleInit lit  -> pp ":=" <+> pretty lit
            CompoundInit fs -> pp ":=" <+> tupled (map pretty fs)

data TypedName = TypedName Text (Maybe Location) Type (Maybe Init)
               | TypedLocation Location Type (Maybe Init)
      deriving (Eq, Show)

instance Pretty TypedName where
      pretty = \ case
            TypedName x Nothing t i    -> pretty x                                <+> colon <+> pretty t <+> ppMaybeInit i
            TypedName x (Just loc) t i -> pretty x <+> pp "AT" <+> pretty loc <+> colon <+> pretty t <+> ppMaybeInit i
            TypedLocation loc t i      ->              pp "AT" <+> pretty loc <+> colon <+> pretty t <+> ppMaybeInit i

            where ppMaybeInit :: Maybe Init -> Doc ann
                  ppMaybeInit = \ case
                        Nothing -> semi
                        Just i  -> pretty i <+> semi

data Location = InputLoc Text | OutputLoc Text | MemoryLoc Text
      deriving (Eq, Show)

instance Pretty Location where
      pretty = \ case
            InputLoc a  -> pp "%I" <+> pretty a
            OutputLoc a -> pp "%Q" <+> pretty a
            MemoryLoc a -> pp "%M" <+> pretty a

data Qualifier = None | Retain | NonRetain | Constant
      deriving (Eq, Show)

instance Pretty Qualifier where
      pretty = \ case
            None      -> emptyDoc
            Retain    -> pp "RETAIN"
            NonRetain -> pp "NON_RETAIN"
            Constant  -> pp "CONSTANT"

data VarDecl = Var         Qualifier [TypedName]
             | VarInput    Qualifier [TypedName]
             | VarOutput   Qualifier [TypedName]
             | VarInOut    Qualifier [TypedName]
             | VarExternal Qualifier [TypedName]
             | VarGlobal   Qualifier [TypedName]
             | VarAccess   Qualifier [TypedName]
      deriving (Eq, Show)

instance Pretty VarDecl where
      pretty = \ case
            Var q ts         -> ppVar "VAR" q ts
            VarInput q ts    -> ppVar "VAR_INPUT" q ts
            VarOutput q ts   -> ppVar "VAR_OUTPUT" q ts
            VarInOut q ts    -> ppVar "VAR_IN_OUT" q ts
            VarExternal q ts -> ppVar "VAR_EXTERNAL" q ts
            VarGlobal q ts   -> ppVar "VAR_GLOBAL" q ts
            VarAccess q ts   -> ppVar "VAR_ACCESS" q ts

            where ppVar :: Text -> Qualifier -> [TypedName] -> Doc ann
                  ppVar x q ts = pretty x <+> pretty q $$ nest 2 (vcat $ map pretty ts) $$ pp "END_VAR"

data Arg = Arg Expr
         | ArgIn Text Expr
         | ArgOut Text Text
         | ArgOutNeg Text Text
      deriving (Eq, Ord, Show)

instance Pretty Arg where
      pretty = \ case
            Arg a         -> pretty a
            ArgIn x e     -> pretty x     <+> pp ":=" <+> pretty e
            ArgOut x y    -> pretty x     <+> pp "=>" <+> pretty y
            ArgOutNeg x y -> pp "NOT" <+> pretty x <+> pp "=>" <+> pretty y

data Elsif = Elsif Expr [Stmt]
      deriving (Eq, Ord, Show)

instance Pretty Elsif where
      pretty (Elsif e ss) = pp "ELSIF" <+> pretty e <+> pp "THEN" $$ nest 2 (vcat $ map pretty ss)

data Labeled = Label Expr [Stmt]
             | LabelRange Expr Expr [Stmt]
      deriving (Eq, Ord, Show)

instance Pretty Labeled where
      pretty = \ case
            Label e ss        -> pretty e <+> colon $$ nest 2 (vcat $ map pretty ss)
            LabelRange a b ss -> pretty a <+> pp ".." <+> pretty b <+> colon $$ nest 2 (vcat $ map pretty ss)

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
      deriving (Eq, Ord, Show)

instance Pretty Stmt where
      pretty = \ case
            Assign a b          -> pretty a <+> pp ":=" <+> pretty b <> semi
            Invoke f as         -> pretty f <> tupled (map pretty as) <> semi
            If e ss els [] -> pp "IF" <+> pretty e <+> pp "THEN"
                  $$ nest 2 (vcat $ map pretty ss)
                  $$ vcat (map pretty els)
                  $$ pp "END_IF" <> semi
            If e ss els ss'     -> pp "IF" <+> pretty e <+> pp "THEN"
                  $$ nest 2 (vcat $ map pretty ss)
                  $$ vcat (map pretty els)
                  $$ pp "ELSE"
                  $$ nest 2 (vcat $ map pretty ss')
                  $$ pp "END_IF" <> semi
            Case e lbls []      -> pp "CASE" <+> pretty e <+> pp "OF"
                  $$ nest 2 (vcat $ map pretty lbls) 
                  $$ pp "END_CASE" <> semi
            Case e lbls ss   -> pp "CASE" <+> pretty e <+> pp "OF"
                  $$ nest 2 (vcat $ map pretty lbls) 
                  $$ pp "ELSE"
                  $$ nest 2 (vcat $ map pretty ss)
                  $$ pp "END_CASE" <> semi
            For x a b Nothing ss      -> pp "FOR" <+> pretty x <+> pp ":=" <+> pretty a <+> pp "TO" <+> pretty b <+> pp "DO"
                  $$ nest 2 (vcat $ map pretty ss)
                  $$ pp "END_FOR" <> semi
            For x a b (Just c) ss      -> pp "FOR" <+> pretty x <+> pp ":=" <+> pretty a <+> pp "TO" <+> pretty b <+> pp "BY" <+> pretty c <+> pp "DO"
                  $$ nest 2 (vcat $ map pretty ss)
                  $$ pp "END_FOR" <> semi
            While e ss          -> pp "WHILE" <+> pretty e <+> pp "DO"
                  $$ nest 2 (vcat $ map pretty ss)
                  $$ pp "END_WHILE" <> semi
            Repeat ss e         -> pp "REPEAT"
                  $$ nest 2 (vcat $ map pretty ss)
                  $$ pp "UNTIL" <+> pretty e
                  $$ pp "END_REPEAT" <> semi
            Return              -> pp "RETURN" <> semi
            Exit                -> pp "EXIT" <> semi
            Empty               -> semi
            LTL p               -> pp "// LTL:" <+> pretty p

data Op = Plus | Minus | Mult | Div | Mod | Exp
        | Lt | Gt | Lte | Gte | Eq | Neq | And | Xor | Or
      deriving (Eq, Ord, Show)

instance Pretty Op where
      pretty = \ case
            Plus  -> pp "+"
            Minus -> pp "-"
            Mult  -> pp "*"
            Div   -> pp "/"
            Mod   -> pp "MOD"
            Exp   -> pp "**"
            Lt    -> pp "<"
            Gt    -> pp ">"
            Lte   -> pp "<="
            Gte   -> pp ">="
            Eq    -> pp "="
            Neq   -> pp "<>"
            And   -> pp "AND"
            Xor   -> pp "XOR"
            Or    -> pp "OR"

data LVal = Id Text
          | QualId Text Text
          | Index Text Expr
      deriving (Eq, Ord, Show)

instance Pretty LVal where
      pretty = \ case
            Id x       -> pretty x
            QualId x y -> pretty x <> dot <> pretty y
            Index x e  -> pretty x <> lbracket <> pretty e <> rbracket

data Lit = Bool Bool
         | Int Int
         | Float Double
         | Duration Int Int Int Int Int -- days, hours, mins, secs, msecs
         | String Text
         | WString Text
      deriving (Eq, Ord, Show)

instance Pretty Lit where
      pretty = \ case
            Bool True          -> pp "TRUE"
            Bool False         -> pp "FALSE"
            Int a              -> pretty a
            Float a            -> pretty a
            Duration a b c d e -> pp "TIME" <+> pp "#"
                  <+> pretty a <+> pp "D"
                  <+> pretty b <+> pp "H"
                  <+> pretty c <+> pp "M"
                  <+> pretty d <+> pp "S"
                  <+> pretty e <+> pp "MS"
            String a           -> pretty a
            WString a          -> pretty a

data Expr = LV LVal
          | BinOp Op Expr Expr
          | Negate Expr
          | Not Expr
          | AddrOf Expr
          | Call Text [Arg]
          | Lit Lit
          | Paren Expr
      deriving (Eq, Ord, Show)

instance Pretty Expr where
      pretty = \ case
            LV lv        -> pretty lv
            BinOp op a b -> pretty a <+> pretty op <+> pretty b
            Negate a     -> pp "-" <> pretty a
            Not a        -> pp "NOT" <+> pretty a
            AddrOf a     -> pp "@" <> pretty a
            Call f as    -> pretty f <> tupled (map pretty as)
            Lit lit      -> pretty lit
            Paren a      -> lparen <> pretty a <> rparen

vars :: Expr -> [Text]
vars = \ case
      LV (Id x) -> [x]
      BinOp _ a b -> vars a ++ vars b
      Negate a -> vars a
      Not a -> vars a
      AddrOf a -> vars a
      Paren a -> vars a
      _  -> []

instance LTL.AtomicProp Expr where
      atTrue = Lit (Bool True)
      atNot = Not . Paren

data Type = TBool   | TBoolREdge | TBoolFEdge
          | TId Text
          | TReal   | TLReal
          | TInt    | TUInt | TSInt | TUSInt | TDInt | TUDInt | TLInt | TULInt
          | TByte   | TWord | TDWord | TLWord
          | TTime   | TDate | TTimeOfDay | TDateTime
          | TString | TWString
      deriving (Eq, Show)

instance Pretty Type where
      pretty = \ case
            TBool      -> pp "BOOL"
            TBoolREdge -> pp "BOOL" <+> pp "R_EDGE"
            TBoolFEdge -> pp "BOOL" <+> pp "F_EDGE"
            TId a      -> pretty a
            TReal      -> pp "REAL"
            TLReal     -> pp "LREAL"
            TInt       -> pp "INT"
            TUInt      -> pp "UINT"
            TSInt      -> pp "SINT"
            TUSInt     -> pp "USINT"
            TDInt      -> pp "DINT"
            TUDInt     -> pp "UDINT"
            TLInt      -> pp "LINT"
            TULInt     -> pp "ULINT"
            TByte      -> pp "BYTE"
            TWord      -> pp "WORD"
            TDWord     -> pp "DWORD"
            TLWord     -> pp "LWORD"
            TTime      -> pp "TIME"
            TDate      -> pp "DATE"
            TTimeOfDay -> pp "TIME_OF_DAY"
            TDateTime  -> pp "DATE_AND_TIME"
            TString    -> pp "STRING"
            TWString   -> pp "WSTRING"

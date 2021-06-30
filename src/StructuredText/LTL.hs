module StructuredText.LTL
      ( parseLtl
      , LTL (..)
      , BasicTerm (..), basicTerm
      , NormLTL (..), negNormLTL, normalize
      , AtomicProp (..)
      , depth, atoms, atomSet
      ) where

import Data.Text ( Text, singleton )
import Data.Char ( isAlpha, isAlphaNum )
import Data.Void ( Void )
import Data.Set  ( fromList, Set )
import Text.Megaparsec ( Parsec, (<?>), (<|>), between, empty, takeWhileP, try, satisfy )

import Control.Monad.Combinators.Expr ( makeExprParser, Operator (..) )

import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Set as S

import Prettyprinter ((<+>), Pretty (..), Doc)

type Parser = Parsec Void Text

class AtomicProp a where
      atTrue :: a
      atNot :: a -> a
      atFalse :: a
      atFalse = atNot atTrue

instance (AtomicProp a, AtomicProp b) => AtomicProp (a, b) where
      atTrue       = (atTrue, atTrue)
      atNot (a, b) = (atNot a, atNot b)

--NOT SURE WHAT THE CORRECT INSTANTIATION OF ATOMICPROP FOR CHAR IS
{-
instance AtomicProp Char where
     atTrue = ""
     atNot  = ""
-}

parseLtl :: Parser a -> Parser (LTL a)
parseLtl term = makeExprParser term' opTable
      <?> "LTL expression"
      where term' = parens (parseLtl term) <|> (Term <$> braces term)

opTable :: [[Operator Parser (LTL a)]]
opTable =
      [ [ prefix "!"  Not
        , prefix "¬"  Not
        , prefix "[]" Always
        , prefix "□"  Always
        , prefix "<>" Eventually
        , prefix "◇"  Eventually
        , prefix "()" Next
        , prefix "◯"  Next
        ]
      , [ binop "U"   Until
        , binop "⋃"   Until
        ]
      , [ binop "/\\" And
        , binop "∧"   And
        ]
      , [ binop "\\/" Or
        , binop "∨"   Or
        ]
      , [ binop "->"  Implies
        , binop "→"   Implies
        ]
      ]

binop :: Text -> (LTL a -> LTL a -> LTL a) -> Operator Parser (LTL a)
binop name f = InfixL  (f <$ symbol name)

prefix :: Text -> (LTL a -> LTL a) -> Operator Parser (LTL a)
prefix name f = Prefix  (f <$ symbol name)

space :: Parser ()
space = L.space C.space1 empty empty

parens :: Parser a -> Parser a
parens = between (symbol "(") $ symbol ")"

braces :: Parser a -> Parser a
braces = between (symbol "{") $ symbol "}"

symbol :: Text -> Parser Text
symbol = L.symbol space

data LTL a = Term a
           | And (LTL a) (LTL a)
           | Or (LTL a) (LTL a)
           | Implies (LTL a) (LTL a)
           | Not (LTL a)
           | Always (LTL a)
           | Eventually (LTL a)
           | Until (LTL a) (LTL a)
           | Release (LTL a) (LTL a)
           | Next (LTL a)
      deriving (Eq, Show)

parenify :: Pretty a => a -> Doc ann
parenify a = pretty ("(" :: Text) <+> pretty a <+> pretty (")" :: Text)

pBinOp :: (Pretty a, Pretty b) => Text -> a -> b -> Doc ann
pBinOp op a b = parenify a <+> pretty op <+> parenify b

pUnOp :: Pretty a => Text -> a -> Doc ann
pUnOp op a = pretty op <+> parenify a

instance Pretty a => Pretty (LTL a) where
      pretty = \ case
            Term a -> pretty a
            And e1 e2 -> pBinOp "/\\" e1 e2
            Or e1 e2 -> pBinOp "\\/" e1 e2
            Implies e1 e2 -> pBinOp "->" e1 e2
            Not e1 -> pUnOp "!" e1
            Always e1 -> pUnOp "[]" e1
            Eventually e1 -> pUnOp "<>" e1
            Until e1 e2 -> pBinOp "U" e1 e2
            Release e1 e2 -> pBinOp "R" e1 e2
            Next e1 -> pUnOp "X" e1

data NormLTL a = TermN a
               | AndN (NormLTL a) (NormLTL a)
               | OrN (NormLTL a) (NormLTL a)
               | UntilN (NormLTL a) (NormLTL a)
               | ReleaseN (NormLTL a) (NormLTL a)
               | NextN (NormLTL a)
      deriving (Eq, Show, Ord)

instance Functor NormLTL where
      fmap f = \ case
            TermN a        -> TermN $ f a
            AndN e1 e2     -> AndN (fmap f e1) (fmap f e2)
            OrN e1 e2      -> OrN (fmap f e1) (fmap f e2)
            UntilN e1 e2   -> UntilN (fmap f e1) (fmap f e2)
            ReleaseN e1 e2 -> ReleaseN (fmap f e1) (fmap f e2)
            NextN e        -> NextN (fmap f e)

instance Foldable NormLTL where
      foldMap f = \ case
            TermN a        -> f a
            AndN e1 e2     -> foldMap f e1 <> foldMap f e2
            OrN e1 e2      -> foldMap f e1 <> foldMap f e2
            UntilN e1 e2   -> foldMap f e1 <> foldMap f e2
            ReleaseN e1 e2 -> foldMap f e1 <> foldMap f e2
            NextN e        -> foldMap f e

instance Traversable NormLTL where
      traverse f = \ case
            TermN a        -> TermN    <$> f a
            AndN e1 e2     -> AndN     <$> traverse f e1 <*> traverse f e2
            OrN e1 e2      -> OrN      <$> traverse f e1 <*> traverse f e2
            UntilN e1 e2   -> UntilN   <$> traverse f e1 <*> traverse f e2
            ReleaseN e1 e2 -> ReleaseN <$> traverse f e1 <*> traverse f e2
            NextN e        -> NextN    <$> traverse f e

instance Pretty a => Pretty (NormLTL a) where
      pretty = \ case
            TermN a        -> pretty a
            AndN e1 e2     -> pBinOp "/\\" e1 e2
            OrN e1 e2      -> pBinOp "\\/" e1 e2
            UntilN e1 e2   -> pBinOp "U" e1 e2
            ReleaseN e1 e2 -> pBinOp "R" e1 e2
            NextN e        -> pUnOp "X" e

negNormLTL :: (AtomicProp a) => NormLTL a -> NormLTL a
negNormLTL ltl = case ltl of
     TermN a        -> TermN (atNot a)
     AndN e1 e2     -> OrN (negNormLTL e1) (negNormLTL e2) 
     OrN e1 e2      -> AndN (negNormLTL e1) (negNormLTL e2)
     UntilN e1 e2   -> ReleaseN (negNormLTL e1) (negNormLTL e2)
     ReleaseN e1 e2 -> UntilN (negNormLTL e1) (negNormLTL e2)
     NextN e1       -> NextN (negNormLTL e1)

atoms :: NormLTL a -> [(a, Int)]
atoms = atoms' 0
      where atoms' :: Int -> NormLTL a -> [(a, Int)]
            atoms' n = \ case
                  TermN a        -> [(a, n)]
                  AndN e1 e2     -> atoms' n e1 ++ atoms' n e2
                  OrN e1 e2      -> atoms' n e1 ++ atoms' n e2
                  UntilN e1 e2   -> atoms' n e1 ++ atoms' n e2
                  ReleaseN e1 e2 -> atoms' n e1 ++ atoms' n e2
                  NextN e1       -> atoms' (n + 1) e1

atomSet :: (Ord a) => NormLTL a -> Set a
atomSet phi = S.fromList (map fst (atoms phi))

depth :: NormLTL a -> Int
depth ltl = maximum (map snd (atoms ltl)) + 1

-- | Rewrite an LTL prop into "negation normal form":
--   - elminate Implies, Always, Eventually
--   - Negation only on atomic props
normalize :: AtomicProp a => LTL a -> NormLTL a
normalize = toNormLTL . elimSugar

-- | Eliminates Implies, Always, Eventually.
elimSugar :: AtomicProp a => LTL a -> LTL a
elimSugar = \ case
      Term a        -> Term a
      And e1 e2     -> And (elimSugar e1) (elimSugar e2)
      Or e1 e2      -> Or (elimSugar e1) (elimSugar e2)
      Implies e1 e2 -> Or (Not (elimSugar e1)) (elimSugar e2)
      Not e1        -> Not (elimSugar e1)
      Always e1     -> Until (elimSugar e1) (Term atFalse)
      Eventually e1 -> Until (Term atTrue) (elimSugar e1)
      Until e1 e2   -> Until (elimSugar e1) (elimSugar e2)
      Release e1 e2 -> Release (elimSugar e1) (elimSugar e2)
      Next e1       -> Next (elimSugar e1)

-- | Eliminates Not.
toNormLTL :: AtomicProp a => LTL a -> NormLTL a
toNormLTL = \ case
      Not (Term a)        -> TermN (atNot a)
      Not (And e1 e2)     -> OrN (toNormLTL (Not e1)) (toNormLTL (Not e2))
      Not (Or e1 e2)      -> AndN (toNormLTL (Not e1)) (toNormLTL (Not e2))
      Not (Not e1)        -> toNormLTL e1
      Not (Until e1 e2)   -> ReleaseN (toNormLTL (Not e1)) (toNormLTL (Not e2))
      Not (Release e1 e2) -> UntilN (toNormLTL (Not e1)) (toNormLTL (Not e2))
      Not (Next e1)       -> NextN (toNormLTL (Not e1))

      Term a              -> TermN a
      And e1 e2           -> AndN (toNormLTL e1) (toNormLTL e2)
      Or e1 e2            -> OrN (toNormLTL e1) (toNormLTL e2)
      Until e1 e2         -> UntilN (toNormLTL e1) (toNormLTL e2)
      Release e1 e2       -> ReleaseN (toNormLTL e1) (toNormLTL e2)
      Next e1             -> NextN (toNormLTL e1)

      _                   -> error "toNormLTL: missing case (sugar not eliminated?)"

data BasicTerm = BTVar Text
               | BTTrue
               | BTFalse
               | BTNot BasicTerm
      deriving (Eq, Show)

instance Pretty BasicTerm where
      pretty = \ case
            BTVar x -> pretty x
            BTTrue -> pretty ("true" :: Text)
            BTFalse -> pretty ("false" :: Text)
            BTNot t -> pretty ("!" :: Text) <+> pretty t

instance AtomicProp BasicTerm where
      atTrue = BTTrue
      atNot = BTNot

basicTerm :: Parser BasicTerm
basicTerm = try (BTTrue <$ symbol "true")
        <|> try (BTFalse <$ symbol "false")
        <|> try (BTNot <$> (symbol "!" *> basicTerm))
        <|> try (BTVar <$> ident)

-- | Ids (atomic props) are alpha-numeric strings starting with a letter.
ident :: Parser Text
ident = do
      x <- satisfy isAlpha
      xs <- takeWhileP (Just "legal LTL basic term identifier character") isAlphaNum
      space
      pure $ singleton x <> xs


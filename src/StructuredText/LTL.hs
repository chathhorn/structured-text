module StructuredText.LTL ( parseLtl , LTL (..) ) where

import Data.Text ( Text )
import Data.Void ( Void )
import Text.Megaparsec ( Parsec , (<?>) , (<|>) , between , empty )

import Control.Monad.Combinators.Expr ( makeExprParser, Operator (..) )

import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

parseLtl :: Parser a -> Parser (LTL a)
parseLtl term = makeExprParser term' opTable
      <?> "LTL expression"
      where term' = parens (parseLtl term) <|> (Term <$> braces term)

opTable :: [[Operator Parser (LTL a)]]
opTable =
      [ [ prefix "!"   Not
        , prefix "[]"  Always
        , prefix "<>"  Eventually
        , prefix "()"  Next
        ]
      , [ binop "/\\" $ And
        ]
      , [ binop "\\/" $ Or
        ]
      , [ binop "->"  $ Implies
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
           | Next (LTL a)
      deriving (Eq, Show)

module StructuredText.ToSTxt ( toSTxt ) where

import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import StructuredText.Syntax (STxt (..))
import qualified StructuredText.Syntax as ST
import StructuredText.DFA (DFA (..), LDFA)
import Data.Text (Text, pack)
import Data.List (sortOn)

-- | First argument: name.
toSTxt :: [ST.TypedName] -> Text -> LDFA ST.Expr -> STxt
toSTxt gvs n dfa = STxt
      [ ST.FunctionBlock ("dfa_" <> n) [ST.VarInput ST.None inputs, ST.Var ST.None lvars, ST.VarOutput ST.None outputs]
        (  atomicProps
        <> transition
        <> alarm
        )
      ]
      where inputs :: [ST.TypedName]
            inputs = filter (\ case { ST.TypedName x _ _ _ -> x `elem` vars; _ -> False} ) gvs

            lvars :: [ST.TypedName]
            lvars = [ST.TypedName "STATE" Nothing ST.TInt (Just $ ST.SimpleInit $ ST.Int $ currDFA dfa)]
                 <> map toTypedName (S.toList alpha)

            toTypedName :: ST.Expr -> ST.TypedName
            toTypedName x = ST.TypedName (nameExpr x) Nothing ST.TBool Nothing

            nameExpr :: ST.Expr -> Text
            nameExpr x = "P" <> pack (show $ S.findIndex x alpha)

            outputs :: [ST.TypedName]
            outputs = [ST.TypedName "ALARM" Nothing ST.TBool (Just $ ST.SimpleInit $ ST.Bool False)]

            alpha :: Set ST.Expr
            alpha = S.unions $ alphaDFA dfa

            vars :: Set Text
            vars = S.fromList $ foldMap ST.vars alpha

            atomicProps :: [ST.Stmt]
            atomicProps = map (\ x -> ST.Assign (ST.Id $ nameExpr x) x) $ S.toList alpha

            transition :: [ST.Stmt]
            transition = case map toElsIf $ sortOn propSize $ M.toList (deltaDFA dfa) of
                  ST.Elsif c ss : els -> [ST.If c ss els []]
                  _                   -> []

            propSize :: ((Int, Set ST.Expr), Int) -> Int
            propSize ((_, s), _) = S.size alpha - S.size s

            toElsIf :: ((Int, Set ST.Expr), Int) -> ST.Elsif
            toElsIf ((s, xs), s') = ST.Elsif
                  (foldr (ST.BinOp ST.And . ST.LV . ST.Id . nameExpr) (ST.BinOp ST.Eq (ST.LV $ ST.Id "STATE") (ST.Lit $ ST.Int s)) xs)
                  [ST.Assign (ST.Id "STATE") $ ST.Lit $ ST.Int s']

            alarm :: [ST.Stmt]
            alarm = [ST.Assign (ST.Id "ALARM") $ ST.BinOp ST.Eq (ST.LV (ST.Id "STATE")) $ ST.Lit $ ST.Int 0]

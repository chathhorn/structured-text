import Test.QuickCheck
      ( forAll, elements, Property (..)
      , Arbitrary (..)
      , quickCheck
      , verboseCheck
      , verboseCheckWith
      , oneof, sized
      , Gen (..)
      , quickCheckWith
      , stdArgs
      , Args (..)
      )


prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs


data B = T | F | And B B | Or B B
      deriving (Eq, Show)

instance Arbitrary B where
      -- arbitrary :: Gen B
      arbitrary = sized arbB

arbB :: Int -> Gen B
arbB 0 = oneof [pure T, pure F]
arbB n = do
      a <- arbB (n - 1)
      b <- arbB (n - 1)
      oneof [pure (And a b), pure (Or a b)]

simpl :: B -> B
simpl b = case b of
      And T b -> simpl b
      And b T -> simpl b
      And F b -> F
      Or F b  -> simpl b
      Or b F  -> simpl b
      Or T b  -> T
      b       -> b

prop_simpl :: B -> Bool
prop_simpl b = simpl (simpl b) == simpl b

eval :: B -> Bool
eval b = case b of
      T -> True
      F -> False
      And x y -> (eval x) && (eval y)
      Or x y -> (eval x) || (eval y)

prop_simpl_eval :: B -> Bool
prop_simpl_eval b = eval (simpl b) == eval b


{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

fiveRands :: [Integer]
fiveRands = let
  s       = mkSeed 1
  (a, s')    = rand s
  (b, s'')   = rand s'
  (c, s''')  = rand s''
  (d, s'''') = rand s'''
  (e, _)     = rand s''''
  in [a, b, c, d, e]

randLetter :: Gen Char
randLetter s = let
  (n, s') = rand s
  in (toLetter n, s')

randString3 :: String
randString3 = let
  s = mkSeed 1
  (a, s')  = randLetter s
  (b, s'') = randLetter s'
  (c, _)   = randLetter s''
  in [a, b, c]

type Gen a = Seed -> (a, Seed)

randEven :: Gen Integer
randEven s = let
  (n, s') = rand s
  in (n * 2, s')

randOdd :: Gen Integer
randOdd s = let
  (n, s') = randEven s
  in (n + 1, s')

randTen :: Gen Integer
randTen s = let
  (n, s') = rand s
  in (n * 10, s')

generalA :: Gen a -> (a -> b) -> Gen b
generalA genF f = (\(n, s) -> (f n, s)) . genF

randEven' = generalA rand (*2)
randOdd' = generalA randEven (+1)
randTen' = generalA rand (*10)

randPair :: Gen (Char, Integer)
randPair s = let
  (l, s') = randLetter s
  (n, s'')  = rand s'
  in ((l, n), s'')

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair g1 g2 = \s -> let
  (x, s')  = g1 s
  (y, s'') = g2 s'
  in ((x, y), s'')

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f g1 g2 = \s -> let
  (x, s')  = g1 s
  (y, s'') = g2 s'
  in (f x y, s'')

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 = generalB (,)
  
repRandom :: [Gen a] -> Gen [a]
repRandom = foldr (generalB (:)) (\s -> ([], s))

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo g1 f = \s -> let
  (a, s') = g1 s
  in f a s'

mkGen :: a -> Gen a
mkGen = \x s -> (x, s)

-- From Set4!

generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB2 f g1 g2 = g1 `genTwo` (\a -> g2 `genTwo` (\b -> mkGen $ f a b))

-- How does this even work...
repRandom' :: [Gen a] -> Gen [a]
repRandom' []     = mkGen []
repRandom' (x:xs) = x `genTwo` (\a -> (repRandom' xs) `genTwo` (\as -> mkGen $ a : as))


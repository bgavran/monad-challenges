{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude

-- Generating combinations

allPairs :: [a] -> [b] -> [(a, b)]
allPairs xs (y:ys) = map (\z -> (z, y)) xs ++ (allPairs xs ys)
allPairs _      _      = []

-- Poker hands

data MyCard = Card Int String

instance Show MyCard where
  show (Card r s) = show r ++ s

allCards :: [Int] -> [String] -> [MyCard]
allCards (x:xs) ys = map (\z -> Card x z) ys ++ (allCards xs ys)
allCards _  _      = []

-- Generalizing pairs and cards
  
allCombs0 :: (a -> b) -> [a] -> [b]
allCombs0 f xs = map f xs

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs f (x:xs) ys   = (allCombs0 (f x) ys) ++ (allCombs f xs ys)
allCombs _ _    _      = []

allPairs' :: [a] -> [b] -> [(a, b)]
allPairs' = allCombs (,)

allCards' :: [Int] -> [String] -> [MyCard]
allCards' = allCombs Card

-- Combinations of three things

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f (x:xs) ys zs = (allCombs (f x) ys zs) ++ (allCombs3 f xs ys zs)
allCombs3 f _      _  _  = []

-- Combinations of more things

allCombs4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
allCombs4 f (x:xs) ys zs ws = (allCombs3 (f x) ys zs ws) ++ (allCombs4 f xs ys zs ws)
allCombs4 f  _     _  _  _  = []

combStep :: [a -> b] -> [a] -> [b]
combStep (f:fs) xs = map f xs ++ combStep fs xs
combStep _      _  = []

allCombs' :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs' f xs ys = combStep (map f xs) ys

allCombs3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3' f xs ys zs = (map f xs) `combStep` ys `combStep` zs

allCombs4' :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
allCombs4' f xs ys zs ws = (map f xs) `combStep` ys `combStep` zs `combStep` ws


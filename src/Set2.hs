{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

-- The Maybe type

data Maybe a
  = Nothing
  | Just a

instance Show a => Show (Maybe a) where
  show Nothing   = "Nothing"
  show (Just a)  = "Just " ++ show a

-- Build a library of things that can fail


headMay :: [a] -> Maybe a
headMay []  = Nothing
headMay (x:_) = Just x

tailMay :: [a] -> Maybe [a]
tailMay []     = Nothing
tailMay (_:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ []          = Nothing
lookupMay e ((a, b):xs) = case a == e of
  True  -> Just b
  False -> lookupMay e xs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay a b = Just $ a / b

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay (x:xs) = Just $ foldr1 max xs

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay xs = Just $ foldr1 min xs

-- Chains of failing computations

queryGreek :: GreekData -> String -> Maybe Double
queryGreek gd s = let
  xs = lookupMay s gd
  m = case xs of
    Nothing -> Nothing
    Just xs -> case tailMay xs of
                   Nothing -> Nothing
                   Just xs -> maximumMay xs
  h = case xs of
    Nothing -> Nothing
    Just xs -> headMay xs
  q = case (m, h) of
    (Nothing, _) -> Nothing
    (_, Nothing) -> Nothing
    (Just m, Just d) -> divMay (fromIntegral m) (fromIntegral d)
  in q

-- Generalizing chains of failures
 
chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain _ Nothing = Nothing
chain f (Just a) = f a

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = flip chain

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 gd s = let 
  xs = (lookupMay s gd)
  h = xs `link` headMay
  in xs `link` tailMay `link` maximumMay `link` 
      (\m -> h `link` (\h -> divMay (fromIntegral m) (fromIntegral h)))

-- Chaining variations

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries xs a b = case lookupMay a xs of 
                       Nothing  -> Nothing
                       (Just a) -> case lookupMay b xs of 
                                       Nothing  -> Nothing
                                       (Just b) -> Just $ a + b

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink f ma mb = ma `link` (\a -> mb `link` (\b -> Just $ f a b))

mkMaybe :: a -> Maybe a
mkMaybe x = Just x

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 xs a b = yLink (+) (lookupMay a xs) (lookupMay b xs)

-- Tailprod

tailProd :: Num a => [a] -> Maybe a
tailProd xs = case tailMay xs of
  Nothing   -> Nothing
  (Just xs) -> mkMaybe $ product xs

tailSum :: Num a => [a] -> Maybe a
tailSum xs = case tailMay xs of
  Nothing   -> Nothing
  (Just xs) -> mkMaybe $ sum xs

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe f ma = ma `link` (\x -> mkMaybe $ f x)

tailProd' :: Num a => [a] -> Maybe a
tailProd' = transMaybe product . tailMay

tailSum' :: Num a => [a] -> Maybe a
tailSum' = transMaybe sum . tailMay

tailMax :: Ord a => [a] -> Maybe (Maybe a)
tailMax xs = case tailMay xs of
  Nothing   -> Nothing
  (Just []) -> mkMaybe Nothing
  (Just xs) -> mkMaybe $ maximumMay xs

tailMin :: Ord a => [a] -> Maybe (Maybe a)
tailMin xs = case tailMay xs of
  Nothing   -> Nothing
  (Just []) -> mkMaybe Nothing
  (Just xs) -> mkMaybe $ minimumMay xs

combine :: Maybe (Maybe a) -> Maybe a
combine (Just (Just a)) = Just a
combine _               = Nothing


tailMax' :: Ord a => [a] -> Maybe a
tailMax' = combine . tailMax

tailMin' :: Ord a => [a] -> Maybe a
tailMin' = combine . tailMin

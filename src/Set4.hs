{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

import MCPrelude
import Set2

-- Generalizing State and Maybe
-- Formalizing the pattern

class Monad m where
  bind :: m a -> (a -> m b) -> m b
  return :: a -> m a

unifiedImpl :: Monad m => (a -> b -> c) -> m a -> m b -> m c
unifiedImpl f ma mb = ma `bind` (\a -> mb `bind` (\b -> return $ f a b)) 

-- Creating instances

newtype Gen a = Gen { runGen :: Seed -> (a, Seed) }

evalGen :: Gen a -> Seed -> a
evalGen g s = fst $ runGen g s


instance Monad Maybe where
  return = Just
  bind Nothing _  = Nothing
  bind (Just a) f = f a

instance Monad [] where
  return x = [x]
  bind [] _     = []
  bind (x:xs) f = (f x) ++ (xs `bind` f)

instance Monad Gen where
  return a  = Gen $ \s -> (a, s)
  bind ma f = Gen $ \s -> let (b, s') = runGen ma s
                          in runGen (f b) s'


-- Revisiting Other Generic Functions

sequence :: Monad m => [m a] -> m [a]
sequence (ma:mas) = ma `bind` (\a -> sequence mas `bind` (\as -> return $ a:as))
sequence []       = return []

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb = ma `bind` (\a -> mb `bind` (\b -> return $ f a b)) 

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip bind

join :: Monad m => m (m a) -> m a
join = ((return =<<) =<<)

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f ma mb mc = ma `bind` (\a -> mb `bind` (\b -> mc `bind` (\c -> return $ f a b c)))

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma = mf `bind` (\f -> ma `bind` (\a -> return $ f a) )

--liftM3' :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
--liftM3' f ma mb mc = f `ap` ma `ap` mb `ap` mc






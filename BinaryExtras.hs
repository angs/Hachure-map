module BinaryExtras where

import Data.Binary 

-- | 'getMany n' get 'n' elements in order, without blowing the stack.
getMany :: Binary a => Int -> Get [a]
getMany n = go [] n
  where
    go xs 0 = return $! reverse xs
    go xs i = do x <- get
                 -- we must seq x to avoid stack overflows due to laziness in
                 -- (>>=)
                 x `seq` go (x:xs) (i-1)
{-# INLINE getMany #-}

{-# LANGUAGE BangPatterns                 #-}

module Tokenomia.Common.Data.Function.Memoize
    ( memoize
    ) where

--
-- The function `memoize` takes a function defined with explicit open recursion
-- and return a memoized version. An open recursion happens when the recursive
-- function does not call itself by name. Memoized results are stored in a tree
-- for accessing them in sublinear time.
--

import Data.Function            ( fix )


data Tree a = Tree (Tree a) a (Tree a)

instance Functor Tree where
    fmap f (Tree l a r) =
        Tree (fmap f l) (f a) (fmap f r)

naturals :: Tree Integer
naturals = go 0 1
  where
    go !n !s =
        let l = n + s
            r = l + s
            s' = s * 2
        in
            Tree (go l s') n (go r s')

-- | Create the trie for the entire domain of a function
trie :: (Integer -> a) -> Tree a
trie f = fmap f naturals

-- | Convert a trie to a function, i.e., access a field of the trie
untrie :: Tree a -> Integer -> a
untrie (Tree _ a _) 0 = a
untrie (Tree l _ r) n =
    case (n - 1) `divMod` 2 of
        (q, 0) -> untrie l q
        (q, _) -> untrie r q

-- | Trie-based function memoizer
memoizer :: (Integer -> a) -> Integer -> a
memoizer = untrie . trie

-- | Memoizing recursion. Use like `fix`.
memoize :: ((Integer -> a) -> Integer -> a) -> Integer -> a
memoize f = fix (memoizer . f)

module Tokenomia.Common.Arbitrary.Utils
    ( growingFrequency
    , inBijection
    , isIdentity
    , shrinkListStructure
    ) where

import Test.Tasty.QuickCheck                           ( Gen, frequency, shrinkList )

-- | Helper generator to choose element from a list with increasing frequency
growingFrequency :: [a] -> Gen a
growingFrequency xs = frequency $ zip [1..] (pure <$> xs)

-- | Helper function to test identity property
isIdentity :: Eq a => (a -> a) -> a -> Bool
isIdentity f x = f x == x

-- | Helper function to test bijection property
inBijection :: (Eq a, Eq b) => (a -> b) -> (b -> a) -> a -> b -> Bool
inBijection f g x y =
        isIdentity (f . g) y
    &&  isIdentity (g . f) x

-- | Shrinks only the list structure but not its elements
shrinkListStructure :: [a] -> [[a]]
shrinkListStructure = shrinkList (const mempty)

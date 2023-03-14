{-# LANGUAGE ImportQualifiedPost                       #-}

module Tokenomia.Common.Data.List.NonEmpty
    ( prependMaybe
    , singleton
    , zipWith3
    ) where

import Data.List qualified
    as L                                               ( zipWith3 )
import Data.List.NonEmpty                              ( NonEmpty((:|)), (<|) )
import Prelude hiding                                  ( zipWith3 )


prependMaybe :: Maybe a -> NonEmpty a -> NonEmpty a
prependMaybe x xs = maybe xs (<| xs) x

singleton :: a -> NonEmpty a
singleton = (:| [])

zipWith3 :: (a -> b -> c -> d) -> NonEmpty a -> NonEmpty b -> NonEmpty c -> NonEmpty d
zipWith3 f (a :| as) (b :| bs) (c :| cs) =
    f a b c :| L.zipWith3 f as bs cs

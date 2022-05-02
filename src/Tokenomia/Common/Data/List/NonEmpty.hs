{-# LANGUAGE ImportQualifiedPost          #-}

module Tokenomia.Common.Data.List.NonEmpty
    ( singleton
    , zipWith3
    ) where

import Data.List.NonEmpty       ( NonEmpty((:|)) )
import Data.List qualified as L ( zipWith3 )
import Prelude           hiding ( zipWith3 )


singleton :: a -> NonEmpty a
singleton = (:| [])

zipWith3 :: (a -> b -> c -> d) -> NonEmpty a -> NonEmpty b -> NonEmpty c -> NonEmpty d
zipWith3 f (a :| as) (b :| bs) (c :| cs) =
    f a b c :| L.zipWith3 f as bs cs

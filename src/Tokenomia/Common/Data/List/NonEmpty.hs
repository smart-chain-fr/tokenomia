module Tokenomia.Common.Data.List.NonEmpty
    ( singleton
    ) where

import Data.List.NonEmpty       ( NonEmpty((:|)) )

singleton :: a -> NonEmpty a
singleton = (:| [])

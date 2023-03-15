module Tokenomia.Common.Data.Either.Extra
    ( toEither
    ) where

import Control.Monad                                   ( guard )
import Data.Either.Combinators                         ( maybeToRight )

-- | Constructs a Right if the boolean is True
toEither :: Bool -> e -> a -> Either e a
toEither b e a = maybeToRight e (guard b >> pure a)

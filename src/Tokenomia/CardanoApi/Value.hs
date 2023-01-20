module Tokenomia.CardanoApi.Value
    ( unLovelace
    , unQuantity
    ) where

import Cardano.Api
    ( Lovelace(..)
    , Quantity(..)
    )


unLovelace :: Lovelace -> Integer
unLovelace (Lovelace l) = l

unQuantity :: Quantity -> Integer
unQuantity (Quantity l) = l

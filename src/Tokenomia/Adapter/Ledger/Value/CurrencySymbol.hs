module Tokenomia.Adapter.Ledger.Value.CurrencySymbol
    ( shortHashView ) where

import           Ledger.Value

shortHashView :: CurrencySymbol -> String
shortHashView x = take (length (show x) `div` 2) (show x)
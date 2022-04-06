{-# LANGUAGE OverloadedStrings            #-}

module Tokenomia.TokenDistribution.Distribution.Recipient ( Recipient (..) ) where

import Ledger.Address           ( Address(..) )
import Ledger.Value             ( AssetClass )

data  Recipient
    = Recipient
    { address :: Address
    , amount :: Integer
    , assetClass :: AssetClass
    } deriving (Show)

module Tokenomia.TokenDistribution.Wallet.ChildAddress.ChildAddressRef
    ( maxChildAddressIndex
    ) where

import Data.List.NonEmpty ( NonEmpty )

import Tokenomia.Wallet.ChildAddress.ChildAddressRef ( ChildAddressIndex(..) )
import Tokenomia.TokenDistribution.Distribution      ( Distribution )


maxChildAddressIndex :: NonEmpty Distribution -> ChildAddressIndex
maxChildAddressIndex = ChildAddressIndex . fromIntegral . length

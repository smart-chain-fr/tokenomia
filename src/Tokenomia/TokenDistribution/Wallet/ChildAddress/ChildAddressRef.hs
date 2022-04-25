module Tokenomia.TokenDistribution.Wallet.ChildAddress.ChildAddressRef
    ( maxChildAddressIndexRequired
    ) where

import Data.List.NonEmpty ( NonEmpty )

import Tokenomia.Wallet.ChildAddress.ChildAddressRef ( ChildAddressIndex(..) )
import Tokenomia.TokenDistribution.Distribution      ( Distribution )


maxChildAddressIndexRequired :: NonEmpty Distribution -> ChildAddressIndex
maxChildAddressIndexRequired = ChildAddressIndex . fromIntegral . length

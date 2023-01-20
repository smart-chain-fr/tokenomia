module Tokenomia.CardanoApi.Arbitrary.Time
    () where

import Ouroboros.Consensus.BlockchainTime.WallClock.Types
    ( RelativeTime(..) )

import Test.Tasty.QuickCheck
    ( Arbitrary
    , arbitrary
    , getPositive
    )


instance Arbitrary RelativeTime where
    arbitrary = RelativeTime . fromInteger . getPositive <$> arbitrary

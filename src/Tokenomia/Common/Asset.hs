{-# LANGUAGE DerivingStrategies                        #-}
{-# LANGUAGE RecordWildCards                           #-}
module Tokenomia.Common.Asset
    ( Asset(..)
    , Assets(..)
    , ToValue(..)
    ) where

import Data.Set.NonEmpty                               ( NESet )
import Ledger.Value                                    ( AssetClass, Value, assetClassValue )


data    Asset
    =   Asset
    { assetClass :: AssetClass
    , amount :: Integer
    } deriving stock (Show)

type NonEmptySet = NESet

newtype Assets
    =   Assets (NonEmptySet Asset)
    deriving stock (Show)

class ToValue a where
    toValue :: a -> Value

instance ToValue Asset where
    toValue Asset{..}
        = assetClassValue assetClass amount

instance ToValue Assets where
    toValue (Assets xs)
        = foldMap toValue xs

{-# LANGUAGE ImportQualifiedPost          #-}
{-# LANGUAGE InstanceSigs                 #-}
{-# LANGUAGE RecordWildCards              #-}
{-# LANGUAGE TypeApplications             #-}

module Tokenomia.Common.MultiAsset
    ( MultiAsset(..)
    , MultiAssetFormat(..)
    , FromValue(..)
    , ToValue(..)
    , adjustAda
    , groupByAssetClass
    ) where

--
-- This wrapper module allows working on multiformat values
-- with a flat representation as a map indexed by asset classes
-- instead of the default nested maps representation
-- indexed in turns by the components of the asset class tuple.
--
-- Also, the underlying representation of plutus values
-- are not true maps but lists of (key, value) pairs.
-- This module provides a way to express multiformat values
-- that can be indexed by unique asset class keys.
--
-- Therefore, the type Value holds more inhabitants than MultiAsset.
-- When duplicate keys occur in a Value, since the map is built fromList
-- only the last value for the key is retained.
-- In other words, the conversion fromValue :: Value -> MultiAssetFormat
-- can suffer loss of data.
--
-- Otherwise, for any well-formed value, there is a bijection
-- between Value and MultiAsset. The following properies holds :
--      fromValue . toValue == id
--      toValue . fromValue == id
--

import Data.List.NonEmpty       ( NonEmpty, groupAllWith )
import Data.Map                 ( Map, fromList, toList, adjust, keysSet)
import Data.Set                 ( Set )

import Ledger.Value
    ( AssetClass(..)
    , Value(..)
    , assetClassValue
    , flattenValue
    )

import Tokenomia.Common.AssetClass  ( adaAssetClass )


newtype MultiAsset
    =   MultiAsset { unMultiAsset :: Map AssetClass Integer }
    deriving (Show, Eq, Ord )

newtype MultiAssetFormat
    =   MultiAssetFormat { unMultiAssetFormat :: Set AssetClass }
    deriving (Show, Eq, Ord)

class FromValue a where
    fromValue :: Value -> a

instance FromValue MultiAsset where
    fromValue :: Value -> MultiAsset
    fromValue value =
        MultiAsset . fromList $
            (\(cs, tn, a) -> (AssetClass (cs, tn), a)) <$> flattenValue value

instance FromValue MultiAssetFormat where
    fromValue :: Value -> MultiAssetFormat
    fromValue =
        MultiAssetFormat . keysSet . unMultiAsset . fromValue

class ToValue a where
    toValue :: a -> Value

instance ToValue MultiAsset where
    toValue :: MultiAsset -> Value
    toValue (MultiAsset xs) =
        mconcat $ uncurry assetClassValue <$> toList xs

groupByAssetClass :: (a -> Value) -> [a] -> [NonEmpty a]
groupByAssetClass value = groupAllWith $ fromValue @MultiAssetFormat . value

adjustAda :: Integer -> MultiAsset -> MultiAsset
adjustAda amount =
    wrap $ adjust (const amount) adaAssetClass
  where
    wrap f = MultiAsset . f . unMultiAsset

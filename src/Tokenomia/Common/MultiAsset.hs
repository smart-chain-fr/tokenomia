{-# LANGUAGE DerivingVia                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving   #-}
{-# LANGUAGE ImportQualifiedPost          #-}
{-# LANGUAGE InstanceSigs                 #-}
{-# LANGUAGE TypeApplications             #-}
{-# LANGUAGE TypeFamilies                 #-}

module Tokenomia.Common.MultiAsset
    ( MultiAsset(..)
    , MultiAssetFormat(..)
    , FromValue(..)
    , ToValue(..)

    -- Value
    , groupByAssetClass

    -- MultiAssetFormat
    , getFormat
    , singletonFormat
    , intersection
    , difference
    , withoutAdaFormat
    , restrictFormat
    , withoutFormat
    , restrictFormatWith

    -- MultiAsset
    , singleton
    , withoutAda
    , onlyAda
    , adjustAda

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
import Data.Map.Strict          ( Map, adjust, keysSet, restrictKeys, withoutKeys )
import Data.Set                 ( Set )

import Data.Map.Strict qualified as Map
    ( singleton
    , fromList
    , toList
    )

import Data.Set qualified as Set
    ( intersection
    , difference
    , singleton
    )

import Data.MonoTraversable     ( MonoFunctor, Element, omap )
import Data.Containers          ( MonoZip, ozipWith )

import Ledger.Value
    ( AssetClass(..)
    , Value(..)
    , assetClassValue
    , flattenValue
    )

import Tokenomia.Common.AssetClass
    ( adaAssetClass )

import Tokenomia.Common.Data.MonoTraversable.Instance
    ( Wrap(..) )


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type instance Element MultiAsset = Map AssetClass Integer

newtype MultiAsset
    =   MultiAsset { unMultiAsset :: Map AssetClass Integer }
    deriving stock   ( Show, Eq, Ord )
    deriving newtype ( Semigroup, Monoid )
    deriving         ( MonoFunctor, MonoZip ) via Wrap MultiAsset

type instance Element MultiAssetFormat = Set AssetClass

newtype MultiAssetFormat
    =   MultiAssetFormat { unMultiAssetFormat :: Set AssetClass }
    deriving stock   ( Show, Eq, Ord )
    deriving newtype ( Semigroup, Monoid)
    deriving         ( MonoFunctor, MonoZip ) via Wrap MultiAssetFormat

-------------------------------------------------------------------------------
-- Value
-------------------------------------------------------------------------------

class FromValue a where
    fromValue :: Value -> a

instance FromValue MultiAsset where
    fromValue :: Value -> MultiAsset
    fromValue value =
        MultiAsset . Map.fromList $
            (\(cs, tn, a) -> (AssetClass (cs, tn), a)) <$> flattenValue value

instance FromValue MultiAssetFormat where
    fromValue :: Value -> MultiAssetFormat
    fromValue = getFormat . fromValue

class ToValue a where
    toValue :: a -> Value

instance ToValue MultiAsset where
    toValue :: MultiAsset -> Value
    toValue (MultiAsset m) =
        mconcat $ uncurry assetClassValue <$> Map.toList m

groupByAssetClass :: (a -> Value) -> [a] -> [NonEmpty a]
groupByAssetClass value = groupAllWith $ fromValue @MultiAssetFormat . value

-------------------------------------------------------------------------------
-- MultiAssetFormat
-------------------------------------------------------------------------------

getFormat :: MultiAsset -> MultiAssetFormat
getFormat = MultiAssetFormat . keysSet . unMultiAsset

singletonFormat :: AssetClass -> MultiAssetFormat
singletonFormat = MultiAssetFormat . Set.singleton

intersection :: MultiAssetFormat -> MultiAssetFormat -> MultiAssetFormat
intersection = ozipWith Set.intersection

difference :: MultiAssetFormat -> MultiAssetFormat -> MultiAssetFormat
difference = ozipWith Set.difference

withoutAdaFormat :: MultiAssetFormat -> MultiAssetFormat
withoutAdaFormat = flip difference (singletonFormat adaAssetClass)

restrictFormat :: MultiAsset -> MultiAssetFormat -> MultiAsset
restrictFormat m s =
    MultiAsset $ unMultiAsset m `restrictKeys` unMultiAssetFormat s

withoutFormat :: MultiAsset -> MultiAssetFormat -> MultiAsset
withoutFormat m s =
    MultiAsset $ unMultiAsset m `withoutKeys` unMultiAssetFormat s

restrictFormatWith :: (a -> MultiAssetFormat -> MultiAssetFormat) -> a -> MultiAsset -> MultiAsset
restrictFormatWith (.*.) a m =
    let s = a .*. getFormat m
    in  m `restrictFormat` s

-------------------------------------------------------------------------------
-- MultiAsset
-------------------------------------------------------------------------------

singleton :: AssetClass -> Integer -> MultiAsset
singleton assetClass amount =
    MultiAsset $ Map.singleton assetClass amount

withoutAda :: MultiAsset -> MultiAsset
withoutAda m = m `restrictFormat` withoutAdaFormat (getFormat m)

onlyAda :: MultiAsset -> MultiAsset
onlyAda m = m `restrictFormat` singletonFormat adaAssetClass

adjustAda :: Integer -> MultiAsset -> MultiAsset
adjustAda amount =
    omap $ adjust (const amount) adaAssetClass

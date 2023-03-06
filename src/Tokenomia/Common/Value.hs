{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Tokenomia.Common.Value
  ( getTokenFrom
  , getTokensFrom
  , assetClassValueOfWith
  , containsAssetClass
  , maximumAssetClassValueOf
  , maximumByAssetClassValueOf
  , maximumByAssetClassValueOf'
  , containingOneToken
  , containingGivenNativeToken
  , containingOnlyGivenAssetClass
  , containingStrictlyADAs
  , containsCollateral
  , showValueUtf8) where

import Tokenomia.Common.Serialise

import Data.Function            ( on )
import Data.List                ( intersperse )
import Data.List.NonEmpty       ( NonEmpty )

import Text.Hex                 ( encodeHex )

import Plutus.V1.Ledger.Value
    ( AssetClass
    , CurrencySymbol
    , TokenName
    , Value
    , assetClassValueOf
    , flattenValue
    , singleton
    , symbols
    , toString
    )
import Plutus.V1.Ledger.Value qualified as Ledger ( assetClass )
import Ledger.Ada
import Data.Foldable
import qualified Data.Text                        as Text

import Data.Attoparsec.Text (parseOnly)

import Tokenomia.Common.Parser.Value qualified as Parser ( value )
import Tokenomia.Common.Data.Convertible ( convert )


getTokenFrom :: Value -> (CurrencySymbol,TokenName,Integer)
getTokenFrom  = head . filter (\(c,_,_) -> c /= adaSymbol ) .flattenValue  -- should contains only one native token (filtering ADAs)

getTokensFrom :: Value -> Value
getTokensFrom = mkValue . filter (\(c,_,_) -> c /= adaSymbol ) .flattenValue  -- should contains only one native token (filtering ADAs)


mkValue :: [(CurrencySymbol, TokenName, Integer)] -> Value
mkValue = foldMap (\(a,b,c) -> singleton a b c)

containingOneToken :: Value -> Bool
containingOneToken value
    = 1 == (length . filter (\(c,_,_) -> c /= adaSymbol ) .flattenValue) value

containingGivenNativeToken :: CurrencySymbol -> Value -> Bool
containingGivenNativeToken policyhash value
    = 1 == (length . filter (\(c,_,_) -> c == policyhash ) .flattenValue) value

containingOnlyGivenAssetClass :: AssetClass -> Value -> Bool
containingOnlyGivenAssetClass givenAssettClass value
    = 1 == (length . filter (\(c,tn,_) -> Ledger.assetClass c tn  == givenAssettClass ) .flattenValue) value

-- | Check if a value contains the right amount of an asset class.
assetClassValueOfWith :: (Integer -> Bool) -> Value -> AssetClass -> Bool
assetClassValueOfWith predicate = (predicate .) . assetClassValueOf

-- | Check if a value contains a non-zero amount of an asset class.
containsAssetClass :: Value -> AssetClass -> Bool
containsAssetClass = assetClassValueOfWith (/=0)

-- | Maximum amount of an asset class in a list of values.
maximumAssetClassValueOf :: NonEmpty Value -> AssetClass -> Integer
maximumAssetClassValueOf values assetClass =
    maximum $ flip assetClassValueOf assetClass <$> values

-- | Element with the maximum amount of an asset class in a list of values.
maximumByAssetClassValueOf :: NonEmpty Value -> AssetClass -> Value
maximumByAssetClassValueOf = maximumByAssetClassValueOf' id

-- | Element with the maximum amount of an asset class in a list convertible to values.
maximumByAssetClassValueOf' :: (a -> Value) -> NonEmpty a -> AssetClass -> a
maximumByAssetClassValueOf' value xs assetClass =
    maximumBy (compare `on` flip assetClassValueOf assetClass . value) xs

containsCollateral :: Value -> Bool
containsCollateral = (adaValueOf 2.0 ==)

containingStrictlyADAs :: Value -> Bool
containingStrictlyADAs value
    = symbols value == [adaSymbol]

instance ToCLI Value where
  toCLI =
    Text.pack
    . fold
    . intersperse " + "
    . map (\case
            ("","",c) -> show c <> " lovelace"
            (a,"" ,c) -> show c <> " " <> show a <> " "
            (a, b ,c) -> show c <> " " <> show a <> "." <> showHexadecimal b <> " "  )
    . reverse
    . lovelacesFirst
    . flattenValue

showHexadecimal :: TokenName -> String
showHexadecimal = convert . encodeHex . convert . toString

showValueUtf8 :: Value -> String
showValueUtf8 =
  fold
  . intersperse " + "
  . map (\case
           ("","",c) -> show c <> " lovelace"
           (a, "" ,c) -> show c <> " " <> show a <> " "
           (a, b ,c) -> show c <> " " <> show a <> "." <> toString b <> " "  )
  . reverse
  . lovelacesFirst
  . flattenValue


lovelacesFirst :: [(CurrencySymbol,TokenName,Integer)] -> [(CurrencySymbol,TokenName,Integer)]
lovelacesFirst values = let (x,y) =  span (\(c,_,_) -> c == adaSymbol) values in x <> y

instance FromCLI Value where
    fromCLI input = either (error "") id (parseOnly Parser.value input)

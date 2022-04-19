{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Tokenomia.Common.Value 
  ( getTokenFrom
  , getTokensFrom
  , containingOneToken
  , containingGivenNativeToken
  , containingOnlyGivenAssetClass
  , containingStrictlyADAs
  , containsCollateral
  , showValueUtf8) where

import Tokenomia.Common.Serialise

import Data.List as L

import Plutus.V1.Ledger.Value
import Ledger.Ada
import Data.String
import Data.Foldable
import qualified Data.Text                        as Text

import qualified Data.Text.Encoding as TSE
import Text.Hex (decodeHex)
import Data.Attoparsec.Text (parseOnly)

import Tokenomia.Common.Parser.Value qualified as Parser ( value )

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
    = 1 == (length . filter (\(c,tn,_) -> assetClass c tn  == givenAssettClass ) .flattenValue) value

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
            --(a, b ,c) -> show c <> " " <> show a <> "." <> showHexadecimal b <> " "  )
            (a, b ,c) -> show c <> " " <> show a <> "." <> toString b <> " "  )
    . reverse
    . lovelacesFirst
    . flattenValue

--showHexadecimal :: TokenName -> String
--showHexadecimal = Text.unpack . encodeHex . BSU.fromString . toString
--showHexadecimal = Text.unpack . encodeHex . fromBuiltin . unTokenName

decodeToUtf8' :: TokenName -> String 
decodeToUtf8' name 
  = case decodeHex (Text.pack $ toString name) of
      Nothing -> error ("unexpected tokenName format " <> toString name <> " should be hex.")
      Just hex -> (Text.unpack . TSE.decodeUtf8) hex

showValueUtf8 :: Value -> String
showValueUtf8 =
  fold
  . intersperse " + "
  . map (\case
           ("","",c) -> show c <> " lovelace"
           (a, "" ,c) -> show c <> " " <> show a <> " "
           (a, b ,c) -> show c <> " " <> show a <> "." <> decodeToUtf8' b <> " "  )
  . reverse
  . lovelacesFirst
  . flattenValue


lovelacesFirst :: [(CurrencySymbol,TokenName,Integer)] -> [(CurrencySymbol,TokenName,Integer)]
lovelacesFirst values = let (x,y) =  span (\(c,_,_) -> c == adaSymbol) values in x <> y

instance FromCLI Value where
    fromCLI input = either (error "") id (parseOnly Parser.value input)

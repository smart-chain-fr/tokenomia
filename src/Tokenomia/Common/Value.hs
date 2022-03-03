{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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

import qualified Data.Text as T
import Data.Text (Text)

import Data.List as L

import Plutus.V1.Ledger.Value
import Ledger.Ada
import Data.String
import Data.Foldable
import qualified Data.Text                        as Text
import qualified Data.Text.Encoding as TSE
import Text.Hex (decodeHex)

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
    T.pack
    . fold
    . intersperse " + "
    . map (\case
            ("","",c) -> show c <> " lovelace"
            (a, b ,c) -> show c <> " " <> show a <> "." <> toString b <> " "  )
    . reverse
    . lovelacesFirst
    . flattenValue

decodeToUtf8' :: TokenName -> String 
decodeToUtf8' name 
  = case decodeHex (Text.pack $ toString name) of
      Nothing -> error "unexpected tokenName format " <> toString name <> " should be hex."
      Just hex -> (Text.unpack . TSE.decodeUtf8) hex

showValueUtf8 :: Value -> String
showValueUtf8 =
  fold
  . intersperse " + "
  . map (\case
           ("","",c) -> show c <> " lovelace"
           (a, b ,c) -> show c <> " " <> show a <> "." <> decodeToUtf8' b <> " "  )
  . reverse
  . lovelacesFirst
  . flattenValue


lovelacesFirst :: [(CurrencySymbol,TokenName,Integer)] -> [(CurrencySymbol,TokenName,Integer)]
lovelacesFirst values = let (x,y) =  span (\(c,_,_) -> c == adaSymbol) values in x <> y


instance FromCLI Value  where
   fromCLI = parse . tokenize
     where 
      parse ::  [(Text,Text,Text)] -> Value
      parse =
        foldMap (
          ( \(a,b,c) -> singleton a b c) 
          .(\(a,b,c) ->
              ( (fromString . T.unpack )    a
              , (fromString . T.unpack)     b
              , (read @Integer . T.unpack)  c )))

      tokenize :: Text ->  [(Text,Text,Text)]
      tokenize 
        = tokenizeToken
          . removeDatum
          . map T.words
          . T.splitOn "+" 
        where
          tokenizeToken :: [[Text]] -> [(Text,Text,Text)]
          tokenizeToken 
            = fmap (\case
                [a,"lovelace"] -> ("","", a)
                [a,b] -> case T.splitOn "." b of
                          [ph,tn] -> (ph,tn,a)
                          x -> error $ "unexpected format :" <> show (T.unpack <$> x)
                x ->  error $ "unexpected format :" <> show (T.unpack <$> x) )
          removeDatum = L.filter (\a -> 2 == L.length a)
{-# LANGUAGE OverloadedStrings            #-}
{-# LANGUAGE ImportQualifiedPost          #-}
{-# LANGUAGE FlexibleInstances            #-}

module Tokenomia.TokenDistribution.Distribution
    ( Distribution(..)
    , Recipient(..)
    , WithNetworkId(..)
    , countRecipients
    , readDistributionFile
    ) where

import Cardano.Api              ( NetworkId )
import Data.Text                ( pack, unpack )
import Data.ByteString.Lazy     ( readFile )
import Data.String              ( IsString(fromString) )
import Data.Aeson.Types         ( Parser, parseJSON )
import Data.Aeson
    ( FromJSON
    , ToJSON (toJSON)
    , Value
    , eitherDecode
    , object
    , withObject
    , (.:)
    , (.=)
    )
import Data.Either.Combinators  ( fromRight' )

import Prelude           hiding ( readFile, lines )

import Ledger.Address                   ( Address(..) )
import Ledger.Value                     ( AssetClass )
import Ledger.Value qualified as Ledger ( assetClass )
import Plutus.V1.Ledger.Value qualified as Value

import Tokenomia.TokenDistribution.CLI.Parameters
   ( Parameters(..) )
import Tokenomia.TokenDistribution.Parser.Address
   ( deserialiseCardanoAddress, serialiseCardanoAddress )

data  Recipient
    = Recipient
    { address :: Address
    , amount :: Integer
    } deriving (Show)

data  Distribution
    = Distribution
    { assetClass :: AssetClass
    , recipients :: [Recipient]
    } deriving (Show)

-- Unfortunately, address serialisation requires a network Id. As such, one cannot correctly ToJSON a Distribution.
-- Instead, we use a proxy type that allows us to provide this information
data WithNetworkId a = WithNetworkId a NetworkId

instance FromJSON Recipient where
    parseJSON = withObject "Recipient" $ \o ->
        Recipient
            <$> (o .: "address" >>= addressParser)
            <*> (o .: "amount")
      where
        addressParser :: String -> Parser Address
        addressParser s =
            either
                (fail . unpack)
                pure
                (deserialiseCardanoAddress . pack $ s)

instance ToJSON (WithNetworkId Recipient) where
    toJSON (Recipient addr amt `WithNetworkId` netId) =
        object
            [ "address" .= fromRight' (serialiseCardanoAddress netId addr)
            , "amount" .= amt
            ]

instance FromJSON Distribution where
    parseJSON = withObject "Distribution" $ \o ->
        Distribution
            <$> (o .: "assetClass" >>= assetClassParser)
            <*> (o .: "recipients")
      where
        assetClassParser :: Value -> Parser AssetClass
        assetClassParser = withObject "AssetClass" $ \o ->
            Ledger.assetClass
                <$> (fromString <$> o .: "currencySymbol")
                <*> (fromString <$> o .: "tokenName")

instance ToJSON (WithNetworkId Distribution) where
    toJSON (Distribution (Value.AssetClass (cs, tn)) recips `WithNetworkId` netId) =
        object
            [ "assetClass" .= object [ "currencySymbol" .= show cs, "tokenName" .= show tn ]
            , "recipients" .= toJSON (flip WithNetworkId netId <$> recips)
            ]

countRecipients :: Distribution -> Integer
countRecipients = toInteger . length . recipients

readDistributionFile :: Parameters -> IO (Either String Distribution)
readDistributionFile parameters =
    eitherDecode <$> readFile (distributionFilePath parameters)

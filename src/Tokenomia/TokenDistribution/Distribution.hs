{-# LANGUAGE OverloadedStrings            #-}
{-# LANGUAGE ImportQualifiedPost          #-}

module Tokenomia.TokenDistribution.Distribution
    ( Distribution(..)
    , Recipient(..)
    , countRecipients
    , readDistributionFile
    ) where

import Data.Text                ( pack, unpack )
import Data.ByteString.Lazy     ( readFile )
import Data.String              ( IsString(fromString) )
import Data.Aeson.Types         ( Parser, parseJSON )
import Data.Aeson
    ( FromJSON
    , Value
    , eitherDecode
    , withObject
    , (.:)
    )

import Prelude           hiding ( readFile, lines )

import Ledger.Address                   ( Address(..) )
import Ledger.Value                     ( AssetClass )
import Ledger.Value qualified as Ledger ( assetClass )

import Tokenomia.TokenDistribution.CLI.Parameters
   ( Parameters(..) )
import Tokenomia.TokenDistribution.Parser.Address
   ( deserialiseCardanoAddress )

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

countRecipients :: Distribution -> Integer
countRecipients = toInteger . length . recipients

readDistributionFile :: Parameters -> IO (Either String Distribution)
readDistributionFile parameters =
    eitherDecode <$> readFile (distributionFilePath parameters)

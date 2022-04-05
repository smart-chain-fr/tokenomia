{-# LANGUAGE OverloadedStrings            #-}

module Tokenomia.TokenDistribution.Distribution ( readDistributionFile ) where

import Ledger.Address           ( Address(..) )
import Ledger.Value             ( AssetClass )

import Data.Char                ( isSpace )
import Data.Text                ( Text, lines )
import Data.Text.IO             ( readFile )

import Prelude           hiding ( readFile, lines )

import Data.Attoparsec.Text
    ( Parser
    , parseOnly
    , decimal
    , skipSpace
    , takeTill
    )

import Tokenomia.TokenDistribution.Config
import Tokenomia.TokenDistribution.Parser.Address
import Tokenomia.TokenDistribution.Parser.AssetClass

data  Recipient
    = Recipient
    { recipientAddress :: !Address
    , recipientAmount :: !Integer
    , recipientAssetClass :: !AssetClass
    } deriving (Show)

recipientParser :: Parser Recipient
recipientParser = do
    address    <- addressParser $ takeTill isSpace
    amount     <- skipSpace *> decimal
    assetClass <- skipSpace *> assetClassParser 

    return $ Recipient address amount assetClass

parseRecipient :: Text -> Either String Recipient
parseRecipient recipient =
    parseOnly recipientParser recipient

parseDistributionFile :: Text -> Either String [Recipient]
parseDistributionFile content =
    traverse parseRecipient (lines content)

readDistributionFile :: Config -> IO (Either String [Recipient])
readDistributionFile config = 
    readFile filePath >>= return . parseDistributionFile
  where
    filePath = configDistributionFile config

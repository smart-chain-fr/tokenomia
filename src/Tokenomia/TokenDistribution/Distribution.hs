{-# LANGUAGE OverloadedStrings            #-}
{-# LANGUAGE RecordWildCards              #-}

module Tokenomia.TokenDistribution.Distribution ( readDistributionFile ) where

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

import Tokenomia.TokenDistribution.CLI.Parameters
import Tokenomia.TokenDistribution.Parser.Address
import Tokenomia.TokenDistribution.Parser.AssetClass
import Tokenomia.TokenDistribution.Distribution.Recipient

recipientParser :: Parser Recipient
recipientParser = do
    address    <- addressParser $ takeTill isSpace
    amount     <- skipSpace *> decimal
    assetClass <- skipSpace *> assetClassParser 

    return $ Recipient {..}

parseRecipient :: Text -> Either String Recipient
parseRecipient recipient =
    parseOnly recipientParser recipient

parseDistributionFile :: Text -> Either String [Recipient]
parseDistributionFile content =
    traverse parseRecipient (lines content)

readDistributionFile :: Parameters -> IO (Either String [Recipient])
readDistributionFile config = 
    readFile filePath >>= return . parseDistributionFile
  where
    filePath = distributionFilePath config

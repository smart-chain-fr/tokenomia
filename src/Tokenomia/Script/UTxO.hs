{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tokenomia.Script.UTxO (
  ScriptUTxO (..),
) where

import Tokenomia.Common.Shell.InteractiveMenu

import Data.List qualified as L
import Data.Text (Text)
import Data.Text qualified as T

import Data.String
import Ledger.Value
import Tokenomia.Common.Hash
import Tokenomia.Common.Serialise
import Tokenomia.Common.TxOutRef
import Tokenomia.Common.Value

data ScriptUTxO = ScriptUTxO
  { txOutRef :: TxOutRef
  , value :: Value
  , datumHash :: Hash
  }
  deriving stock (Eq)

instance Show ScriptUTxO where
  show ScriptUTxO {..} = showTxOutRef txOutRef <> " : " <> showValueUtf8 value

instance DisplayMenuItem ScriptUTxO where
  displayMenuItem ScriptUTxO {..} = showTxOutRef txOutRef <> " : " <> showValueUtf8 value

instance ToCLI ScriptUTxO where
  toCLI ScriptUTxO {..} = T.pack (showTxOutRef txOutRef <> " : ") <> toCLI value

instance FromCLI [ScriptUTxO] where
  fromCLI = parse . tokenize
    where
      parse :: [(Text, Text, [Text])] -> [ScriptUTxO]
      parse =
        fmap
          ( \(txHash, txIx, x) ->
              ScriptUTxO
                { txOutRef = TxOutRef ((fromString . T.unpack) txHash) ((read @Integer . T.unpack) txIx)
                , value = (fromCLI . T.unwords) x
                , datumHash = Hash "TODO"
                }
          )

      tokenize :: Text -> [(Text, Text, [Text])]
      tokenize =
        map
          ( ( \a ->
                ( L.head a
                , a L.!! 1
                , (filterEmptyLines . L.drop 2) a
                )
            )
              . T.words
          )
          . (removeHeader . filterEmptyLines . T.lines)

      removeHeader :: [Text] -> [Text]
      removeHeader = L.drop 2

      filterEmptyLines :: [Text] -> [Text]
      filterEmptyLines = L.filter (\a -> T.strip a /= mempty)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tokenomia.Script.UTxO
    ( ScriptUTxO (..)
    ) where

import Tokenomia.Common.Shell.InteractiveMenu

import qualified Data.Text as T
import           Data.Text (Text)
import           Data.List as L
           
import           Data.String
import           Tokenomia.Common.Serialise
import           Tokenomia.Common.Value 
import           Ledger.Value
import           Tokenomia.Common.TxOutRef 
import           Tokenomia.Common.Hash

data ScriptUTxO = ScriptUTxO
              { txOutRef :: TxOutRef
              , value :: Value
              , datumHash :: Hash} deriving (Eq)


instance Show ScriptUTxO where
  show ScriptUTxO {..} = showTxOutRef txOutRef <> " : " <> showValueUtf8 value


instance DisplayMenuItem ScriptUTxO where
  displayMenuItem ScriptUTxO {..} = showTxOutRef txOutRef <> " : " <> showValueUtf8 value


instance ToCLI ScriptUTxO where
  toCLI ScriptUTxO {..} = T.pack (showTxOutRef txOutRef <> " : ") <> toCLI value

instance FromCLI [ScriptUTxO] where
  fromCLI = parse . tokenize
    where
      parse :: [(Text,Text,[Text])] -> [ScriptUTxO]
      parse = fmap (\(txHash,txIx,x) ->
                      ScriptUTxO
                        { txOutRef = TxOutRef((fromString . T.unpack) txHash) ((read @Integer . T.unpack)  txIx)
                        , value = ( fromCLI . T.unwords ) x
                        , datumHash = Hash "TODO" })


      tokenize :: Text ->  [(Text,Text,[Text])]
      tokenize
          =  map (\a ->
                  ( L.head a
                  , a L.!! 1
                  , (filterEmptyLines . L.drop 2) a ))
          .  map T.words
          .  (removeHeader . filterEmptyLines . T.lines)

      removeHeader :: [Text] -> [Text]
      removeHeader = L.drop 2

      filterEmptyLines :: [Text] -> [Text]
      filterEmptyLines = L.filter (\a -> T.strip a /= mempty )
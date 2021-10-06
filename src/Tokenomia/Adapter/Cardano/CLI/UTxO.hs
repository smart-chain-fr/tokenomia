{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tokenomia.Adapter.Cardano.CLI.UTxO 
    ( UTxO (..)) where

import Tokenomia.Common.Shell.InteractiveMenu

import qualified Data.Text as T
import Data.Text (Text)
import Data.List as L

import Ledger ( TxOutRef (..) )
import Plutus.V1.Ledger.Value

import Data.String
import Tokenomia.Adapter.Cardano.CLI.Serialise 
import Tokenomia.Adapter.Cardano.CLI.Value ()
import Data.Foldable ( Foldable(fold) )


data UTxO = UTxO 
              { txOutRef :: TxOutRef
              , value :: Value} deriving (Eq)

instance Show UTxO where 
  show UTxO {..} = showTxOutRef txOutRef <> " : " <> showValue value

instance DisplayMenuItem UTxO where
  displayMenuItem UTxO {..} = showTxOutRef txOutRef <> " : " <> showValue value

instance ToCLI TxOutRef where
  toCLI = T.pack .showTxOutRef

showTxOutRef :: TxOutRef -> String
showTxOutRef TxOutRef {..} = show txOutRefId <> "#" <> show txOutRefIdx

showValue :: Value -> String
showValue =
  fold
  . intersperse " + "
  . map (\case 
           ("","",c) -> show c <> " Lovelace"
           (a, b ,c) -> show c <> " " <> show a <> "." <> toString b <> " "  ) 
  . flattenValue

instance FromCLI [UTxO] where 
  fromCLI = parse . tokenize
    where 
      parse :: [(Text,Text,[Text])] -> [UTxO]
      parse = fmap (\(txHash,txIx,x) ->
                      UTxO 
                        { txOutRef = TxOutRef((fromString . T.unpack) txHash) ((read @Integer . T.unpack)  txIx)
                        , value = ( fromCLI . T.unwords ) x})


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




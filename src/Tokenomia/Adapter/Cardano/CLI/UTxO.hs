{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE RecordWildCards #-}


{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Tokenomia.Adapter.Cardano.CLI.UTxO
    ( UTxO (..)
    , getTokenFrom
    , getTokensFrom
    , containingOneToken
    , containingStrictlyADAs
    , containsCollateral
    ) where

import Tokenomia.Common.Shell.InteractiveMenu

import qualified Data.Text as T
import Data.Text (Text)
import Data.List as L

import Ledger ( TxOutRef (..) )

import Data.String
import Tokenomia.Adapter.Cardano.CLI.Serialise
import Tokenomia.Adapter.Cardano.CLI.Value ()
import Data.Foldable ( Foldable(fold) )
import Plutus.V1.Ledger.Ada
import Ledger.Value

type Address = String



getTokenFrom :: UTxO -> (CurrencySymbol,TokenName,Integer)
getTokenFrom UTxO {..} = (head . filter (\(c,_,_) -> c /= adaSymbol ) .flattenValue) value -- should contains only one native token (filtering ADAs) 

getTokensFrom :: Value -> Value
getTokensFrom = mkValue . filter (\(c,_,_) -> c /= adaSymbol ) .flattenValue  -- should contains only one native token (filtering ADAs) 


mkValue :: [(CurrencySymbol, TokenName, Integer)] -> Value
mkValue = foldMap (\(a,b,c) -> singleton a b c)

containingOneToken :: UTxO -> Bool
containingOneToken UTxO {..}
    = 1 == (length . filter (\(c,_,_) -> c /= adaSymbol ) .flattenValue) value


containsCollateral :: UTxO -> Bool
containsCollateral UTxO {..}
   = adaValueOf 2.0 == value

containingStrictlyADAs :: UTxO -> Bool
containingStrictlyADAs UTxO {..} 
    = 1 == (length . filter (\(c,_,_) -> c == adaSymbol ) .flattenValue) value

data UTxO = UTxO
              { txOutRef :: TxOutRef
              , value :: Value} deriving (Eq)

instance Show UTxO where
  show UTxO {..} = showTxOutRef txOutRef <> " : " <> showValue value

instance DisplayMenuItem UTxO where
  displayMenuItem UTxO {..} = showTxOutRef txOutRef <> " : " <> showValue value

instance ToCLI TxOutRef where
  toCLI = T.pack . showTxOutRef

showTxOutRef :: TxOutRef -> String
showTxOutRef TxOutRef {..} = show txOutRefId <> "#" <> show txOutRefIdx

instance ToCLI Value where
  toCLI = T.pack . showValue

showValue :: Value -> String
showValue =
  fold
  . intersperse " + "
  . map (\case
           ("","",c) -> show c <> " lovelace"
           (a, b ,c) -> show c <> " " <> show a <> "." <> toString b <> " "  )
  . reverse
  . lovelacesFirst
  . flattenValue

lovelacesFirst :: [(CurrencySymbol,TokenName,Integer)] -> [(CurrencySymbol,TokenName,Integer)]
lovelacesFirst values = let (x,y) =  span (\(c,_,_) -> c == adaSymbol) values in x <> y


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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tokenomia.Wallet.UTxO
    ( WalletUTxO (..)
    , UTxO (..)
    ) where

import Tokenomia.Common.Shell.InteractiveMenu
    ( DisplayMenuItem(..) )

import qualified Data.Text as T
import           Data.Text (Text)
import           Data.List as L ( filter, (!!), drop, head )
          
import           Ledger ( TxOutRef (..) )
          
import           Data.String ( IsString(fromString) )
import           Tokenomia.Common.Serialise ( FromCLI(..) )
import           Tokenomia.Common.Value ( showValue ) 
import           Ledger.Value ( Value )
import           Tokenomia.Common.TxOutRef ( showTxOutRef ) 
import           Tokenomia.Wallet.Type () 
import           Tokenomia.Wallet.ChildAddress.ChildAddressRef
import           Tokenomia.Common.Hash    

data WalletUTxO = WalletUTxO
              { childAddressRef :: ChildAddressRef 
              , utxo :: UTxO} deriving (Eq)

data UTxO = UTxO
              { txOutRef :: TxOutRef
              , value :: Value
              , maybeDatumHash :: Maybe Hash } deriving (Eq)

instance Show WalletUTxO where
  show WalletUTxO {utxo = UTxO {..}} = showTxOutRef txOutRef <> " : " <> showValue value

instance DisplayMenuItem WalletUTxO where
  displayMenuItem WalletUTxO {utxo = UTxO {..}} = showTxOutRef txOutRef <> " : " <> showValue value


instance FromCLI [UTxO] where
  fromCLI = parse . tokenize
    where
      parse :: [(Text,Text,[Text])] -> [UTxO]
      parse = fmap (\(txHash,txIx,x) ->
                      UTxO
                        { txOutRef = TxOutRef((fromString . T.unpack) txHash) ((read @Integer . T.unpack)  txIx)
                        , value = ( fromCLI . T.unwords ) x
                        , maybeDatumHash = Nothing})


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

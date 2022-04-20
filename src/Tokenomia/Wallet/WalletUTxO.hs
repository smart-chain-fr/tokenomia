{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tokenomia.Wallet.WalletUTxO
    ( WalletUTxO (..)
    , getAdas
    , getValue
    , getDatumHashesAndAdaStrict
    ) where

import Tokenomia.Common.Shell.InteractiveMenu
    ( DisplayMenuItem(..) )

import           Prelude as P
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Maybe
import           Data.List as L ( filter, (!!), drop, head )
import           Ledger.Ada          
import           Ledger ( TxOutRef (..) )
import           Control.Monad.Except
import           Data.String ( IsString(fromString) )
import           Tokenomia.Common.Serialise ( FromCLI(..) )
import           Tokenomia.Common.Error
import           Ledger.Value ( Value )
import           Tokenomia.Common.TxOutRef ( showTxOutRef ) 
import           Tokenomia.Wallet.Type () 
import           Tokenomia.Wallet.ChildAddress.ChildAddressRef
import           Tokenomia.Common.Hash    
import           Tokenomia.Common.Value    
import qualified Data.List.NonEmpty as NEL
import Tokenomia.Wallet.UTxO

data  WalletUTxO
    = WalletUTxO
    { childAddressRef :: ChildAddressRef 
    , utxo :: UTxO
    } deriving ( Eq )

getValue :: WalletUTxO -> Value
getValue = value . utxo 

getAdas :: WalletUTxO -> Ada 
getAdas = fromValue . getValue


getDatumHashesAndAdaStrict 
  :: (MonadError  TokenomiaError m)
  => NEL.NonEmpty WalletUTxO -> m (NEL.NonEmpty (Hash,Ada,WalletUTxO))
getDatumHashesAndAdaStrict xs = 
    (return . NEL.nonEmpty . catMaybes . NEL.toList $ ( getDatumHashAndAdaMaybe <$> xs)) 
      >>= whenNothingThrow ICOExchangeUtxoWithoutHash 
      >>= (\case
          ys | NEL.length ys /= NEL.length xs -> throwError ICOExchangeUtxoWithoutHash
          ys -> return ys ) 

getDatumHashAndAdaMaybe :: WalletUTxO -> Maybe (Hash,Ada,WalletUTxO)
getDatumHashAndAdaMaybe w@WalletUTxO {utxo = UTxO {maybeDatumHash = Just hash,..}} | containingStrictlyADAs value = Just (hash,fromValue value,w)
getDatumHashAndAdaMaybe _ = Nothing

instance Ord WalletUTxO where 
  compare x y = compare (utxo x) (utxo y)

instance Show WalletUTxO where
  show WalletUTxO {utxo = UTxO {maybeDatumHash = Nothing , ..}} = showTxOutRef txOutRef <> " : " <> showValueUtf8 value
  show WalletUTxO {utxo = UTxO {maybeDatumHash = Just datumHash , ..}} = showTxOutRef txOutRef <> " : " <> showValueUtf8 value <>  " | " <> show datumHash

instance DisplayMenuItem WalletUTxO where
  displayMenuItem WalletUTxO {utxo = UTxO {..}} = showTxOutRef txOutRef <> " : " <> showValueUtf8 value
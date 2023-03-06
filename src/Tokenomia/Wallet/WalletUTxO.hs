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
    , value
    ) where

import Tokenomia.Common.Shell.InteractiveMenu
    ( DisplayMenuItem(..) )

import           Prelude as P
import           Data.Maybe
import           Data.List ( intercalate )
import           Ledger.Ada
import           Control.Monad.Except
import           Tokenomia.Common.Error
import           Ledger.Value ( Value )
import           Tokenomia.Common.TxOutRef ( showTxOutRef ) 
import           Tokenomia.Wallet.Type () 
import           Tokenomia.Wallet.ChildAddress.ChildAddressRef
import           Tokenomia.Common.Hash    
import           Tokenomia.Common.Value    
import qualified Data.List.NonEmpty as NEL
import           Tokenomia.Wallet.UTxO hiding  ( value )
import qualified Tokenomia.Wallet.UTxO as UTxO ( value )

data  WalletUTxO
    = WalletUTxO
    { childAddressRef :: ChildAddressRef 
    , utxo :: UTxO
    } deriving ( Eq )

value :: WalletUTxO -> Value
value = UTxO.value . utxo

getAdas :: WalletUTxO -> Ada
getAdas = fromValue . value

getDatumHashAndAdaMaybe :: WalletUTxO -> Maybe (Hash,Ada,WalletUTxO)
getDatumHashAndAdaMaybe w@WalletUTxO {utxo = UTxO {maybeDatumHash = Just hash,..}} | containingStrictlyADAs value = Just (hash,fromValue value,w)
getDatumHashAndAdaMaybe _ = Nothing

instance Ord WalletUTxO where
    compare x y = compare (utxo x) (utxo y)

-- | Intercalate non-null elements.
sepBy :: [a] -> [[a]] -> [a]
sepBy sep xs = intercalate sep $ filter (not . null) xs
 
showWalletUTxO :: WalletUTxO -> String
showWalletUTxO walletUTxO  = sepBy " : " $
        [ showTxOutRef . txOutRef . utxo
        , showValueUtf8 . value
        ]
    <*> pure walletUTxO

showDatumHash :: WalletUTxO -> String
showDatumHash walletUTxO =
    maybe "" show (maybeDatumHash . utxo $ walletUTxO)

showWalletUTxOWithDatumHash :: WalletUTxO -> String
showWalletUTxOWithDatumHash walletUTxO = sepBy " | " $
        [ showWalletUTxO
        , showDatumHash 
        ]
    <*> pure walletUTxO

instance Show WalletUTxO where
    show = showWalletUTxOWithDatumHash

instance DisplayMenuItem WalletUTxO where
    displayMenuItem = showWalletUTxO

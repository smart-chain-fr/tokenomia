{-# LANGUAGE DerivingStrategies                        #-}
{-# LANGUAGE DuplicateRecordFields                     #-}
{-# LANGUAGE FlexibleContexts                          #-}
{-# LANGUAGE FlexibleInstances                         #-}
{-# LANGUAGE ImportQualifiedPost                       #-}
{-# LANGUAGE OverloadedStrings                         #-}
{-# LANGUAGE RecordWildCards                           #-}
{-# OPTIONS_GHC -Wno-name-shadowing                    #-}
{-# OPTIONS_GHC -Wno-orphans                           #-}

module Tokenomia.Wallet.WalletUTxO
    ( WalletUTxO(..)
    , getAdas
    , getDatumHashAndAdaMaybe
    , value
    ) where

import Tokenomia.Common.Shell.InteractiveMenu          ( DisplayMenuItem(..) )

import Data.List                                       ( intercalate )
import Ledger.Ada                                      ( Ada, fromValue )
import Ledger.Value                                    ( Value )
import Prelude as P
import Tokenomia.Common.Hash                           ( Hash )
import Tokenomia.Common.TxOutRef                       ( showTxOutRef )
import Tokenomia.Common.Value                          ( containingStrictlyADAs, showValueUtf8 )
import Tokenomia.Wallet.ChildAddress.ChildAddressRef   ( ChildAddressRef )
import Tokenomia.Wallet.Type                           ()
import Tokenomia.Wallet.UTxO                           ( UTxO(UTxO, maybeDatumHash, txOutRef) )
import Tokenomia.Wallet.UTxO qualified
    as UTxO                                            ( value )

data  WalletUTxO
    = WalletUTxO
    { childAddressRef :: ChildAddressRef
    , utxo :: UTxO
    } deriving stock ( Eq )

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

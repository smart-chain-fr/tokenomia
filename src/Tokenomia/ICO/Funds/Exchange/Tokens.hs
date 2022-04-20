{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tokenomia.ICO.Funds.Exchange.Tokens
    ( fetchTokens
    , ExchangeToken (..) ) where


import           Prelude hiding (round,print)


import Tokenomia.Wallet.ChildAddress.ChildAddressRef

import Tokenomia.Wallet.UTxO
import Tokenomia.Wallet.WalletUTxO

import Control.Monad.Reader


import Data.List.NonEmpty as NEL

import           Control.Monad.Except

import           Tokenomia.Common.Error
import           Tokenomia.ICO.Round.Settings
import Tokenomia.Common.Value
import Tokenomia.Wallet.CLI
import           Tokenomia.Common.Environment
import Tokenomia.Common.Token
import Plutus.V1.Ledger.Value as Ledger
import Tokenomia.ICO.Balanceable
import Ledger.Ada

data ExchangeToken
    = ExchangeToken
        { source :: WalletUTxO
        , token  :: Token} deriving Show

fetchTokens
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError  TokenomiaError m)
    => RoundSettings
    -> m (Maybe ExchangeToken)
fetchTokens RoundSettings {addresses = RoundAddresses {tokens = IndexedAddress {childAddressRef = tokensAddress}},..}
    = fetchUTxOFilterBy (containingOnlyGivenAssetClass exchangeTokenId . value . utxo) tokensAddress
        >>= \case
                Nothing -> return Nothing
                Just (source :| []) -> do
                    let (c,tn,amount) = getTokenFrom ((value . utxo) source)
                    (return . Just)
                        ExchangeToken
                            { token = Token { assetClass = Ledger.assetClass c tn, amount = amount, minimumAdaRequired = (fromValue . value . utxo) source }
                            , source = source}
                Just _ -> throwError ICOTokensDispatchedOnMultipleUTxOs


instance AdaBalanceable ExchangeToken where 
    adaBalance ExchangeToken {..} = (fromValue . value . utxo) source

instance TokenBalanceable ExchangeToken where 
    tokenBalance ExchangeToken {..} =  amount token
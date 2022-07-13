{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Tokenomia.ICO.Funds.Exchange.Tokens (
  fetchTokens,
  ExchangeToken (..),
) where

import Prelude hiding (print, round)

import Tokenomia.Wallet.ChildAddress.ChildAddressRef

import Tokenomia.Wallet.UTxO
import Tokenomia.Wallet.WalletUTxO hiding (value)

import Control.Monad.Reader

import Data.List.NonEmpty as NEL

import Control.Monad.Except

import Ledger.Ada
import Plutus.V1.Ledger.Value as Ledger
import Tokenomia.Common.Environment
import Tokenomia.Common.Error
import Tokenomia.Common.Token
import Tokenomia.Common.Value
import Tokenomia.ICO.Balanceable
import Tokenomia.ICO.Round.Settings
import Tokenomia.Wallet.CLI

data ExchangeToken = ExchangeToken
  { source :: WalletUTxO
  , token :: Token
  }
  deriving stock (Show)

fetchTokens ::
  ( MonadIO m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  RoundSettings ->
  m (Maybe ExchangeToken)
fetchTokens RoundSettings {addresses = RoundAddresses {tokens = IndexedAddress {childAddressRef = tokensAddress}}, ..} =
  fetchUTxOFilterBy (containingOnlyGivenAssetClass exchangeTokenId . value . utxo) tokensAddress
    >>= \case
      Nothing -> return Nothing
      Just (source :| []) -> do
        let (c, tn, amount) = getTokenFrom ((value . utxo) source)
        (return . Just)
          ExchangeToken
            { token = Token {assetClass = Ledger.assetClass c tn, amount = amount, minimumAdaRequired = (fromValue . value . utxo) source}
            , source = source
            }
      Just _ -> throwError ICOTokensDispatchedOnMultipleUTxOs

instance AdaBalanceable ExchangeToken where
  adaBalance ExchangeToken {..} = (fromValue . value . utxo) source

instance TokenBalanceable ExchangeToken where
  tokenBalance ExchangeToken {..} = amount token

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

module Tokenomia.Adapter.Cardano.CLI.UTxO.Query
    ( query
    , queryUTxOsFilterBy
    , getCurrency
    , askSelectUTxOByType
    ) where


import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Encoding as TLE ( decodeUtf8 )


import           Control.Monad.Reader ( MonadReader, MonadIO(..), asks )
import           Shh.Internal
import           Ledger.Value

import           Tokenomia.Adapter.Cardano.CLI.Wallet
import           Tokenomia.Adapter.Cardano.CLI.Serialise
import           Tokenomia.Adapter.Data.List (short, rmdups)
import           Tokenomia.Adapter.Cardano.CLI.Environment
import           Tokenomia.Adapter.Cardano.CLI.Value ()
import           Tokenomia.Adapter.Cardano.CLI.UTxO
import           Tokenomia.Adapter.Cardano.Types
import           Data.List.NonEmpty (fromList)
import           Tokenomia.Common.Shell.InteractiveMenu (askMenu, DisplayMenuItem, displayMenuItem)


load SearchPath ["cardano-cli"]

query ::
  ( MonadIO m
  , MonadReader Environment m )
  => Address
  -> m [UTxO]
query (Address address) = do
    magicN <- asks magicNumber
    fromCLI . TL.toStrict . TLE.decodeUtf8 <$> liftIO (cardano_cli "query" "utxo" "--testnet-magic" magicN "--address" address |> capture)


queryUTxOsFilterBy ::
    ( MonadIO m
    , MonadReader Environment m )
    => Wallet
    -> (UTxO -> Bool)
    -> m [UTxO]
queryUTxOsFilterBy Wallet{..} f = filter f <$> query paymentAddress

getCurrency :: UTxO -> (CurrencySymbol, TokenName)
getCurrency utxo = (currency, _tokenName)
    where (currency, _tokenName, _) = getTokenFrom utxo

queryTokensList :: (MonadIO m, MonadReader Environment m) => Wallet -> m [(CurrencySymbol, TokenName)]
queryTokensList wallet = do
    utxos <- queryUTxOsFilterBy wallet (not . containingStrictlyADAs)
    if null utxos then return [] else do
        let currencyList = map getCurrency utxos
        return $ rmdups currencyList

askSelectUTxOByType :: (MonadIO m, MonadReader Environment m) => Wallet -> m (Maybe [UTxO])
askSelectUTxOByType wallet = do
    tokensList <- queryTokensList wallet
    case tokensList of
        [] -> return Nothing
        tokenList -> do
            (currency, _) <- askMenu $ fromList tokenList --displayMenuItem
            queryUTxOsFilterBy wallet (containingGivenNativeToken currency) >>=
                \case
                    [] -> return Nothing 
                    utxos -> return $ Just utxos

instance DisplayMenuItem (CurrencySymbol, TokenName) where
    displayMenuItem (currency, _tokenName) = toString _tokenName <> " - " <> short (show currency)
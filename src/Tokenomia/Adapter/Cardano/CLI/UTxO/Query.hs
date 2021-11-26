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
    , askSelectUTxOByType
    , retrieveTotalAmountFromAsset
    ) where


import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Encoding as TLE ( decodeUtf8 )
import           Data.List.NonEmpty

import           Control.Monad.Reader ( MonadReader, MonadIO(..), asks )
import           Shh.Internal
import           Ledger.Value

import           Tokenomia.Adapter.Cardano.CLI.Wallet
import           Tokenomia.Adapter.Cardano.CLI.Serialise
import           Tokenomia.Adapter.Data.List (removeDuplicates)
import           Tokenomia.Adapter.Cardano.CLI.Environment
import           Tokenomia.Adapter.Cardano.CLI.Value ()
import           Tokenomia.Adapter.Cardano.CLI.UTxO
import           Tokenomia.Adapter.Cardano.Types
import           Tokenomia.Common.Shell.InteractiveMenu (askMenu, DisplayMenuItem, displayMenuItem)
import Ledger (Value)

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
queryUTxOsFilterBy Wallet{..} f = Prelude.filter f <$> query paymentAddress

getAssetClassList :: [UTxO] -> [AssetClass]
getAssetClassList [] = []
getAssetClassList (UTxO{..} : xs) =
    Prelude.map (\(c, t, _) -> assetClass c t) (flattenValue value) ++ getAssetClassList xs

queryAssetClassList :: (MonadIO m, MonadReader Environment m) => Wallet -> m [AssetClass]
queryAssetClassList wallet = do
    utxos <- queryUTxOsFilterBy wallet (not . containingStrictlyADAs)
    if null utxos then return [] else do
        let currencyList = getAssetClassList utxos
        return $ removeDuplicates currencyList

queryAssetClassFilterBy ::
    ( MonadIO m
    , MonadReader Environment m )
    => Wallet
    -> (AssetClass -> Bool)
    -> m [AssetClass]
queryAssetClassFilterBy wallet f = do
    assetClasses <- queryAssetClassList wallet
    return $ Prelude.filter f assetClasses

askSelectUTxOByType :: (MonadIO m, MonadReader Environment m) => Wallet -> m (Maybe (NonEmpty AssetClass))
askSelectUTxOByType wallet = do
    tokensList <- queryAssetClassList wallet
    case tokensList of
        [] -> return Nothing
        tokenList -> do
            _assetClass <- askMenu $ fromList tokenList
            utxos <- queryAssetClassFilterBy wallet (== _assetClass)
            if null utxos then return Nothing else return (Just $ fromList utxos)

instance DisplayMenuItem AssetClass where
    displayMenuItem _assetClass = show _tokenName <> " - " <> show _currencySymbol --TODO : showHashView for currencySymbol
        where (_currencySymbol, _tokenName) = unAssetClass _assetClass

retrieveTotalAmountFromAsset :: CurrencySymbol  -> NonEmpty UTxO -> Integer
retrieveTotalAmountFromAsset currency utxos = do
    allFlattenValues <- fold (value <$> utxos)
    _flattenValues <- Prelude.filter (\(c, _, _) -> c == currency) allFlattenValues 
    return $ sum (Prelude.map (\(_, _, a) -> a) _flattenValues)
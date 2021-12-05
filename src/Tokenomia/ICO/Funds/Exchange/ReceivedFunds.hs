{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tokenomia.ICO.Funds.Exchange.ReceivedFunds
    ( fetchRawReceivedFundsByTx
    , authentifyTxsAsComingFromRoundWallet
    , discardRejectedTxs
    , RawReceivedFundsByTx (..)
    , AuthentifiedFunds (..)
    , ) where


import           Prelude hiding (round,print)

import qualified Data.Text as T

import Ledger ( TxOutRef(..), TxId )
import Tokenomia.Wallet.ChildAddress.ChildAddressRef
import Data.Coerce
import Tokenomia.Common.Address

import Tokenomia.Wallet.UTxO

import Control.Monad.Reader

import qualified Blockfrost.Client as B

import Data.String ( IsString(fromString) )
import Data.List.NonEmpty as NEL

import           Control.Monad.Except
import           Ledger.Ada
import           Tokenomia.Common.Error
import           Tokenomia.ICO.RoundSettings
import Tokenomia.Common.Value
import Tokenomia.Wallet.CLI
import           Tokenomia.Common.Environment
import Tokenomia.Wallet.ChildAddress.LocalRepository
import Ledger.Slot
import Tokenomia.Common.Hash
import Tokenomia.ICO.Funds.Reception.CardanoCLI.Datum
import Tokenomia.ICO.Funds.WhiteListing.Repository


data RawReceivedFundsByTx
    = RawReceivedFundsByTx
        { txId :: TxId
        , inputAdresses :: NonEmpty Address
        , sources :: NonEmpty WalletUTxO } deriving Show

data RejectedTx
    = TxMadeFromOutsideTheICORoundWallet
      { tx :: RawReceivedFundsByTx
      , externalAddresses :: NonEmpty Address } deriving Show


data AuthentifiedFunds
    = AuthentifiedFunds
      { source :: WalletUTxO
      , adas :: Ada
      , receivedAt :: Slot
      , paybackAddress :: Address
      } deriving (Show,Eq)


instance Ord  AuthentifiedFunds where 
    compare x y = case compare (receivedAt x) (receivedAt y) of
      LT -> LT
      EQ -> compare (source x) (source y)
      GT -> GT

authentifyTxsAsComingFromRoundWallet
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError  TokenomiaError m)
    => RoundSettings
    -> NonEmpty RawReceivedFundsByTx
    -> m (NonEmpty (Either RejectedTx (NonEmpty AuthentifiedFunds)))
authentifyTxsAsComingFromRoundWallet a = mapM (authentifyTx a)

discardRejectedTxs 
    :: (MonadError  TokenomiaError m ) 
    => NonEmpty (Either RejectedTx (NonEmpty AuthentifiedFunds))
    -> m (NonEmpty AuthentifiedFunds) 
discardRejectedTxs xs = 
     let  x :: [[AuthentifiedFunds]] = discard <$>  NEL.toList xs
     in case (nonEmpty . join) x of 
       Nothing -> throwError ICONoValidTxs
       Just res -> return res

    where 
        discard :: Either RejectedTx (NonEmpty AuthentifiedFunds) -> [AuthentifiedFunds]
        discard (Left _) = []
        discard (Right x) = NEL.toList x

authentifyTx
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError  TokenomiaError m)
    => RoundSettings
    -> RawReceivedFundsByTx
    -> m (Either RejectedTx (NonEmpty AuthentifiedFunds))
authentifyTx round@RoundSettings {wallet = Wallet {name}} tx@RawReceivedFundsByTx {..} =
 retrieveAddressesNotFromWallet name inputAdresses
  >>= \case
        Nothing                 -> do
            hashesAndUxtos <- getDatumHashesAndAdaStrict sources
            hashesAnddatums <- fetchDatums ((\(a,_,_) -> a) <$> hashesAndUxtos)
            Right <$> mkAuthentifiedTxFunds round hashesAndUxtos hashesAnddatums 
        Just externalAddresses  -> return . Left  $ TxMadeFromOutsideTheICORoundWallet {..}


mkAuthentifiedTxFunds
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError  TokenomiaError m)
    => RoundSettings
    -> NonEmpty (Hash,Ada,WalletUTxO) 
    -> NonEmpty (Hash,Slot,ChildAddressIndex) 
    -> m (NonEmpty AuthentifiedFunds)
mkAuthentifiedTxFunds round xs ys = 
    sequence $ NEL.zipWith (mkAuthentifiedFunds round) (sortWith (\(a,_,_) -> a) xs) (sortWith (\(a,_,_) -> a) ys)

mkAuthentifiedFunds
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError  TokenomiaError m)
    => RoundSettings
    -> (Hash,Ada,WalletUTxO) 
    -> (Hash,Slot,ChildAddressIndex)
    -> m AuthentifiedFunds
mkAuthentifiedFunds RoundSettings {wallet = Wallet {name}} a@(hash1,adas,source) b@(hash2,receivedAt,index) = do
    if hash1 /= hash2 
        then error $ "Internal inconsistencies : " <> show a <> " and " <> show b
        else do
            paybackAddress <- fetchPaybackAddress $ ChildAddressRef name index
            return AuthentifiedFunds {..}


fetchDatums 
    :: ( MonadIO m 
       , MonadError  TokenomiaError m) 
    => NonEmpty Hash 
    -> m (NonEmpty (Hash,Slot,ChildAddressIndex))
fetchDatums hashes = do
    prj <- liftIO B.projectFromEnv
    (liftIO $ B.runBlockfrost prj $ fetchDatums' hashes)
                            >>= (\case
                                    Left e -> throwError $ BlockFrostError e
                                    Right res -> return res)

fetchDatums' :: NonEmpty Hash -> B.BlockfrostClient (NonEmpty (Hash,Slot,ChildAddressIndex))
fetchDatums' = mapM fetchDatum'

fetchDatum' :: Hash -> B.BlockfrostClient (Hash,Slot,ChildAddressIndex)
fetchDatum' (Hash hash ) = do
    (slot,index) <- datumFromJson . B._scriptDatumJsonValue <$> B.getScriptDatum (fromString hash)
    return (Hash hash,slot,index)


fetchRawReceivedFundsByTx
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError  TokenomiaError m)
    => RoundSettings
    -> m (NonEmpty RawReceivedFundsByTx)
fetchRawReceivedFundsByTx RoundSettings {addresses = RoundAddresses {exchange = IndexedAddress {childAddressRef = exchangeAddress} }}
    = do
        walletUtxos <- fetchUTxOFilterBy (containingStrictlyADAs . value . utxo) exchangeAddress >>= whenNothingThrow NoUTxOsFound
        let getTxOutRefId = txOutRefId . txOutRef . utxo
            walletUtxosGroupedByTx =  (\(txs :: NonEmpty WalletUTxO) -> ((getTxOutRefId . NEL.head) txs,txs))
                                            <$> NEL.groupWith1 getTxOutRefId walletUtxos

        prj <- liftIO B.projectFromEnv
        utxosAdresses <- (liftIO $ B.runBlockfrost prj $ fetchUTxOsAddresses' $ fst <$> walletUtxosGroupedByTx)
                            >>= (\case
                                    Left e -> throwError $ BlockFrostError e
                                    Right res -> return res)

        return $ mkAllReceivedFundsByTx utxosAdresses walletUtxosGroupedByTx

mkAllReceivedFundsByTx :: NonEmpty (TxId,NonEmpty Address) -> NonEmpty (TxId,NonEmpty WalletUTxO) -> NonEmpty RawReceivedFundsByTx
mkAllReceivedFundsByTx xs ys =
    let txs = NEL.zipWith mkReceivedFundsByTx (sortWith fst ys) (sortWith fst xs)
    in if (NEL.length txs == NEL.length ys) == (NEL.length txs == NEL.length xs)
        then txs
        else error $ "BlockFrost Internal Inconsistencies : " <> show xs <> " , " <> show xs <> " , " <> show txs

mkReceivedFundsByTx :: (TxId,NonEmpty WalletUTxO) -> (TxId,NonEmpty Address)  ->  RawReceivedFundsByTx
mkReceivedFundsByTx a@(txIdx, sources) b@(txIdy,inputAdresses) =
    if txIdx == txIdy
        then RawReceivedFundsByTx {txId = txIdx,..}
        else error $ "BlockFrost Internal Inconsistencies : " <> show a <> " and " <> show b


fetchUTxOsAddresses' :: NonEmpty TxId -> B.BlockfrostClient (NonEmpty (TxId,NonEmpty Address))
fetchUTxOsAddresses' = mapM fetchUTxOAddresses'

fetchUTxOAddresses' :: TxId -> B.BlockfrostClient (TxId,NonEmpty Address)
fetchUTxOAddresses' txId = do
    addresses <- (\(x :: B.TransactionUtxos )-> Address . T.unpack . coerce . B._utxoInputAddress <$> B._transactionUtxosInputs x)
                                        <$> B.getTxUtxos ((fromString . show) txId)
    case nonEmpty addresses of
      Nothing -> error $ "BlockFrost Internal Inconsistencies : returned Tx with inputs without adresses : " <> show txId
      Just xs -> return (txId,xs)


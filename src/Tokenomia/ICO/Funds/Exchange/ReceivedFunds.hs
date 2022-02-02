{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Tokenomia.ICO.Funds.Exchange.ReceivedFunds
    ( fetchRawReceivedFundsByTx
    , authentifyTxsAsComingFromRoundWallet
    , discardRejectedTxs
    , RawReceivedFundsByTx (..)
    , AuthentifiedFunds (..)
    , ) where


import           Prelude hiding (round,print)

import qualified Data.Text as T

import Ledger ( TxId )
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
import           Tokenomia.ICO.Round.Settings
import Tokenomia.Common.Value
import Tokenomia.Wallet.CLI
import           Tokenomia.Common.Environment
import Tokenomia.Wallet.ChildAddress.LocalRepository
import Ledger.Slot
import Tokenomia.Common.Hash
import Tokenomia.ICO.Funds.Validation.CardanoCLI.Datum
import Tokenomia.ICO.Funds.WhiteListing.Repository
import Tokenomia.Common.TxOutRef
import Data.Maybe ( catMaybes )
import qualified Tokenomia.Common.Blockfrost as B

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
      , index :: ChildAddressIndex 
      , paybackAddress :: Address
      } deriving (Eq)

instance Show AuthentifiedFunds where 
 show AuthentifiedFunds {..} 
    = "\n" <> (showTxOutRef . txOutRef . utxo ) source
        <> "\n   | index : " <> (show @Integer . fromIntegral) index
        <> "\n   | received at : "      <> show (getSlot receivedAt)
        <> "\n   | amount  : "          <> (show . getLovelace) adas <> " Lovelaces"
        <> "\n   | payback Address  : " <> show paybackAddress

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
       Nothing -> throwError $ ICONoValidTxs (show xs)
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
authentifyTx round@RoundSettings {investorsWallet = Wallet {name = investorsWalletName}, previousRoundMaybe = Nothing} tx@RawReceivedFundsByTx {..} =
 retrieveAddressesFromWallet investorsWalletName inputAdresses
  >>= \case
        Just _ -> do
            hashesAndUxtos <- getDatumHashesAndAdaStrict sources
            hashesAnddatums <- fetchDatums ((\(a,_,_) -> a) <$> hashesAndUxtos)
            Right <$> mkAuthentifiedTxFunds round hashesAndUxtos hashesAnddatums 
        Nothing  -> return . Left  $ TxMadeFromOutsideTheICORoundWallet {externalAddresses = inputAdresses,..}

authentifyTx round@RoundSettings { investorsWallet = Wallet {name = currentRoundWallet}
                                 , previousRoundMaybe 
                                    = Just PreviousRound {investorsWallet = Wallet {name = previousInvestorsWallet}, exchangeWallet = Wallet {name = previousExchangeWallet}}} 
             tx@RawReceivedFundsByTx {..} = do 
 txFromCurrentRoundMaybe  <- retrieveAddressesFromWallet currentRoundWallet inputAdresses
 txFromPreviousInvestorsWalletMaybe <- retrieveAddressesFromWallet previousInvestorsWallet inputAdresses
 txFromPreviousExchangeRoundMaybe <- retrieveAddressesFromWallet previousExchangeWallet inputAdresses
 case catMaybes [txFromCurrentRoundMaybe,txFromPreviousInvestorsWalletMaybe,txFromPreviousExchangeRoundMaybe] of
    [] -> return . Left  $ TxMadeFromOutsideTheICORoundWallet {externalAddresses = inputAdresses,..}
    _  -> do  
        hashesAndUxtos <- getDatumHashesAndAdaStrict sources
        hashesAnddatums <- fetchDatums ((\(a,_,_) -> a) <$> hashesAndUxtos)
        Right <$> mkAuthentifiedTxFunds round hashesAndUxtos hashesAnddatums 
                             

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
mkAuthentifiedFunds settings a@(hash1,adas,source) b@(hash2,receivedAt,index) = do
    if hash1 /= hash2 
        then error $ "Internal inconsistencies : " <> show a <> " and " <> show b
        else do
            paybackAddress <- fetchPaybackAddressStrict settings index
            return AuthentifiedFunds {..}


fetchDatums 
    :: ( MonadIO m 
       , MonadError  TokenomiaError m
       , MonadReader Environment m) 
    => NonEmpty Hash 
    -> m (NonEmpty (Hash,Slot,ChildAddressIndex))
fetchDatums hashes = do
    prj <- B.projectFromEnv''
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

        prj <- B.projectFromEnv''
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


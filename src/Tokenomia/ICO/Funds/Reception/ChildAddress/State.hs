{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
module Tokenomia.ICO.Funds.Reception.ChildAddress.State
    ( fetchActiveAddresses
    , fetchAllWhiteListedFunds
    , AddressVolumes (..)
    , WhiteListedInvestorState (..)
    , ReceivedFunds (..)
    , NativeTokens
    , Funds
    , ) where

import           Prelude hiding (round,print)

import Data.Set.Ordered ( fromList, OSet )
import Plutus.V1.Ledger.Ada ( Ada(..) )


import Ledger.Ada as Ada ( lovelaceOf )
import qualified Data.Text as T

import Blockfrost.Types.Shared.Amount ( Amount(..) )
import Data.Foldable ( Foldable(fold) )
import           Ledger ( Slot(..), TxOutRef(..) )



import Control.Monad.Reader 

import qualified Blockfrost.Client as B

import Data.String ( IsString(fromString) )
import Data.Coerce ( coerce )
import Data.List.NonEmpty hiding (fromList)

import Tokenomia.Common.Address ( Address(..) )
import           Control.Monad.Except
import           Tokenomia.ICO.Funds.Reception.ChildAddress.Types
import           Tokenomia.Common.Error
import           Tokenomia.Wallet.Type
import           Tokenomia.Wallet.ChildAddress.ChildAddressRef

fetchActiveAddresses 
    :: ( MonadIO m
       , MonadError  TokenomiaError m)
    => Wallet
    -> m (NonEmpty Address)
fetchActiveAddresses  Wallet {stakeAddress = Address stakeAddress}= do 
    prj <- liftIO B.projectFromEnv
    liftIO $ B.runBlockfrost prj $ do 
        addresses :: [Address] <- (fmap . fmap) (Address . T.unpack . coerce)  (B.getAccountAssociatedAddresses $ fromString stakeAddress)
        (return . nonEmpty) addresses  
    >>= (\case
            Left e -> throwError $ BlockFrostError e
            Right Nothing -> throwError NoActiveAddressesOnWallet
            Right (Just xs) ->  return xs)


fetchAllWhiteListedFunds
    :: ( MonadIO m
       , MonadError  TokenomiaError m)
    => NonEmpty WhiteListedInvestorRef
    -> m (NonEmpty WhiteListedInvestorState)
fetchAllWhiteListedFunds whiteListedInvestorRefs = do
    prj <- liftIO B.projectFromEnv
    liftIO $ B.runBlockfrost prj $ do 
        fetchAllWhiteListedFunds' whiteListedInvestorRefs
    >>= (\case
            Left e -> throwError $ BlockFrostError e
            Right  xs -> return $ (\(a,b,c) -> mkWhiteListedInvestorState a b c ) <$>xs)

mkWhiteListedInvestorState
    :: WhiteListedInvestorRef
    -> AddressVolumes 
    -> OSet ReceivedFunds 
    -> WhiteListedInvestorState 
mkWhiteListedInvestorState investorRef volumes allReceivedFunds 
    = WhiteListedInvestorState {..} 

fetchAllWhiteListedFunds'
    :: NonEmpty WhiteListedInvestorRef
    -> B.BlockfrostClient (NonEmpty (WhiteListedInvestorRef,AddressVolumes,OSet ReceivedFunds))
fetchAllWhiteListedFunds'  whiteListedInvestorRefs  = do
    mapM fetchWhiteListedFunds whiteListedInvestorRefs


fetchWhiteListedFunds
    :: WhiteListedInvestorRef
    -> B.BlockfrostClient (WhiteListedInvestorRef,AddressVolumes,OSet ReceivedFunds)
fetchWhiteListedFunds w@WhiteListedInvestorRef{indexedAddress = IndexedAddress {..} } = do
    volumes <- fetchAddressVolumes  address
    allReceivedFunds <- fetchAllReceivedFunds address
    pure (w,volumes,allReceivedFunds)


fetchAllReceivedFunds :: Address -> B.BlockfrostClient (OSet ReceivedFunds)
fetchAllReceivedFunds addr = fromList <$> (fmap .fmap) (\(a,b,c) -> mkReceivedFunds a b c ) (fetchUTxOsDetails addr)


fetchUTxOsDetails :: Address -> B.BlockfrostClient [(B.AddressUtxo ,B.Transaction,B.TransactionUtxos)]
fetchUTxOsDetails (Address addr) = do
    utxos <- B.getAddressUtxos (fromString addr)
    mapM(\x@B.AddressUtxo {_addressUtxoTxHash = hash} -> do
        transaction <- B.getTx hash
        transactionUtxos <- B.getTxUtxos hash
        return (x,transaction,transactionUtxos)) utxos

mkReceivedFunds :: B.AddressUtxo -> B.Transaction -> B.TransactionUtxos -> ReceivedFunds
mkReceivedFunds
    B.AddressUtxo {..}
    B.Transaction {..}
    _ =
        ReceivedFunds { txOutRef = TxOutRef {
                                    txOutRefId =  fromString . T.unpack . coerce $_addressUtxoTxHash,
                                    txOutRefIdx = _addressUtxoOutputIndex }
                      , receivedAt = (Slot . coerce) _transactionSlot
                      , funds = splitNativeTokensAndAdas _addressUtxoAmount}

splitNativeTokensAndAdas :: [Amount] -> Either NativeTokens Ada
splitNativeTokensAndAdas [AdaAmount  x] = Right $ Ada.lovelaceOf (fromIntegral x)
splitNativeTokensAndAdas x = Left x


fetchAddressVolumes :: Address -> B.BlockfrostClient AddressVolumes
fetchAddressVolumes (Address addr) = do
    B.AddressDetails {..} <- B.getAddressDetails (fromString addr)
    return AddressVolumes { received = fold (filterOnlyLovelaces <$> _addressDetailsReceivedSum)
                          , sent     = fold (filterOnlyLovelaces <$> _addressDetailsSentSum)}


filterOnlyLovelaces :: Amount -> Ada
filterOnlyLovelaces (AdaAmount x)   = Ada.lovelaceOf (fromIntegral x)
filterOnlyLovelaces (AssetAmount _) = Ada.lovelaceOf 0


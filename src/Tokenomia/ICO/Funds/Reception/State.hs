{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
module Tokenomia.ICO.Funds.Reception.State
    (fetchState,fetchStateFromStakeAddress) where

import Prelude hiding (round,print)


import           Control.Monad.Reader
import Data.List (group,sort)
import           Plutus.V1.Ledger.Ada

import qualified Blockfrost.Client as B

import           Tokenomia.Adapter.Cardano.CLI.Transaction

import Ledger.Ada as Ada
import qualified Data.Text as T

import Blockfrost.Types.Shared.Amount
import Data.String
import Data.Foldable
import Ledger ( Slot(..), TxOutRef(..) )
import Data.Coerce
import Data.Set.Ordered

import Tokenomia.ICO.Funds.Reception.Types


fetchStateFromStakeAddress
    :: Address
       -> B.BlockfrostClient [AddressFundsState]
fetchStateFromStakeAddress (Address addr) = do
    addresses <- B.getAccountAssociatedAddresses (fromString addr)
    mapM fetchState (Address . T.unpack . coerce <$> addresses)



fetchState
    :: Address
       -> B.BlockfrostClient AddressFundsState
fetchState address = do
    references <- fetchAddressGenerationIndex address
    volumes <- fetchAddressVolumes address
    transactions <- fetchInvestorContexts address
    pure AddressFundsState {..}


fetchAddressGenerationIndex :: MonadIO m =>  Address -> m AddressRef 
fetchAddressGenerationIndex address = return AddressRef {generationIndex = AddressGenerationIndex 0,..}

fetchInvestorContexts :: Address -> B.BlockfrostClient (OSet FundsTransaction)
fetchInvestorContexts addr = fromList <$> (fmap .fmap) (\(a,b,c) -> mkFundsTransaction a b c ) (fetchUTxOsDetails addr)


fetchUTxOsDetails :: Address -> B.BlockfrostClient [(B.AddressUtxo ,B.Transaction,B.TransactionUtxos)]
fetchUTxOsDetails (Address addr) = do
    utxos <- B.getAddressUtxos (fromString addr)
    mapM(\x@B.AddressUtxo {_addressUtxoTxHash = hash} -> do
        transaction <- B.getTx hash
        transactionUtxos <- B.getTxUtxos hash
        return (x,transaction,transactionUtxos)) utxos

mkFundsTransaction :: B.AddressUtxo -> B.Transaction -> B.TransactionUtxos -> FundsTransaction
mkFundsTransaction
    B.AddressUtxo {..}
    B.Transaction {..}
    B.TransactionUtxos {..} =
        FundsTransaction { txOutRef = TxOutRef {
                                                    txOutRefId =  fromString . T.unpack . coerce $_addressUtxoTxHash,
                                                    txOutRefIdx = _addressUtxoOutputIndex }
                                , inputAddresses = rmdups $ Address . T.unpack . coerce . B._utxoInputAddress <$> _transactionUtxosInputs
                                , transactionSlot = (Slot . coerce) _transactionSlot
                                , funds = splitNativeTokensAndAdas _addressUtxoAmount}

splitNativeTokensAndAdas :: [Amount] -> Either NativeTokens Ada
splitNativeTokensAndAdas [AdaAmount  x] = Right $ Ada.lovelaceOf (fromIntegral x)
splitNativeTokensAndAdas x = Left x

rmdups :: (Ord a) => [a] -> [a]
rmdups = Prelude.map Prelude.head . group . sort


fetchAddressVolumes :: Address -> B.BlockfrostClient AddressVolumes
fetchAddressVolumes (Address addr) = do
    B.AddressDetails {..} <- B.getAddressDetails (fromString addr)
    return AddressVolumes { received = fold (filterOnlyLovelaces <$> _addressDetailsReceivedSum)
                          , sent     = fold (filterOnlyLovelaces <$> _addressDetailsSentSum)}




filterOnlyLovelaces :: Amount -> Ada
filterOnlyLovelaces (AdaAmount x)   = Ada.lovelaceOf (fromIntegral x)
filterOnlyLovelaces (AssetAmount _) = Ada.lovelaceOf 0


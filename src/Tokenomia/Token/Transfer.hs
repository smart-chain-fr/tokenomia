{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Tokenomia.Token.Transfer 
    ( transfer) where

import           Prelude hiding ((+),(-))
import           PlutusTx.Prelude  (AdditiveSemigroup((+)),AdditiveGroup((-)))

import           Data.List.NonEmpty
import           Control.Monad.Reader hiding (ask)
import           Control.Monad.Except

import           Ledger.Value
import           Tokenomia.Adapter.Cardano.CLI.Environment

import           Ledger.Ada
import           Tokenomia.Adapter.Cardano.CLI.UTxO 
import           Tokenomia.Adapter.Cardano.CLI.Transaction 
import           Tokenomia.Adapter.Cardano.CLI.Wallet
import           Tokenomia.Common.Error
import           Tokenomia.Wallet.Collateral.Read
import           Tokenomia.Wallet.CLI
import           Tokenomia.Common.Shell.Console (printLn)
import           Tokenomia.Common.Shell.InteractiveMenu  (ask,askString, askStringLeaveBlankOption)



transfer 
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError BuildingTxError m)  
    => m ()
transfer = do
    wallet <- fetchWalletsWithCollateral >>= whenNullThrow NoWalletWithCollateral 
        >>= \wallets -> do
            printLn "Select the minter wallet : "
            askToChooseAmongGivenWallets wallets 
    utxoWithToken <- askUTxOFilterBy containingOneToken wallet >>= whenNothingThrow NoUTxOWithOnlyOneToken        
    amount <- ask @Integer                  "- Amount of Token to transfer : "
    receiverAddr <- Address <$> askString   "- Receiver address : "
    labelMaybe <- askStringLeaveBlankOption "- Add label to your transaction (leave blank if no) : " 
    
    transfer' wallet receiverAddr utxoWithToken  amount labelMaybe

type MetadataLabel = String

transfer' 
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError BuildingTxError m)  
    => Wallet 
    -> Address 
    -> UTxO
    -> Integer
    -> Maybe MetadataLabel    
    -> m ()
transfer' senderWallet receiverAddr utxoWithToken amount labelMaybe = do
    
    metadataMaybe <- mapM (fmap Metadata . createMetadataFile)  labelMaybe

    let (tokenPolicyHash,tokenNameSelected,totalAmount) = getTokenFrom utxoWithToken
        tokenId = singleton tokenPolicyHash tokenNameSelected
        valueToTransfer = tokenId amount + lovelaceValueOf 1344798
        change = tokenId (totalAmount - amount) + lovelaceValueOf 1344798

    submit'
      TxBuild
        { wallet = senderWallet
        , txIns =  [FromWallet (txOutRef utxoWithToken)]
        , txOuts = ToWallet receiverAddr valueToTransfer 
                :| [ToWallet (paymentAddress senderWallet) change]
        , validitySlotRangeMaybe = Nothing
        , tokenSupplyChangesMaybe = Nothing
        , ..}

    
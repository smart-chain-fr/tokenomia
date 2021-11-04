{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Tokenomia.Vesting.Retrieve
    ( retrieveFunds) where

import           Prelude hiding ((+),(-))

import           Data.List.NonEmpty

import           Control.Monad.Reader
import           Control.Monad.Except

import           PlutusTx.Prelude  (AdditiveSemigroup((+)))

import           Tokenomia.Adapter.Cardano.CLI.UTxO


import           Tokenomia.Adapter.Cardano.CLI.Environment
import           Tokenomia.Adapter.Cardano.CLI.Node
import           Tokenomia.Adapter.Cardano.CLI.Transaction
import           Tokenomia.Adapter.Cardano.CLI.Wallet
import           Tokenomia.Adapter.Cardano.CLI.Scripts
import           Tokenomia.Vesting.Repository
import           Tokenomia.Common.Error


retrieveFunds
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError BuildingTxError m)
    => m ()
retrieveFunds = do
    WalletWithVestedFunds{..} <- selectWalletWithVestedFunds >>= whenNothingThrow NoVestingInProgress
    selectVesting vestedFunds >>= retrieveFundsC wallet
     

retrieveFundsC
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError BuildingTxError m)
    => Wallet
    -> Vesting
    -> m ()
retrieveFundsC wallet (Vesting 
                            VestingContext { tranches = ( TrancheContext {valueVested = v1}
                                                        , TrancheContext {valueVested = v2}) , ..}
                            VestingState { tranches = (s1,s2), ..}) = do
    
    currentSlot <- getCurrentSlotSynced
    voidDataFilePath <- persistDataInTMP ()
    datumVoidHash <- getDataHash ()

    let txIns = fmap (\UTxO {..} -> 
                        FromScript 
                            { script = offChain scriptLocation 
                            , datum = voidDataFilePath
                            , redeemer = voidDataFilePath
                            , utxoRef = txOutRef}) utxosOnScript 
                                
        validitySlotRangeMaybe = Just (ValiditySlotRange currentSlot (currentSlot + 100) )
        tokenSupplyChangesMaybe = Nothing
        metadataMaybe = Nothing 

        txOutsEither = case (s1,s2) of
            (Available,Available) ->  
                let tokensThatCanBeVested = v1 + v2
                in Right $ ToWallet (paymentAddress wallet) tokensThatCanBeVested :| [] 
            (Available,Locked ) -> 
                let tokensThatCanBeVested = v1
                    remainingTokensOnScript = v2
                in Right $ ToWallet (paymentAddress wallet) tokensThatCanBeVested 
                   :| [ToScript 
                        { address   = onChain scriptLocation
                        , value     = remainingTokensOnScript
                        , datumHash = datumVoidHash}] 
            (Locked,Available) -> 
                let remainingTokensOnScript = v1
                    tokensThatCanBeVested = v2
                in Right $ ToWallet (paymentAddress wallet) tokensThatCanBeVested 
                    :| [ToScript 
                        { address   = onChain scriptLocation
                        , value     = remainingTokensOnScript
                        , datumHash = datumVoidHash}] 
            (Retrieved ,Available) -> 
                let tokensThatCanBeVested = v2
                in Right $ ToWallet (paymentAddress wallet) tokensThatCanBeVested :| [] 
            (Available ,Retrieved) -> 
                let tokensThatCanBeVested = v1
                in Right $ ToWallet (paymentAddress wallet) tokensThatCanBeVested :| []               
            (Retrieved ,Locked )  -> Left NoFundsToBeRetrieved
            (Locked  ,Retrieved) -> Left NoFundsToBeRetrieved
            (Retrieved ,Retrieved) -> Left FundAlreadyRetrieved 
            (Locked   ,Locked )  -> Left AllFundsLocked
            
    txOuts <- liftEither txOutsEither
    submit' TxBuild{..} 


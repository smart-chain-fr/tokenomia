{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Tokenomia.Vesting.Retrieve
    ( retrieveFunds) where

import           Prelude hiding ((+),(-))
import qualified Data.Text as T

import           Control.Monad.Reader
import           Control.Monad.Except

import           PlutusTx.Prelude  (AdditiveSemigroup((+)))
import           Ledger hiding (singleton,Address)


import           Tokenomia.Adapter.Cardano.CLI.Serialise
import           Tokenomia.Adapter.Cardano.CLI.UTxO
import           Tokenomia.Common.Shell.Console

import           Tokenomia.Adapter.Cardano.CLI.Environment
import           Tokenomia.Adapter.Cardano.CLI.Node
import           Tokenomia.Adapter.Cardano.CLI.Transaction
import           Tokenomia.Adapter.Cardano.CLI.Wallet
import           Tokenomia.Adapter.Cardano.CLI.Scripts
import           Tokenomia.Vesting.Repository
import           Tokenomia.Common.Error
import           Tokenomia.Wallet.Collateral
import           Tokenomia.Wallet.CLI



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
    collateral <- fetchCollateral wallet >>= whenNothingThrow WalletWithoutCollateral  
    utxoForFees <- selectBiggestStrictlyADAsNotCollateral wallet >>= whenNothingThrow NoADAInWallet
    (Slot currentSlot) <- getCurrentSlotSynced
    voidDataFilePath <- persistDataInTMP ()
    let txInVestingUTxOs = foldMap (\vestingUTxO -> ["--tx-in"  , (T.unpack . toCLI . txOutRef) vestingUTxO
                                    , "--tx-in-script-file" , offChain scriptLocation
                                    , "--tx-in-datum-file" , voidDataFilePath
                                    , "--tx-in-redeemer-file" , voidDataFilePath]) utxosOnScript
        commonParams = [ "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoForFees
                        , "--tx-in-collateral", (T.unpack . toCLI . txOutRef) collateral
                        , "--change-address"  , paymentAddress wallet
                        , "--invalid-before" , show currentSlot
                        , "--invalid-hereafter" , show (currentSlot + 180)]
                        <> txInVestingUTxOs
    case (s1,s2) of
        (Available,Available) -> do
            let tokensThatCanBeVested = v2 + v1
            submit (paymentSigningKeyPath wallet) utxoForFees
                (commonParams <> [ "--tx-out" , paymentAddress wallet <> "  " <> (T.unpack . toCLI) tokensThatCanBeVested])

        (Available,Locked ) -> do
            let remainingTokensOnScript = v2
                tokensThatCanBeVested = v1
            datumVoidHash <- getDataHash ()
            submit (paymentSigningKeyPath wallet) utxoForFees
                (commonParams
                <> [ "--tx-out" , onChain scriptLocation <> "  " <> (T.unpack . toCLI) remainingTokensOnScript
                    , "--tx-out-datum-hash"  , datumVoidHash
                    , "--tx-out" , paymentAddress wallet <> "  " <> (T.unpack . toCLI) tokensThatCanBeVested])
        (Locked,Available) -> do
            let remainingTokensOnScript = v1
                tokensThatCanBeVested = v2
            datumVoidHash <- getDataHash ()
            submit (paymentSigningKeyPath wallet) utxoForFees
                (commonParams
                <> [ "--tx-out" , onChain scriptLocation <> "  " <> (T.unpack . toCLI) remainingTokensOnScript
                    , "--tx-out-datum-hash"  , datumVoidHash
                    , "--tx-out" , paymentAddress wallet <> "  " <> (T.unpack . toCLI) tokensThatCanBeVested])
        (Retrieved ,Available) -> do
            let tokensThatCanBeVested = v2 
            submit (paymentSigningKeyPath wallet) utxoForFees
                (commonParams <> [ "--tx-out" , paymentAddress wallet <> "  " <> (T.unpack . toCLI) tokensThatCanBeVested])
        (Available ,Retrieved) -> do 
            let tokensThatCanBeVested = v1
            submit (paymentSigningKeyPath wallet) utxoForFees
                (commonParams <> [ "--tx-out" , paymentAddress wallet <> "  " <> (T.unpack . toCLI) tokensThatCanBeVested])              
        (Retrieved ,Locked )  -> printLn "No funds to be retrieved"
        (Locked  ,Retrieved) -> printLn "No funds to be retrieved"
        (Retrieved ,Retrieved) -> printLn "All the funds retrieved"       
        (Locked   ,Locked )  -> printLn "All the funds are locked and can't be retrieve so far.."


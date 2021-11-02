{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Tokenomia.Token.CLAPStyle.Burn (burn) where

import           Prelude hiding ((+),(-),print)
import           PlutusTx.Prelude  (AdditiveSemigroup((+)),AdditiveGroup((-)))

import           Control.Monad.Reader hiding (ask)
import           Control.Monad.Except


import           Data.List.NonEmpty
import           Ledger.Value
import           Ledger.Ada

import           Tokenomia.Adapter.Cardano.CLI.Environment
import           Tokenomia.Adapter.Cardano.CLI.UTxO 
import           Tokenomia.Adapter.Cardano.CLI.Transaction
import           Tokenomia.Adapter.Cardano.CLI.Scripts

import           Tokenomia.Adapter.Cardano.CLI.Wallet
import           Tokenomia.Wallet.Collateral
import           Tokenomia.Wallet.CLI
import           Tokenomia.Common.Error
import           Tokenomia.Common.Shell.Console (printLn)
import           Tokenomia.Common.Shell.InteractiveMenu (ask)


burn
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError BuildingTxError m)  
    => m ()
burn = do 
    wallet <- fetchWalletsWithCollateral >>= whenNullThrow NoWalletWithCollateral 
        >>= \wallets -> do
            printLn "Select the burner wallet : "
            askToChooseAmongGivenWallets wallets 
    printLn "- Select the utxo containing the tokens to burn :" 
    utxoWithTokensToBurn <- askUTxOFilterBy containingOneToken wallet >>= whenNothingThrow NoUTxOWithOnlyOneToken
    amountToBurn  <- ask @Integer "- Amount to burn : "
    burn' wallet utxoWithTokensToBurn amountToBurn

burn' 
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError BuildingTxError m)  
    => Wallet 
    -> UTxO
    -> Integer
    -> m ()
burn' wallet utxoWithTokensToBurn@UTxO {value = totalAmountBurnable} amountToBurn = do 
    collateral <-  txOutRef <$> (fetchCollateral wallet >>= whenNothingThrow WalletWithoutCollateral) 
    utxoForFees <- txOutRef <$> (selectBiggestStrictlyADAsNotCollateral wallet >>= whenNothingThrow NoADAInWallet)

    let (tokenPolicyHash,tokenNameSelected,_) = getTokenFrom utxoWithTokensToBurn
        valueToBurn = singleton tokenPolicyHash tokenNameSelected amountToBurn
        change = totalAmountBurnable - valueToBurn

    monetaryScript <- getMonetaryPolicyPath tokenPolicyHash >>= whenNothingThrow TryingToBurnTokenWithoutScriptRegistered

    submit'
      TxBuild
        { signingKeyPath = paymentSigningKeyPath wallet
        , txIns =  FromWallet utxoForFees :| [FromWallet (txOutRef  utxoWithTokensToBurn)] 
        , txOuts = ToWallet (paymentAddress wallet) (change + lovelaceValueOf 1344798):| [] 
        , changeAdress = paymentAddress wallet
        , validitySlotRangeMaybe = Nothing
        , tokenSupplyChangesMaybe = Just $ Burn { amount = valueToBurn, script = monetaryScript} :| []
        , metadataMaybe = Nothing 
        , ..}

 


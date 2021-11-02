{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Tokenomia.Token.CLAPStyle.Mint
    ( mint ) where
    
import           Prelude hiding ((+),print)
import           PlutusTx.Prelude  (AdditiveSemigroup((+)) )


import           Control.Monad.Reader hiding (ask)


import           Data.List.NonEmpty
import qualified Data.ByteString.UTF8 as BSU 

import           Control.Monad.Except

import           Ledger hiding (mint,Address,Mint)
import qualified Ledger.Value as L
import           Ledger.Ada

import           Tokenomia.Token.CLAPStyle.MonetaryPolicy
import           Tokenomia.Adapter.Cardano.CLI.Environment
import           Tokenomia.Wallet.CLI

import           Tokenomia.Adapter.Cardano.CLI.UTxO 
import           Tokenomia.Adapter.Cardano.CLI.Transaction

import           Tokenomia.Adapter.Cardano.CLI.Wallet
import           Tokenomia.Adapter.Cardano.CLI.Scripts
import           Tokenomia.Wallet.Collateral
import           Tokenomia.Common.Error

import           Tokenomia.Common.Shell.Console (printLn)
import           Tokenomia.Common.Shell.InteractiveMenu (ask,askString)



mint 
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError BuildingTxError m)  
    => m ()
mint = do
    wallet <- fetchWalletsWithCollateral >>= whenNullThrow NoWalletWithCollateral 
        >>= \wallets -> do
            printLn "Select the minter wallet : "
            askToChooseAmongGivenWallets wallets 
    tokenNameToMint  <- L.tokenName . BSU.fromString <$> askString "> Token Name : "
    amountToMint     <- ask @Integer "> Total Supply to Mint : "
    mint' wallet tokenNameToMint amountToMint  

mint'
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError BuildingTxError m)  
    => Wallet
    -> TokenName
    -> Integer
    -> m ()
mint' wallet tokenName amount = do

    collateral <-  txOutRef <$> (fetchCollateral wallet >>= whenNothingThrow WalletWithoutCollateral) 
    txOutRefToConsume <- txOutRef <$> (selectBiggestStrictlyADAsNotCollateral wallet >>= whenNothingThrow NoADAInWallet)

    let monetaryPolicy = mkMonetaryPolicyScript Params { .. }
        policyhash = scriptCurrencySymbol monetaryPolicy
        valueToMint = L.singleton policyhash tokenName amount
      
    printLn   "-------------------------"
    printLn $ "Policy hash will be : " <> show policyhash
    printLn   "-------------------------"

    monetaryScript <- registerMintingScriptFile monetaryPolicy

    submit'
      TxBuild
        { signingKeyPath = paymentSigningKeyPath wallet
        , txIns =  FromWallet txOutRefToConsume :| [] 
        , txOuts = ToWallet (paymentAddress wallet) (valueToMint + lovelaceValueOf 1344798 ):| [] 
        , changeAdress = paymentAddress wallet
        , validitySlotRangeMaybe = Nothing
        , tokenSupplyChangesMaybe = Just $ Mint { amount = valueToMint, script = monetaryScript} :| []
        , metadataMaybe = Nothing 
        , ..}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Tokenomia.Token.CLAPStyle.Mint
    ( mint ) where

import           Prelude hiding (print)

import           Control.Monad.Reader hiding (ask)

import qualified Data.Text as T

import qualified Data.ByteString.UTF8 as BSU 

import           Control.Monad.Except

import           Ledger hiding (mint)
import qualified Ledger.Value as L

import           Tokenomia.Token.CLAPStyle.MonetaryPolicy
import           Tokenomia.Adapter.Cardano.CLI.Environment
import           Tokenomia.Wallet.CLI
import           Tokenomia.Adapter.Cardano.CLI.Serialise
import           Tokenomia.Adapter.Cardano.CLI.UTxO 
import           Tokenomia.Adapter.Cardano.CLI.Transaction

import           Tokenomia.Adapter.Cardano.CLI.Wallet
import           Tokenomia.Adapter.Cardano.CLI.Scripts
import           Tokenomia.Wallet.Collateral
import           Tokenomia.Common.Error
import           Tokenomia.Common.Shell.Console (printLn)
import           Tokenomia.Common.Shell.InteractiveMenu (ask)



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
    tokenNameToMint  <- L.tokenName . BSU.fromString <$> ask @String "> Token Name : "
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
mint' wallet@Wallet {paymentAddress = minterAddr,..} tokenName amount = do

    collateral <- fetchCollateral wallet >>= whenNothingThrow WalletWithoutCollateral  
    utxoForMintingAndFees <- selectBiggestStrictlyADAsNotCollateral wallet >>= whenNothingThrow NoADAInWallet

    let monetaryPolicy = mkMonetaryPolicyScript Params { txOutRefToConsume = txOutRef utxoForMintingAndFees, .. }
        policyhash = scriptCurrencySymbol monetaryPolicy

    printLn "-------------------------"
    printLn $ "Policy hash will be : " <> show policyhash
    printLn "-------------------------"

    monetaryScriptFilePath <- registerMintingScriptFile monetaryPolicy
    submit paymentSigningKeyPath utxoForMintingAndFees
            [ "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoForMintingAndFees 
            , "--tx-out" , minterAddr <> " + 1344798 lovelace + " <> show amount <> " " <> show policyhash <> "." <> L.toString tokenName
            , "--tx-in-collateral", (T.unpack . toCLI . txOutRef) collateral
            , "--change-address"  , minterAddr
            , "--mint" , show amount <> " " <> show policyhash <> "." <> L.toString tokenName
            , "--mint-script-file" , monetaryScriptFilePath
            , "--mint-redeemer-value",  "[]"]

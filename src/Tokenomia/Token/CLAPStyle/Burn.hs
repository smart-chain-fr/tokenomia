{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Tokenomia.Token.CLAPStyle.Burn (burn) where


import           Prelude hiding (print)
import           Control.Monad.Reader hiding (ask)
import           Control.Monad.Except


import qualified Data.Text as T

import           Ledger.Value



import           Tokenomia.Adapter.Cardano.CLI.Environment
import           Tokenomia.Adapter.Cardano.CLI.Serialise
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
burn' wallet@Wallet {paymentAddress = burnerAddr,..} utxoWithTokensToBurn amountToBurn = do 
    collateral <- fetchCollateral wallet >>= whenNothingThrow WalletWithoutCollateral  
    utxoForFees <- selectBiggestStrictlyADAsNotCollateral wallet >>= whenNothingThrow NoADAInWallet
    let (tokenPolicyHash,tokenNameSelected,totalAmount) = getTokenFrom utxoWithTokensToBurn
    monetaryScriptFilePath <- getMonetaryPolicyPath tokenPolicyHash >>= whenNothingThrow TryingToBurnTokenWithoutScriptRegistered
    submit paymentSigningKeyPath utxoForFees
        [ "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoWithTokensToBurn 
        , "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoForFees 
        , "--tx-out" , burnerAddr <> " + 1344798 lovelace + " <> show (totalAmount - amountToBurn) <> " " <> show tokenPolicyHash <> "." <> toString tokenNameSelected
        , "--tx-in-collateral", (T.unpack . toCLI . txOutRef) collateral
        , "--change-address"  , burnerAddr
        , "--mint" , "-" <> show amountToBurn <> " " <> show tokenPolicyHash <> "." <> toString tokenNameSelected
        , "--mint-script-file" , monetaryScriptFilePath
        , "--mint-redeemer-value",  "[]"]


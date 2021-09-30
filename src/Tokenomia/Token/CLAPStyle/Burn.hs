{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Tokenomia.Token.CLAPStyle.Burn (burn) where

import           Prelude
import           Shh

import           Control.Monad.Reader
import           Control.Monad.Catch ( MonadMask ) 


import qualified Data.Text as T



import           Ledger hiding (value)
import           Ledger.Value
import           Plutus.V1.Ledger.Ada


import           Tokenomia.Adapter.Cardano.CLI
import           Tokenomia.Adapter.Cardano.CLI.Serialise
import           Tokenomia.Adapter.Cardano.CLI.UTxO 
import qualified Tokenomia.Wallet.CLI as Wallet


load SearchPath ["echo"]

burn :: (MonadMask m,MonadIO m, MonadReader Environment m)  => m ()
burn = do
    liftIO $ echo "Select the sender's wallet" 
    Wallet.select
        >>= \case 
            Nothing -> liftIO $ print "No Wallet Registered !"
            Just burnerWallet@Wallet {paymentAddress = burnerAddr,..} -> do 

                liftIO $ echo "> Select the collateral utxo :" 
                Wallet.selectUTxO burnerWallet
                    >>= \case 
                        Nothing -> liftIO $ echo "Please, add a collateral to your wallet"
                        Just utxoWithCollateral -> do 
                            liftIO $ echo "> Select the utxo containing ADAs for fees  :" 
                            Wallet.selectUTxO burnerWallet
                            >>= \case 
                                Nothing -> liftIO $ echo "Please, add a ADA to your wallet"
                                Just utxoWithFees -> do 
                                    liftIO $ echo "> Select the utxo containing the token to burn :" 
                                    Wallet.selectUTxOFilterBy utxoWithContainingOneToken burnerWallet 
                                        >>= \case  
                                            Nothing -> liftIO $ echo "Tokens not found in your wallet."
                                            Just utxoWithToken  -> do
                                                let (tokenPolicyHash,tokenNameSelected,totalAmount) = getTokenFrom utxoWithToken
                                                get_monetary_policy_path tokenPolicyHash
                                                    >>= \case 
                                                       Nothing -> liftIO $ echo "You can only burn token minted via tokenomia (Monetary Policy existing in ~/.tokenomia-cli/transactions/ )"
                                                       Just monetaryScriptFilePath -> do
                                                        amountToBurn  <- liftIO $ echo "-n" "> Amount to burn : "  >>  read @Integer <$> getLine
                                                        run_tx paymentSigningKeyPath
                                                            [ "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoWithToken 
                                                            , "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoWithFees 
                                                            , "--tx-out" , burnerAddr <> " + 1344798 lovelace + " <> show (totalAmount - amountToBurn) <> " " <> show tokenPolicyHash <> "." <> toString tokenNameSelected
                                                            , "--tx-in-collateral", (T.unpack . toCLI . txOutRef) utxoWithCollateral
                                                            , "--change-address"  , burnerAddr
                                                            , "--mint" , "-" <> show amountToBurn <> " " <> show tokenPolicyHash <> "." <> toString tokenNameSelected
                                                            , "--mint-script-file" , monetaryScriptFilePath
                                                            , "--mint-redeemer-value",  "[]"]

getTokenFrom :: UTxO -> (CurrencySymbol,TokenName,Integer)
getTokenFrom UTxO {..} = (head . filter (\(c,_,_) -> c /= adaSymbol ) .flattenValue) value -- should contains only one native token (filtering ADAs) 

utxoWithContainingOneToken :: UTxO -> Bool
utxoWithContainingOneToken UTxO {..} 
    = 1 == (length . filter (\(c,_,_) -> c /= adaSymbol ) .flattenValue) value
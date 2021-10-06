{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}


module Tokenomia.Token.Transfer 
    ( transfer) where

import Control.Monad.Reader

import Shh
    ( load,
      ExecReference(SearchPath) )

import Tokenomia.Adapter.Cardano.CLI
import Control.Monad.Catch ( MonadMask ) 
import qualified Tokenomia.Wallet.CLI as Wallet
import qualified Data.Text as T
import           Tokenomia.Adapter.Cardano.CLI.Serialise
import           Tokenomia.Adapter.Cardano.CLI.UTxO 
import Ledger.Value
import Plutus.V1.Ledger.Ada

{-# ANN module "HLINT: ignore Use camelCase" #-}

load SearchPath ["echo","ssh","cat","pwd","awk","grep","mkdir"]

transfer :: (MonadMask m,MonadIO m, MonadReader Environment m)  => m ()
transfer = do
    liftIO $ echo "Select the sender's wallet" 
    Wallet.select
        >>= \case 
            Nothing -> liftIO $ print "No Wallet Registered !"
            Just senderWallet@Wallet {paymentAddress = senderAddr,..} -> do 
                receiverAddr    <- liftIO $ echo "-n" "> Receiver address : "  >>  getLine
                
                liftIO $ echo "> Select the collateral utxo :" 
                Wallet.selectUTxO senderWallet
                    >>= \case 
                        Nothing -> liftIO $ echo "Please, add a collateral to your wallet"
                        Just utxoWithCollateral -> do 
                            liftIO $ echo "> Select the utxo containing ADAs for fees  :" 
                            Wallet.selectUTxO senderWallet
                            >>= \case 
                                Nothing -> liftIO $ echo "Please, add a ADA to your wallet"
                                Just utxoWithFees -> do 
                                    liftIO $ echo "> Select the utxo containing the token to transfer  :" 
                                    Wallet.selectUTxOFilterBy utxoWithContainingOneToken senderWallet 
                                        >>= \case  
                                            Nothing -> liftIO $ echo "Tokens not found in your wallet."
                                            Just utxoWithToken  -> do
                                                let (tokenPolicyHash,tokenNameSelected,totalAmount) = getTokenFrom utxoWithToken
                                                amount          <- liftIO $ echo "-n" "> Amount of Token : "   >>  read @Integer <$> getLine
                                                run_tx paymentSigningKeyPath 
                                                        [ "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoWithToken
                                                        , "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoWithFees 
                                                        , "--tx-out" , receiverAddr <> " + 1344798 lovelace + " <> show amount <> " " <> show tokenPolicyHash <> "." <> toString tokenNameSelected 
                                                        , "--tx-out" , senderAddr   <> " + 1344798 lovelace + " <> show (totalAmount - amount) <> " " <> show tokenPolicyHash <> "." <> toString tokenNameSelected 
                                                        , "--tx-in-collateral", (T.unpack . toCLI . txOutRef) utxoWithCollateral 
                                                        , "--change-address"  , senderAddr]



getTokenFrom :: UTxO -> (CurrencySymbol,TokenName,Integer)
getTokenFrom UTxO {..} = (head . filter (\(c,_,_) -> c /= adaSymbol ) .flattenValue) value -- should contains only one native token (filtering ADAs) 

utxoWithContainingOneToken :: UTxO -> Bool
utxoWithContainingOneToken UTxO {..} 
    = 1 == (length . filter (\(c,_,_) -> c /= adaSymbol ) .flattenValue) value
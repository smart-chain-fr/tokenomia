{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tokenomia.ICO.Funds.Validation.CardanoCLI.Convert
    ( convertInvestorPlans) where

import           Prelude hiding (round,print)
import           Control.Monad.Reader hiding (ask)
import           Control.Monad.Except
import           Data.Set.Ordered as Set
import           Data.List.NonEmpty as NEL
import           Tokenomia.Common.Environment
import           Data.Foldable
import           Tokenomia.ICO.Funds.Validation.Investor.Command as Plan
import           Tokenomia.ICO.Funds.Validation.ChildAddress.Types
import           Tokenomia.Common.Error
import           Tokenomia.ICO.Funds.Validation.Investor.Plan
import           Tokenomia.ICO.Funds.Validation.CardanoCLI.Command as CardanoCLI
import           Tokenomia.Wallet.ChildAddress.ChainIndex
import Data.Coerce
import Tokenomia.Wallet.ChildAddress.ChildAddressRef
import Tokenomia.Wallet.UTxO
import Ledger.Ada as Ada
import Plutus.V2.Ledger.Api (TxOutRef)
import Tokenomia.Common.Datum

import           Tokenomia.ICO.Funds.Validation.CardanoCLI.Datum


convertInvestorPlans
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError TokenomiaError m)
    => NonEmpty InvestorPlan
    -> m [CardanoCLI.Command]
convertInvestorPlans addressFundsPlans = do
    res <- mapM convertInvestorPlan addressFundsPlans
    (return . Set.toAscList . unbiased ) $ fold (toBiasR <$> res)



toBiasR :: a -> Bias R a
toBiasR = coerce


convertInvestorPlan
    :: (  MonadIO m
       , MonadReader Environment m
       , MonadError TokenomiaError m)
    => InvestorPlan  -> m (OSet CardanoCLI.Command)
convertInvestorPlan
    InvestorPlan {investorRef = WhiteListedInvestorRef {indexedAddress = IndexedAddress {..}}
         , ..}
    = do
    sources <- queryUTxO childAddressRef
    Set.fromList  <$> mapM (convertCommand sources) (toAscList commands)


convertCommand
    :: (  MonadIO m
       ,  MonadReader Environment m
       ,  MonadError TokenomiaError m)
    => [WalletUTxO]
    -> Plan.Command
    -> m CardanoCLI.Command
convertCommand sources planCommand =
    case planCommand of
        c@Plan.Refund   {investorRef = WhiteListedInvestorRef {..},..} -> do
            source <- findUTxOInCardanoCLI sources txOutRef (Plan.getAdas c)
            return 
                CardanoCLI.Refund { source  = source
                       , refundAddress = exchangePaybackAddress
                       , adasToBeRefund = refundAmount 
                       , receivedAt = receivedAt}
        c@Plan.SendOnExchangeAddressWithPartialRefund {investorRef = w@WhiteListedInvestorRef {..},..} -> do
            source <- findUTxOInCardanoCLI sources txOutRef (Plan.getAdas c) 
            datumFile <- registerDatum 
                            $ mkExchangeDatum receivedAt (getIndex w) 
            return 
                CardanoCLI.SendOnExchangeAddressWithPartialRefund
                 { source  = source
                 , refundAddress = exchangePaybackAddress
                 , adasToSendOnExchange = adasToSendOnExchange
                 , adasToBeRefund = refundAmount
                 , receivedAt = receivedAt
                 , datum = datumFile}
        c@Plan.SendOnExchangeAddress {investorRef = w,..} -> do
            source <- findUTxOInCardanoCLI sources txOutRef (Plan.getAdas c)
            datumFile <- registerDatum 
                            $ mkExchangeDatum receivedAt (getIndex w) 
            return 
                CardanoCLI.SendOnExchangeAddress
                  { source  = source
                  , adasToSendOnExchange = adasToSendOnExchange
                  , receivedAt = receivedAt
                  , datum = datumFile}


findUTxOInCardanoCLI
    :: (MonadError TokenomiaError m)
    => [WalletUTxO]
    -> TxOutRef
    -> Ada
    -> m  WalletUTxO
findUTxOInCardanoCLI utxos refGiven adas = do
    let res = find (\WalletUTxO{utxo = UTxO {txOutRef = txOutRefFromSources,..}}
                -> txOutRefFromSources == refGiven && value == Ada.toValue adas) utxos
    case res of
        Nothing -> throwError
                    (InconsistenciesBlockFrostVSLocalNode
                        $ "ref from BlockFrost with adas not found on cardano-cli" <> show refGiven)
        Just w -> return w





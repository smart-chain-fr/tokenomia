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
import Tokenomia.ICO.Round.Settings
import           Tokenomia.ICO.Funds.Validation.CardanoCLI.Datum


convertInvestorPlans
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError TokenomiaError m)
    => RoundSettings 
    -> NonEmpty InvestorPlan
    -> m [CardanoCLI.Command]
convertInvestorPlans settings addressFundsPlans = do
    res <- mapM (convertInvestorPlan settings) addressFundsPlans
    (return . Set.toAscList . unbiased ) $ fold (toBiasR <$> res)



toBiasR :: a -> Bias R a
toBiasR = coerce


convertInvestorPlan
    :: (  MonadIO m
       , MonadReader Environment m
       , MonadError TokenomiaError m)
    => RoundSettings -> InvestorPlan  -> m (OSet CardanoCLI.Command)
convertInvestorPlan 
    settings
    InvestorPlan {investorRef = WhiteListedInvestorRef {indexedAddress = IndexedAddress {..}}
         , ..}
    = do
    sources <- queryUTxO childAddressRef
    Set.fromList  <$> mapM (convertCommand settings sources) (toAscList commands)


convertCommand
    :: (  MonadIO m
       ,  MonadReader Environment m
       ,  MonadError TokenomiaError m)
    => RoundSettings
    -> [WalletUTxO]
    -> Plan.Command
    -> m CardanoCLI.Command
convertCommand RoundSettings { addresses = RoundAddresses {..},..} sources planCommand =
    case (planCommand,nextRoundMaybe) of
        (c@Plan.Reject   {investorRef = WhiteListedInvestorRef {..},..},Nothing )-> do
            source <- findUTxOInCardanoCLI sources txOutRef (Plan.getAdas c)
            return 
                CardanoCLI.Refund { source  = source
                       , refundAddress = paybackAddress
                       , adasToRefund = amountToReject 
                       , receivedAt = receivedAt}
        (c@Plan.Reject   {..}, Just NextRound {exchangeAddress = nextRoundExchangeAddress} ) -> do
            source <- findUTxOInCardanoCLI sources txOutRef (Plan.getAdas c)
            datumFile <- registerDatum 
                            $ mkExchangeDatum receivedAt (getIndex investorRef) 
            return 
                CardanoCLI.MoveToNextRound { source  = source
                       , adasToMove = amountToReject 
                       , receivedAt = receivedAt
                       , datum = datumFile
                       , nextRoundExchangeAddress = nextRoundExchangeAddress}
        (c@Plan.SendOnExchangeAddressWithPartialReject {investorRef = w@WhiteListedInvestorRef {..},..}, Nothing )-> do
            source <- findUTxOInCardanoCLI sources txOutRef (Plan.getAdas c) 
            datumFile <- registerDatum 
                            $ mkExchangeDatum receivedAt (getIndex w) 
            return 
                CardanoCLI.SendOnExchangeAddressAndPartiallyRefund
                 { source  = source
                 , refundAddress = paybackAddress
                 , adasToSendOnExchange = adasToSendOnExchange
                 , adasToRefund = amountToReject
                 , receivedAt = receivedAt
                 , datum = datumFile
                 , exchangeAddress = address exchange}
        (c@Plan.SendOnExchangeAddressWithPartialReject {investorRef = w,..}, Just NextRound {exchangeAddress = nextRoundExchangeAddress} ) -> do
            source <- findUTxOInCardanoCLI sources txOutRef (Plan.getAdas c) 
            datumFile <- registerDatum 
                            $ mkExchangeDatum receivedAt (getIndex w) 
            return 
                CardanoCLI.SendOnExchangeAddressAndPartiallyMoveToNextRound 
                 { source  = source
                 , adasToSendOnExchange = adasToSendOnExchange
                 , adasToMove = amountToReject
                 , receivedAt = receivedAt
                 , datum = datumFile
                 , nextRoundExchangeAddress = nextRoundExchangeAddress
                 , exchangeAddress = address exchange}
        (c@Plan.SendOnExchangeAddress {investorRef = w,..}, _) -> do
            source <- findUTxOInCardanoCLI sources txOutRef (Plan.getAdas c)
            datumFile <- registerDatum 
                            $ mkExchangeDatum receivedAt (getIndex w) 
            return 
                CardanoCLI.SendOnExchangeAddress
                  { source  = source
                  , adasToSendOnExchange = adasToSendOnExchange
                  , receivedAt = receivedAt
                  , datum = datumFile
                  , exchangeAddress = address exchange}


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





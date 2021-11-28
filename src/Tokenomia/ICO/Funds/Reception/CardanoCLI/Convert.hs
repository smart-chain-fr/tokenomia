{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tokenomia.ICO.Funds.Reception.CardanoCLI.Convert
    ( convertAll
    , discardRejections
    , displayCommands) where

import           Prelude hiding (round,print)
import           Control.Monad.Reader hiding (ask)
import           Control.Monad.Except
import           Data.Set.Ordered as Set
import           Data.List.NonEmpty as NEL
import           Tokenomia.Common.Environment
import Data.Foldable
import           Tokenomia.ICO.Funds.Reception.Command as Plan
import           Tokenomia.ICO.Funds.Reception.ChildAddress.Types
import           Tokenomia.Common.Error
import           Tokenomia.ICO.Funds.Reception.Plan
import           Tokenomia.ICO.Funds.Reception.CardanoCLI.Command as CardanoCLI
import           Tokenomia.Wallet.ChildAddress.ChainIndex
import Data.Coerce
import Tokenomia.Wallet.ChildAddress.ChildAddressRef
import Tokenomia.Wallet.UTxO
import Ledger.Ada as Ada
import Plutus.V2.Ledger.Api (TxOutRef)
import Tokenomia.Common.Datum
import           Data.Either
import           Tokenomia.Common.Shell.Console (printLn)

convertAll
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError TokenomiaError m)
    => NonEmpty AddressFundsPlan
    -> m (OSet (Either Plan.Command CardanoCLI.Command))
convertAll addressFundsPlans = do
    res <- mapM convertAddressPlan addressFundsPlans
    (return . unbiased ) $ fold (toBiasR <$> res)



toBiasR :: a -> Bias R a
toBiasR = coerce

discardRejections
  :: OSet (Either Plan.Command CardanoCLI.Command)
  -> [CardanoCLI.Command]
discardRejections = rights . toAscList

displayCommands
    :: (  MonadIO m)
    => OSet (Either Plan.Command CardanoCLI.Command)
    -> m (OSet (Either Plan.Command CardanoCLI.Command))
displayCommands commandsAndRejections = do
    printLn "--------------------------------------"
    printLn "|| Commands and Rejections  ||"
    mapM_ displayCommand (toAscList commandsAndRejections)
    printLn "--------------------------------------"
    return commandsAndRejections

displayCommand
    :: (  MonadIO m)
    => Either Plan.Command CardanoCLI.Command
    -> m ()
displayCommand (Left x) = printLn $  "<Discarded> " <> show x
displayCommand (Right ccCommand) = printLn $ show ccCommand


convertAddressPlan
    :: (  MonadIO m
       , MonadReader Environment m
       , MonadError TokenomiaError m)
    => AddressFundsPlan  -> m (OSet (Either Plan.Command CardanoCLI.Command))
convertAddressPlan
    Plan {investorRef = WhiteListedInvestorRef {indexedAddress = IndexedAddress {..}}
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
    -> m (Either Plan.Command CardanoCLI.Command)
convertCommand sources planCommand =
    case planCommand of
        c@RejectFundsWithNativeTokens {} -> (return . Left) c
        c@Reject {reason = InsufficientFundsReceived } -> (return . Left) c
        Reject   {investorRef = WhiteListedInvestorRef {..},..} -> do
            source <- findUTxOInCardanoCLI sources txOutRef adas
            (return . Right)
                Refund { source  = source
                       , refundAddress = exchangePaybackAddress
                       , adasToBeRefund = adas
                       , receivedAt = receivedAt}
        AcceptWithPartialRefund {investorRef = WhiteListedInvestorRef {..},..} -> do
            source <- findUTxOInCardanoCLI sources txOutRef adas
            datumFile <- registerDatum receivedAt
            (return . Right)
                TransferAndPartiallyRefund
                 { source  = source
                 , refundAddress = exchangePaybackAddress
                 , adas = adas
                 , adasToBeRefund = refundAmount
                 , receivedAt = receivedAt
                 , datum = datumFile}
        Accept {..} -> do
            source <- findUTxOInCardanoCLI sources txOutRef adas
            datumFile <- registerDatum receivedAt
            (return . Right)
                Transfer
                  { source  = source
                  , adas = adas
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





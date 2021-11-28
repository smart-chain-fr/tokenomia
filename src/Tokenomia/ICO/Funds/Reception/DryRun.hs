{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}
module Tokenomia.ICO.Funds.Reception.DryRun
    (dryRun) where

import Prelude hiding (round,print)
import           Control.Monad.Reader
import           Control.Monad.Except

import Data.List (intersperse)
import           Plutus.V1.Ledger.Ada

import           Tokenomia.Common.Shell.Console (printLn)

import Data.Foldable
import           Plutus.V1.Ledger.Interval
import           Tokenomia.Common.Environment
import           Tokenomia.ICO.Funds.Reception.ChildAddress.State
import           Tokenomia.ICO.Funds.Reception.Plan as Plan
import           Data.Set.Ordered hiding (null)
import           Tokenomia.Wallet.Type
import           Tokenomia.Wallet.LocalRepository as Wallet
import           Tokenomia.Wallet.CLI
import           Tokenomia.Common.Error
import           Tokenomia.Wallet.ChildAddress.ChildAddressRef
import           Tokenomia.ICO.RoundSettings
import           Tokenomia.Wallet.ChildAddress.LocalRepository as ChildAddress
import           Tokenomia.ICO.Funds.Reception.ChildAddress.Types
import           Tokenomia.Common.Address ( Address(..) )
import           Tokenomia.ICO.Funds.Reception.CardanoCLI.Convert as CardanoCLICommand
import           Tokenomia.ICO.Funds.Reception.CardanoCLI.Transact
import qualified Data.List.NonEmpty as NEL
import Tokenomia.Common.Transacting
import Tokenomia.ICO.Funds.Reception.Command as Plan
import Data.Coerce
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold as SF
import Tokenomia.Common.PageNumber
import Data.Function ( (&) )
import Data.Maybe
import qualified Data.Set.NonEmpty as NES

dryRun
    :: ( MonadIO m
       , S.MonadAsync m
       , MonadReader Environment m
       , MonadError TokenomiaError m)
       => m ()
dryRun = do
    wallet@Wallet {name} <- Wallet.fetchAll >>= whenNullThrow NoWalletWithCollateral
        >>= \wallets -> do
            printLn "\nSelect a wallet : "
            askToChooseAmongGivenWallets wallets

    exchange <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef name 0)
    collateral <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef name 0)
    fees <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef name 1)

    let nbFundsPerTx = 1000
        roundAddresses = RoundAddresses
                            { exchange = exchange
                            , collateral = collateral
                            , fees }
        round = RoundSettings
                { maximumAdaPerAdress = adaOf 50
                , fundRange = interval (adaOf 3) (adaOf 1000)
                , timeRange = interval 40354960 44653861
                , wallet = wallet
                , addresses = roundAddresses }
    displayRoundContext round

    S.drain
         $ S.fromList (PageNumber <$> [1..])
         & S.mapM (\pageNumber -> fetchActiveAddresses roundAddresses pageNumber wallet)
         & S.takeWhile isJust
         & S.map fromJust
         & S.mapM (fetchByAddresses name)
         & S.mapM fetchWhiteListedAddresses
         & S.mapM (fmap (getStateAndPlans round) . fetchAllWhiteListedFunds)
        --  & S.mapM displayStateAndPlans
         & S.mapM (displayGlobalPlans . fmap snd)
         & S.mapM CardanoCLICommand.convertAll
         & S.map discardRejections
         & S.concatMap S.fromList  
         & S.chunksOf nbFundsPerTx SF.toList
         & S.mapM (\commands -> do
            printLn $ "> " <> (show . length)  commands <> " commands will be sent : \n"
            (return . NES.fromList . NEL.fromList) commands) -- could break...
         & S.mapM (buildTx roundAddresses)
         & S.mapM (\BuiltTx {..} -> do
            printLn $ "Tx built at " <> txBuiltPath
            printLn "--------------------------------------")

    printLn "--------------------------------------"
    printLn "Successfully performed the dry run"
    printLn "--------------------------------------"


-- N.H : TODO 
fetchWhiteListedAddresses
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError TokenomiaError m)
    => NEL.NonEmpty IndexedAddress
    -> m (NEL.NonEmpty WhiteListedInvestorRef)
fetchWhiteListedAddresses activeAddresses =
    return $ WhiteListedInvestorRef
        (Address "addr_test1qplqq9grqdvqguxzu5wjxas67kmarug46x4p0naugj4p3tjn7km0ryhhy4g2wnzle6vt4qfm2u5jxknk4lyzyutnxlyseuet7y")
            <$> activeAddresses



displayRoundContext :: ( MonadIO m) => RoundSettings -> m ()
displayRoundContext round =
    printLn $ "|| Round Context  ||"
    <> "\n" <> show round
    <> "\n--------------------------------------"

getStateAndPlans :: RoundSettings -> NEL.NonEmpty WhiteListedInvestorState -> NEL.NonEmpty (WhiteListedInvestorState, AddressFundsPlan)
getStateAndPlans round whiteListedInvestorStates =  (\a -> (a,plan round a)) <$> whiteListedInvestorStates

displayStateAndPlans
    :: ( MonadIO m)
    => NEL.NonEmpty (WhiteListedInvestorState, AddressFundsPlan)
    ->  m (NEL.NonEmpty (WhiteListedInvestorState, AddressFundsPlan))
displayStateAndPlans statesAndplans = do
    sequence_ $ uncurry displayStateAndPlan  <$> statesAndplans
    return statesAndplans

displayStateAndPlan :: ( MonadIO m) => WhiteListedInvestorState ->  AddressFundsPlan  -> m ()
displayStateAndPlan WhiteListedInvestorState {allReceivedFunds,volumes} Plan {investorRef,commands} | not (null allReceivedFunds)
    =
    printLn $  "\n|| Investor ||\n" <> show investorRef
        <> "\n|| Address Status ||\n"
        <> "\n> Volumes :"
        <> "\n" <> show volumes
        <> "\n> Funds Received : " <> (show . length) allReceivedFunds
        <> "\n" <> fold (intersperse "\n" (show <$> toAscList allReceivedFunds))
        <> "\n> Plan :\n"
        <> "\n" <> fold (intersperse "\n" (show <$> toAscList commands))
        <> "\n---------"
displayStateAndPlan _ Plan {..}
    = do
    printLn $ show investorRef
    printLn "> no Funds Received"
    printLn "---------"


displayGlobalPlans
    :: ( MonadIO m)
    => NEL.NonEmpty AddressFundsPlan
    ->  m (NEL.NonEmpty AddressFundsPlan)
displayGlobalPlans plans = do
    let allTxPlan = toAscList . unbiased $ fold (toBiasR . commands  <$> plans)
        rejectWithNativeTokens = Prelude.filter isRejectFundsWithNativeTokens allTxPlan
        reject = Prelude.filter isReject allTxPlan
        acceptWithPartialRefund = Prelude.filter isAcceptWithPartialRefund allTxPlan
        accept = Prelude.filter isAccept allTxPlan
    printLn $ "\n> Batch Plan :"
           <> "\n  - nb Planned Transactions      : " <> (show.length) allTxPlan
           <> "\n  - nb Accept                    : " <> (show.length) accept
           <> "\n  - nb AcceptPartialRefund       : " <> (show.length) acceptWithPartialRefund  
           <> "\n  - nb Reject                    : " <> (show.length) reject
           <> "\n  - nb Reject with native tokens : " <> (show.length) rejectWithNativeTokens        
        --    <> "\n" <> fold (intersperse "\n" (show <$> x))
    return plans


toBiasR :: a -> Bias R a
toBiasR = coerce
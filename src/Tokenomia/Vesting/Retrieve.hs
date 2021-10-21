{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tokenomia.Vesting.Retrieve
    ( retrieveFunds) where

import           Prelude hiding ((+),(-))
import           Data.Maybe
import qualified Data.Text as T
import           Data.List.NonEmpty hiding (filter,length,map)
import qualified Data.Time.Clock.POSIX as POSIX

import           Control.Monad.Reader

import           Shh

import           PlutusTx.Prelude  (AdditiveSemigroup((+)))
import           Ledger.Value
import           Ledger hiding (singleton,Address)

import           Tokenomia.Adapter.Cardano.CLI
import           Tokenomia.Adapter.Cardano.CLI.Serialise
import           Tokenomia.Adapter.Cardano.CLI.UTxO
import qualified Tokenomia.Wallet.CLI as Wallet
import           Tokenomia.Vesting.Contract
import           Tokenomia.Common.Shell.InteractiveMenu
import           Tokenomia.Adapter.Cardano.CLI.Environment


load SearchPath ["echo"]

retrieveFunds :: (MonadIO m, MonadReader Environment m)  => m ()
retrieveFunds = do
    liftIO $ echo "Select the investor's wallet : "
    selectWalletWithVestedFunds
        >>= \case
            Nothing -> liftIO $ echo "No Vested Funds !"
            Just WalletWithVestedFunds{..} -> do
                liftIO $ echo "Select a Vesting : "
                selectVesting vestedFunds
                >>= \case
                     Nothing -> liftIO $ echo "No Vesting In Progress !"
                     Just VestingInProgress { tranches = ( Tranche {status = s1, value = v1}
                                                         , Tranche {status = s2, value = v2})
                                            , ..} -> do
                        liftIO $ echo "> Select the utxo containing ADAs for collateral  :"
                        Wallet.selectUTxOFilterBy containingStrictlyADAs wallet
                            >>= \case
                                Nothing -> liftIO $ echo "Please, add a collateral to your wallet"
                                Just utxoWithCollateral -> do
                                    liftIO $ echo "> Select the utxo containing ADAs for fees  :"
                                    Wallet.selectUTxOFilterBy containingStrictlyADAs wallet
                                    >>= \case
                                        Nothing -> liftIO $ echo "Please, add a ADA to your wallet"
                                        Just utxoWithFees -> do
                                            (Slot currentSlot) <- getCurrentSlotSynced
                                            voidDataFilePath <- persistDataInTMP ()
                                            let txInVestingUTxOs = foldMap (\vestingUTxO -> ["--tx-in"  , (T.unpack . toCLI . txOutRef) vestingUTxO
                                                                          , "--tx-in-script-file" , offChain scriptLocation
                                                                          , "--tx-in-datum-file" , voidDataFilePath
                                                                          , "--tx-in-redeemer-file" , voidDataFilePath]) vestingUTxOs
                                                commonParams = [ "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoWithFees
                                                               , "--tx-in-collateral", (T.unpack . toCLI . txOutRef) utxoWithCollateral
                                                               , "--change-address"  , paymentAddress wallet
                                                               , "--invalid-before" , show currentSlot
                                                               , "--invalid-hereafter" , show (currentSlot + 180)]
                                                               <> txInVestingUTxOs
                                            case (s1,s2) of
                                                (Available,Available) -> do
                                                    let tokensThatCanBeVested = v2 + v1
                                                    run_tx (paymentSigningKeyPath wallet)
                                                        (commonParams <> [ "--tx-out" , paymentAddress wallet <> "  " <> (T.unpack . toCLI) tokensThatCanBeVested])

                                                (Available,Locked _) -> do
                                                    let remainingTokensOnScript = v2
                                                        tokensThatCanBeVested = v1
                                                    datumVoidHash <- getDataHash ()
                                                    run_tx (paymentSigningKeyPath wallet)
                                                        (commonParams
                                                        <> [ "--tx-out" , onChain scriptLocation <> "  " <> (T.unpack . toCLI) remainingTokensOnScript
                                                            , "--tx-out-datum-hash"  , datumVoidHash
                                                            , "--tx-out" , paymentAddress wallet <> "  " <> (T.unpack . toCLI) tokensThatCanBeVested])
                                                (Locked _,Available) -> do
                                                    let remainingTokensOnScript = v1
                                                        tokensThatCanBeVested = v2
                                                    datumVoidHash <- getDataHash ()
                                                    run_tx (paymentSigningKeyPath wallet)
                                                        (commonParams
                                                        <> [ "--tx-out" , onChain scriptLocation <> "  " <> (T.unpack . toCLI) remainingTokensOnScript
                                                            , "--tx-out-datum-hash"  , datumVoidHash
                                                            , "--tx-out" , paymentAddress wallet <> "  " <> (T.unpack . toCLI) tokensThatCanBeVested])
                                                (Retrieved ,Available) -> do
                                                    let tokensThatCanBeVested = v2 
                                                    run_tx (paymentSigningKeyPath wallet)
                                                        (commonParams <> [ "--tx-out" , paymentAddress wallet <> "  " <> (T.unpack . toCLI) tokensThatCanBeVested])
                                                (Available ,Retrieved) -> do 
                                                    let tokensThatCanBeVested = v1
                                                    run_tx (paymentSigningKeyPath wallet)
                                                        (commonParams <> [ "--tx-out" , paymentAddress wallet <> "  " <> (T.unpack . toCLI) tokensThatCanBeVested])              
                                                (Retrieved ,Locked _)  -> liftIO $ echo "No funds to be retrieved"
                                                (Locked _  ,Retrieved) -> liftIO $ echo "No funds to be retrieved"
                                                (Retrieved ,Retrieved) -> liftIO $ echo "All the funds retrieved"       
                                                (Locked _  ,Locked _)  -> liftIO $ echo "All the funds are locked and can't be retrieve so far.."

                                                   


data WalletWithVestedFunds = WalletWithVestedFunds {wallet :: Wallet, vestedFunds :: [VestingInProgress]}

selectWalletWithVestedFunds :: (MonadIO m , MonadReader Environment m)=> m (Maybe WalletWithVestedFunds)
selectWalletWithVestedFunds = do
    getWalletWithVestedFunds
      >>=  \case
            Nothing -> return Nothing
            Just a -> Just <$> showMenu a
          . nonEmpty
  where
  showMenu :: (MonadIO m) => NonEmpty WalletWithVestedFunds -> m WalletWithVestedFunds
  showMenu wallets = liftIO $ askSelect wallets

instance DisplayMenuItem WalletWithVestedFunds where
    displayMenuItem WalletWithVestedFunds{wallet = Wallet{..},..}
        = name <> " (" <> (show . length ) vestedFunds <> " vesting script(s))"


getWalletWithVestedFunds :: (MonadIO m , MonadReader Environment m) => m [WalletWithVestedFunds]
getWalletWithVestedFunds = do
    allVestingRegistered <- getVestingInProgress
    allWallets <- (fmap.fmap)
                    (\wallet@Wallet {publicKeyHash} ->
                        WalletWithVestedFunds
                            {  vestedFunds = filter (\VestingInProgress{investorId} -> investorId == publicKeyHash) allVestingRegistered
                            , .. })
                    query_registered_wallets
    return $ filter(\WalletWithVestedFunds {..} -> not (null vestedFunds) ) allWallets

selectVesting :: (MonadIO m) => [VestingInProgress] -> m (Maybe VestingInProgress)
selectVesting a = case nonEmpty a of
                    Nothing -> return Nothing
                    Just x -> Just <$> showMenu x


  where
  showMenu :: (MonadIO m) => NonEmpty VestingInProgress -> m VestingInProgress
  showMenu wallets = liftIO $ askSelect wallets

instance DisplayMenuItem VestingInProgress where
    displayMenuItem VestingInProgress {tranches = (Tranche {status = s1,value = v1 },Tranche {status = s2,value = v2})}
        = case (s1,s2) of
            (Available ,Available)   -> "Funds available :" <> "\n\t   * " <> (T.unpack . toCLI) (v1 + v2)
            (Retrieved,Retrieved)    -> "Funds totally retrieved"
            (Locked d1,Available)    -> "Funds Partially Unlocked :"
                                            <> "\n\t   * " <> (T.unpack . toCLI) v1  <> "(locked till " <> (formatISO8601 . convertToExternalPosix) d1 <> ")"
                                            <> "\n\t   * " <> (T.unpack . toCLI) v2  <> "(available)"         
            (Available,Locked d2)  -> "Funds Partially Unlocked :"
                                    <> "\n\t   * " <> (T.unpack . toCLI) v1  <> "(available)"
                                    <> "\n\t   * " <> (T.unpack . toCLI) v2  <> "(locked till " <> (formatISO8601 . convertToExternalPosix) d2 <> ")"
            (Locked d1,Locked d2) -> "Funds Fully Locked :"
                                    <> "\n\t   * " <> (T.unpack . toCLI) v1  <> "(locked till " <> (formatISO8601 . convertToExternalPosix) d1 <> ")"
                                    <> "\n\t   * " <> (T.unpack . toCLI) v2  <> "(locked till " <> (formatISO8601 . convertToExternalPosix) d2 <> ")"
            (Retrieved ,Locked d2) -> "Funds Partially Locked :"
                                    <> "\n\t   * " <> (T.unpack . toCLI) v1  <> "(retrieved)"
                                    <> "\n\t   * " <> (T.unpack . toCLI) v2  <> "(locked till " <> (formatISO8601 . convertToExternalPosix) d2 <> ")"
            (Retrieved,Available) -> "Funds Partially Locked :"
                                    <> "\n\t   * " <> (T.unpack . toCLI) v1  <> "(retrieved)"
                                    <> "\n\t   * " <> (T.unpack . toCLI) v2  <> "(available)"
            (Locked d1,Retrieved) -> "Funds Partially Locked :"
                                    <> "\n\t   * " <> (T.unpack . toCLI) v1  <> "(locked till " <> (formatISO8601 . convertToExternalPosix) d1 <> ")"
                                    <> "\n\t   * " <> (T.unpack . toCLI) v2  <> "(retrieved)"
            (Available,Retrieved) -> "Funds Partially Locked :"
                                    <> "\n\t   * " <> (T.unpack . toCLI) v1  <> "(available)"                           
                                    <> "\n\t   * " <> (T.unpack . toCLI) v2  <> "(retrieved)"



data VestingInProgress
    = VestingInProgress
    { scriptLocation :: ScriptLocation
    , validator :: Validator
    , vestingUTxOs :: [UTxO]
    , vestingParam :: VestingParams
    , tranches :: (Tranche,Tranche)
    , investorId :: PubKeyHash
    } deriving (Eq,Show)



data Tranche = Tranche {status :: TrancheStatus, value :: Value } deriving (Eq,Show)

data TrancheStatus
    = Locked {deadline :: POSIXTime}
    | Available 
    | Retrieved deriving (Eq,Show)


availableTokens :: [UTxO] -> Value
availableTokens  = foldMap (\UTxO{value = v} -> v)

getVestingInProgress :: (MonadIO m, MonadReader Environment m)  => m [VestingInProgress]
getVestingInProgress  = do
    now <- convertToInternalPosix <$> liftIO POSIX.getPOSIXTime
    vestingParams <- getVestingIndex
    catMaybes <$> mapM (convert now) vestingParams
    where
        convert :: (MonadIO m, MonadReader Environment m)  => POSIXTime ->  VestingParams -> m (Maybe VestingInProgress)
        convert now vestingParam@VestingParams
            {  vestingTranche1 =
                    VestingTranche
                    { vestingTrancheDate   = deadline1
                    , vestingTrancheAmount = token1}
            ,  vestingTranche2 =
                    VestingTranche
                    { vestingTrancheDate   = deadline2
                    , vestingTrancheAmount = token2}
            , vestingOwner = investorId } = do
          let validator = vestingScript vestingParam
          scriptLocation <- getScriptLocation validator
          getUTxOs (onChain scriptLocation)
            >>= \case
                [] -> return Nothing
                vestingUTxOs@[_] -> do
                    let (s1,s2) = getStatuses now deadline1 deadline2 OneTrancheRetrieved
                    (return . Just) VestingInProgress {tranches = (Tranche s1 token1,Tranche s2 token2),..}
                vestingUTxOs@[_,_] -> do
                    let (s1,s2) = getStatuses now deadline1 deadline2 ZeroTrancheRetrieved
                    (return . Just) VestingInProgress {tranches = (Tranche s1 token1,Tranche s2 token2),..}
                _ ->  error $ "retrieved an unexpected number of UTXos at " <> onChain scriptLocation
                       

getStatuses :: POSIXTime -> POSIXTime -> POSIXTime -> RetrieveState -> (TrancheStatus,TrancheStatus) 
getStatuses now d1 d2 state = 
    case (reached d1, reached d2, state) of 
        (False ,False,ZeroTrancheRetrieved) -> (Locked d1 , Locked d2)
        (False ,False,OneTrancheRetrieved) -> error "unexpected State : 2 Tranches Locked and One Tranche Retrieved " 
        (True,False,ZeroTrancheRetrieved) -> (Available , Locked d2)
        (True,False,OneTrancheRetrieved)  -> (Retrieved , Locked d2)
        (False,True,ZeroTrancheRetrieved) -> (Locked d1 , Available )
        (False,True,OneTrancheRetrieved)  -> (Locked d1 , Retrieved )
        (True,True,ZeroTrancheRetrieved)  -> (Available , Available)
        (True,True,OneTrancheRetrieved)  -> (Retrieved , Available) -- by convention we retrieve tranche 1 firstly      
  where 
    reached = (< now)

data RetrieveState = OneTrancheRetrieved | ZeroTrancheRetrieved
       
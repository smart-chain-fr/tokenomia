{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE EmptyCase #-}
module Smartchain.Contract.CLAP.MonetaryPolicy.Trace (main) where


import           Control.Monad                     (void)
import           Control.Monad.Freer.Error         (throwError)
import qualified Data.Semigroup                    as Semigroup
import           Ledger ( pubKeyHash, POSIXTime,PubKeyHash, CurrencySymbol, TokenName, Value, txId )
import Ledger.Constraints ( mustPayToPubKey )
import Ledger.Value as Value
    ( singleton )
import Plutus.Contract
    ( EmptySchema,
      awaitTxConfirmed,
      ownPubKey, 
      submitTx,
      waitNSlots,
      Contract,
      tell)
import qualified Plutus.Contracts.Currency         as Currency
import Plutus.Contracts.Vesting as Vesting
    ( vestingContract,
      VestingError,
      VestingParams(..),
      VestingSchema,
      VestingTranche(VestingTranche, vestingTrancheDate,
                     vestingTrancheAmount) )

import           Plutus.Trace.Emulator             (EmulatorRuntimeError (GenericError), EmulatorTrace,runEmulatorTraceIO)
import qualified Plutus.Trace.Emulator             as Emulator
import           Wallet.Emulator.Types             (Wallet (..), walletPubKey)
import Data.Time ()
import Data.Time.Clock.POSIX ()
import Ledger.TimeSlot ()
import Data.Text ( Text )
import Plutus.Trace.Effects.EmulatedWalletAPI ()

import Control.Monad.Freer.Reader ( ask, runReader, Reader )
import Control.Monad.Freer ( Eff, Members )
import Plutus.Trace.Effects.RunContract ( RunContract )
import qualified Plutus.Trace.Effects.Waiting as EffectsW
import qualified Ledger.TimeSlot          as TimeSlot  
import           Data.Default             (Default (def))

type TokenId = (CurrencySymbol ,TokenName)
type TokenSupplyAmount = Integer

main :: IO ()
main = do
    print @String "Welcome to the Simulator"
    runEmulatorTraceIO $ do
        let clapTokenName  = "CLAP"
            clapSupplyAmount = 10^(9 :: Integer) 
            minterWallet = getSCWallet Minter


        -- Minting CLAPs    
        policyHash <- mintTokenS minterWallet clapTokenName clapSupplyAmount

        -- Distributing CLAPs to Wallets
        runReader (policyHash,clapTokenName)
            . runReader minterWallet
            . runReader clapSupplyAmount $ do

            moveBudgetDirectlyTo PrivateSale     
            moveBudgetDirectlyTo PublicSale 

            vestBudgetS     Team Lock24MonthsThenRelease25PercentQuartlyTwice
            retrieveBudgetS Team Lock24MonthsThenRelease25PercentQuartlyTwice

            vestBudgetS     Team Lock30MonthsThenRelease25PercentQuartlyTwice
            retrieveBudgetS Team Lock30MonthsThenRelease25PercentQuartlyTwice

            vestBudgetS     Treasury Lock50Percent6Months50Percent12Months
            retrieveBudgetS Treasury Lock50Percent6Months50Percent12Months

            vestBudgetS     Partnerships Lock50Percent6Months50Percent12Months
            retrieveBudgetS Partnerships Lock50Percent6Months50Percent12Months

            vestBudgetS     Ambassadors Lock50Percent6Months50Percent12Months
            retrieveBudgetS Ambassadors Lock50Percent6Months50Percent12Months

            vestBudgetS  Communication Lock50Percent6Months50Percent12Months
            retrieveBudgetS Communication Lock50Percent6Months50Percent12Months

    print @String "End of the Simulator"


moveBudgetDirectlyTo
    :: (Members '[EffectsW.Waiting,RunContract,Reader Wallet,Reader TokenId,Reader TokenSupplyAmount] effs)
    => Account
    -> Eff effs ()
moveBudgetDirectlyTo account  = do
    let pkh = pubKeyHash $ walletPubKey (getSCWallet account)
    tokenCreatorWallet <- ask @Wallet
    budget <- getBudget account >>= valueOfCLAP
    _ <- Emulator.activateContractWallet tokenCreatorWallet  
        ((submitTx @() @EmptySchema @Text $ mustPayToPubKey pkh budget) >>=  awaitTxConfirmed . txId)
    _ <- Emulator.waitNSlots 10
    pure ()

data Account
    = Minter
    | PrivateSale
    | PublicSale
    | Team
    | Treasury
    | Partnerships
    | Ambassadors
    | Communication

data VestingScheme =
     Lock50Percent6Months50Percent12Months
   | Lock24MonthsThenRelease25PercentQuartlyTwice
   | Lock30MonthsThenRelease25PercentQuartlyTwice
   | LockSixMonths


getSCWallet :: Account -> Wallet
getSCWallet = \case
 Minter -> Wallet 1
 PrivateSale -> Wallet 2
 PublicSale -> Wallet 3
 Team  -> Wallet 4
 Treasury  -> Wallet 5
 Partnerships -> Wallet 6
 Ambassadors -> Wallet 7
 Communication -> Wallet 8

getBudget
    :: (Members '[Reader TokenSupplyAmount] effs)
    => Account
    -> Eff effs Integer
getBudget account = do
    clapSupplyAmount <- ask @TokenSupplyAmount
    return . truncate @Double . (fromIntegral clapSupplyAmount *)
        $ case account of
            Minter -> 0.0
            PrivateSale -> 0.055
            PublicSale ->  0.2
            Team  -> 0.15
            Treasury  -> 0.509
            Partnerships -> 0.045
            Ambassadors -> 0.04
            Communication -> 0.001

getVestingParams
    :: (Members '[RunContract,Reader TokenId,Reader TokenSupplyAmount] effs)
    => VestingScheme -> Account
    -> Eff effs Vesting.VestingParams
getVestingParams scheme account = do
    budget <- getBudget account
    let _0_month = TimeSlot.scSlotZeroTime def
        _6_months = TimeSlot.scSlotZeroTime def
        _12_month = TimeSlot.scSlotZeroTime def
        _27_months = TimeSlot.scSlotZeroTime def
        _24_months = TimeSlot.scSlotZeroTime def
        _30_months = TimeSlot.scSlotZeroTime def
        _36_months = TimeSlot.scSlotZeroTime def
        wallet = getSCWallet account
    _0_percent_budget   <- valueOfCLAP 0
    _25_percent_budget  <- valueOfCLAP $ budget `div` 4
    _50_percent_budget  <- valueOfCLAP $ budget `div` 2
    _100_percent_budget <- valueOfCLAP budget
    return $ case scheme of
        Lock50Percent6Months50Percent12Months ->
            mkVestingScheme wallet
                _6_months _50_percent_budget
                _12_month _50_percent_budget
        Lock24MonthsThenRelease25PercentQuartlyTwice ->
            mkVestingScheme wallet
                _24_months _25_percent_budget
                _27_months _25_percent_budget
        Lock30MonthsThenRelease25PercentQuartlyTwice ->
            mkVestingScheme wallet
                _30_months _25_percent_budget
                _36_months _25_percent_budget
        LockSixMonths ->
            mkVestingScheme wallet
                _6_months
                _100_percent_budget
                _0_month
                _0_percent_budget


vestBudgetS :: (Members '[RunContract,Reader Wallet,Reader TokenId,Reader TokenSupplyAmount] effs)
    => Account
    -> VestingScheme
    -> Eff effs ()
vestBudgetS account vestingScheme = do
        tokenCreatorWallet <- ask @Wallet
        vestingsParams <- getVestingParams vestingScheme account
        let  contract :: Contract () VestingSchema Vesting.VestingError () = Vesting.vestingContract vestingsParams
        cid1 <- Emulator.activateContractWallet tokenCreatorWallet contract
        _ <- Emulator.callEndpoint @"vest funds" cid1 ()

        pure ()


retrieveBudgetS :: (Members '[EffectsW.Waiting, RunContract,Reader Wallet,Reader TokenId,Reader TokenSupplyAmount] effs) 
    => Account
    -> VestingScheme
    -> Eff effs ()
retrieveBudgetS
    account
    vestingScheme = do
        vestingparams@(VestingParams (VestingTranche slot1 tranche1) (VestingTranche slot2 tranche2) _) <- getVestingParams vestingScheme account
        let  contract   = Vesting.vestingContract vestingparams
        cid2 <- Emulator.activateContractWallet (getSCWallet account)  contract
        _ <- Emulator.waitNSlots (fromIntegral slot1)
        _ <-Emulator.callEndpoint @"retrieve funds" cid2 tranche1
        _ <-Emulator.waitNSlots (fromIntegral slot2)
        Emulator.callEndpoint @"retrieve funds" cid2 tranche2


mintTokenS :: Wallet -> TokenName -> Integer -> EmulatorTrace CurrencySymbol
mintTokenS tokenCreator tokenName tokenAmount =  do
    mintByOwnerHandle <- Emulator.activateContract tokenCreator (mintByOwner tokenName tokenAmount)  "minting CLAPs"
    _ <- Emulator.waitNSlots 10
    Emulator.observableState mintByOwnerHandle >>= \case
                Just (Semigroup.Last monetaryPolicyId) -> pure monetaryPolicyId
                _                                      -> throwError $ GenericError "failed to create currency"

mintByOwner
    :: TokenName
    -> Integer
    -> Contract (Maybe (Semigroup.Last CurrencySymbol)) Currency.CurrencySchema Currency.CurrencyError ()
mintByOwner tokenNameGiven amountGiven =
     ownPubKey >>= mint' tokenNameGiven amountGiven . pubKeyHash
        >>= tell . Just . Semigroup.Last
        >> void (waitNSlots 10) -- not necessary in newer versions (to be removed)


mint'
    :: forall w s e.
    ( Currency.AsCurrencyError e
    )
    => TokenName
    -> Integer
    -> PubKeyHash
    -> Contract w s e CurrencySymbol
mint' tokenName amount pk = Currency.currencySymbol <$> Currency.mintContract pk [(tokenName,amount)]

mkVestingScheme :: Wallet -> POSIXTime-> Value -> POSIXTime -> Value ->  Vesting.VestingParams
mkVestingScheme wallet slot1 amount1 slot2 amount2
    = VestingParams
        { vestingTranche1 = VestingTranche {vestingTrancheDate = slot1, vestingTrancheAmount = amount1 }
        , vestingTranche2 = VestingTranche {vestingTrancheDate = slot2, vestingTrancheAmount = amount2 }
        , vestingOwner = (pubKeyHash . walletPubKey)  wallet }


valueOfCLAP
    :: (Members '[Reader TokenId] effs)
    => Integer
    -> Eff effs Value
valueOfCLAP amount = do
    (policyHash,tokenName) <- ask @(CurrencySymbol,TokenName)
    return $ Value.singleton policyHash tokenName amount

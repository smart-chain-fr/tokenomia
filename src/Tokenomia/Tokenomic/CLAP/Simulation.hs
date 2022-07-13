{-# LANGUAGE DuplicateRecordFields #-}

module Tokenomia.Tokenomic.CLAP.Simulation () where

-- import Control.Monad (void)
-- import           Control.Monad.Freer.Error         (throwError)
-- import qualified Data.Semigroup                    as Semigroup
-- import           Ledger ( TokenName , pubKeyHash, POSIXTime, CurrencySymbol, Value, txId )
-- import Ledger.Constraints ( mustPayToPubKey )
-- import Ledger.Value as Value
--     ( singleton )
-- import Plutus.Contract
--     ( EmptySchema,
--       awaitTxConfirmed,
--       submitTx,
--       Contract,
--       tell)

-- import           Plutus.Trace.Emulator             (EmulatorRuntimeError (GenericError), EmulatorTrace,runEmulatorTraceIO)
-- import qualified Plutus.Trace.Emulator             as Emulator
-- import Wallet.Emulator ( knownWallet, Wallet )
-- import Data.Time ()
-- import Data.Time.Clock.POSIX ()
-- import Ledger.TimeSlot ()
-- import Data.Text ( Text )
-- import Plutus.Trace.Effects.EmulatedWalletAPI ()

-- import Control.Monad.Freer.Reader ( ask, Reader,runReader )
-- import Control.Monad.Freer ( Eff, Members )
-- import Plutus.Trace.Effects.RunContract ( RunContract )
-- import qualified Plutus.Trace.Effects.Waiting as EffectsW
-- import qualified Ledger.TimeSlot          as TimeSlot
-- import           Data.Default             (Default (def))
-- import           Data.Semigroup         (Last (..))
-- import Tokenomia.Token.CLAPStyle.MonetaryPolicy
--     ( mintContract, CLAPMonetaryPolicyError )

-- import Tokenomia.Vesting.Contract as Vesting
--     ( vestingContract,
--       VestingError,
--       VestingParams(..),
--       VestingSchema,
--       VestingTranche(VestingTranche, vestingTrancheDate,
--                      vestingTrancheAmount) )
-- type TokenId = (CurrencySymbol ,TokenName)
-- type TokenSupplyAmount = Integer

-- main :: IO ()
-- main = do
--     print @String "Welcome to the Simulator"
--     runEmulatorTraceIO $ do
--         let clapTokenName :: TokenName = "CLAP"
--             clapSupplyAmount :: TokenSupplyAmount = 10^(12 :: Integer)
--         let minterWallet = getSCWallet Minter

--         -- Minting CLAPs
--         policyHash <- mintTokenS minterWallet clapTokenName clapSupplyAmount
--         -- Distributing CLAPs to Wallets
--         runReader (policyHash,clapTokenName)
--             . runReader minterWallet
--             . runReader clapSupplyAmount $ do

--             moveBudgetDirectlyTo PrivateSale
--             moveBudgetDirectlyTo PublicSale

--             vestBudgetS  Team Lock24MonthsThenRelease25PercentQuartlyTwice
--             vestBudgetS  Team Lock30MonthsThenRelease25PercentQuartlyTwice
--             vestBudgetS  Treasury Lock50Percent6Months50Percent12Months
--             vestBudgetS  Partnerships Lock50Percent6Months50Percent12Months
--             vestBudgetS  Ambassadors Lock50Percent6Months50Percent12Months
--             vestBudgetS  Communication Lock50Percent6Months50Percent12Months

--             retrieveBudgetS Treasury Lock50Percent6Months50Percent12Months
--             retrieveBudgetS Partnerships Lock50Percent6Months50Percent12Months
--             retrieveBudgetS Ambassadors Lock50Percent6Months50Percent12Months
--             retrieveBudgetS Communication Lock50Percent6Months50Percent12Months

--             retrieveBudgetS Team Lock30MonthsThenRelease25PercentQuartlyTwice
--             retrieveBudgetS Team Lock24MonthsThenRelease25PercentQuartlyTwice

--     print @String "End of the Simulator"

-- moveBudgetDirectlyTo
--     :: (Members '[EffectsW.Waiting,RunContract,Reader Wallet,Reader TokenId,Reader TokenSupplyAmount] effs)
--     => Account
--     -> Eff effs ()
-- moveBudgetDirectlyTo account  = do
--     let pkh = pubKeyHash $ walletPubKey (getSCWallet account)
--     tokenCreatorWallet <- ask @Wallet
--     budget <- getBudget account >>= valueOfCLAP
--     _ <- Emulator.activateContractWallet tokenCreatorWallet
--         ((submitTx @() @EmptySchema @Text $ mustPayToPubKey pkh budget) >>=  awaitTxConfirmed . txId)
--     _ <- Emulator.waitNSlots 10
--     pure ()

-- data Account
--     = Minter
--     | PrivateSale
--     | PublicSale
--     | Team
--     | Treasury
--     | Partnerships
--     | Ambassadors
--     | Communication

-- data VestingScheme =
--      Lock50Percent6Months50Percent12Months
--    | Lock24MonthsThenRelease25PercentQuartlyTwice
--    | Lock30MonthsThenRelease25PercentQuartlyTwice
--    | LockSixMonths

-- getSCWallet :: Account -> Wallet
-- getSCWallet = \case
--  Minter -> knownWallet 1
--  PrivateSale -> knownWallet 2
--  PublicSale -> knownWallet 3
--  Team  -> knownWallet 4
--  Treasury  -> knownWallet 5
--  Partnerships -> knownWallet 6
--  Ambassadors -> knownWallet 7
--  Communication -> knownWallet 8

-- getBudget
--     :: (Members '[Reader TokenSupplyAmount] effs)
--     => Account
--     -> Eff effs Integer
-- getBudget account = do
--     clapSupplyAmount <- ask @TokenSupplyAmount
--     return . truncate @Double . (fromIntegral clapSupplyAmount *)
--         $ case account of
--             Minter -> 0.0
--             PrivateSale -> 0.055
--             PublicSale ->  0.2
--             Team  -> 0.15
--             Treasury  -> 0.509
--             Partnerships -> 0.045
--             Ambassadors -> 0.04
--             Communication -> 0.001

-- getVestingParams
--     :: (Members '[RunContract,Reader TokenId,Reader TokenSupplyAmount] effs)
--     => VestingScheme -> Account
--     -> Eff effs Vesting.VestingParams
-- getVestingParams scheme account = do
--     budget <- getBudget account
--     let _0_month = TimeSlot.scSlotZeroTime def + 10000
--         _6_months = TimeSlot.scSlotZeroTime def + 11000
--         _12_month = TimeSlot.scSlotZeroTime def + 12000
--         _27_months = TimeSlot.scSlotZeroTime def + 13000
--         _24_months = TimeSlot.scSlotZeroTime def + 14000
--         _30_months = TimeSlot.scSlotZeroTime def + 15000
--         _36_months = TimeSlot.scSlotZeroTime def + 16000
--         wallet = getSCWallet account
--     _0_percent_budget   <- valueOfCLAP 0
--     _25_percent_budget  <- valueOfCLAP $ budget `div` 4
--     _50_percent_budget  <- valueOfCLAP $ budget `div` 2
--     _100_percent_budget <- valueOfCLAP budget
--     return $ case scheme of
--         Lock50Percent6Months50Percent12Months ->
--             mkVestingScheme wallet
--                 _6_months _50_percent_budget
--                 _12_month _50_percent_budget
--         Lock24MonthsThenRelease25PercentQuartlyTwice ->
--             mkVestingScheme wallet
--                 _24_months _25_percent_budget
--                 _27_months _25_percent_budget
--         Lock30MonthsThenRelease25PercentQuartlyTwice ->
--             mkVestingScheme wallet
--                 _30_months _25_percent_budget
--                 _36_months _25_percent_budget
--         LockSixMonths ->
--             mkVestingScheme wallet
--                 _6_months
--                 _100_percent_budget
--                 _0_month
--                 _0_percent_budget

-- vestBudgetS :: (Members '[EffectsW.Waiting,RunContract,Reader Wallet,Reader TokenId,Reader TokenSupplyAmount] effs)
--     => Account
--     -> VestingScheme
--     -> Eff effs ()
-- vestBudgetS account vestingScheme = do
--         tokenCreatorWallet <- ask @Wallet
--         vestingsParams <- getVestingParams vestingScheme account
--         let  contract :: Contract () VestingSchema Vesting.VestingError () = Vesting.vestingContract vestingsParams
--         cid1 <- Emulator.activateContractWallet tokenCreatorWallet contract
--         _ <- Emulator.callEndpoint @"vest funds" cid1 ()
--         _ <- Emulator.waitNSlots 5
--         pure ()

-- retrieveBudgetS :: (Members '[EffectsW.Waiting, RunContract,Reader Wallet,Reader TokenId,Reader TokenSupplyAmount] effs)
--     => Account
--     -> VestingScheme
--     -> Eff effs ()
-- retrieveBudgetS
--     account
--     vestingScheme = do
--         vestingparams@(VestingParams (VestingTranche _ tranche1) (VestingTranche _ tranche2) _) <- getVestingParams vestingScheme account
--         let  contract   = Vesting.vestingContract vestingparams
--         cid2 <- Emulator.activateContractWallet (getSCWallet account)  contract
--         _ <- Emulator.waitNSlots 5
--         _ <-Emulator.callEndpoint @"retrieve funds" cid2 tranche1
--         _ <-Emulator.waitNSlots 5
--         Emulator.callEndpoint @"retrieve funds" cid2 tranche2
--         void $ Emulator.waitNSlots 5

-- mintTokenS :: Wallet -> TokenName -> TokenSupplyAmount ->  EmulatorTrace CurrencySymbol
-- mintTokenS tokenCreator tokenName tokenSupplyAmount =  do
--     mintByOwnerHandle <- Emulator.activateContract tokenCreator (mintCLAPContract' tokenCreator tokenName tokenSupplyAmount) "Minting Claps"
--     _ <- Emulator.waitNSlots 2
--     r <- Emulator.observableState mintByOwnerHandle >>= \case
--                 Just (Semigroup.Last monetaryPolicyId) -> pure monetaryPolicyId
--                 _                                      -> throwError $ GenericError "failed to create currency"
--     _ <- Emulator.waitNSlots 2
--     pure r

-- mintCLAPContract' :: Wallet -> TokenName -> TokenSupplyAmount -> Contract (Maybe (Semigroup.Last CurrencySymbol)) EmptySchema CLAPMonetaryPolicyError CurrencySymbol
-- mintCLAPContract' w tokenName tokenSupplyAmount = do
--     result <- fst <$> mintContract (Ledger.pubKeyHash $ walletPubKey w) tokenName tokenSupplyAmount
--     (tell . Just . Last) result
--     pure result

-- mkVestingScheme :: Wallet -> POSIXTime-> Value -> POSIXTime -> Value ->  Vesting.VestingParams
-- mkVestingScheme wallet slot1 amount1 slot2 amount2
--     = VestingParams
--         { vestingTranche1 = VestingTranche {vestingTrancheDate = slot1, vestingTrancheAmount = amount1 }
--         , vestingTranche2 = VestingTranche {vestingTrancheDate = slot2, vestingTrancheAmount = amount2 }
--         , vestingOwner = (pubKeyHash . walletPubKey)  wallet }

-- valueOfCLAP
--     :: (Members '[Reader TokenId] effs)
--     => Integer
--     -> Eff effs Value
-- valueOfCLAP amount = do
--     (policyHash,tokenName) <- ask @(CurrencySymbol,TokenName)
--     return $ Value.singleton policyHash tokenName amount

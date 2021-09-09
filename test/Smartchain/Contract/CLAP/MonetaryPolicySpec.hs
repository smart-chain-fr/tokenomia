{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Smartchain.Contract.CLAP.MonetaryPolicySpec (tests) where

import           Control.Monad             (void)
import qualified Ledger
import           Plutus.Contract hiding (throwError)
import           Plutus.Contract.Test
import           Ledger.Value           (CurrencySymbol)
import           Smartchain.Contract.CLAP.MonetaryPolicy
import qualified Plutus.Trace.Emulator     as Trace
import qualified Ledger.Ada                           as Ada
import           Test.Tasty
import           Data.Semigroup         (Last (..))                   
import           Plutus.Trace.Emulator             (EmulatorRuntimeError (GenericError))
import           Control.Monad.Freer.Error         (throwError)

--- WARNING : WORK IN PROGRESS

-- Gen TxOutRef 
-- invariants 
    -- 10^12 Claps minted with Policy Hash == f (TxOutRef) 
    -- Wallet calling the contract receive this amount of token minted
    -- (TxOutRef1 != TxOutRef2) -> (f TxOutRef1 != f TxOutRef2)

-- State Machine for Mint and Burn
-- Burning CLAPs is open to anyone owning CLAPs
-- The script size is reasonable and is X 
-- Script hash is XXXXXX in mainnet

tests :: TestTree
tests = testGroup "Monetary CLAP Policy"
    [ checkPredicate
        "minting provides 10^9 CLAPs token to caller" -- To improve
           -- CheckWalletFundsChange w1 (\value -> value & filter (isToken == "CLAP" and amount == 10^9) == 1  )
          -- (walletFundsChange w1 (Ada.lovelaceValueOf (-20)) .&&. 
        (assertDone mintCLAPContract' (Trace.walletInstanceTag w1) (const True) "CLAPs not minted")
        $ do
            _ <- Trace.activateContractWallet w1 (void mintCLAPContract')
            void $ Trace.waitNSlots 2
    , checkPredicate
        "burn claps"
        (assertDone mintCLAPContract' (Trace.walletInstanceTag w1) (const True) "CLAPs not burned"
         .&&. walletFundsChange w1 (Ada.lovelaceValueOf 150) )
        $ do
            cid <- Trace.activateContractWallet w1 (void mintCLAPContract')
            void $ Trace.waitNSlots 2
            (_,txOutRef) <- Trace.observableState cid >>= \case
                                                            Just (Last v) -> pure v
                                                            _ -> throwError $ GenericError "initialisation failed"                                                                                             
            _ <- Trace.activateContractWallet w1 (void $ burnCLAPContract' w1 txOutRef (-1000)) 
            void $ Trace.waitNSlots 2
    , checkPredicate
        "script size is reasonable"
        (assertDone mintCLAPContract' (Trace.walletInstanceTag w1)
          ((30000 >=)
            . Ledger.scriptSize
            . Ledger.unMintingPolicyScript
            . mkCLAPMonetaryPolicyScript . snd) "script too large")
        $ do
            _ <- Trace.activateContractWallet w1 (void mintCLAPContract')
            void $ Trace.waitNSlots 2

    ]

w1 :: Wallet
w1 = Wallet 1

-- genesis :: (InitialDistribution  -> )

burnCLAPContract' ::  Wallet -> Ledger.TxOutRef ->  Integer -> Contract () EmptySchema CLAPMonetaryPolicyError ()
burnCLAPContract' wallet = burnCLAPContract  (Ledger.pubKeyHash $ walletPubKey wallet)

mintCLAPContract' :: Contract (Maybe (Last (CurrencySymbol,Ledger.TxOutRef))) EmptySchema CLAPMonetaryPolicyError (CurrencySymbol,Ledger.TxOutRef)
mintCLAPContract' = do 
    result <- mintCLAPContract (Ledger.pubKeyHash $ walletPubKey w1)
    (tell . Just . Last) result
    pure result

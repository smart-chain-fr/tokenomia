{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
<<<<<<< HEAD:test/Spec/Tokenomia/Token/CLAPStyle/MonetaryPolicy.hs

module Spec.Tokenomia.Token.CLAPStyle.MonetaryPolicy (tests) where
=======
module Spec.Smartchain.Contract.CLAP.MonetaryPolicy (tests) where
>>>>>>> main:test/Spec/Smartchain/Contract/CLAP/MonetaryPolicy.hs

import           Control.Monad             (void)
import qualified Ledger
import           Plutus.Contract hiding (throwError)
import           Plutus.Contract.Test
import           Ledger.Value           (CurrencySymbol,TokenName (..))
import           Tokenomia.Token.CLAPStyle.MonetaryPolicy
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
            (_,params) <- Trace.observableState cid >>= \case
                                                            Just (Last v) -> pure v
                                                            _ -> throwError $ GenericError "initialisation failed"                                                                                             
            _ <- Trace.activateContractWallet w1 (void $ burnContract' w1 params (-1000)) 
            void $ Trace.waitNSlots 2
    , checkPredicate
        "script size is reasonable"
        (assertDone mintCLAPContract' (Trace.walletInstanceTag w1)
          ((30000 >=)
            . Ledger.scriptSize
            . Ledger.unMintingPolicyScript
            . mkMonetaryPolicyScript . snd) "script too large")
        $ do
            _ <- Trace.activateContractWallet w1 (void mintCLAPContract')
            void $ Trace.waitNSlots 2

    ]


-- genesis :: (InitialDistribution  -> )

burnContract' ::  Wallet -> Params -> Integer -> Contract () EmptySchema CLAPMonetaryPolicyError ()
burnContract' wallet params amountGiven
    = void $ burnContract  @() @EmptySchema @CLAPMonetaryPolicyError
        (Ledger.pubKeyHash $ walletPubKey wallet) params amountGiven

mintCLAPContract' :: Contract (Maybe (Last (CurrencySymbol,Params))) EmptySchema CLAPMonetaryPolicyError (CurrencySymbol,Params)
mintCLAPContract' = do 
    result <- mintContract (Ledger.pubKeyHash $ walletPubKey w1) (TokenName "CLAP") (1000000000000 :: Integer)
    (tell . Just . Last) result
    pure result

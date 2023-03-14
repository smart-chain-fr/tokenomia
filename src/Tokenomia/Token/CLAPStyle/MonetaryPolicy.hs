{-# LANGUAGE DataKinds                                 #-}
{-# LANGUAGE DeriveAnyClass                            #-}
{-# LANGUAGE DerivingStrategies                        #-}
{-# LANGUAGE FlexibleContexts                          #-}
{-# LANGUAGE MonoLocalBinds                            #-}
{-# LANGUAGE NamedFieldPuns                            #-}
{-# LANGUAGE NoImplicitPrelude                         #-}
{-# LANGUAGE OverloadedStrings                         #-}
{-# LANGUAGE TemplateHaskell                           #-}
{-# LANGUAGE TypeApplications                          #-}
{-# LANGUAGE TypeOperators                             #-}
{-# LANGUAGE ViewPatterns                              #-}

{-# LANGUAGE DeriveGeneric                             #-}
{-# LANGUAGE ImportQualifiedPost                       #-}
{-# LANGUAGE MultiParamTypeClasses                     #-}
{-# LANGUAGE NumericUnderscores                        #-}
{-# LANGUAGE RankNTypes                                #-}
{-# LANGUAGE RecordWildCards                           #-}
{-# LANGUAGE ScopedTypeVariables                       #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas          #-}

module Tokenomia.Token.CLAPStyle.MonetaryPolicy
    ( AsCLAPMonetaryPolicyError(..)
    , CLAPMonetaryPolicyError(..)
    , MonetaryPolicySchema
    , Params(..)
    , burnContract
    , mintContract
    , mkMonetaryPolicyScript
    ) where


import Control.Lens                                    ( makeClassyPrisms, review )
import PlutusTx.Prelude
    ( Applicative(pure)
    , Bool(..)
    , Eq((==))
    , traceIfFalse
    , ($)
    , (&&)
    , (.)
    , (<)
    , (>>)
    , (>>=)
    , (||)
    )

import Plutus.Contract
    as Contract
    ( AsContractError(_ContractError)
    , Contract
    , ContractError
    , Endpoint
    , awaitTxConfirmed
    , mapError
    , submitTxConstraintsWith
    , type (.\/)
    , utxosAt
    )
import Plutus.Contract.Wallet                          ( getUnspentOutput )

import Ledger
    ( CardanoAddress
    , CurrencySymbol
    , MintingPolicy
    , TxOutRef(..)
    , getCardanoTxId
    , mkMintingPolicyScript
    )
import Ledger.Constraints qualified as Constraints
import Plutus.Script.Utils.V1.Scripts                  ( scriptCurrencySymbol )
import Plutus.V1.Ledger.Contexts qualified as V
import PlutusTx                                        ( BuiltinData, applyCode, compile, liftCode )

import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value                                    ( TokenName(..), singleton, valueOf )

import Data.Aeson                                      ( FromJSON, ToJSON )
import GHC.Generics                                    ( Generic )
import PlutusTx qualified
import PlutusTx.Builtins.Internal                      ()
import Prelude                                         ( Integer, Semigroup(..) )
import Prelude qualified as Haskell



-- /////////////////
-- // On-Chain Part
-- /////////////////

data Params = Params
  { txOutRefToConsume :: TxOutRef
  , amount    :: Integer
  , tokenName :: TokenName }
  deriving stock (Generic, Haskell.Show, Haskell.Eq)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''Params


mkMonetaryPolicyScript :: Params -> MintingPolicy
mkMonetaryPolicyScript param = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.mkUntypedMintingPolicy . monetaryPolicy ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode param

{-# INLINABLE monetaryPolicy #-}
monetaryPolicy :: Params -> BuiltinData -> V.ScriptContext -> Bool
monetaryPolicy a b c =  burningPolicy a b c || mintingPolicy a b c

{-# INLINABLE mintingPolicy #-}
mintingPolicy :: Params -> BuiltinData -> V.ScriptContext -> Bool
mintingPolicy Params{ txOutRefToConsume = (TxOutRef refHash refIdx),..} _ ctx@V.ScriptContext{V.scriptContextTxInfo=txinfo}
    =  traceIfFalse "E1" {- Value minted different from expected" -}
        (singleton (V.ownCurrencySymbol ctx) tokenName  amount == V.txInfoMint txinfo)
    && traceIfFalse "E2" {- Pending transaction does not spend the designated transaction output (necessary for one-time minting Policy) -}
        (V.spendsOutput txinfo refHash refIdx)

{-# INLINABLE burningPolicy #-}
burningPolicy :: Params -> BuiltinData -> V.ScriptContext -> Bool
burningPolicy Params {tokenName} _ context@V.ScriptContext{V.scriptContextTxInfo=txinfo}
    = valueOf (V.txInfoMint txinfo) (V.ownCurrencySymbol context) tokenName < 0



-- /////////////////
-- // Off-Chain Part
-- /////////////////

type Amount = Integer
type MonetaryPolicySchema
    = Endpoint   "Mint" ()
    .\/ Endpoint "Burn" (CurrencySymbol,Amount)

newtype CLAPMonetaryPolicyError =
    CLAPMonetaryPolicyError ContractError
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''CLAPMonetaryPolicyError

instance AsContractError CLAPMonetaryPolicyError where
    _ContractError = _CLAPMonetaryPolicyError


burnContract
    :: forall w s e.
    ( AsCLAPMonetaryPolicyError e
    )
    => CardanoAddress
    -> Params
    -> Integer
    -> Contract w s e ()
burnContract addr monetaryPolicyParams@Params {..} amountToBurn =
    mapError (review _CLAPMonetaryPolicyError) $ do
    let policyHash = (scriptCurrencySymbol . mkMonetaryPolicyScript) monetaryPolicyParams
        monetaryPolicyScript = mkMonetaryPolicyScript monetaryPolicyParams
        valueToBurn = singleton policyHash tokenName amountToBurn
    utxosInBurnerWallet <- utxosAt addr
    submitTxConstraintsWith
            @Scripts.Any
            (Constraints.plutusV1MintingPolicy monetaryPolicyScript <> Constraints.unspentOutputs utxosInBurnerWallet)
            (Constraints.mustMintValue valueToBurn)
     >>= awaitTxConfirmed . getCardanoTxId



mintContract
    :: forall w s e.
    ( AsCLAPMonetaryPolicyError e
    )
    => CardanoAddress
    -> TokenName
    -> Integer
    -> Contract w s e (CurrencySymbol,Params)
mintContract addr tokenName amount =
    mapError (review _CLAPMonetaryPolicyError) $ do
    txOutRefToConsume <- getUnspentOutput
    let monetaryPolicyParams = Params {..}
        policyHash = (scriptCurrencySymbol . mkMonetaryPolicyScript) monetaryPolicyParams
        monetaryPolicyScript = mkMonetaryPolicyScript monetaryPolicyParams
        valueToMint = singleton policyHash tokenName amount
    utxosInWallet <- utxosAt addr
    submitTxConstraintsWith
            @Scripts.Any
            (Constraints.plutusV1MintingPolicy monetaryPolicyScript <> Constraints.unspentOutputs utxosInWallet)
            (Constraints.mustSpendPubKeyOutput txOutRefToConsume <> Constraints.mustMintValue valueToMint)
     >>= awaitTxConfirmed . getCardanoTxId
     >>  pure (policyHash,monetaryPolicyParams)

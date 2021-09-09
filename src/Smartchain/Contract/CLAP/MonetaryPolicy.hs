{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ViewPatterns       #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

module Smartchain.Contract.CLAP.MonetaryPolicy(
    MonetaryPolicySchema
    , CLAPMonetaryPolicyError(..)
    , AsCLAPMonetaryPolicyError(..)
    , mintCLAPContract
    , burnCLAPContract
    , mkCLAPMonetaryPolicyScript
    , clapTotalSupply
    ) where

import Control.Lens ( makeClassyPrisms, review )
import PlutusTx.Prelude
    ( (>>),
      (>>=),
      (<),
      Bool(..),
      (.),
      Eq((==)),
      Applicative(pure),
      (&&),
      (||),
      (<$>),
      ($),
      traceIfFalse )

import Plutus.Contract as Contract
    ( awaitTxConfirmed,
      submitTxConstraintsWith,
      utxoAt,
      mapError,
      Endpoint,
      type (.\/),
      Contract,
      AsContractError(_ContractError),
      ContractError )
import           Plutus.Contract.Wallet (getUnspentOutput)

import Ledger
    ( TxOutRef(..),
      scriptCurrencySymbol,
      txId,
      pubKeyHashAddress,
      mkMintingPolicyScript,
      PubKeyHash,
      MintingPolicy,
      AssetClass,
      CurrencySymbol,
      Value )                 
import qualified Ledger.Constraints     as Constraints
import qualified Ledger.Contexts        as V
import PlutusTx ( BuiltinData, applyCode, liftCode, compile )

import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           (TokenName (..),assetClass,assetClassValue, valueOf)

import           Data.Aeson             (FromJSON, ToJSON)
import           GHC.Generics           (Generic)
import           Prelude                (Semigroup (..),Integer)
import qualified Prelude                as Haskell

import PlutusTx.Builtins.Internal ()

{-# INLINABLE clapAssetClass #-}
clapAssetClass :: CurrencySymbol  -> AssetClass
clapAssetClass clapPolicyHash = assetClass clapPolicyHash (TokenName "CLAP")

{-# INLINABLE clapTotalSupply #-}
clapTotalSupply :: CurrencySymbol -> Value
clapTotalSupply clapPolicyHash
    = assetClassValue
        (clapAssetClass clapPolicyHash )
        1_000_000_000_000

-- /////////////////
-- // On-Chain Part
-- /////////////////


mkCLAPMonetaryPolicyScript :: TxOutRef -> MintingPolicy
mkCLAPMonetaryPolicyScript txOutRef = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \c -> Scripts.wrapMintingPolicy (monetaryPolicy c) ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode txOutRef
    where
        {-# INLINABLE monetaryPolicy #-}
        monetaryPolicy :: TxOutRef -> BuiltinData -> V.ScriptContext -> Bool
        monetaryPolicy a b c =  burningPolicy a b c || mintingPolicy a b c

        {-# INLINABLE mintingPolicy #-}
        mintingPolicy :: TxOutRef -> BuiltinData -> V.ScriptContext -> Bool
        mintingPolicy (TxOutRef refHash refIdx) _ ctx@V.ScriptContext{V.scriptContextTxInfo=txinfo}
            =  traceIfFalse "E1" {- Value minted different from expected (10^9 CLAPs)" -}
                (clapTotalSupply (V.ownCurrencySymbol ctx) == V.txInfoMint txinfo)
            && traceIfFalse "E2" {- Pending transaction does not spend the designated transaction output (necessary for one-time minting Policy) -}
                (V.spendsOutput txinfo refHash refIdx)

        {-# INLINABLE burningPolicy #-}
        burningPolicy :: TxOutRef -> BuiltinData -> V.ScriptContext -> Bool
        burningPolicy _ _ context@V.ScriptContext{V.scriptContextTxInfo=txinfo}
            = valueOf (V.txInfoMint txinfo) (V.ownCurrencySymbol context) (TokenName "CLAP") < 0



-- /////////////////
-- // Off-Chain Part
-- /////////////////

type Amount = Integer
type MonetaryPolicySchema
    = Endpoint   "Mint CLAPs" ()
    .\/ Endpoint "Burn CLAPs" (CurrencySymbol,Amount)

newtype CLAPMonetaryPolicyError =
    CLAPMonetaryPolicyError ContractError
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''CLAPMonetaryPolicyError

instance AsContractError CLAPMonetaryPolicyError where
    _ContractError = _CLAPMonetaryPolicyError


burnCLAPContract
    :: forall w s e.
    ( AsCLAPMonetaryPolicyError e
    )
    => PubKeyHash
    -> TxOutRef
    -> Integer
    -> Contract w s e ()
burnCLAPContract burnerPK txOutRef amount =
    mapError (review _CLAPMonetaryPolicyError) $ do
    let clapPolicyHash = (scriptCurrencySymbol . mkCLAPMonetaryPolicyScript) txOutRef
        clapMonetaryPolicyScript = mkCLAPMonetaryPolicyScript txOutRef
    utxosInBurnerWallet <- utxoAt (pubKeyHashAddress burnerPK)
    submitTxConstraintsWith
            @Scripts.Any
            (Constraints.mintingPolicy clapMonetaryPolicyScript <> Constraints.unspentOutputs utxosInBurnerWallet)
            (Constraints.mustMintValue $ assetClassValue (clapAssetClass clapPolicyHash) amount)
     >>= awaitTxConfirmed . txId



mintCLAPContract
    :: forall w s e.
    ( AsCLAPMonetaryPolicyError e
    )
    => PubKeyHash
    -> Contract w s e (CurrencySymbol,Ledger.TxOutRef)
mintCLAPContract pk =
    mapError (review _CLAPMonetaryPolicyError) $ do
    (txOutRef,clapMonetaryPolicyScript , clapPolicyHash )
        <- clapInstance
            Haskell.id
            mkCLAPMonetaryPolicyScript
            (scriptCurrencySymbol . mkCLAPMonetaryPolicyScript) <$> getUnspentOutput
    utxosInWallet <- utxoAt (pubKeyHashAddress pk)
    submitTxConstraintsWith
            @Scripts.Any
            (Constraints.mintingPolicy clapMonetaryPolicyScript <> Constraints.unspentOutputs utxosInWallet)
            (Constraints.mustSpendPubKeyOutput txOutRef         <> Constraints.mustMintValue (clapTotalSupply clapPolicyHash))
     >>= awaitTxConfirmed . txId
     >>  pure (clapPolicyHash,txOutRef)



clapInstance
    :: (TxOutRef -> TxOutRef)
    -> (TxOutRef -> MintingPolicy )
    -> (TxOutRef -> CurrencySymbol)
    -> (TxOutRef -> (TxOutRef,MintingPolicy,CurrencySymbol))
clapInstance a b c = do
    x <- a
    y <- b
    z <- c
    Haskell.return (x,y,z)
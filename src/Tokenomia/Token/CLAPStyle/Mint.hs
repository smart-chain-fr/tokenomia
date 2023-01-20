{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tokenomia.Token.CLAPStyle.Mint
    ( mint
    , mint' ) where
    
import           Prelude hiding ((+),print)
import           PlutusTx.Prelude  (AdditiveSemigroup((+)) )

import           Control.Monad.Reader hiding (ask)

import qualified Data.Text as Text
import qualified Data.Text.Encoding as E
import           Data.List.NonEmpty
import qualified Data.ByteString.UTF8 as BSU 

import           Control.Monad.Except

import           Ledger hiding (mint, Address, Mint, Params, scriptCurrencySymbol)
import           Plutus.Script.Utils.V1.Scripts (scriptCurrencySymbol)
import qualified Ledger.Value as L
import           Ledger.Ada

import           Tokenomia.Token.CLAPStyle.MonetaryPolicy
import           Tokenomia.Common.Environment
import           Tokenomia.Wallet.CLI

import           Tokenomia.Wallet.UTxO 
import           Tokenomia.Wallet.WalletUTxO
import           Tokenomia.Common.Transacting

import           Tokenomia.Wallet.LocalRepository hiding (fetchById)
import           Tokenomia.Script.LocalRepository
import           Tokenomia.Wallet.Collateral.Read
import           Tokenomia.Common.Error

import           Tokenomia.Common.Shell.Console (printLn)
import           Tokenomia.Common.Shell.InteractiveMenu (ask,askString)

import           Tokenomia.Wallet.ChildAddress.ChildAddressRef
import           Tokenomia.Wallet.Type
import           Tokenomia.Wallet.ChildAddress.LocalRepository
import            Data.Text.Encoding as TSE

mint :: 
    ( MonadIO m
    , MonadReader Environment m
    , MonadError TokenomiaError m)  
    => m ()
mint = do
    Wallet{name} <- fetchWalletsWithCollateral >>= whenNullThrow NoWalletWithCollateral 
        >>= \wallets -> do
            printLn "Select the minter wallet : "
            askToChooseAmongGivenWallets wallets 
    tokenNameToMint  <- fromText . Text.pack <$> askString "> Token Name : "
    amountToMint     <- ask @Integer "> Total Supply to Mint : "
    _ <- mint' name tokenNameToMint amountToMint  
    return ()
  where 
        fromText :: Text.Text -> TokenName
        fromText = L.tokenName . E.encodeUtf8


mint' :: 
    ( MonadIO m
    , MonadReader Environment m
    , MonadError TokenomiaError m)  
    => WalletName
    -> TokenName
    -> Integer
    -> m CurrencySymbol
mint' walletName tokenName amount = do
    let firstChildAddress = ChildAddressRef walletName 0
    walletUTxOToConsume <- selectBiggestStrictlyADAsNotCollateral firstChildAddress >>= whenNothingThrow NoADAsOnChildAddress
    ChildAddress {address} <- fetchById firstChildAddress
    let monetaryPolicy = mkMonetaryPolicyScript Params { txOutRefToConsume = txOutRef . utxo $ walletUTxOToConsume , .. }
        policyhash = scriptCurrencySymbol monetaryPolicy
        valueToMint = L.singleton policyhash tokenName amount
      
    printLn   "-------------------------"
    printLn $ "Policy hash will be : " <> show policyhash
    printLn   "-------------------------"

    monetaryScript <- registerMintingScriptFile monetaryPolicy

    buildAndSubmit
      (Unbalanced (FeeAddressRef firstChildAddress))
      (Just $ CollateralAddressRef firstChildAddress)
      TxBuild
        { inputsFromWallet =  FromWallet walletUTxOToConsume :| []
        , inputsFromScript = Nothing
        , outputs = ToWallet address (valueToMint + lovelaceValueOf 1379280 ) Nothing :| [] 
        , validitySlotRangeMaybe = Nothing
        , tokenSupplyChangesMaybe = Just $ Mint { amount = valueToMint, script = monetaryScript} :| []
        , metadataMaybe = Nothing 
        , ..}
    return policyhash

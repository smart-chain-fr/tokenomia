{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE RecordWildCards #-}

module Tokenomia.Token.Consolidate where


import           Data.List.NonEmpty
import qualified Data.ByteString.Lazy.Char8 as C
import           Control.Monad.Reader hiding (ask)
import           Control.Monad.Except
import           Ledger.Ada ( lovelaceValueOf )
import           Data.Foldable
import           Ledger (Slot(Slot))

import           Tokenomia.Adapter.Cardano.CLI.Node

import           Tokenomia.Adapter.Cardano.CLI.Environment
import           Tokenomia.Adapter.Cardano.CLI.UTxO
import           Tokenomia.Adapter.Cardano.CLI.Transaction hiding (value)
import Shh.Internal
    ( load,
      (|>),
      ExecArg(asArg),
      ExecReference(SearchPath),
      captureWords )

import           Tokenomia.Adapter.Cardano.CLI.Wallet
import           Tokenomia.Adapter.Cardano.CLI.UTxO.Query (askSelectUTxOByType)
import           Tokenomia.Adapter.Cardano.CLI.Folder ( Folder(Transactions), getFolderPath )
import           Tokenomia.Common.Error
import           Tokenomia.Wallet.Collateral.Read
import Tokenomia.Wallet.CLI
    ( selectBiggestStrictlyADAsNotCollateral,
      askToChooseAmongGivenWallets )
import           Tokenomia.Common.Shell.Console (printLn)
import           PlutusTx.Prelude  (AdditiveGroup((-)))


load SearchPath ["cardano-cli", "md5sum", "mv", "echo"]


consolidate
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError BuildingTxError m)
    => m ()
consolidate = do
    wallet@Wallet{..} <- fetchWalletsWithCollateral >>= whenNullThrow NoWalletWithCollateral
        >>= \wallets -> do
            printLn "Select the minter wallet : "
            askToChooseAmongGivenWallets wallets
    tokensChoosen <- askSelectUTxOByType wallet >>= whenNothingThrow NoFundsToBeRetrieved
    consolidate' wallet tokensChoosen

consolidate'
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError BuildingTxError m)
    => Wallet
    -> [UTxO]
    -> m ()
consolidate' wallet@Wallet {..} utxos = do
    let amount = fold (value <$> utxos)
    (txFolder, rawTx ) <- (\a-> (a,a <> "tx.raw")) <$> getFolderPath Transactions
    aGivenTxOutRef <- txOutRef <$> (selectBiggestStrictlyADAsNotCollateral wallet >>= whenNothingThrow NoADAInWallet)

    buildRaw "0" (toCardanoCLIOptions TxBuild
        { wallet = wallet
        , txIns = fromList (fmap (FromWallet . txOutRef) utxos)
        , txOuts = ToWallet paymentAddress (lovelaceValueOf 1) :| []
        , metadataMaybe = Nothing
        , validitySlotRangeMaybe = Nothing
        , tokenSupplyChangesMaybe = Nothing
        , ..})
    fees <- computeFees (Prelude.length utxos) 1
    Slot synSlotAsInt <- getCurrentSlotSynced
    buildRaw (show fees) (toCardanoCLIOptions TxBuild
        { wallet = wallet
        , txIns = fromList (fmap (FromWallet . txOutRef) utxos)
        , txOuts = ToWallet paymentAddress (amount PlutusTx.Prelude.- lovelaceValueOf fees) :| []
        , metadataMaybe = Nothing
        , validitySlotRangeMaybe = Nothing
        , tokenSupplyChangesMaybe = Nothing
        , ..}
        <> ["--invalid-hereafter", show (synSlotAsInt + 600)])
        -- Hashing the tx.raw and getting its hash x for renaming the tx.raw into x.raw    
    (rawHashTx,signedHashTx) <- (\txHash -> ( txFolder <> txHash <> ".raw"
                                            , txFolder <> txHash <> ".signed" ) )
                                    . C.unpack . Prelude.head <$> liftIO (md5sum rawTx |> captureWords )
    liftIO $ mv rawTx rawHashTx

    printLn "Signing Tx"    >> sign_tx   rawHashTx signedHashTx paymentSigningKeyPath
    printLn "Submitting Tx" >> submit_tx signedHashTx
    printLn "Waiting for confirmation..."
    awaitTxCommitted aGivenTxOutRef 0
    printLn "\nTx committed into ledger"


buildRaw
    :: ( ExecArg a, MonadIO m, MonadReader Environment m)
    => String
    -> a
    -> m ()
buildRaw fee buildTxBody = do
    (_, rawTx ) <- (\a-> (a,a <> "tx.raw")) <$> getFolderPath Transactions

    liftIO $ cardano_cli
        "transaction"
        "build-raw"
        (asArg buildTxBody)
        "--fee" fee
        "--out-file" rawTx
    liftIO $ echo "transaction"
        "build-raw"
        (asArg buildTxBody)
        "--fee" fee
        "--out-file" rawTx
        
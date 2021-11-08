
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module Tokenomia.ICO.Funds.Emission.SpecialCases.Multisig 
        ( multisigOneUTxO
        , multisigTwoUTxOs ) 
        where
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Coerce ( coerce )
import           Control.Monad.Reader ( asks, MonadIO(..), MonadReader )
import           Control.Monad.Except ( MonadError )
import           Shh.Internal ( load, (|>), ExecArg(asArg), ExecReference(SearchPath), captureWords )
import           Ledger ( TxOutRef (..) ) 
import           Ledger.Ada ( lovelaceValueOf )
import           Tokenomia.Wallet.Collateral.Read ( fetchCollateral )
import           Tokenomia.Common.Error ( whenNothingThrow )
import           Tokenomia.Common.Shell.Console (printLn)
import           Tokenomia.Adapter.Cardano.Types ( Address(..) )
import           Tokenomia.Adapter.Cardano.CLI.Serialise (toCLI)
import           Tokenomia.Adapter.Cardano.CLI.Wallet ( Wallet(..), queryWallet )
import           Tokenomia.Adapter.Cardano.CLI.UTxO ( UTxO(txOutRef) )
import           Tokenomia.Adapter.Cardano.CLI.Transaction ( BuildingTxError(NoADAInWallet, WalletWithoutCollateral), submit_tx, awaitTxCommitted, doubleSign_tx, register_protocol_parameters, TxBuild(..), TxIn(..), TxOut(..), toCardanoCLIOptions )
import           Tokenomia.Adapter.Cardano.CLI.Folder ( Folder(Transactions), getFolderPath )
import           Tokenomia.Adapter.Cardano.CLI.Environment ( Environment(magicNumber) )
import           Tokenomia.Wallet.CLI ( selectBiggestStrictlyADAsNotCollateral )

load SearchPath ["cardano-cli","md5sum","mv" ]

type WalletName = String

multisigOneUTxO :: 
    ( MonadIO m
    , MonadReader Environment m
    , MonadError BuildingTxError m ) 
    => WalletName
    -> WalletName
    -> Address 
    -> Integer
    -> m ()
multisigOneUTxO sender1 sender2 receiver amount = do 
    wallet1 <- queryWallet sender1
    Wallet {paymentSigningKeyPath = keyPathWallet2} <- queryWallet sender2
    adaAndFees <- txOutRef <$> (selectBiggestStrictlyADAsNotCollateral wallet1 >>= whenNothingThrow NoADAInWallet)
    let metadataMaybe = Nothing
    submitMultisigTx' keyPathWallet2
      TxBuild
        { wallet = wallet1
        , txIns = FromWallet adaAndFees NE.:| []
        , txOuts = ToWallet receiver (lovelaceValueOf amount) NE.:| []
        , validitySlotRangeMaybe = Nothing
        , tokenSupplyChangesMaybe = Nothing
        , ..}


multisigTwoUTxOs :: 
    ( MonadIO m
    , MonadReader Environment m
    , MonadError BuildingTxError m ) 
    => WalletName
    -> WalletName
    -> Address 
    -> Integer
    -> m ()
multisigTwoUTxOs sender1 sender2 receiver amount = do 
    wallet1 <- queryWallet sender1
    wallet2@Wallet {paymentSigningKeyPath = keyPathWallet2} <- queryWallet sender2
    adaAndFees <- txOutRef <$> (selectBiggestStrictlyADAsNotCollateral wallet1 >>= whenNothingThrow NoADAInWallet)
    utxowallet2 <- txOutRef <$> (selectBiggestStrictlyADAsNotCollateral wallet2 >>= whenNothingThrow NoADAInWallet)
    let metadataMaybe = Nothing
    submitMultisigTx' keyPathWallet2 
      TxBuild
        { wallet = wallet1
        , txIns = FromWallet adaAndFees NE.:| [FromWallet utxowallet2]
        , txOuts = ToWallet receiver (lovelaceValueOf amount) NE.:| []
        , validitySlotRangeMaybe = Nothing
        , tokenSupplyChangesMaybe = Nothing
        , ..}


submitMultisigTx' 
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError BuildingTxError m )
    => FilePath
    -> TxBuild
    -> m ()
submitMultisigTx' keyPathWallet2 txBuild@TxBuild {wallet = wallet@Wallet {..},..} = do
    collateral  <- txOutRef <$> (fetchCollateral wallet >>= whenNothingThrow WalletWithoutCollateral) 
    utxoForFees <- txOutRef <$> (selectBiggestStrictlyADAsNotCollateral wallet >>= whenNothingThrow NoADAInWallet)

    submitMultisigTx 
      paymentSigningKeyPath 
      keyPathWallet2
       ((utxoRef . NE.head) txIns) 
       (toCardanoCLIOptions txBuild <> [ "--tx-in"  , (T.unpack . toCLI) utxoForFees]
        <> [ "--tx-in-collateral" , (T.unpack . toCLI) collateral]
        <> [ "--change-address"   , coerce paymentAddress])


submitMultisigTx :: 
    ( ExecArg a
    , MonadIO m
    , MonadReader Environment m )
    => FilePath
    -> FilePath
    -> TxOutRef
    -> a
    -> m ()
submitMultisigTx keyPathWallet1 keyPathWallet2 aGivenTxOutRef buildTxBody = do
    magicN <- asks magicNumber
    (txFolder, rawTx ) <- (\a-> (a,a <> "tx.raw")) <$> getFolderPath Transactions
    protocolParametersPath <- register_protocol_parameters
    liftIO $ cardano_cli
        "transaction"
        "build"
        "--alonzo-era"
        "--testnet-magic" magicN
        (asArg buildTxBody)
        "--required-signer" keyPathWallet1
        "--required-signer" keyPathWallet2
        "--protocol-params-file" protocolParametersPath
        "--out-file" rawTx

    -- Hashing the tx.raw and getting its hash x for renaming the tx.raw into x.raw    
    (rawHashTx,signedHashTx) <- (\txHash -> ( txFolder <> txHash <> ".raw"
                                            , txFolder <> txHash <> ".signed" ) )
                                    . C.unpack . head <$> liftIO (md5sum rawTx |> captureWords )
    liftIO $ mv rawTx rawHashTx

    printLn "Signing Tx"    >> doubleSign_tx   rawHashTx signedHashTx keyPathWallet1 keyPathWallet2
    printLn "Submitting Tx" >> submit_tx signedHashTx
    printLn "Waiting for confirmation..."
    awaitTxCommitted aGivenTxOutRef 0
    printLn "\nTx committed into ledger"
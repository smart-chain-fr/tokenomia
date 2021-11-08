{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Tokenomia.ICO.Funds.Emission.SpecialCases.MultiUTxOs
    ( multiUTxOsSameAddress )
    where
import Data.List.NonEmpty ( NonEmpty((:|)) )
import           Control.Monad.Reader ( MonadIO(..), MonadReader )
import           Control.Monad.Except ( MonadError )
import           Ledger.Ada ( lovelaceValueOf )
import           Tokenomia.Common.Error ( whenNothingThrow )
import           Tokenomia.Adapter.Cardano.Types ( Address(..) )
import           Tokenomia.Adapter.Cardano.CLI.Wallet ( queryWallet )
import           Tokenomia.Adapter.Cardano.CLI.UTxO ( UTxO(txOutRef), containsCollateral )
import           Tokenomia.Adapter.Cardano.CLI.Transaction ( BuildingTxError(NoADAInWallet), submit', TxBuild(..), TxIn(..), TxOut(..) )
import           Tokenomia.Adapter.Cardano.CLI.Environment ( Environment )
import           Tokenomia.Wallet.CLI ( fetchUTxOFilterBy )

type WalletName = String

multiUTxOsSameAddress ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError BuildingTxError m )
    => WalletName
    -> Address
    -> Integer
    -> m ()
multiUTxOsSameAddress sender receiver amount = do
    wallet <- queryWallet sender
    utxos <- fetchUTxOFilterBy (not . containsCollateral) wallet >>= whenNothingThrow NoADAInWallet 
    let metadataMaybe = Nothing
    submit'
      TxBuild
        { wallet = wallet
        , txIns = fmap (FromWallet . txOutRef) utxos
        , txOuts = ToWallet receiver (lovelaceValueOf amount) :| []
        , validitySlotRangeMaybe = Nothing
        , tokenSupplyChangesMaybe = Nothing
        , ..}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Tokenomia.ICO.Funds.Emission.SpecialCases.NativeTokens ( sendTokensAndADAs ) where
import           Prelude hiding ((+),(-))
import           PlutusTx.Prelude  (AdditiveSemigroup((+)),AdditiveGroup((-)))
import           Data.List.NonEmpty ( NonEmpty((:|)) )
import           Control.Monad.Reader ( MonadIO(..), MonadReader )
import           Control.Monad.Except ( MonadError )
import           Ledger.Value ( singleton )
import           Ledger.Ada ( lovelaceValueOf )
import           Tokenomia.Adapter.Cardano.Types ( Address(..) )
import           Tokenomia.Adapter.Cardano.CLI.Wallet ( Wallet(..), queryWallet )
import           Tokenomia.Adapter.Cardano.CLI.UTxO ( UTxO(txOutRef), getTokenFrom )
import           Tokenomia.Adapter.Cardano.CLI.Transaction ( BuildingTxError(), submit', TxBuild(..), TxIn(..), TxOut(..) )
import           Tokenomia.Adapter.Cardano.CLI.Environment ( Environment )

type WalletName = String

sendTokensAndADAs :: 
    ( MonadIO m
    , MonadReader Environment m
    , MonadError BuildingTxError m ) 
    => WalletName
    -> Address
    -> UTxO 
    -> Integer
    -> Integer
    -> m ()
sendTokensAndADAs sender receiver utxoWithToken tokenAmount adaAmount = do 
    wallet <- queryWallet sender
    let (tokenPolicyHash,tokenNameSelected,totalAmount) = getTokenFrom utxoWithToken
        tokenId = singleton tokenPolicyHash tokenNameSelected
        valueToTransfer = tokenId tokenAmount + lovelaceValueOf 1344798
        change = tokenId (totalAmount - tokenAmount) + lovelaceValueOf 1344798
        metadataMaybe = Nothing
    submit'
      TxBuild
        { wallet = wallet
        , txIns =  FromWallet (txOutRef utxoWithToken) :| []
        , txOuts = ToWallet receiver valueToTransfer 
                :| [ToWallet (paymentAddress wallet) change, ToWallet receiver (lovelaceValueOf adaAmount)]
        , validitySlotRangeMaybe = Nothing
        , tokenSupplyChangesMaybe = Nothing
        , ..}
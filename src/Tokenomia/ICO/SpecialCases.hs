{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Tokenomia.ICO.SpecialCases where
import           Control.Concurrent
import qualified Data.ByteString.UTF8 as BSU
import qualified Ledger.Value as L
import           Control.Monad.Reader ( MonadIO(..), MonadReader )
import           Control.Monad.Except ( MonadError )
import           Tokenomia.Adapter.Cardano.CLI.Transaction ( BuildingTxError )
import           Tokenomia.Common.Shell.Console (printLn)
import           Tokenomia.Wallet.Collateral.Write ( createCollateral' )
import qualified Tokenomia.Ada.Transfer as ADA
import qualified Tokenomia.Token.Transfer as Token
import           Tokenomia.Token.CLAPStyle.Mint ( mint' )
import           Tokenomia.Adapter.Cardano.Types ( Address )
import           Tokenomia.Adapter.Cardano.CLI.Wallet ( Wallet(Wallet, paymentAddress), queryWallet, register_shelley_wallet )
import           Tokenomia.Adapter.Cardano.CLI.Environment ( Environment )
import           Tokenomia.Adapter.Cardano.CLI.UTxO.Query (queryUTxOsFilterBy)
import           Tokenomia.Adapter.Cardano.CLI.UTxO

import           Tokenomia.ICO.SpecialCases.Multisig (multisigOneUTxO, multisigTwoUTxOs)
import           Tokenomia.ICO.SpecialCases.MultiUTxOs ( multiUTxOsSameAddress )
import           Tokenomia.ICO.SpecialCases.NativeTokens ( sendTokensAndADAs )


specialCases :: 
      ( MonadIO m
      , MonadReader Environment m
      , MonadError BuildingTxError m ) 
      => Address
      -> Integer
      -> m ()
specialCases ico_address max_cap_per_investor = do
      printLn "############################"
      printLn "#       Special cases      #"
      printLn "############################"
      printLn ""
      rootWallet <- queryWallet "ICO_root"


      printLn "Multisig 1"
      printLn "(UTxO from one wallet)"
      printLn ""
      printLn "############################"
      register_shelley_wallet "ICO_special_multisig_1_1"
      wallet_multisig_1_1@Wallet {paymentAddress = address_multisig_1_1} <- queryWallet "ICO_special_multisig_1_1"
      printLn "Transfering funds to wallet ICO_special_multisig_1_1"
      ADA.transfer' rootWallet address_multisig_1_1 6000000 Nothing
      printLn "Creating collateral for wallet ICO_special_multisig_1_1"
      createCollateral' wallet_multisig_1_1
      register_shelley_wallet "ICO_special_multisig_1_2"
      printLn "Sending funds from wallet ICO_special_multisig_1_1"
      multisigOneUTxO "ICO_special_multisig_1_1" "ICO_special_multisig_1_2" ico_address 2000000
      printLn "############################"
      printLn ""


      printLn "Multisig 2"
      printLn "(1 UTxO from one wallet, 1 UTxO from the other wallet)"
      printLn ""
      printLn "#######################################################"
      register_shelley_wallet "ICO_special_multisig_2_1"
      wallet_multisig_2_1@Wallet {paymentAddress = address_multisig_2_1} <- queryWallet "ICO_special_multisig_2_1"
      printLn "Transfering funds to wallet ICO_special_multisig_2_1"
      ADA.transfer' rootWallet address_multisig_2_1 5000000 Nothing
      printLn "Creating collateral for wallet ICO_special_multisig_2_1"
      createCollateral' wallet_multisig_2_1
      register_shelley_wallet "ICO_special_multisig_2_2"
      Wallet {paymentAddress = address_multisig_2_2} <- queryWallet "ICO_special_multisig_2_2"
      printLn "Transfering funds to wallet ICO_special_multisig_2_2"
      ADA.transfer' rootWallet address_multisig_2_2 3000000 Nothing
      printLn "Sending funds from wallet ICO_special_multisig_2_1"
      multisigTwoUTxOs "ICO_special_multisig_2_1" "ICO_special_multisig_2_2" ico_address 3000000
      printLn "############################"
      printLn ""


      printLn "Multi UTxOs from same address 1"
      printLn "(amount < max cap)"
      printLn "################################"
      register_shelley_wallet "ICO_special_multiUTxOs_1"
      wallet_multiUTxOs_1@Wallet {paymentAddress = address_multiUTxOs_1} <- queryWallet "ICO_special_multiUTxOs_1"
      printLn "Transfering funds to wallet ICO_special_multiUTxOs_1"
      ADA.transfer' rootWallet address_multiUTxOs_1 5000000 Nothing
      printLn "Creating collateral for wallet ICO_special_multiUTxOs_1"
      createCollateral' wallet_multiUTxOs_1
      printLn "Transfering funds to wallet ICO_special_multiUTxOs_1"
      ADA.transfer' rootWallet address_multiUTxOs_1 5000000 Nothing
      printLn "Sending funds from wallet ICO_special_multiUTxOs_1"
      multiUTxOsSameAddress "ICO_special_multiUTxOs_1" ico_address 5000000
      printLn "############################"
      printLn ""


      printLn "Multi UTxOs from same address 2"
      printLn "(amount > max cap)"
      printLn "################################"
      register_shelley_wallet "ICO_special_multiUTxOs_2"
      wallet_multiUTxOs_2@Wallet {paymentAddress = address_multiUTxOs_2} <- queryWallet "ICO_special_multiUTxOs_2"
      printLn "Transfering funds to wallet ICO_special_multiUTxOs_2"
      ADA.transfer' rootWallet address_multiUTxOs_2 5000000 Nothing
      printLn "Transfering funds to wallet ICO_special_multiUTxOs_2"
      ADA.transfer' rootWallet address_multiUTxOs_2 max_cap_per_investor Nothing
      printLn "Creating collateral for wallet ICO_special_multiUTxOs_2"
      createCollateral' wallet_multiUTxOs_2
      printLn "Sending funds from wallet ICO_special_multiUTxOs_2"
      multiUTxOsSameAddress "ICO_special_multiUTxOs_2" ico_address (max_cap_per_investor + 1000000)
      printLn "############################"
      printLn ""


      printLn "Native tokens 1"
      printLn "(UTxO containing native tokens)"
      printLn "################################"
      register_shelley_wallet "ICO_special_tokens_1"
      wallet_tokens_1@Wallet {paymentAddress = address_tokens_1} <- queryWallet "ICO_special_tokens_1"
      printLn "Transfering funds to wallet ICO_special_tokens_1"
      ADA.transfer' rootWallet address_tokens_1 7000000 Nothing
      printLn "Creating collateral for wallet ICO_special_tokens_1"
      createCollateral' wallet_tokens_1
      policyhash <- mint' wallet_tokens_1 (L.tokenName (BSU.fromString "ICO")) 100
      utxoWithNativeTokens <- head <$> queryUTxOsFilterBy wallet_tokens_1 (containingGivenNativeToken policyhash)
      printLn "Sending funds from wallet ICO_special_tokens_1"
      Token.transfer' wallet_tokens_1 ico_address utxoWithNativeTokens 100 Nothing
      printLn "############################"
      printLn ""


      printLn "Native tokens 2"
      printLn "(1 UTxO containing (native tokens), 1 valid UTxO"
      printLn "################################################"
      register_shelley_wallet "ICO_special_tokens_2"
      wallet_tokens_2@Wallet {paymentAddress = address_tokens_2} <- queryWallet "ICO_special_tokens_2"
      printLn "Transfering funds to wallet ICO_special_tokens_2"
      ADA.transfer' rootWallet address_tokens_2 10000000 Nothing
      printLn "Creating collateral for wallet ICO_special_tokens_2"
      createCollateral' wallet_tokens_2
      policyhash2 <- mint' wallet_tokens_2 (L.tokenName (BSU.fromString "ICO2")) 100
      utxoWithNativeTokens2 <- head <$> queryUTxOsFilterBy wallet_tokens_2 (containingGivenNativeToken policyhash2)
      printLn "Sending funds from wallet ICO_special_tokens_2"
      sendTokensAndADAs "ICO_special_tokens_2" ico_address utxoWithNativeTokens2 50 3000000
      printLn "############################"
      printLn ""


      printLn "Dust"
      printLn "(1 UTxO with too small amount to participate in the ICO)"
      printLn "########################################################"
      register_shelley_wallet "ICO_special_dust"
      wallet_dust@Wallet {paymentAddress = address_dust} <- queryWallet "ICO_special_dust"
      printLn "Transfering funds to wallet ICO_special_dust"
      ADA.transfer' rootWallet address_dust 5000000 Nothing
      printLn "Creating collateral for wallet ICO_special_dust"
      createCollateral' wallet_dust
      printLn "Transfering funds from wallet ICO_special_dust"
      ADA.transfer' wallet_dust ico_address 1000000 Nothing
      printLn "############################"
      printLn ""


      printLn "Sequence 1"
      printLn "(1st and 2nd tx valids, sum < max cap)"
      printLn "######################################"
      register_shelley_wallet "ICO_special_sequence_1"
      wallet_sequence_1@Wallet {paymentAddress = address_sequence_1} <- queryWallet "ICO_special_sequence_1"
      printLn "Transfering funds to wallet ICO_special_sequence_1"
      ADA.transfer' rootWallet address_sequence_1 (max_cap_per_investor*2) Nothing
      printLn "Creating collateral for wallet ICO_special_sequence_1"
      createCollateral' wallet_sequence_1
      printLn "Transfering funds from wallet ICO_special_sequence_1"
      ADA.transfer' wallet_sequence_1 ico_address (quot max_cap_per_investor 3) Nothing
      printLn "Waiting 10s"
      liftIO $ threadDelay 10000000 -- 10s
      printLn "Transfering funds from wallet ICO_special_sequence_1"
      ADA.transfer' wallet_sequence_1 ico_address (quot max_cap_per_investor 3) Nothing
      printLn "############################"
      printLn ""


      printLn "Sequence 2"
      printLn "(1st and 2nd tx valids, sum > max cap)"
      printLn "######################################"
      register_shelley_wallet "ICO_special_sequence_2"
      wallet_sequence_2@Wallet {paymentAddress = address_sequence_2} <- queryWallet "ICO_special_sequence_2"
      printLn "Transfering funds to wallet ICO_special_sequence_2"
      ADA.transfer' rootWallet address_sequence_2 (max_cap_per_investor*2) Nothing
      printLn "Creating collateral for wallet ICO_special_sequence_2"
      createCollateral' wallet_sequence_2
      printLn "Transfering funds from wallet ICO_special_sequence_2"
      ADA.transfer' wallet_sequence_2 ico_address (quot max_cap_per_investor 3) Nothing
      printLn "Waiting 10s"
      liftIO $ threadDelay 10000000 -- 10s
      printLn "Transfering funds from wallet ICO_special_sequence_2"
      ADA.transfer' wallet_sequence_2 ico_address max_cap_per_investor Nothing
      printLn "############################"
      printLn ""


      printLn "Sequence 3"
      printLn "(1st tx valid, 2nd tx invalid (too small))"
      printLn "##########################################"
      register_shelley_wallet "ICO_special_sequence_3"
      wallet_sequence_3@Wallet {paymentAddress = address_sequence_3} <- queryWallet "ICO_special_sequence_3"
      printLn "Transfering funds to wallet ICO_special_sequence_3"
      ADA.transfer' rootWallet address_sequence_3 10000000 Nothing
      printLn "Creating collateral for wallet ICO_special_sequence_3"
      createCollateral' wallet_sequence_3
      printLn "Transfering funds from wallet ICO_special_sequence_3"
      ADA.transfer' wallet_sequence_3 ico_address 4000000 Nothing
      printLn "Waiting 10s"
      liftIO $ threadDelay 10000000 -- 10s
      printLn "Transfering funds from wallet ICO_special_sequence_3"
      ADA.transfer' wallet_sequence_3 ico_address 1000000 Nothing
      printLn "############################"
      printLn ""


      printLn "Sequence 4"
      printLn "(1st tx invalid (too small), 2nd tx valid)"
      printLn "##########################################"
      register_shelley_wallet "ICO_special_sequence_4"
      wallet_sequence_4@Wallet {paymentAddress = address_sequence_4} <- queryWallet "ICO_special_sequence_4"
      printLn "Transfering funds to wallet ICO_special_sequence_4"
      ADA.transfer' rootWallet address_sequence_4 10000000 Nothing
      printLn "Creating collateral for wallet ICO_special_sequence_4"
      createCollateral' wallet_sequence_4
      printLn "Transfering funds from wallet ICO_special_sequence_4"
      ADA.transfer' wallet_sequence_4 ico_address 1000000 Nothing
      printLn "Waiting 10s"
      liftIO $ threadDelay 10000000 -- 10s
      printLn "Transfering funds from wallet ICO_special_sequence_4"
      ADA.transfer' wallet_sequence_4 ico_address 4000000 Nothing
      printLn "############################"
      printLn ""

      printLn "#############################"
      printLn "#   End of special cases    #"
      printLn "#############################"
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Tokenomia.ICO.Simulation ( simulation ) where
import           Control.Monad.Reader
import           Control.Monad.Except
import           Data.Coerce ( coerce )
import qualified Ledger.Value as L
import           Tokenomia.Common.Error
import           Tokenomia.Common.Shell.Console ( printLn, clearConsole )
import           Tokenomia.Wallet.CLI
import           Tokenomia.Wallet.Collateral.Read
import           Tokenomia.Wallet.Collateral.Write
import           Tokenomia.Ada.Transfer
import           Tokenomia.Adapter.Cardano.CLI.Wallet
import           Tokenomia.Adapter.Cardano.CLI.UTxO
import           Tokenomia.Adapter.Cardano.CLI.Transaction
import           Tokenomia.Adapter.Cardano.CLI.Environment

import           Tokenomia.ICO.HappyPath ( happyPath )
import           Tokenomia.ICO.SpecialCases ( specialCases )


simulation :: IO ()
simulation = do
    clearConsole
    printLn "################################"
    printLn "# Welcome to the ICO simulator #"
    printLn "################################"
    printLn ""
    environment <- getTestnetEnvironmment 1097911063

    let a = runExceptT $ runReaderT setup environment
        b = runExceptT $ runReaderT (specialCases (coerce "addr_test1vzzj78m4jw2sudver47u3ad9v92n3kzeusqdk2ply32znysqx0835") 10000000) environment
        c = runExceptT $ runReaderT happyPath environment
    result <- sequence [a,b,c]
    

    printLn "#############################"
    printLn "#   End of ICO simulator    #"
    printLn "#############################"




setup :: 
    ( MonadIO m
    , MonadReader Environment m
    , MonadError BuildingTxError m ) 
    =>  m ()
setup = do 
    printLn "Select the wallet containing the ADA UTxO to split"
    askAmongAllWallets 
        >>= \case
            Nothing -> printLn "No Wallet Registered !"
            Just initWallet -> do
                _ <- fetchCollateral initWallet >>= whenNothingThrow WalletWithoutCollateral
                selectBiggestStrictlyADAsNotCollateral initWallet
                    >>= \case
                        Nothing -> printLn "No UTxOs in wallet !"
                        Just UTxO{..} -> do
                            printLn "Registering ICO wallets"
                            register_shelley_wallet "ICO_root"
                            register_shelley_wallet "ICO_reception"
                            rootWallet@Wallet {paymentAddress = rootAddress} <- queryWallet "ICO_root"
                            let (_,_,amountOfADAs) = (head . L.flattenValue) value
                            printLn "Sending funds to root wallet"
                            transfer' initWallet rootAddress (amountOfADAs - 2000000) Nothing
                            printLn "Creating collateral for root wallet"
                            createCollateral' rootWallet
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Tokenomia.ICO.AddressesGeneration 
                    ( generator
                    , generateAddress
                    ) where
import           Data.String ( IsString(fromString) )
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.List.NonEmpty as NonEmpty ( NonEmpty, fromList )
import           Control.Monad.Reader ( MonadIO(..), ReaderT(runReaderT), MonadReader, asks )
import           Shh.Internal ( (&>), capture, load, (|>), ExecReference(SearchPath), Stream(Truncate, Append) )
import           Tokenomia.Common.Shell.Console ( printLn, clearConsole )
import           Tokenomia.Common.Shell.InteractiveMenu ( DisplayMenuItem(..), askMenu, ask )
import           Tokenomia.Adapter.Cardano.CLI.Environment ( Environment(Mainnet, Testnet), getMainnetEnvironmment, getTestnetEnvironmment )
import           Tokenomia.Adapter.Cardano.CLI.Folder (getFolderPath, getRootCLIFolder,Folder (..))
import           Tokenomia.Adapter.Cardano.CLI.Wallet ( Wallet(..) )
import           Tokenomia.Wallet.CLI ( askAmongAllWallets )

load SearchPath ["cat","mkdir","cardano-cli","mv", "cardano-address", "echo", "rm"]

type WalletName = String


generator :: IO ()
generator = do
    clearConsole
    printLn "#######################"
    printLn "# Addresses Generator #"
    printLn "#######################"
    printLn ""
    selectNetwork

generator' ::
    ( MonadIO m
    , MonadReader Environment m )
    => m ()
generator' = do
    printLn "Select the root wallet"
    askAmongAllWallets 
        >>= \case
            Nothing -> printLn "No Wallet Registered ! Please create one with tokenomia-cli."
            Just Wallet{..} -> do
                count <- ask @Int "How many addresses do you want to generate ? : "
                walletFolder <- ( <> "/"<> "ICO" <>"/" <> name <> "/") <$> getRootCLIFolder
                liftIO $ rm "-rf" walletFolder
                clearConsole
                printLn "########################"
                printLn "# Generating addresses #"
                printLn "########################"
                printLn ""
                mapM_ generateAddress (take count [ (name,i) | i <- [0..] ::[Integer]])


generateAddress ::
    ( MonadIO m
    , MonadReader Environment m )
    => (WalletName, Integer)
    -> m ()
generateAddress (walletName, index) = do
    printLn $ "Generating " <> show index
    keyPath <- getFolderPath Keys
    walletFolder <- ( <> "/"<> "ICO" <>"/" <> walletName <> "/") <$> getRootCLIFolder
    let addressFolder = walletFolder <> show index
    liftIO $ mkdir "-p" addressFolder

    let walletKeyPath = keyPath <> walletName <> "/"
        root = walletKeyPath <> "root.xsk"
        stakeVerification = walletKeyPath <> "stake.xvk"
        paymentSigning = addressFolder <> "/payment-signing.xsk"
        paymentVerification = addressFolder <> "/payment-verification.xvk"
        paymentAddress = addressFolder <> "/payment.addr"
        derivationPath = "1852H/1815H/0H/0/" <> show index
        addressesCSV = walletFolder <> "addresses.csv"

    liftIO $ (cat root |> cardano_address "key" "child" derivationPath)
        &> (Truncate . fromString) paymentSigning
    liftIO $ (cat paymentSigning |> cardano_address "key" "public" "--with-chain-code")
        &> (Truncate . fromString) paymentVerification

    netWorkTag <- asks (\case
                        Testnet {} -> "testnet"
                        Mainnet {} -> "mainnet")

    stakeKey <- liftIO $ cat stakeVerification |> capture
    let s = C.unpack stakeKey

    liftIO $ (cat paymentVerification |> cardano_address "address" "payment" "--network-tag" netWorkTag |> cardano_address "address" "delegation" s)
        &> (Truncate . fromString) paymentAddress

    let paymentSigningConvertedPath = addressFolder <> "/payment-signing-cardano-cli-format.skey"
        paymentVerificationConvertedPath = addressFolder <> "/payment-verification-cardano-cli-format.vkey"

    liftIO $ cardano_cli "key" "convert-cardano-address-key" "--shelley-payment-key" "--signing-key-file" paymentSigning "--out-file" paymentSigningConvertedPath
    liftIO $ cardano_cli "key" "verification-key" "--signing-key-file" paymentSigningConvertedPath "--verification-key-file" paymentVerificationConvertedPath

    address <- liftIO $ cat paymentAddress |> capture
    let a = C.unpack address
    renamedICORootFolder <- ( <> "/" <> "ICO" <> "/" <> walletName <> "/" <> show index <> "_" <> a) <$> getRootCLIFolder

    liftIO $ mv addressFolder renamedICORootFolder
    liftIO $ echo (show index <> "," <> a) &> (Append . fromString) addressesCSV


selectNetwork :: IO()
selectNetwork = do
  printLn "----------------------"
  printLn "  Select a network"
  printLn "----------------------"  
  environment <- liftIO $ askMenu networks >>= \case 
      SelectTestnet     -> getTestnetEnvironmment 1097911063 
      SelectMainnet     -> getMainnetEnvironmment 764824073
  clearConsole
  runReaderT generator' environment


networks :: NonEmpty SelectEnvironment
networks = NonEmpty.fromList [
  SelectTestnet,
  SelectMainnet
  ]

data SelectEnvironment 
  = SelectTestnet
  | SelectMainnet

instance DisplayMenuItem SelectEnvironment where
  displayMenuItem item = case item of
    SelectTestnet   -> "Testnet (magicNumber 1097911063)" 
    SelectMainnet   -> "Mainnet (magicNumber 764824073)"
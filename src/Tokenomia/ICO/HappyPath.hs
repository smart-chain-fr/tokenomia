{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Tokenomia.ICO.HappyPath ( happyPath ) where
import           System.Random
import           Control.Concurrent.Lifted
import           Control.Monad.Reader 
import           Control.Monad.Except
import           Data.List.Split
import           Data.List.NonEmpty hiding ((!!), length, take, head, last)
import qualified Ledger.Value as L
import           Ledger.Ada ( lovelaceValueOf )
import           Shh.Internal
import           Tokenomia.Common.Error
import           Tokenomia.Common.Shell.Console (printLn)
import qualified Tokenomia.Common.Shell.InteractiveMenu as IM
import           Tokenomia.Ada.Transfer
import           Tokenomia.Wallet.CLI
import           Tokenomia.Adapter.Cardano.Types
import           Tokenomia.Adapter.Cardano.CLI.Wallet
import qualified Tokenomia.Adapter.Cardano.CLI.UTxO as TUXTO
import           Tokenomia.Adapter.Cardano.CLI.Transaction
import           Tokenomia.Adapter.Cardano.CLI.Environment
import           Tokenomia.Adapter.Cardano.CLI.Folder (getRootCLIFolder)

import           Tokenomia.ICO.AddressesGeneration

load SearchPath ["rm"]

happyPath ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError BuildingTxError m ) 
    => m ()
happyPath = do
    printLn "##########################"
    printLn "#       Happy Path       #"
    printLn "##########################"
    printLn ""
    rootWallet <- queryWallet "ICO_root"
    printLn ""
    numberOfReceptionAddresses <- IM.ask @Int "- Enter the number of reception addresses to generate : "
    numberOfThreads <- IM.ask @Int "- Enter the number of threads to simulate load : "
    printLn ""

    walletFolder <- ( <> "/"<> "ICO" <>"/" <> "ICO_reception" <> "/") <$> getRootCLIFolder
    liftIO $ rm "-rf" walletFolder 
    
    mapM_ generateAddress (take numberOfReceptionAddresses [ ("ICO_reception",i) | i <- [0..] ::[Integer]])

    mapM_ register_shelley_wallet (take numberOfThreads [ "ICO_sending_" <> show i | i <- [0..] ::[Integer]])

    sendingWallets <- mapM queryWallet (take numberOfThreads [ "ICO_sending_" <> show i | i <- [0..] ::[Integer]])
    
    mapM_ createCollateral sendingWallets

    (numberOfTxsPerThread,adasPerTx) <- splitUTxO rootWallet sendingWallets

    let addressesCSV = walletFolder <> "addresses.csv"
    addressesFile <- liftIO $ readFile addressesCSV
    let addressesArray = lines addressesFile
        addresses = Address . last . splitWhen (==',') <$> addressesArray

    environment::Environment <- ask 
    
    mapM_ (createThread addresses numberOfTxsPerThread adasPerTx environment) sendingWallets
    
    liftIO $ threadDelay 10000000000 -- 2h



createCollateral ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError BuildingTxError m )
    => Wallet
    -> m ()
createCollateral sendingWallet = do
    rootWallet <- queryWallet "ICO_root"
    printLn "Creating collateral for thread"
    transfer' rootWallet (paymentAddress sendingWallet) 2000000 Nothing


splitUTxO ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError BuildingTxError m )
    => Wallet
    -> [Wallet]
    -> m (Int,L.Value)
splitUTxO senderWallet threadsWallets = do
    ada <- selectBiggestStrictlyADAsNotCollateral senderWallet >>= whenNothingThrow NoADAInWallet
    let (_,_,amountOfADAs) = (head . L.flattenValue) (TUXTO.value ada)
        adasPerThread = lovelaceValueOf ((amountOfADAs - 2000000) `div` fromIntegral (length threadsWallets))
        numberOfPossibleTxs = amountOfADAs `div` 3000000
        adasPerTx = lovelaceValueOf (amountOfADAs `div` numberOfPossibleTxs)
        numberOfTxsPerThread = fromIntegral (numberOfPossibleTxs `div` fromIntegral (length threadsWallets)) - 5
        outputs = take (length threadsWallets) [ ToWallet (paymentAddress (threadsWallets !! i)) adasPerThread | i <- [0..] ::[Int]]
    submit'
      TxBuild
        { wallet = senderWallet
        , txIns = FromWallet (TUXTO.txOutRef ada) :| []
        , txOuts = fromList outputs
        , validitySlotRangeMaybe = Nothing
        , tokenSupplyChangesMaybe = Nothing
        , metadataMaybe = Nothing
        , ..}
    return (numberOfTxsPerThread, adasPerTx)



createThread ::
    ( MonadIO m)
    => [Address]
    -> Int
    -> L.Value
    -> Environment
    -> Wallet
    -> m ThreadId
createThread addresses numberOfTxsPerThread adasPerTx environment sendingWallet = do
    liftIO $ forkOS (void $ runExceptT $ runReaderT (replicateM_ numberOfTxsPerThread (send ("thread-"<> name sendingWallet) sendingWallet addresses adasPerTx)) environment)


send ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError BuildingTxError m )
    => String
    -> Wallet
    -> [Address]
    -> L.Value
    -> m ()
send thread senderWallet@Wallet{..} addresses amount = do
    randomInt <- liftIO ( abs <$> randomIO :: IO Int )
    let delay = randomInt `mod` 5000000 --5s
        addressID = randomInt `mod` (length addresses - 1)
        address = addresses !!addressID
    utxos <- fetchUTxOFilterBy (not . TUXTO.containsCollateral) senderWallet >>= whenNothingThrow NoADAInWallet
    printLn $ thread <> ": Waiting " <> show delay
    liftIO $ threadDelay delay
    printLn $ thread <> ": Sending funds from " <> show paymentAddress <> " to " <> show address
    submit'
        TxBuild
            { wallet = senderWallet
            , txIns = fmap (FromWallet . TUXTO.txOutRef) utxos
            , txOuts = ToWallet address amount :| []
            , validitySlotRangeMaybe = Nothing
            , tokenSupplyChangesMaybe = Nothing
            , metadataMaybe = Nothing
            , ..}
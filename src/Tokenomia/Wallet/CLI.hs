{-# LANGUAGE FlexibleContexts                          #-}
{-# LANGUAGE ImportQualifiedPost                       #-}
{-# LANGUAGE LambdaCase                                #-}
{-# LANGUAGE NamedFieldPuns                            #-}
{-# LANGUAGE OverloadedStrings                         #-}
{-# LANGUAGE RecordWildCards                           #-}
{-# LANGUAGE ScopedTypeVariables                       #-}
{-# LANGUAGE TypeApplications                          #-}
{-# OPTIONS_GHC -Wno-redundant-constraints             #-}

module Tokenomia.Wallet.CLI
    ( askAmongAllWallets
    , askToChooseAmongGivenUTxOs
    , askToChooseAmongGivenWallets
    , askUTxO
    , askUTxOFilterBy
    , fetchUTxOFilterBy
    , generateChildAddresses
    , selectBiggestStrictlyADAsNotCollateral
      -- "UI" for Wallet Repository
    , askDisplayOne
    , askDisplayOneWithinIndexRange
    , displayAll
    , register
    , remove
    , restoreByMnemonics
    ) where

import Control.Monad.Except                            ( MonadError, MonadIO )
import Control.Monad.Reader                            ( MonadReader )
import Data.Coerce                                     ( coerce )
import Data.List.NonEmpty                              ( NonEmpty, head, last, nonEmpty, sortWith )
import Data.Set qualified as S
import Data.Set.NonEmpty                               ( size, toAscList )
import Prelude hiding                                  ( filter, head, last )
import Prelude qualified as P
import Tokenomia.Common.Error                          ( TokenomiaError(NoWalletRegistered), whenNullThrow )

import Plutus.V1.Ledger.Value                          ( flattenValue )
import Tokenomia.Common.Shell.Console                  ( printLn )

import Tokenomia.Common.Shell.InteractiveMenu          ( askFilterM, askMenu, askString, askStringFilterM )

import Tokenomia.Common.Environment                    ( Environment )
import Tokenomia.Wallet.ChildAddress.ChainIndex        ( queryUTxO, queryUTxOsFilterBy )
import Tokenomia.Wallet.LocalRepository qualified as Repository
import Tokenomia.Wallet.UTxO                           ( UTxO(UTxO, value) )
import Tokenomia.Wallet.WalletUTxO                     ( WalletUTxO(WalletUTxO, utxo) )

import Tokenomia.Common.Address                        ( Address(Address) )
import Tokenomia.Common.Value                          ( containingStrictlyADAs, containsCollateral )
import Tokenomia.Wallet.ChildAddress.ChildAddressRef   ( ChildAddressIndex(..), ChildAddressRef(..) )
import Tokenomia.Wallet.ChildAddress.LocalRepository
    ( ChildAddress(..)
    , Wallet(..)
    , deriveChildAddress
    , fetchByWallet
    , fetchByWalletWithinIndexRange
    )


askWalletName :: (MonadIO m) => m String
askWalletName = askString "Wallet Name : "

askAmongAllWallets :: (MonadIO m, MonadReader Environment m) => m (Maybe Wallet)
askAmongAllWallets =
    Repository.fetchAll
      >>=  \case
            Nothing -> return Nothing
            Just a -> Just <$> askMenu a
          . nonEmpty

askToChooseAmongGivenWallets :: (MonadIO m, MonadReader Environment m)
  => NonEmpty Wallet
  -> m Wallet
askToChooseAmongGivenWallets = askMenu

askUTxO
  ::( MonadIO m
    , MonadReader Environment m)
  =>  ChildAddressRef
  ->  m (Maybe WalletUTxO)
askUTxO = askUTxOFilterBy (const True)


selectBiggestStrictlyADAsNotCollateral
  ::( MonadIO m
    , MonadReader Environment m)
  => ChildAddressRef
  -> m (Maybe WalletUTxO)
selectBiggestStrictlyADAsNotCollateral childAddressRef  = do
  adas :: Maybe (NonEmpty WalletUTxO)
    <- nonEmpty
       . P.filter ((&&) <$> containingStrictlyADAs . value . utxo <*> not . containsCollateral . value . utxo)  <$> queryUTxO childAddressRef
  return (last . sortWith (\WalletUTxO { utxo = UTxO {value}} ->
                        maybe
                          0
                          (third . head)
                          (nonEmpty $ flattenValue value))
           <$> adas)
  where
    third :: (a,b,c) -> c
    third (_,_,c) = c

askUTxOFilterBy
  ::( MonadIO m
    , MonadReader Environment m)
  => (WalletUTxO -> Bool)
  -> ChildAddressRef
  ->  m (Maybe WalletUTxO)
askUTxOFilterBy predicate  childAddressRef  =
  queryUTxO childAddressRef
  >>= (\case
          Nothing -> return Nothing
          Just a -> Just <$> askMenu a) . nonEmpty . P.filter predicate


fetchUTxOFilterBy
  ::( MonadIO m
    , MonadReader Environment m)
  => (WalletUTxO -> Bool)
  -> ChildAddressRef
  ->  m (Maybe (NonEmpty WalletUTxO))
fetchUTxOFilterBy predicate childAddressRef  =  nonEmpty <$> queryUTxOsFilterBy childAddressRef predicate


askToChooseAmongGivenUTxOs :: (MonadIO m, MonadReader Environment m)
  => NonEmpty WalletUTxO
  -> m WalletUTxO
askToChooseAmongGivenUTxOs = askMenu


generateChildAddresses
  ::( MonadIO m
    , MonadReader Environment m
    , MonadError TokenomiaError m)
  => m ()
generateChildAddresses = do
   w@Wallet {name} <- Repository.fetchAll >>= whenNullThrow NoWalletRegistered
        >>= \wallets -> do
            printLn "Select the minter wallet : "
            askToChooseAmongGivenWallets wallets
   from <- askFilterM @Integer "> from : " (\i -> return $ 0 < i)
   to <-   askFilterM @Integer "> to : " (\i -> return $ from < i)

   mapM_ (\childAddressRef@ChildAddressRef{index} -> do
           deriveChildAddress childAddressRef
           printLn $ " - Derived Child Address " <> (show @Integer . coerce $ index)
            ) $ ChildAddressRef name . ChildAddressIndex <$> [from..to]

   displayOne w

register
  ::( MonadIO m, MonadReader Environment m)
  => m ()
register = do
  printLn "-----------------------------------"
  walletName <- askWalletName
  exists <- Repository.exists walletName
  if exists
    then
      printLn "Wallet already exists!"
    else do
      _ <- Repository.register walletName
      printLn "Wallet Created and Registered!"
  printLn "-----------------------------------"



askDisplayOne
  ::( MonadIO m
    , MonadReader Environment m
    , MonadError TokenomiaError m)
  => m ()
askDisplayOne = do
  w <- Repository.fetchAll >>= whenNullThrow NoWalletRegistered
          >>= \wallets -> do
              printLn "Select the wallet to display : "
              askToChooseAmongGivenWallets wallets
  displayOne w

askDisplayOneWithinIndexRange
  ::( MonadIO m
    , MonadReader Environment m
    , MonadError TokenomiaError m)
  => m ()
askDisplayOneWithinIndexRange = do
  w <- Repository.fetchAll >>= whenNullThrow NoWalletRegistered
          >>= \wallets -> do
              printLn "Select the wallet to display : "
              askToChooseAmongGivenWallets wallets
  from <- askFilterM @Int "> from : " (\i -> return $ 0 <= i)
  to <-   askFilterM @Int "> to : "   (\i -> return $ from < i)
  displayOneWithinIndexRange from to w

displayOneWithinIndexRange
  ::( MonadIO m
    , MonadReader Environment m
    , MonadError TokenomiaError m)
  => Int
  -> Int
  -> Wallet
  -> m ()
displayOneWithinIndexRange from to Wallet{..} =  do
  addresses <- fetchByWalletWithinIndexRange from to name
  printLn $ "| " <> name
        <> "\n   | Stake Address: " <> coerce stakeAddress
        <> "\n   | Child Addresses: " <> (show . S.size) addresses
  mapM_ (\ChildAddress {childAddressRef = ChildAddressRef {index = index@(ChildAddressIndex indexInt)},..} -> do
      printLn $ "      [" <> show indexInt <> "] " <> coerce address
      utxos <- queryUTxO (ChildAddressRef name index)
      case utxos of
        [] -> return ()
        a  -> mapM_ (\utxo -> printLn ("         - " <> show utxo)) a) (S.toAscList addresses)


displayOne
  ::( MonadIO m
    , MonadReader Environment m
    , MonadError TokenomiaError m)
  => Wallet
  -> m ()
displayOne Wallet{..} =  do
  addresses <- fetchByWallet name
  printLn $ "| " <> name
        <> "\n   | Stake Address: " <> coerce stakeAddress
        <> "\n   | Child Addresses: " <> (show . size) addresses
  mapM_ (\ChildAddress {childAddressRef = ChildAddressRef {index = index@(ChildAddressIndex indexInt)},..} -> do
      printLn $ "      [" <> show indexInt <> "] " <> coerce address
      utxos <- queryUTxO $ ChildAddressRef name index
      case utxos of
        [] -> return ()
        a  -> mapM_ (\utxo -> printLn ("         - " <> show utxo)) a) (toAscList addresses)



displayAll
  ::( MonadIO m
    , MonadReader Environment m
    , MonadError TokenomiaError m)
  => m ()
displayAll =
  Repository.fetchAll
 >>= \case
       [] -> printLn "No Wallet Registered!"
       wallets -> do
         printLn "-----------------------------------"
         printLn "Wallets Registered"
         printLn "-----------------------------------"
         mapM_ displayOne wallets
         printLn "-----------------------------------"

remove :: (MonadIO m, MonadReader Environment m) => m ()
remove = do
  printLn "-----------------------------------"
  printLn "Select the Wallet to remove :"
  askAmongAllWallets
    >>= \case
        Nothing ->
          printLn "No Wallet Registered !"
        Just Wallet {..} -> Repository.remove name

  printLn "-----------------------------------"

getSeedPhrase' :: (MonadIO m) => String -> m Bool
getSeedPhrase' seedPhrase =  if Prelude.length (words seedPhrase) /= 24 then
  do
    printLn "we said 24 words !"
    return False
  else return True

getSeedPhrase :: MonadIO m => m String
getSeedPhrase = askStringFilterM "> please enter your 24 words mnemonics then press enter : " getSeedPhrase'

restoreByMnemonics :: (MonadIO m, MonadReader Environment m) => m ()
restoreByMnemonics = do
  printLn "-----------------------------------"
  walletName <- askWalletName
  seedPhrase <- words <$> getSeedPhrase
  _ <- Repository.restoreByMnemonics walletName seedPhrase
  printLn "-----------------------------------"

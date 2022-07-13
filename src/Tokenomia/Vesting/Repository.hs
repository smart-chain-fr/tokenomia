{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Tokenomia.Vesting.Repository (
  register,
  WalletWithVestedFunds (..),
  Vesting (..),
  VestingContext (..),
  VestingState (..),
  TrancheContext (..),
  TrancheState (..),
  selectWalletWithVestedFunds,
  selectVesting,
) where

import Data.Aeson hiding (Value)

import System.Directory

import Data.List.NonEmpty hiding (filter, length, map)
import Data.Maybe
import Data.Text qualified as T
import Data.Time.Clock.POSIX qualified as POSIX
import Tokenomia.Common.Folder (Folder (..), getFolderPath)
import Prelude hiding ((+), (-))

import Control.Monad.Reader

import Ledger hiding (Address, singleton)
import PlutusTx.Prelude (AdditiveSemigroup ((+)))

import Tokenomia.Common.Serialise
import Tokenomia.Script.ChainIndex qualified as Script
import Tokenomia.Script.LocalRepository qualified as Script
import Tokenomia.Script.UTxO qualified as Script

import Tokenomia.Wallet.LocalRepository qualified as Wallet
import Tokenomia.Wallet.Type

import Tokenomia.Common.Environment
import Tokenomia.Common.Shell.InteractiveMenu
import Tokenomia.Vesting.Contract
import Tokenomia.Wallet.ChildAddress.ChildAddressRef
import Tokenomia.Wallet.ChildAddress.LocalRepository

data WalletWithVestedFunds = WalletWithVestedFunds {wallet :: Wallet, vestedFunds :: NonEmpty Vesting}
data Vesting = Vesting VestingContext VestingState deriving stock (Eq, Show)

data VestingContext = VestingContext
  { scriptLocation :: Script.ScriptLocation
  , validator :: Validator
  , vestingParam :: VestingParams
  , investorId :: PubKeyHash
  , tranches :: (TrancheContext, TrancheContext)
  }
  deriving stock (Eq, Show)

data TrancheContext = TrancheContext {deadline :: POSIXTime, valueVested :: Value} deriving stock (Eq, Show)

data VestingState = VestingState
  { scriptUTxOs :: [Script.ScriptUTxO]
  , tranches :: (TrancheState, TrancheState)
  }
  deriving stock (Eq, Show)

data TrancheState
  = Locked
  | Available
  | Retrieved
  deriving stock (Eq, Show)

selectWalletWithVestedFunds :: (MonadIO m, MonadReader Environment m) => m (Maybe WalletWithVestedFunds)
selectWalletWithVestedFunds =
  getWalletWithVestedFunds
    >>= \case
      Nothing -> return Nothing
      Just a -> Just <$> showMenu a
  where
    showMenu :: (MonadIO m) => NonEmpty WalletWithVestedFunds -> m WalletWithVestedFunds
    showMenu wallets = liftIO $ askMenu wallets

instance DisplayMenuItem WalletWithVestedFunds where
  displayMenuItem WalletWithVestedFunds {wallet = Wallet {..}, ..} =
    name <> " (" <> (show . length) vestedFunds <> " vesting script(s))"

getWalletWithVestedFunds :: (MonadIO m, MonadReader Environment m) => m (Maybe (NonEmpty WalletWithVestedFunds))
getWalletWithVestedFunds =
  getVestingInProgress
    >>= \case
      Nothing -> return Nothing
      Just allVestingInProgress -> do
        wallets <- Wallet.fetchAll
        x <-
          mapM
            ( \wallet@Wallet {name} -> do
                ChildAddress {..} <- fetchById $ ChildAddressRef name 0
                let vestingForWallet = filter (\(Vesting VestingContext {investorId} _) -> investorId == publicKeyHash) . toList $ allVestingInProgress
                return $ case nonEmpty vestingForWallet of
                  Nothing -> Nothing
                  Just vestedFunds -> Just WalletWithVestedFunds {..}
            )
            wallets
        return . nonEmpty . catMaybes $ x

selectVesting :: (MonadIO m) => NonEmpty Vesting -> m Vesting
selectVesting = askMenu

getVestingInProgress :: (MonadIO m, MonadReader Environment m) => m (Maybe (NonEmpty Vesting))
getVestingInProgress = do
  now <- convertToInternalPosix <$> liftIO POSIX.getPOSIXTime
  vestingParams <- getAll
  nonEmpty . filter isInProgress <$> mapM (convert now) vestingParams
  where
    convert :: (MonadIO m, MonadReader Environment m) => POSIXTime -> VestingParams -> m Vesting
    convert
      now
      vestingParam@VestingParams
        { vestingTranche1 =
          VestingTranche
            { vestingTrancheDate = deadline1
            , vestingTrancheAmount = token1
            }
        , vestingTranche2 =
          VestingTranche
            { vestingTrancheDate = deadline2
            , vestingTrancheAmount = token2
            }
        , vestingOwner = investorId
        } = do
        let validator = vestingScript vestingParam
        scriptLocation <- Script.getScriptLocation validator
        (scriptUTxOs, retrieveState) <-
          Script.queryUTxO (Script.onChain scriptLocation)
            >>= \case
              [] -> return ([], AllTranchesRetrieved)
              scriptUTxOs@[_] -> return (scriptUTxOs, AllTranchesRetrieved)
              scriptUTxOs@[_, _] -> return (scriptUTxOs, ZeroTrancheRetrieved)
              _ -> error $ "retrieved an unexpected number of UTXos at " <> show (Script.onChain scriptLocation)
        let (s1, s2) = getTrancheStates now deadline1 deadline2 retrieveState
        return
          ( Vesting
              VestingContext
                { tranches =
                    ( TrancheContext deadline1 token1
                    , TrancheContext deadline2 token2
                    )
                , ..
                }
              VestingState {tranches = (s1, s2), ..}
          )

isInProgress :: Vesting -> Bool
isInProgress (Vesting _ VestingState {..}) =
  case tranches of
    (Retrieved, Retrieved) -> False
    _ -> True

getTrancheStates :: POSIXTime -> POSIXTime -> POSIXTime -> RetrieveState -> (TrancheState, TrancheState)
getTrancheStates now d1 d2 state =
  case (reached d1, reached d2, state) of
    (False, False, ZeroTrancheRetrieved) -> (Locked, Locked)
    (False, False, OneTrancheRetrieved) -> error "unexpected State : 2 Tranches Locked and One Tranche Retrieved "
    (True, False, ZeroTrancheRetrieved) -> (Available, Locked)
    (True, False, OneTrancheRetrieved) -> (Retrieved, Locked)
    (False, True, ZeroTrancheRetrieved) -> (Locked, Available)
    (False, True, OneTrancheRetrieved) -> (Locked, Retrieved)
    (True, True, ZeroTrancheRetrieved) -> (Available, Available)
    (True, True, OneTrancheRetrieved) -> (Retrieved, Available) -- by convention we retrieve tranche 1 firstly
    (_, _, AllTranchesRetrieved) -> (Retrieved, Retrieved)
  where
    reached = (< now)

data RetrieveState = OneTrancheRetrieved | ZeroTrancheRetrieved | AllTranchesRetrieved

register :: (MonadIO m, MonadReader Environment m) => VestingParams -> m ()
register vestingParams = do
  indexFolder <- getFolderPath Validators
  let filePath = indexFolder <> "vesting.index"
  liftIO (doesFileExist filePath)
    >>= \case
      False -> liftIO $ encodeFile filePath [vestingParams]
      True ->
        liftIO $
          decodeFileStrict @[VestingParams] filePath
            >>= \case
              Nothing -> error "Vesting index is badly formed"
              Just params -> liftIO $ encodeFile filePath (vestingParams : params)

getAll :: (MonadIO m, MonadReader Environment m) => m [VestingParams]
getAll = do
  indexFolder <- getFolderPath Validators
  let filePath = indexFolder <> "vesting.index"
  liftIO (doesFileExist filePath)
    >>= \case
      False -> return []
      True ->
        liftIO $
          decodeFileStrict @[VestingParams] filePath
            >>= \case
              Nothing -> error "Vesting index is badly formed"
              Just params -> return params

instance DisplayMenuItem Vesting where
  displayMenuItem
    ( Vesting
        VestingContext
          { tranches =
            ( TrancheContext {deadline = d1, valueVested = v1}
              , TrancheContext {deadline = d2, valueVested = v2}
              )
          }
        VestingState {tranches = (s1, s2)}
      ) =
      case (s1, s2) of
        (Available, Available) -> "Funds available :" <> "\n\t   * " <> (T.unpack . toCLI) (v1 + v2)
        (Retrieved, Retrieved) -> "Funds totally retrieved"
        (Locked, Available) ->
          "Funds Partially Unlocked :"
            <> "\n\t   * "
            <> (T.unpack . toCLI) v1
            <> "(locked till "
            <> (formatISO8601 . convertToExternalPosix) d1
            <> ")"
            <> "\n\t   * "
            <> (T.unpack . toCLI) v2
            <> "(available)"
        (Available, Locked) ->
          "Funds Partially Unlocked :"
            <> "\n\t   * "
            <> (T.unpack . toCLI) v1
            <> "(available)"
            <> "\n\t   * "
            <> (T.unpack . toCLI) v2
            <> "(locked till "
            <> (formatISO8601 . convertToExternalPosix) d2
            <> ")"
        (Locked, Locked) ->
          "Funds Fully Locked :"
            <> "\n\t   * "
            <> (T.unpack . toCLI) v1
            <> "(locked till "
            <> (formatISO8601 . convertToExternalPosix) d1
            <> ")"
            <> "\n\t   * "
            <> (T.unpack . toCLI) v2
            <> "(locked till "
            <> (formatISO8601 . convertToExternalPosix) d2
            <> ")"
        (Retrieved, Locked) ->
          "Funds Partially Locked :"
            <> "\n\t   * "
            <> (T.unpack . toCLI) v1
            <> "(retrieved)"
            <> "\n\t   * "
            <> (T.unpack . toCLI) v2
            <> "(locked till "
            <> (formatISO8601 . convertToExternalPosix) d2
            <> ")"
        (Retrieved, Available) ->
          "Funds Partially Locked :"
            <> "\n\t   * "
            <> (T.unpack . toCLI) v1
            <> "(retrieved)"
            <> "\n\t   * "
            <> (T.unpack . toCLI) v2
            <> "(available)"
        (Locked, Retrieved) ->
          "Funds Partially Locked :"
            <> "\n\t   * "
            <> (T.unpack . toCLI) v1
            <> "(locked till "
            <> (formatISO8601 . convertToExternalPosix) d1
            <> ")"
            <> "\n\t   * "
            <> (T.unpack . toCLI) v2
            <> "(retrieved)"
        (Available, Retrieved) ->
          "Funds Partially Locked :"
            <> "\n\t   * "
            <> (T.unpack . toCLI) v1
            <> "(available)"
            <> "\n\t   * "
            <> (T.unpack . toCLI) v2
            <> "(retrieved)"

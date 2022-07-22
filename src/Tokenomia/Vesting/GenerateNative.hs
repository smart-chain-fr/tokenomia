{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tokenomia.Vesting.GenerateNative (generatePrivateSaleFiles, nativeScriptToLedgerAddr, NativeScript (NativeScript)) where

import Cardano.Api (
  Hash,
  PaymentCredential (PaymentCredentialByScript),
  PaymentKey,
  Script (SimpleScript),
  SimpleScript (
    RequireAllOf,
    RequireSignature,
    RequireTimeAfter
  ),
  SimpleScriptVersion (SimpleScriptV2),
  SlotNo,
  StakeAddressReference (NoStakeAddress),
  TimeLocksSupported (TimeLocksInSimpleScriptV2),
  hashScript,
  makeShelleyAddress,
  serialiseToBech32,
 )
import qualified Blockfrost.Client as Blockfrost
import qualified Cardano.Api as Api
import Control.Applicative (ZipList (ZipList, getZipList))
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, asks)
import Data.Aeson (eitherDecodeFileStrict, encodeFile)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Bifunctor (Bifunctor (bimap), first)
import Data.Either.Combinators (maybeToRight)
import Data.Foldable (traverse_)
import Data.Foldable.Extra (sumOn')
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty, nonEmpty, (<|))
import qualified Data.List.NonEmpty as List.NonEmpty
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as Map.NonEmpty
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Tuple.Extra (firstM, second)
import Ledger (POSIXTime, Slot (Slot, getSlot), toPubKeyHash)
import Ledger.Address (Address)
import Ledger.Value (AssetClass (unAssetClass))
import Numeric.Natural
import Plutus.V1.Ledger.Value (toString)
import System.FilePath (replaceFileName)
import Tokenomia.Vesting.Sendings (checkMalformedAddr)
import Tokenomia.Common.Environment (Environment (Mainnet, Testnet, magicNumber), convertToExternalPosix, toSlot)
import Tokenomia.Common.Error (TokenomiaError (InvalidPrivateSale, MalformedAddress))
import Tokenomia.TokenDistribution.Distribution (Distribution (Distribution), Recipient (Recipient), WithNetworkId (WithNetworkId))
import Tokenomia.TokenDistribution.Parser.Address (deserialiseCardanoAddress)

type Amount = Natural

data Tranche = Tranche
  { percentage :: Natural -- out of 10,000
  , duration :: Integer -- number of slots
  }
  deriving stock (Show)

$(deriveJSON defaultOptions ''Tranche)

-- Invariants
-- Σ percentages = 100%
-- Description : Represent Vesting Tranches (Time Sequential and contiguous)

newtype Tranches = Tranches (NonEmpty Tranche)
  deriving stock (Show)

-- Separate to keep the derived json instance clean
unTranches :: Tranches -> NonEmpty Tranche
unTranches (Tranches x) = x

$(deriveJSON defaultOptions ''Tranches)

data PrivateInvestor = PrivateInvestor
  { address :: Blockfrost.Address
  , allocation :: Amount
  }
  deriving stock (Show)

$(deriveJSON defaultOptions ''PrivateInvestor)

data PrivateSale = PrivateSale
  { start :: POSIXTime
  , tranches :: Tranches
  , assetClass :: AssetClass
  , investors :: NonEmpty PrivateInvestor
  }
  deriving stock (Show)

$(deriveJSON defaultOptions ''PrivateSale)

data NativeScript = NativeScript
  { pkh :: String
  , unlockTime :: Integer
  }
  deriving stock (Show, Eq)

$(deriveJSON defaultOptions ''NativeScript)

-- | Simplified AssetClass that serialises to JSON without newtypes over currency symbol and token name
data AssetClassSimple = AssetClassSimple
  { currencySymbol :: String -- As hex
  , tokenName :: String -- As hex
  }
  deriving stock (Show)

$(deriveJSON defaultOptions ''AssetClassSimple)

data LockedFund = LockedFund
  { nativeScript :: NativeScript
  , asset :: AssetClassSimple
  }
  deriving stock (Show)

$(deriveJSON defaultOptions ''LockedFund)

-- Map AddressAsText [LockedFund]
type DatabaseOutput = NEMap Text (NonEmpty LockedFund)

deriving newtype instance Ord Blockfrost.Address

getNetworkId :: forall (m :: Type -> Type). MonadReader Environment m => m Api.NetworkId
getNetworkId = asks readNetworkId
  where
    readNetworkId :: Environment -> Api.NetworkId
    readNetworkId Mainnet {} = Api.Mainnet
    readNetworkId Testnet {magicNumber} = Api.Testnet . Api.NetworkMagic $ fromInteger magicNumber

parsePrivateSale ::
  forall (m :: Type -> Type).
  ( MonadIO m
  , MonadError TokenomiaError m
  ) =>
  String ->
  m PrivateSale
parsePrivateSale path = do
  eitherErrPriv <- liftIO . (eitherDecodeFileStrict @PrivateSale) $ path
  liftEither $ do
    prvSale <- first InvalidPrivateSale eitherErrPriv

    validateTranches $ tranches prvSale
    traverse_ (checkMalformedAddr . address) $ investors prvSale
    pure prvSale

generatePrivateSaleFiles ::
  forall (m :: Type -> Type).
  ( MonadIO m
  , MonadError TokenomiaError m
  , MonadReader Environment m
  ) =>
  m ()
generatePrivateSaleFiles = do
  liftIO . putStrLn $ "Please enter a filepath with JSON data"
  path <- liftIO getLine

  prvSale <- parsePrivateSale path
  nativeData <- splitInTranches prvSale
  networkId <- getNetworkId

  let dbOutput = toDbOutput prvSale nativeData
  distribution <- toDistribution prvSale nativeData

  liftIO $ do
    encodeFile (replaceFileName path "database.json") dbOutput
    encodeFile (replaceFileName path "distribution.json") $ distribution `WithNetworkId` networkId
    putStrLn "Files database.json and distribution.json generated."

toDistribution ::
  forall (m :: Type -> Type).
  ( MonadError TokenomiaError m
  , MonadReader Environment m
  ) =>
  PrivateSale ->
  NEMap Blockfrost.Address (NonEmpty (NativeScript, Amount)) ->
  m Distribution
toDistribution prvSale nativeData = Distribution (assetClass prvSale) <$> recipients
  where
    elemsList =
      ZipList . List.NonEmpty.toList <$> Map.NonEmpty.elems nativeData

    mergedNativeScriptAmts :: [(NativeScript, Integer)]
    mergedNativeScriptAmts =
      fmap (second toInteger)
        . getZipList
        . foldr (\nsAmt acc -> combineNs <$> nsAmt <*> acc) (List.NonEmpty.head elemsList)
        $ List.NonEmpty.tail elemsList

    combineNs (ns, a1) (_, a2) = (ns, a1 + a2)

    recipients :: m [Recipient]
    recipients = traverse (fmap (uncurry Recipient) . firstM nativeScriptToLedgerAddr) mergedNativeScriptAmts

nativeScriptToLedgerAddr ::
  forall (m :: Type -> Type).
  ( MonadError TokenomiaError m
  , MonadReader Environment m
  ) =>
  NativeScript ->
  m Address
nativeScriptToLedgerAddr ns = do
  networkId <- getNetworkId
  textToLedgerAddress . serialiseToBech32 $ shelleyAddr networkId
  where
    shelleyAddr nid = makeShelleyAddress nid (PaymentCredentialByScript hashedScript) NoStakeAddress
    hashedScript = hashScript $ SimpleScript SimpleScriptV2 cardanNs
    cardanNs =
      RequireAllOf
        [ RequireSignature pkHash
        , RequireTimeAfter TimeLocksInSimpleScriptV2 unlockAfterSlot
        ]

    pkHash :: Hash PaymentKey
    pkHash = fromString (pkh ns)

    unlockAfterSlot :: SlotNo
    unlockAfterSlot = fromInteger (unlockTime ns)

textToLedgerAddress ::
  forall (m :: Type -> Type).
  MonadError TokenomiaError m =>
  Text ->
  m Address
textToLedgerAddress =
  liftEither . first (const MalformedAddress) . deserialiseCardanoAddress

toDbOutput ::
  PrivateSale ->
  NEMap Blockfrost.Address (NonEmpty (NativeScript, Amount)) ->
  DatabaseOutput
toDbOutput ps invDistMap =
    Map.NonEmpty.mapKeys addrToText
    $ fmap ((`LockedFund` acSimple) . fst) <$> invDistMap
  where
    acSimple = uncurry AssetClassSimple . bimap show toString . unAssetClass $ assetClass ps
    addrToText :: Blockfrost.Address -> Text
    addrToText (Blockfrost.Address addr) = addr

assertErr :: String -> Bool -> Either TokenomiaError ()
assertErr _ True = Right ()
assertErr err _ = Left $ InvalidPrivateSale err

validateTranches :: Tranches -> Either TokenomiaError ()
validateTranches tranches = do
  assertErr
    ("The sum of all the tranches must be 10000, but we got: " <> show tranchesSum)
    $ tranchesSum == 10000
  where
    tranchesSum = sumOn' percentage $ unTranches tranches

mergeInvestors :: NonEmpty PrivateInvestor -> NEMap Blockfrost.Address Amount
mergeInvestors = Map.NonEmpty.fromListWith (+) . (toTuple <$>)
  where
    toTuple :: PrivateInvestor -> (Blockfrost.Address, Amount)
    toTuple (PrivateInvestor x y) = (x, y)

{- | We are taking the floor of the corresponding percentage in all items
 except in the last one where we do the corrections to sum the right amount.
-}
splitAmountInTranches ::
  Slot ->
  Amount ->
  Tranches ->
  Amount ->
  NonEmpty (Slot, Amount)
splitAmountInTranches startSlot total trs acc =
  case nonEmpty . List.NonEmpty.tail $ unTranches trs of
    Nothing -> pure (nextSlot, total - acc)
    Just remainTranches ->
      let takenAmount :: Amount
          takenAmount = div (total * percentage tranche) 10000
       in (nextSlot, takenAmount) <| splitAmountInTranches nextSlot total (Tranches remainTranches) (acc + takenAmount)
  where
    tranche :: Tranche
    tranche = List.NonEmpty.head $ unTranches trs
    nextSlot :: Slot
    nextSlot = Slot (duration tranche) + startSlot

splitInTranches ::
  forall (m :: Type -> Type).
  ( MonadIO m
  , MonadError TokenomiaError m
  , MonadReader Environment m
  ) =>
  PrivateSale ->
  m (NEMap Blockfrost.Address (NonEmpty (NativeScript, Amount)))
splitInTranches PrivateSale {..} = do
  startSlot <- toSlot $ posixSecondsToUTCTime $ convertToExternalPosix $ start * 1000
  let f :: Blockfrost.Address -> Amount -> m (NonEmpty (NativeScript, Amount))
      f addr x = traverse (toNative addr) $ splitAmountInTranches startSlot x tranches 0

      toNative :: Blockfrost.Address -> (Slot, Amount) -> m (NativeScript, Amount)
      toNative (Blockfrost.Address addr) (slot, amt) = do
        ledgerAddress <- textToLedgerAddress addr
        pkh <- liftEither $ maybeToRight (InvalidPrivateSale "Address is not PubKeyHash address") $ toPubKeyHash ledgerAddress
        pure $ (NativeScript (show pkh) $ getSlot slot, amt)

      investorsMap :: NEMap Blockfrost.Address Amount
      investorsMap = mergeInvestors investors

  Map.NonEmpty.traverseWithKey f investorsMap

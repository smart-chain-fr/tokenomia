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

module Tokenomia.Vesting.GenerateNative (generatePrivateSaleFiles) where

import qualified Cardano.Api as Api
import Control.Applicative (ZipList (ZipList, getZipList))
import Control.Monad.Except (MonadError (throwError), liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, asks)
import Data.Aeson (ToJSON (toJSON), eitherDecodeFileStrict)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Bifunctor (Bifunctor (bimap), first)
import Data.Either (lefts)
import Data.Foldable (foldl', traverse_)
import Data.Foldable.Extra (sumOn')
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty, nonEmpty, (<|))
import qualified Data.List.NonEmpty as List.NonEmpty
import Data.Map (Map, mapKeys)
import Data.Map.NonEmpty (NEMap, mapWithKey, toMap)
import qualified Data.Map.NonEmpty as Map.NonEmpty
import Data.Text (Text, unpack)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Tuple.Extra (firstM, second)
import Ledger (Address, POSIXTime (POSIXTime), Slot (Slot, getSlot))
import Ledger.Value (AssetClass (unAssetClass))
import Numeric.Natural
import Tokenomia.Common.Environment (Environment (Mainnet, Testnet, magicNumber), convertToExternalPosix, toSlot)
import Tokenomia.Common.Error (TokenomiaError (InvalidPrivateSale))
import Tokenomia.TokenDistribution.Distribution (Distribution (Distribution), Recipient (Recipient))
import Tokenomia.TokenDistribution.Parser.Address (serialiseCardanoAddress)
import Cardano.Api (SimpleScript(RequireAllOf, RequireTimeAfter, RequireSignature), TimeLocksSupported (TimeLocksInSimpleScriptV2), PaymentKey, Hash, SlotNo, hashScript)
import Data.String (IsString(fromString))

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
  -- TODO: Verify the from json instance for this is the longform string addr1..., and not just its raw constructors. If it isn't, use blockfrost's Address and convert
  { address :: Address
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
  -- TODO: validate address here
  liftEither $ do
    prvSale <- first InvalidPrivateSale eitherErrPriv
    validateTranches $ tranches prvSale
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

  {- let dbOutput = mapKeys serialiseAddress (id <$> toMap nativeData) -}
  dbOutput <- toDbOutput prvSale nativeData
  {- disrtibution <- toDistribution prvSale nativeData -}
  -- Generate DatabaseOutput as `path` with filename as database.json
  -- Generate Distribution as `path` with filename as distribution.json
  -- AMIR, start here :)

  liftIO . writeFile (path <> "database.json") $ show dbOutput
  {- liftIO . writeFile (path <> "distribution.json") $ show prvSaleJson -}
  pure ()

toDistribution ::
  forall (m :: Type -> Type).
  ( MonadError TokenomiaError m
  , MonadReader Environment m
  ) =>
  PrivateSale ->
  NEMap Address (NonEmpty (NativeScript, Amount)) ->
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
    recipients = traverse (fmap (uncurry Recipient) . firstM nativeScriptToAddr) mergedNativeScriptAmts


nativeScriptToAddr ::
  forall (m :: Type -> Type).
  ( MonadError TokenomiaError m
  , MonadReader Environment m
  ) =>
  NativeScript -> m Address
nativeScriptToAddr ns = undefined
  where -- scriptHash = hashScript cardanNs
        cardanNs = 
          RequireAllOf [
            RequireSignature pkHash,
            RequireTimeAfter TimeLocksInSimpleScriptV2 unlockAfterSlot
          ]

        pkHash :: Hash PaymentKey
        pkHash =  fromString (pkh ns)

        unlockAfterSlot :: SlotNo
        unlockAfterSlot = fromInteger (unlockTime ns)


toDbOutput ::
  forall (m :: Type -> Type).
  ( MonadError TokenomiaError m
  , MonadReader Environment m
  ) =>
  PrivateSale ->
  NEMap Address (NonEmpty (NativeScript, Amount)) ->
  m DatabaseOutput
toDbOutput ps invDistMap =
  fmap Map.NonEmpty.fromList
    . traverse (firstM addrToText)
    . Map.NonEmpty.toList
    $ fmap ((`LockedFund` acSimple) . fst) <$> invDistMap
  where
    acSimple = uncurry AssetClassSimple . bimap show show . unAssetClass $ assetClass ps
    addrToText :: Address -> m Text

    addrToText addr = getNetworkId >>= (liftEither . first toAddressError . (`serialiseCardanoAddress` addr))

{- liftEither . first InvalidPrivateSale . serialiseCardanoAddress (getNetworkId) -}

toAddressError :: Text -> TokenomiaError
toAddressError err = InvalidPrivateSale $ "Failed to serialise address " <> unpack err

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

mergeInvestors :: NonEmpty PrivateInvestor -> NEMap Address Amount
mergeInvestors = Map.NonEmpty.fromListWith (+) . (toTuple <$>)
  where
    toTuple :: PrivateInvestor -> (Address, Amount)
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
  m (NEMap Address (NonEmpty (NativeScript, Amount)))
splitInTranches PrivateSale {..} = do
  networkId <- getNetworkId
  startSlot <- toSlot $ posixSecondsToUTCTime $ convertToExternalPosix start -- change undefined for posix -> utc
  let f :: Address -> Amount -> m (NonEmpty (NativeScript, Amount))
      f addr x = traverse (toNative addr) $ splitAmountInTranches startSlot x tranches 0

      toNative :: Address -> (Slot, Amount) -> m (NativeScript, Amount)
      toNative addr (slot, amt) = do
        addrStr <- liftEither $ first toAddressError $ serialiseCardanoAddress networkId addr
        pure (NativeScript (unpack addrStr) $ getSlot slot, amt)

      investorsMap :: NEMap Address Amount
      investorsMap = mergeInvestors investors

  Map.NonEmpty.traverseWithKey f investorsMap

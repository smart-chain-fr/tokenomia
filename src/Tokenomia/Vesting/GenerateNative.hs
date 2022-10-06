{-# LANGUAGE DeriveAnyClass                 #-}
{-# LANGUAGE DeriveGeneric                  #-}
{-# LANGUAGE DerivingStrategies             #-}
{-# LANGUAGE FlexibleContexts               #-}
{-# LANGUAGE FlexibleInstances              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving     #-}
{-# LANGUAGE ImportQualifiedPost            #-}
{-# LANGUAGE KindSignatures                 #-}
{-# LANGUAGE NamedFieldPuns                 #-}
{-# LANGUAGE OverloadedStrings              #-}
{-# LANGUAGE RecordWildCards                #-}
{-# LANGUAGE ScopedTypeVariables            #-}
{-# LANGUAGE TypeApplications               #-}

module Tokenomia.Vesting.GenerateNative
    ( Allocation
    , DatabaseOutput(..)
    , InvestorAddress(..)
    , NativeScript(..)
    , NativeScriptInfo (..)
    , PrivateSale(..)
    , PrivateSaleTranche(..)
    , TrancheProperties(..)
    , TranchesProportions(..)
    , calculateDefaultMinimumUTxOFromAssetClass
    , generatePrivateSaleFiles
    , getNetworkId
    , investorAddressPubKeyHash
    , merge
    , minAllocation
    , nativeScriptAddress
    , parsePrivateSale
    , queryError
    , readPrivateSale
    , scaleRatios
    , splitAllocation
    , splitInTranches
    , toDatabaseOutput
    , toDistribution
    , trancheNativeScriptInfos
    , validateAllocations
    , validatePrivateSale
    , validateTranchesProportions
    ) where

import Control.Error.Safe                   ( assertErr )
import Control.Monad                        ( join, (>=>) )
import Control.Monad.Except                 ( MonadError, liftEither )
import Control.Monad.IO.Class               ( MonadIO, liftIO )
import Control.Monad.Reader                 ( MonadReader, asks )
import Data.Bifunctor                       ( first )
import Data.Either.Combinators              ( maybeToRight )
import Data.Foldable                        ( traverse_ )
import Data.Functor.Syntax                  ( (<$$>) )
import Data.Kind                            ( Type )
import Data.Ratio                           ( Ratio, (%), numerator, denominator )
import Data.String                          ( fromString )
import Data.Text                            ( Text )
import Data.Time.Clock                      ( NominalDiffTime )

import Data.List.NonEmpty                   ( NonEmpty((:|)), (<|) )
import Data.List.NonEmpty qualified
    as NEList                               ( fromList, head, toList, zip )

import Data.Map.NonEmpty                    ( NEMap, traverseWithKey )
import Data.Map.NonEmpty qualified
    as NEMap                                ( elems, fromAscList, keys )

import GHC.Generics                         ( Generic )
import GHC.Natural                          ( Natural, naturalFromInteger, naturalToInteger )

import Ledger                               ( PubKeyHash, toPubKeyHash )
import Ledger.Address                       ( Address )
import Ledger.Value                         ( AssetClass(..) )
import System.FilePath                      ( replaceFileName )

import Data.Aeson
    ( FromJSON(..)
    , ToJSON(..)
    , FromJSONKey
    , ToJSONKey
    , (.=)
    , eitherDecodeFileStrict
    , encodeFile
    , object
    )

import Cardano.Api
    ( NetworkMagic(..)
    , PaymentCredential(PaymentCredentialByScript)
    , ShelleyBasedEra(..)
    , Script(SimpleScript)
    , SimpleScript
        ( RequireAllOf
        , RequireSignature
        , RequireTimeAfter
        )
    , SimpleScriptVersion(SimpleScriptV2)
    , SimpleScriptV2
    , StakeAddressReference(NoStakeAddress)
    , TimeLocksSupported(TimeLocksInSimpleScriptV2)
    , hashScript
    , makeShelleyAddress
    , serialiseToBech32
 )

import Cardano.Api qualified
    as Api                                  ( NetworkId(..) )

import Tokenomia.CardanoApi.Fees            ( calculateDefaultMinimumUTxOFromAssetId )
import Tokenomia.Common.Aeson.AssetClass    ( assetClassToJSON )
import Tokenomia.Common.AssetClass          ( adaAssetClass )
import Tokenomia.Common.Data.List.Extra     ( mapLastWith, transpose )
import Tokenomia.Common.Environment         ( Environment(..) )
import Tokenomia.Common.Environment.Query   ( evalQueryWithSystemStart )
import Tokenomia.Common.Error               ( TokenomiaError(InvalidPrivateSale, MalformedAddress) )
import Tokenomia.Common.Time                ( toNextBeginNominalDiffTime )

import Tokenomia.CardanoApi.Query           ( QueryFailure, queryNominalDiffTimeToSlot )

import Tokenomia.CardanoApi.FromPlutus.Value
    ( assetClassAsAssetId )

import Tokenomia.TokenDistribution.Parser.Address
    ( deserialiseCardanoAddress )

import Tokenomia.TokenDistribution.Distribution
    ( Distribution(..)
    , Recipient(..)
    , WithNetworkId(..)
    )


--------------------------------------------------------------------------------
--
-- [Note: Caveat]
--
-- Allocation ratios do not match exactly tranche proportions.
-- Rounding occurs in order to have intregral allocations.
-- See [Note: Rounding Allocation]
--
-- NativeScript requireTimeAfter may not correspond exactly to tranche
-- unlockTime.  On-chain time calculations are expressed in slots and a slot
-- correspond to an interval of POSIXTime.
-- See [Note: Slot Calculation]
--
--------------------------------------------------------------------------------
--
-- [Note: Rounding Allocation]
--
-- Let `[r_1, r_2, ..., r_k]` be the ratios for all `k` tranches.
-- Let `n` be the total allocation.
-- Let `ε` be the minimum amount on a script.
--
-- The allocation `n` will be splitted on `k` scripts with an integer partition.
-- When `Σ r_i = 1`,
--
--     n = ⌊n r_1⌋ + ⌊n r_2⌋ + ... + (⌊n r_k⌋ + δ)  (1)
--
-- where δ corresponds to rounding errors.
--
-- Each terms represent the amount sent on a different script.
-- Thus,
--
--     ∀ i, ⌊n r_i⌋ >= ε                            (2)
--
-- To have (1) and (2), it is sufficient to have the following preconditions
--     ∀ i, n r_i >= ε + 1
--     n >= k ε
--
-- So,
--     n >= ⌈ max [(ε + 1) / r_i, ..., (ε + 1) / r_k, k ε] ⌉
--
--------------------------------------------------------------------------------
--
-- [Note: Slot Calculation]
--
-- Consider the following situation.
--
--          n-1      n      n+1      > slot number
--       |       |       |       |
--    ---|-------|a-b----|-------|---> time in miliseconds since epoch
--                 ^
--                 unlockTime (ms)
--
-- Let `t` be the specified unlockTime.
-- Let `n` be the enclosing slot number of `t`.
--
-- The `RequireTimeAfter` parameter of a native script is expressed in slot.
-- Thus, it is necessary to choose a slot number from an unlockTime.
--
-- When `t` is not a starting POSIXTime of a slot,
--
--    a) With RequireTimeAfter (SlotNo n),
--        the transaction could succeed at `t - 1 ∈ (SlotNo n)`,
--        even if t - 1 < unlockTime.
--    b) With RequireTimeAfter (SlotNo n+1),
--        the transaction could fail at `t + 1 ∈ (SlotNo n)`,
--        even if t + 1 > unlockTime.
--
-- The less misleading alternative is b) for two reasons.
--    It is best to use a stricter condition than a more tolerant one.
--    It is best to succeed later than having no retry to gain understanding.
--
--------------------------------------------------------------------------------

type Allocation = Natural

newtype InvestorAddress
    =   InvestorAddress
    { unInvestorAddress :: Text }
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype TranchesProportions
    =   TranchesProportions
    { unTranchesProportions :: NonEmpty (Ratio Natural) }
    deriving stock (Show)

data    TrancheProperties
    =   TrancheProperties
    { proportion :: Ratio Natural
    , unlockTime :: NominalDiffTime
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data    PrivateSale
    =   PrivateSale
    { tranchesProperties :: NonEmpty TrancheProperties
    , assetClass :: AssetClass
    , allocationByAddress :: NEMap InvestorAddress Allocation
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data    PrivateSaleTranche
    =   PrivateSaleTranche
    { trancheUnlockTime :: NominalDiffTime
    , trancheAssetClass :: AssetClass
    , trancheAllocationByAddress :: NEMap InvestorAddress Allocation
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data    NativeScript
    =   NativeScript
    { requireSignature :: PubKeyHash
    , requireTimeAfter :: NominalDiffTime
    }
    deriving stock (Show)

data    NativeScriptInfo
    =   NativeScriptInfo
    { requiring :: NativeScript
    , recipient :: Recipient
    }
    deriving stock (Show)

instance ToJSON NativeScript  where
    toJSON NativeScript{..} =
        object
            [ "requireSignature" .= toJSON (show requireSignature)
            , "requireTimeAfter" .= toJSON requireTimeAfter
            ]

instance ToJSON (WithNetworkId NativeScriptInfo)  where
    toJSON (NativeScriptInfo{..} `WithNetworkId` networkId) =
        object
            [ "nativeScriptInfo" .= object
                [ "requiring" .= toJSON requiring
                , "recipient" .= toJSON (recipient `WithNetworkId` networkId)
                ]
            ]

data    DatabaseOutput
    =   DatabaseOutput
    { lockedAssetClass :: AssetClass
    , lockedFunds :: NEMap InvestorAddress (NonEmpty NativeScriptInfo)
    }
    deriving stock (Show)

instance ToJSON (WithNetworkId DatabaseOutput)  where
    toJSON (DatabaseOutput{..} `WithNetworkId` networkId) =
        object
            [ "lockedAssetClass" .= assetClassToJSON lockedAssetClass
            , "lockedFunds" .= toJSON ((`WithNetworkId` networkId) <$$> lockedFunds)
            ]

--------------------------------------------------------------------------------

-- | Convert a generic failure to a TokenomiaError
invalidPrivateSale :: Show a => a -> TokenomiaError
invalidPrivateSale = InvalidPrivateSale . show

-- | Convert a QueryFailure to a TokenomiaError
queryError :: QueryFailure -> TokenomiaError
queryError = invalidPrivateSale

-- | Preconditions on the input data
validatePrivateSale ::
     ( MonadError TokenomiaError m )
    => PrivateSale -> m ()
validatePrivateSale PrivateSale{..} =
    let proportions = TranchesProportions $ proportion <$> tranchesProperties
    in
        liftEither $ do
            ε <- calculateDefaultMinimumUTxOFromAssetClass assetClass
            validateTranchesProportions proportions
            validateAllocations ε proportions $ NEMap.elems allocationByAddress
            traverse_ (unsafeDeserialiseCardanoAddress . unInvestorAddress) $ NEMap.keys allocationByAddress

-- | Calculate mininum UTxO from a Plutus AssetClass
calculateDefaultMinimumUTxOFromAssetClass :: AssetClass -> Either TokenomiaError Natural
calculateDefaultMinimumUTxOFromAssetClass assetClass =
    do
        assetId <- first invalidPrivateSale
            (assetClassAsAssetId assetClass)
        naturalFromInteger <$> maybeToRight
            (InvalidPrivateSale "Could not calculate minimum UTxO")
            (calculateDefaultMinimumUTxOFromAssetId ShelleyBasedEraAlonzo assetId)

-- | Preconditions on tranches proportions
validateTranchesProportions :: TranchesProportions -> Either TokenomiaError ()
validateTranchesProportions (TranchesProportions proportions) = do
    assertErr
        (InvalidPrivateSale "All tranche proportion must be strictly positive.")
        (all (> 0) proportions)
    assertErr
        (InvalidPrivateSale "Tranche proportions must sum to 1.")
        (sum proportions == 1)

-- | Preconditions on total allocations
validateAllocations :: Natural -> TranchesProportions -> NonEmpty Allocation -> Either TokenomiaError ()
validateAllocations ε proportions allocations =
    assertErr
        (InvalidPrivateSale "Some allocations are too small.")
        (all (>= minAllocation ε proportions) allocations)

-- | Minimum allowed allocation to divide in tranche
minAllocation :: Natural -> TranchesProportions -> Allocation
minAllocation ε (TranchesProportions xs) =
    let k = naturalFromInteger . toInteger $ length xs
    in
        ceiling $ maximum $ (k*ε % 1) <| (divByRatio (ε + 1) <$> xs)
  where
    inv :: Integral a => Ratio a -> Ratio a
    inv x = denominator x % numerator x

    divByRatio :: Integral a => a -> Ratio a -> Ratio a
    divByRatio a x = (a % 1) * inv x

--------------------------------------------------------------------------------

getNetworkId ::
     ( MonadReader Environment m )
    => m Api.NetworkId
getNetworkId = asks readNetworkId
    where
        readNetworkId :: Environment -> Api.NetworkId
        readNetworkId Mainnet {} = Api.Mainnet
        readNetworkId Testnet {magicNumber} = Api.Testnet . NetworkMagic $ fromInteger magicNumber

unsafeDeserialiseCardanoAddress ::
     ( MonadError TokenomiaError m )
    => Text -> m Address
unsafeDeserialiseCardanoAddress =
    liftEither . first (const MalformedAddress) . deserialiseCardanoAddress

-- | Parse PrivateSale from a JSON file
readPrivateSale ::
     ( MonadIO m
     , MonadError TokenomiaError m
     )
    => FilePath -> m PrivateSale
readPrivateSale path  =
    (liftIO . (eitherDecodeFileStrict @PrivateSale) $ path)
       >>= liftEither . first InvalidPrivateSale

-- | Parse PrivateSale from a JSON file and validate the data
parsePrivateSale ::
     ( MonadIO m
     , MonadError TokenomiaError m
     )
    => FilePath -> m PrivateSale
parsePrivateSale path = do
    privateSale <- readPrivateSale path
    privateSale <$ validatePrivateSale privateSale

generatePrivateSaleFiles ::
     ( MonadIO m
     , MonadError TokenomiaError m
     , MonadReader Environment m
     )
    => m ()
generatePrivateSaleFiles = do
    path <- liftIO $ do
        putStrLn "Please enter a filepath with JSON data"
        getLine

    networkId <- getNetworkId
    privateSaleTranches <- splitInTranches <$> parsePrivateSale path
    databaseOutput <- toDatabaseOutput privateSaleTranches
    let distribution = toDistribution databaseOutput

    liftIO $ do
        encodeFile (replaceFileName path "database.json") (databaseOutput `WithNetworkId` networkId)
        encodeFile (replaceFileName path "distribution.json") (distribution `WithNetworkId` networkId)
        putStrLn "Files database.json and distribution.json generated."

-- | Try to convert an Address to its PubKeyHash
investorAddressPubKeyHash ::
     ( MonadError TokenomiaError m )
    => InvestorAddress -> m PubKeyHash
investorAddressPubKeyHash (InvestorAddress text) = do
    unsafeDeserialiseCardanoAddress text >>=
        liftEither
            . maybeToRight (InvalidPrivateSale "Not a PubKeyHash address")
            . toPubKeyHash

-- | Make a Cardano SimpleScript from a NativeScript and deserialise its address
nativeScriptAddress ::
    forall (m :: Type -> Type).
     ( MonadError TokenomiaError m
     , MonadReader Environment m
     , MonadIO m
     )
    => NativeScript -> m Address
nativeScriptAddress =
    toCardanoSimpleScript >=> simpleScriptAddress
  where
    toCardanoSimpleScript :: NativeScript -> m (SimpleScript SimpleScriptV2)
    toCardanoSimpleScript NativeScript{..} =
        do
            slotNo <-
                evalQueryWithSystemStart queryError
                    queryNominalDiffTimeToSlot requireTimeAfter
            pure $ RequireAllOf
                [ RequireSignature (fromString . show $ requireSignature)
                , RequireTimeAfter TimeLocksInSimpleScriptV2 slotNo
                ]

    simpleScriptAddress :: SimpleScript SimpleScriptV2 -> m Address
    simpleScriptAddress script = do
        networkId <- getNetworkId
        unsafeDeserialiseCardanoAddress . serialiseToBech32 $
            makeShelleyAddress
                networkId
                (PaymentCredentialByScript . hashScript $ SimpleScript SimpleScriptV2 script)
                NoStakeAddress

-- | Construct NativeScriptInfos of a tranche for all addresses
trancheNativeScriptInfos ::
    forall (m :: Type -> Type).
     ( MonadError TokenomiaError m
     , MonadReader Environment m
     , MonadIO m
     )
    => PrivateSaleTranche -> m (NEMap InvestorAddress NativeScriptInfo)
trancheNativeScriptInfos PrivateSaleTranche{..} =
    traverseWithKey nativeScriptInfo trancheAllocationByAddress
  where
    nativeScriptInfo :: InvestorAddress -> Allocation -> m NativeScriptInfo
    nativeScriptInfo investorAddress allocation =
        do
            requiring <- nativeScript investorAddress
            recipient <-
                (`Recipient` naturalToInteger allocation)
                    <$> nativeScriptAddress requiring

            pure NativeScriptInfo{..}

    nativeScript :: InvestorAddress -> m NativeScript
    nativeScript investorAddress =
        do
            requireSignature <- investorAddressPubKeyHash investorAddress
            requireTimeAfter <-
                evalQueryWithSystemStart queryError
                    toNextBeginNominalDiffTime trancheUnlockTime

            pure NativeScript{..}

-- | Merge a list of maps into a single map to list using the keys of the first map
merge :: (Ord k) => NonEmpty (NEMap k v) -> NEMap k (NonEmpty v)
merge xxs@(x :| _) =
    NEMap.fromAscList $
        NEList.zip
            (NEMap.keys x)
            (NEList.fromList $ transpose (NEList.toList . NEMap.elems <$> xxs))

-- | Reshape all tranches NativeScriptInfos into a DatabaseOutput
toDatabaseOutput ::
     ( MonadError TokenomiaError m
     , MonadReader Environment m
     , MonadIO m
     )
    => NonEmpty PrivateSaleTranche -> m DatabaseOutput
toDatabaseOutput tranches =
    do
        allTrancheNativeScriptInfos <- traverse trancheNativeScriptInfos tranches
        let lockedAssetClass = trancheAssetClass $ NEList.head tranches
            lockedFunds = merge allTrancheNativeScriptInfos
        pure DatabaseOutput{..}

-- | Flatten all recipients of a DatabaseOutput for Distribution
toDistribution :: DatabaseOutput -> Distribution
toDistribution DatabaseOutput{..} =
    let recipients = NEList.toList $ join (recipient <$$> NEMap.elems lockedFunds)
        assetClass = lockedAssetClass
    in
        Distribution{..}

--------------------------------------------------------------------------------

-- | Duplicate the PrivateSale data into tranches with allocations partitionned
splitInTranches :: PrivateSale -> NonEmpty PrivateSaleTranche
splitInTranches PrivateSale {..} =
    let properties = NEList.toList tranchesProperties
        tranchesUnlockTimes = unlockTime <$> properties
        tranchesProportions = proportion <$> properties
        tranchesAllocationByAddress =
            transpose $
                scaleRatios tranchesProportions <$> allocationByAddress
    in
        NEList.fromList $ zipWith
            (`PrivateSaleTranche` assetClass)
                tranchesUnlockTimes
                tranchesAllocationByAddress

-- | Partition an integer into `k` parts, each part corresponding to a given ratio
scaleRatios :: (Integral a) => [Ratio a] -> a -> [a]
scaleRatios ratios scale =
    let xs = floor . ((scale % 1) *) <$> ratios
        rounding = floor ((scale % 1) * sum ratios) - sum xs
    in
        mapLastWith id (+ rounding) xs

-- | Split an allocation given a well-defined list of tranches proportions
splitAllocation :: TranchesProportions -> Allocation -> [Allocation]
splitAllocation (TranchesProportions rs) = scaleRatios $ NEList.toList rs

{-# LANGUAGE DerivingStrategies             #-}
{-# LANGUAGE ScopedTypeVariables            #-}
{-# LANGUAGE RecordWildCards                #-}
{-# LANGUAGE ImportQualifiedPost            #-}
{-# LANGUAGE FlexibleInstances              #-}
{-# LANGUAGE TupleSections                  #-}

module Spec.Tokenomia.Vesting.GenerateNative
    ( tests
    ) where

import Control.Applicative                  ( ZipList(..) )
import Control.Monad.Except                 ( runExceptT )
import Control.Monad.IO.Class               ( MonadIO(..) )
import Control.Monad.Reader                 ( runReaderT )
import Data.List.NonEmpty                   ( NonEmpty(..), (<|) )
import Data.List.NonEmpty qualified
    as NEList                               ( fromList, toList, zip, zipWith )

import Data.Map                             ( unionsWith )
import Data.Map.NonEmpty                    ( NEMap )
import Data.Map.NonEmpty qualified
    as NEMap                                ( fromList, toList, toMap, keys, elems )

import Data.Either                          ( isRight )
import Data.Either.Combinators              ( fromRight' )
import Data.Functor                         ( (<&>) )
import Data.Functor.Syntax                  ( (<$$>) )
import Data.Ratio                           ( (%) )

import GHC.Natural                          ( Natural, naturalFromInteger )

import Test.Tasty                           ( TestTree, testGroup )
import Test.Tasty.HUnit                     ( testCase, (@?=) )
import Test.QuickCheck.Modifiers            ( NonEmptyList(..), Positive(..) )
import Test.QuickCheck.Monadic              ( monadicIO )

import Test.Tasty.QuickCheck
    ( Arbitrary
    , Gen
    , Property
    , arbitrary
    , genericShrink
    , mapSize
    , noShrinking
    , scale
    , shrink
    , shrinkList
    , sized
    , testProperty
    , vectorOf
    , withMaxSuccess
    )

import Tokenomia.Common.Arbitrary.AssetClass    ()
import Tokenomia.Common.Arbitrary.Modifiers     ( Restricted(..) )
import Tokenomia.Common.Arbitrary.POSIXTime     ()
import Tokenomia.Common.Arbitrary.Wallet        ( PaymentAddress(..), generateAddresses )

import Tokenomia.Common.Data.Convertible        ( convert )
import Tokenomia.Common.Data.List.Extra         ( transpose )
import Tokenomia.Common.Environment             ( getTestnetEnvironmment )
import Tokenomia.Common.Time                    ( toNextBeginPOSIXTime )

import Tokenomia.TokenDistribution.Distribution ( Distribution(recipients), Recipient(..) )

import Tokenomia.Vesting.GenerateNative
    ( DatabaseOutput(..)
    , InvestorAddress(..)
    , NativeScript (..)
    , NativeScriptInfo (..)
    , PrivateSale(..)
    , PrivateSaleTranche(..)
    , TrancheProperties(..)
    , TranchesProportions(..)
    , calculateDefaultMinimumUTxOFromAssetClass
    , investorAddressPubKeyHash
    , merge
    , minAllocation
    , scaleRatios
    , splitAllocation
    , splitInTranches
    , toDatabaseOutput
    , toDistribution
    , trancheNativeScriptInfos
    , validateAllocations
    , validatePrivateSale
    , validateTranchesProportions
    )

-- import Test.QuickCheck.Monadic              ( assert )
-- import Tokenomia.Vesting.GenerateNativeRefacto ( getNetworkId )
-- import System.FilePath ( replaceFileName )
-- import Data.Aeson ( encodeFile )
-- import Tokenomia.TokenDistribution.Distribution ( WithNetworkId(..) )


instance Arbitrary InvestorAddress where
    arbitrary = InvestorAddress <$> arbitrary
    shrink = genericShrink

instance Arbitrary TrancheProperties where
    arbitrary = TrancheProperties <$> arbitrary <*> arbitrary
    shrink = genericShrink

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (NEMap k v) where
    arbitrary = NEMap.fromList <$> arbitrary
    shrink = fmap NEMap.fromList . shrinkAssocs . NEMap.toList
        where
            shrinkAssocs :: Arbitrary b => NonEmpty (a, b) -> [NonEmpty (a, b)]
            shrinkAssocs xs =
                [ NEList.fromList xs'
                | xs' <- shrinkList shrinkPair (NEList.toList xs)
                , not (null xs')
                ]

            shrinkPair :: Arbitrary b => (a, b) -> [(a, b)]
            shrinkPair (k, v) = (k,)  <$> shrink v

instance Arbitrary PrivateSale where
    arbitrary = PrivateSale <$> arbitrary <*> arbitrary <*> arbitrary
    shrink = genericShrink

-------------------------------------------------------------------------------

instance Arbitrary TranchesProportions where
    arbitrary =
        TranchesProportions <$>
            do
                xs <- getPositive <$$> arbitrary
                pure $ xs <&> (% sum xs)
    shrink (TranchesProportions (x :| xs))
        | null xs = []
        | otherwise =
            let shrinks = shrink $ TranchesProportions $ NEList.fromList xs
            in
                TranchesProportions <$>
                    (x + head xs :| tail xs)
                        : ((x <|) . unTranchesProportions <$> shrinks)

-- | Arbitrary TranchesProportions are well-formed
validTranchesProportions :: TranchesProportions -> Bool
validTranchesProportions = isRight . validateTranchesProportions

-- | All shrinked TranchesProportions are well-formed and smaller
validTranchesProportionsShrinks :: TranchesProportions -> Bool
validTranchesProportionsShrinks x =
    let shrinks = shrink x
        n = length (unTranchesProportions x)
    in
        length shrinks == n - 1
            && all (\y -> length (unTranchesProportions y) == n - 1) shrinks
            && all validTranchesProportions shrinks

-- | Partitionning the minimum allocation still yields valid parts
validMinAllocation :: Natural -> TranchesProportions -> Bool
validMinAllocation ε xs =
    all (>= ε) $
        splitAllocation xs (minAllocation ε xs)

-------------------------------------------------------------------------------
--
-- [Note: Shrinking a record]
--
-- The `a : shrink a` expression provides a way to build shrinks of a record
-- with at least one field shrinked, in an applicative way. Taking the tail
-- ensure to exclude the original record from the shrinks.
--
-- Next improvement step would be to find a way to shrink exactly one field.
--

instance Arbitrary (Restricted (NonEmpty TrancheProperties)) where
    arbitrary =
        Restricted <$>
            do
                proportions <- unTranchesProportions <$> arbitrary
                unlockTimes <- vectorOf (length proportions) arbitrary
                pure $ NEList.fromList . getZipList $
                    TrancheProperties
                        <$> ZipList (NEList.toList proportions)
                        <*> ZipList unlockTimes
    shrink (Restricted xs) =
        let proportions = TranchesProportions $ proportion <$> xs
            unlockTimes = unlockTime <$> xs

            shrinkedProportions = unTranchesProportions <$> shrink proportions
            noshrinkUnlockTimes = repeat unlockTimes

            shrinkedProperties =
                getZipList $
                    NEList.zipWith TrancheProperties
                        <$> ZipList shrinkedProportions
                        <*> ZipList noshrinkUnlockTimes
        in
            Restricted <$> shrinkedProperties

instance Arbitrary (Restricted PrivateSale) where
    arbitrary =
        do
            tranchesProperties <- getRestricted <$> arbitrary
            Restricted assetClass <- arbitrary
            allocationByAddress <- scale (*7) arbitrary

            let proportions = TranchesProportions $ proportion <$> tranchesProperties
                ε = fromRight' $ calculateDefaultMinimumUTxOFromAssetClass assetClass
                µ = minAllocation ε proportions

            pure $
                Restricted $ PrivateSale
                    tranchesProperties
                    assetClass
                    ((+ µ) <$> allocationByAddress)
    shrink (Restricted PrivateSale{..}) =
        let shrinkedProperties = getRestricted <$> shrink' (Restricted tranchesProperties)
            shrinkedAssetClass = getRestricted <$> shrink' (Restricted assetClass)
            shrinkedAllocationByAddress = shrink' allocationByAddress

            shrinkedPrivateSale =
                PrivateSale
                    <$> shrinkedProperties
                    <*> shrinkedAssetClass
                    <*> shrinkedAllocationByAddress
        in
            Restricted <$> filter validPrivateSaleAllocations (tail shrinkedPrivateSale)
        where
            shrink' :: Arbitrary a => a -> [a]
            shrink' a = a : shrink a

-- | Validate only allocations of a PrivateSale
validPrivateSaleAllocations :: PrivateSale -> Bool
validPrivateSaleAllocations PrivateSale{..} =
    let proportions = TranchesProportions $ proportion <$> tranchesProperties
        ε = fromRight' $ calculateDefaultMinimumUTxOFromAssetClass assetClass
    in
        isRight $ validateAllocations ε proportions $ NEMap.elems allocationByAddress

-- | Convert generated PaymentAddress to InvestorAddress
toInvestorAddress :: PaymentAddress -> InvestorAddress
toInvestorAddress = InvestorAddress . convert . unPaymentAddress

-- | Update a PrivateSale with valid generated testnet addresses
useValidAddresses ::
     ( MonadIO m )
    => PrivateSale -> m PrivateSale
useValidAddresses PrivateSale{..} =
    do
        let allocations = NEMap.elems allocationByAddress
        addresses <- toInvestorAddress <$$> generateAddresses "testnet" (indices allocations)
        pure PrivateSale{allocationByAddress=NEMap.fromList $ NEList.zip addresses allocations,..}
  where
    indices :: NonEmpty a -> NonEmpty Integer
    indices = NEList.fromList . (\n -> [0..n-1]) . toInteger . length

-- | Validate an arbitrary PrivateSale updated with valid addresses
validRestrictedPrivateSale :: Restricted PrivateSale -> Property
validRestrictedPrivateSale (Restricted privateSale) =
    monadicIO $
        useValidAddresses privateSale
            <&> isRight . validatePrivateSale

-- | Validate shrinks of an arbitrary PrivateSale updated with valid addresses
validRestrictedPrivateSaleShrinks :: Restricted PrivateSale -> Property
validRestrictedPrivateSaleShrinks (Restricted privateSale) =
    monadicIO $
        useValidAddresses privateSale
            <&> all (isRight . validatePrivateSale . getRestricted)
                    . take 30 . shrink . Restricted

-------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Vesting.GenerateNative" [ unitTests, properties ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testGroup "scaleRatios"
        [ testCase "empty list"      $ scaleRatios []                       ( 0 :: Integer) @?= []
        , testCase "scale == 0"      $ scaleRatios [1%10, 2%10, 3%10, 4%10] ( 0 :: Integer) @?= [0,  0,  0 , 0]
        , testCase "scale == 1"      $ scaleRatios [1%1 , 2%1 , 3%1 , 4%1 ] ( 1 :: Integer) @?= [1,  2,  3,  4]
        , testCase "sum ratios == 1" $ scaleRatios [1%10, 2%10, 3%10, 4%10] (98 :: Integer) @?= [9, 19, 29, 41]
        , testCase "sum ratios != 1" $ scaleRatios [1%10, 2%10, 3%10]       (98 :: Integer) @?= [9, 19, 30]
        ]
    ]

propertiesScaleRatios:: TestTree
propertiesScaleRatios =
    testGroup "scaleRatios"
        [ testProperty "scale == 0"
            ( \(as :: NonEmptyList Integer) ->
                    let bs = getNonEmpty as
                        xs = (% max 1 (sum bs)) <$> bs
                    in
                        all (==0) $ scaleRatios xs 0
            )
        , testProperty "scale == 1"
            ( \(as :: NonEmptyList Integer) ->
                    let bs = getNonEmpty as
                        xs = (% 1) <$> bs
                    in
                        scaleRatios xs 1 == bs
            )
        ]

propertiesArbitraryTranchesProportions :: TestTree
propertiesArbitraryTranchesProportions =
    testGroup "Arbitrary TranchesProportions"
        [ testProperty "valid arbitrary" validTranchesProportions
        , testProperty "valid shrinks" validTranchesProportionsShrinks
        , testProperty "valid minAllocation" validMinAllocation
        ]

propertiesArbitraryRestrictedPrivateSale :: TestTree
propertiesArbitraryRestrictedPrivateSale =
    testGroup "Arbitrary Restricted PrivateSale"
        [ testProperty "valid arbitrary" $
                withMaxSuccess 1 $ mapSize (const 7) validRestrictedPrivateSale
        , testProperty "valid shrinks" $
                withMaxSuccess 1 $ mapSize (const 7) validRestrictedPrivateSaleShrinks
        ]

propertiesSplitInTranches :: TestTree
propertiesSplitInTranches =
    testGroup "splitInTranches"
        [ testProperty "split length equals number of tranches"
            ( \(ps :: PrivateSale) ->
                    length (splitInTranches ps) == length (tranchesProperties ps)
            )
        , testProperty "valid tranches assetclass"
            ( \(ps :: PrivateSale) ->
                    all (==assetClass ps) (trancheAssetClass <$> splitInTranches ps)
            )
        , testProperty "valid tranches unlocktime"
            ( \(ps :: PrivateSale) ->
                    (unlockTime <$>  tranchesProperties ps)
                        == (trancheUnlockTime <$> splitInTranches ps)
            )
        , testProperty "valid tranches allocation sum"
            ( \(Restricted ps :: Restricted PrivateSale) ->
                    NEMap.toMap (allocationByAddress ps)
                        == unionsWith
                            (+)
                            (NEMap.toMap . trancheAllocationByAddress <$> splitInTranches ps)
            )
        ]

propertiesInvestorAddressPubKeyHash :: TestTree
propertiesInvestorAddressPubKeyHash =
    testGroup "investorAddressPubKeyHash"
        [ testProperty "valid tranches allocation sum" $ withMaxSuccess 1 $
                monadicIO $
                    all isRight <$>
                        do
                            addresses <- toInvestorAddress <$$> generateAddresses "testnet" [0..5]
                            traverse (runExceptT . investorAddressPubKeyHash) addresses
        ]

validTrancheNativeScriptUnlockTime :: PrivateSaleTranche -> NEMap InvestorAddress NativeScriptInfo -> Bool
validTrancheNativeScriptUnlockTime PrivateSaleTranche{..} xs =
    all
        (== toNextBeginPOSIXTime trancheUnlockTime)
        (requireTimeAfter . requiring <$> NEMap.elems xs)

validTrancheNativeScriptAddress :: PrivateSaleTranche -> NEMap InvestorAddress NativeScriptInfo -> Bool
validTrancheNativeScriptAddress PrivateSaleTranche{..} xs =
    NEMap.keys trancheAllocationByAddress == NEMap.keys xs

validTrancheNativeScriptAllocation :: PrivateSaleTranche -> NEMap InvestorAddress NativeScriptInfo -> Bool
validTrancheNativeScriptAllocation PrivateSaleTranche{..} xs =
    let allNativeScriptsAllocation =
            naturalFromInteger . amount . recipient <$> NEMap.elems xs
    in
        NEMap.elems trancheAllocationByAddress == allNativeScriptsAllocation

propertiesTrancheNativeScriptInfos :: TestTree
propertiesTrancheNativeScriptInfos =
    testGroup "trancheNativeScriptInfos"
        [ testProperty "valid trancheNativeScriptInfos" $
                withMaxSuccess 1 $ mapSize (const 7)
                    ( \(Restricted ps :: Restricted PrivateSale) ->
                            monadicIO $ do
                                env <- getTestnetEnvironmment 1097911063
                                validPrivateSale <- useValidAddresses ps
                                and
                                    <$> traverse
                                            (runValidTrancheNativeScriptInfos env)
                                            (splitInTranches validPrivateSale)
                    )
        ]
  where
    runValidTrancheNativeScriptInfos env tranche =
        runExceptT (runReaderT (trancheNativeScriptInfos tranche) env)
            <&> validTrancheNativeScriptInfos tranche
    validTrancheNativeScriptInfos tranche e =
        let xs = fromRight' e
        in
            isRight e
                && validTrancheNativeScriptUnlockTime tranche xs
                && validTrancheNativeScriptAddress tranche xs
                && validTrancheNativeScriptAllocation tranche xs

newtype MapToTranspose k v
    =   MapToTranspose
    { unMapToTranspose :: NEMap k (NonEmpty v) }
    deriving stock (Show)

newtype MapsToMerge k v
    =   MapsToMerge
    { unMapsToMerge :: NonEmpty (NEMap k v) }
    deriving stock (Show)

nonEmptyVectorOf :: Int -> Gen a -> Gen (NonEmpty a)
nonEmptyVectorOf n gen = (:|) <$> gen <*> vectorOf n gen

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (MapToTranspose k v) where
    arbitrary =
        sized $ \n ->
            do
                keys <- nonEmptyVectorOf n arbitrary
                vals <- nonEmptyVectorOf n (nonEmptyVectorOf n arbitrary)
                pure $ MapToTranspose $ NEMap.fromList $ NEList.zip keys vals

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (MapsToMerge k v) where
    arbitrary =
        sized $ \n ->
            do
                keys <- nonEmptyVectorOf n arbitrary
                MapsToMerge
                    <$> nonEmptyVectorOf n
                            (NEMap.fromList
                                <$> (NEList.zip keys
                                        <$> nonEmptyVectorOf n arbitrary))

propertiesMerge :: TestTree
propertiesMerge =
    testGroup "merge transposed squared map"
        [ testProperty "merge . transpose == id" $ noShrinking
                ( \((MapToTranspose xs) :: MapToTranspose Integer Integer) ->
                        let ys = transpose (NEList.toList <$> xs)
                        in
                            xs == merge (NEList.fromList ys)
                )
        , testProperty "transpose . merge == id" $ noShrinking
                ( \((MapsToMerge xs) :: MapsToMerge Integer Integer) ->
                        let ys = merge xs
                        in
                            xs == NEList.fromList (transpose (NEList.toList <$> ys))
                )
        ]

validDatabaseOutputLength :: NonEmpty PrivateSaleTranche -> DatabaseOutput -> Bool
validDatabaseOutputLength xs DatabaseOutput{..} =
    all
        (==length lockedFunds)
        (length . trancheAllocationByAddress <$> xs)

validDatabaseOutputScriptsCount :: NonEmpty PrivateSaleTranche -> DatabaseOutput -> Bool
validDatabaseOutputScriptsCount xs DatabaseOutput{..} =
    all
        (==length xs)
        (length <$> NEMap.elems lockedFunds)

propertiesToDatabaseOutput :: TestTree
propertiesToDatabaseOutput =
    testGroup "toDatabaseOutput"
        [ testProperty "valid DatabaseOutput" $ noShrinking $
                withMaxSuccess 1 $ mapSize (const 7)
                    ( \(Restricted ps :: Restricted PrivateSale) ->
                            monadicIO $ do
                                env <- getTestnetEnvironmment 1097911063
                                validPrivateSale <- useValidAddresses ps
                                let tranches = splitInTranches validPrivateSale
                                runToDatabaseOutput env tranches
                                    <&> validDatabaseOutput tranches
                    )
        ]
  where
    runToDatabaseOutput env tranches =
        runExceptT (runReaderT (toDatabaseOutput tranches) env)
    validDatabaseOutput tranches e =
        let xs = fromRight' e
        in
            isRight e
                && validDatabaseOutputLength tranches xs
                && validDatabaseOutputScriptsCount tranches xs

validDistributionLength :: NonEmpty PrivateSaleTranche -> Distribution -> Bool
validDistributionLength xs distribution =
        length (recipients distribution)
            == sum (length . trancheAllocationByAddress <$> xs)

propertiesToDistribution :: TestTree
propertiesToDistribution =
    testGroup "toDistribution"
        [ testProperty "valid Distribution" $ noShrinking $
                withMaxSuccess 1 $ mapSize (const 7)
                    ( \(Restricted ps :: Restricted PrivateSale) ->
                            monadicIO $ do
                                env <- getTestnetEnvironmment 1097911063
                                validPrivateSale <- useValidAddresses ps
                                let tranches = splitInTranches validPrivateSale
                                runToDistribution env tranches
                                    <&> validDistribution tranches
                    )
        ]
  where
    runToDistribution env tranches =
        runExceptT (runReaderT (toDistribution <$> toDatabaseOutput tranches) env)
    validDistribution tranches e =
        let xs = fromRight' e
        in
            isRight e
                && validDistributionLength tranches xs

properties :: TestTree
properties = testGroup "Properties"
    [ propertiesScaleRatios
    , propertiesArbitraryTranchesProportions
    , propertiesArbitraryRestrictedPrivateSale
    , propertiesSplitInTranches
    , propertiesInvestorAddressPubKeyHash
    , propertiesTrancheNativeScriptInfos
    , propertiesMerge
    , propertiesToDatabaseOutput
    , propertiesToDistribution
    --, propertiesToJSON
    ]

-- propertiesToJSON :: TestTree
-- propertiesToJSON =
--     testGroup "toJSON"
--         [ testProperty "toJSON" $ withMaxSuccess 1 $ mapSize (const 7)
--              ( \(Restricted ps :: Restricted PrivateSale) ->
--                     monadicIO $ do
--                         env <- getTestnetEnvironmment 1097911063
--                         networkId <- runReaderT getNetworkId env
--                         validPrivateSale <- useValidAddresses ps
--                         let privateSaleTranches = splitInTranches validPrivateSale
--                         databaseOutput <- runExceptT $ runReaderT (toDatabaseOutput privateSaleTranches) env
--                         liftIO $ encodeFile (replaceFileName "/tmp/" "database.json") (fromRight' databaseOutput `WithNetworkId` networkId)
--                         liftIO $ encodeFile (replaceFileName "/tmp/" "distribution.json") (toDistribution (fromRight' databaseOutput) `WithNetworkId` networkId)
--                         assert True
--             )
--         ]

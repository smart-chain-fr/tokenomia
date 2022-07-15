{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Tokenomia.Vesting.GenerateNative () where

import Control.Monad.Except (MonadError (throwError), liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader)
import Data.Aeson (eitherDecodeFileStrict)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Either (lefts)
import Data.Foldable (foldl')
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as List.NonEmpty
import qualified Data.Map.NonEmpty as Map.NonEmpty
import Numeric.Natural

import Ledger (Address, POSIXTime (POSIXTime))
import Ledger.Value (AssetClass)

import Tokenomia.Common.Environment (Environment)
import Tokenomia.Common.Error (TokenomiaError)

type Amount = Natural

data Tranche = Tranche
  { percentage :: Integer -- out of 10,000
  , duration :: Integer -- number of slots
  }
  deriving stock (Show)
$(deriveJSON defaultOptions ''Tranche)

-- Invariants
-- Σ percentages = 100%
-- Description : Represent Vesting Tranches (Time Sequential and contiguous)

newtype Tranches = Tranches (NonEmpty Tranche)
  deriving stock (Show)

$(deriveJSON defaultOptions ''Tranches)

data PrivateInvestor = PrivateInvestor
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

-- TODO : Change the signature to reflect the return of PrivateSale
parsePrivateSale ::
  forall (m :: Type -> Type).
  ( MonadIO m
  , MonadError TokenomiaError m
  , MonadReader Environment m
  ) =>
  String ->
  m ()
parsePrivateSale path = do
  eitherErrPriv <- liftIO . (eitherDecodeFileStrict @PrivateSale) $ path
  liftIO $ print eitherErrPriv

-- TODO : Fin the right error to use here or register a new one.
--  liftEither $ first (CustomError) eitherErrPriv
--  liftEither $ first (do
--    saleTranches <- tranches <$> eitherErrPriv
--    validateTranches saleTranches)
--  TODO : Sort Tranches by duration.

validateTranches :: Tranches -> Either String ()
validateTranches (Tranches tranchesNE) = do
  checkSum tranchesList
  case lefts (checkDuration <$> tranchesList) of
    [] -> Right ()
    (x : _) -> Left x
  where
    tranchesList :: [Tranche]
    tranchesList = List.NonEmpty.toList tranchesNE

    checkDuration :: Tranche -> Either String ()
    checkDuration tranche =
      if duration tranche >= 0
        then Right ()
        else Left $ "Duration must be positive in: " <> show tranche

    checkSum :: [Tranche] -> Either String ()
    checkSum input =
      let tranchesSum =
            foldl' (\acc x -> percentage x + acc) 0 input
       in if tranchesSum == 10000
            then Right ()
            else
              Left $
                "The sum of all the tranches must be 10000, but we got: "
                  <> show tranchesSum

mergeInverstors :: NonEmpty PrivateInvestor -> Map.NonEmpty.NEMap Address Amount
mergeInverstors = Map.NonEmpty.fromListWith (+) . (toTuple <$>)
  where
    toTuple :: PrivateInvestor -> (Address, Amount)
    toTuple (PrivateInvestor x y) = (x, y)

splitAmountInTranches ::
  POSIXTime -> Amount -> Tranches -> [(POSIXTime, Amount)] -> NonEmpty (POSIXTime, Amount)

-- TODO : Refactor and ask about the rounding behaviour

{- | We are taking the floor of the corresponding percentage in all items
 except in the last one where we do the corrections to sum the right amount.
-}
splitAmountInTranches startTime total trs acc =
  case (List.NonEmpty.uncons . unwrap) trs of
    (lastTranche, Nothing) -> pure ((slot2POSIX . duration) lastTranche + startTime, total - sumValues acc)
    (tipTranche, Just remainTranches) ->
      let takenAmount :: Amount
          takenAmount = div (total * percentage tipTranche) 10000
          newAcc :: [(POSIXTime, Amount)]
          newAcc = ((slot2POSIX . duration) tipTranche + startTime, takenAmount) : acc
       in splitAmountInTranches startTime total (Tranches remainTranches) newAcc
  where
    -- TODO : Find the right way to convert them
    slot2POSIX :: Integer -> POSIXTime
    slot2POSIX = error "uninplemented yet"

    unwrap :: Tranches -> NonEmpty Tranche
    unwrap (Tranches x) = x

    sumValues :: [(POSIXTime, Amount)] -> Amount
    sumValues = foldl' (\accu (_, x) -> accu + x) 0

--generateNativeScripts :: NonEmpty (POSIXTime, Amount) -> NonEmpty NativeScript
--generateNativeScripts = (generateScript <$>)
--  where
--    generateScript :: (POSIXTime, Amount) -> NativeScript
--    generateScript = error "uninplemented yet"

splitInTranches :: PrivateSale -> Map.NonEmpty.NEMap Address (NonEmpty (POSIXTime, Amount))
splitInTranches PrivateSale {..} = Map.NonEmpty.map f investorsMap
  where
    f :: Amount -> NonEmpty (POSIXTime, Amount)
    f x = splitAmountInTranches start x tranches []

    investorsMap :: Map.NonEmpty.NEMap Address Amount
    investorsMap = mergeInverstors investors

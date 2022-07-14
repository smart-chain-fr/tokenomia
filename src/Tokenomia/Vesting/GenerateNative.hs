{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Tokenomia.Vesting.GenerateNative () where

import Control.Monad.Except (MonadError (throwError), liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Either (lefts)
import Data.Foldable (foldl')
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as List.NonEmpty
import qualified Data.Map.NonEmpty as Map.NonEmpty
import Numeric.Natural

import Ledger (Address, POSIXTime)
import Ledger.Value (AssetClass)

import Tokenomia.Common.Environment (Environment)
import Tokenomia.Common.Error (TokenomiaError (BlockFrostError))

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

$(deriveJSON defaultOptions ''Tranches)

data PrivateInvestor = PrivateInvestor
  { address :: Address
  , allocation :: Amount
  }

$(deriveJSON defaultOptions ''PrivateInvestor)

data PrivateSale = PrivateSale
  { start :: POSIXTime
  , tranches :: Tranches
  , assetClass :: AssetClass
  , investors :: NonEmpty PrivateInvestor
  }

$(deriveJSON defaultOptions ''PrivateSale)

--parsePrivateSale ::
--  forall (m :: Type -> Type).
--  ( MonadIO m
--  , MonadError TokenomiaError m
--  , MonadReader Environment m
--  ) =>
--  String ->
--  m ()
--parsePrivateSale path = do
--  eitherErrPriv <- liftIO . eitherDecodeFileStrict $ path
--  liftEither $ first (CustomError) eitherErrPriv

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

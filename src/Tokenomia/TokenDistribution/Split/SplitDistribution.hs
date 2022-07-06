{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Tokenomia.TokenDistribution.Split.SplitDistribution (
  splitDistribution,
) where

import Data.List.NonEmpty (NonEmpty, fromList)
import Data.List.Split (chunksOf)

import Tokenomia.TokenDistribution.CLI.Parameters (Parameters (..))
import Tokenomia.TokenDistribution.Distribution (Distribution (..))

splitDistribution :: Parameters -> Distribution -> NonEmpty Distribution
splitDistribution Parameters {..} Distribution {..} =
  fromList $ Distribution assetClass <$> chunksOf recipientPerTx recipients

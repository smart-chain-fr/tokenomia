{-# LANGUAGE LambdaCase #-}

module Tokenomia.Common.Shell.InteractiveMenu
    (askSelect) where

import qualified Data.List.NonEmpty
import Data.List.NonEmpty (NonEmpty, (!!))
import Prelude hiding ((!!))
import Text.Read (readEither)
import Shh.Internal (ExecArg(asArg),
      captureWords,
      load,
      (|>),
      ExecReference(SearchPath), capture)

load SearchPath ["echo", "clear"]

askSelectRepeatedly :: Show a => NonEmpty a -> IO Int
askSelectRepeatedly choices = do
    mapM_ echo choices
    getLine >>= (
        \case
            Left err -> do
                clear
                putStrLn err ++ ": Wrong parse input. Please try again."
                askSelectRepeatedly choices
            Right ioIdx -> if ioIdx > length choices || ioIdx < 0 then do
                putStrLn ioIdx ++ "Number selected is incorrect"
                askSelectRepeatedly choices else return ioIdx) . readEither


askSelect :: Show a => NonEmpty a -> IO a
askSelect choices = (\idx -> choices !! (idx - 1)) <$> askSelectRepeatedly choices
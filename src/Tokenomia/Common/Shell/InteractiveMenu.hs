{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Tokenomia.Common.Shell.InteractiveMenu (
  askString,
  ask,
  askFilterM,
  askStringFilterM,
  askMenu,
  askLeaveBlankOption,
  askStringLeaveBlankOption,
  DisplayMenuItem (..),
) where

import Control.Monad.Reader hiding (ask)
import Data.List.NonEmpty (NonEmpty, toList, (!!))
import Shh
import Text.Read (readEither)
import Tokenomia.Common.Shell.Console (clearConsole, print, printLn)
import Prelude hiding (print, (!!))

load SearchPath ["echo"]

zipIndex :: DisplayMenuItem a => Int -> [a] -> [(Int, a)]
zipIndex _ [] = []
zipIndex i (x : xs) = (i, x) : zipIndex (i + 1) xs

echoChoices :: (DisplayMenuItem a, MonadIO m) => (Int, a) -> m ()
echoChoices (i, x) = printLn $ "\t" <> show i <> "-" <> displayMenuItem x

askSelectRepeatedly ::
  ( MonadIO m
  , DisplayMenuItem a
  ) =>
  NonEmpty a ->
  m Int
askSelectRepeatedly choices = do
  let orderedChoices = zipIndex 1 (toList choices)
  mapM_ (liftIO . echoChoices) orderedChoices
  print "\n> please choose an action (provide the index) : "
  liftIO getLine
    >>= ( \case
            Left err -> do
              clearConsole
              printLn $ show err ++ ": Wrong parse input. Please try again."
              askSelectRepeatedly choices
            Right ioIdx ->
              if ioIdx > length choices || ioIdx <= 0
                then do
                  clearConsole
                  printLn $ show ioIdx ++ ": Number selected is incorrect"
                  askSelectRepeatedly choices
                else return ioIdx
        )
      . readEither

askMenu ::
  ( MonadIO m
  , DisplayMenuItem a
  ) =>
  NonEmpty a ->
  m a
askMenu choices = (\idx -> choices !! (idx - 1)) <$> askSelectRepeatedly choices
class DisplayMenuItem a where
  displayMenuItem :: a -> String

askString :: (MonadIO m) => String -> m String
askString prompt = do
  print prompt
  liftIO getLine

ask :: (Read a, MonadIO m) => String -> m a
ask prompt = do
  print prompt
  liftIO (readEither <$> getLine)
    >>= \case
      Left err -> do
        print $ show err
        ask prompt
      Right answer -> return answer

askStringFilterM :: (MonadIO m) => String -> (String -> m Bool) -> m String
askStringFilterM prompt f = do
  answer <- askString prompt
  f answer
    >>= \case
      True -> return answer
      False -> askStringFilterM prompt f

askFilterM :: (Read a, MonadIO m) => String -> (a -> m Bool) -> m a
askFilterM prompt f = do
  answer <- ask prompt
  f answer
    >>= \case
      True -> return answer
      False -> askFilterM prompt f

askStringLeaveBlankOption :: (MonadIO m) => String -> m (Maybe String)
askStringLeaveBlankOption prompt = do
  print prompt
  liftIO getLine
    >>= \case
      [] -> return Nothing
      answer -> return (Just answer)

askLeaveBlankOption :: (Read a, MonadIO m) => String -> m (Maybe a)
askLeaveBlankOption prompt = do
  print prompt
  liftIO (readEither <$> getLine)
    >>= \case
      Left _ -> do
        return Nothing
      Right answer -> return (Just answer)

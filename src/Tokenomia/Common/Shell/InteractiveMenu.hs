{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}


module Tokenomia.Common.Shell.InteractiveMenu
    ( ask
    , ask'
    , askMenu
    , askMaybe
    , DisplayMenuItem (..)) where

import Data.List.NonEmpty (NonEmpty, toList, (!!))
import Prelude hiding ((!!), print)
import Text.Read (readEither)
import Shh
import Control.Monad.Reader hiding (ask)
import Tokenomia.Common.Shell.Console (print, printLn, clearConsole)

load SearchPath ["echo"]

zipIndex :: DisplayMenuItem a => Int -> [a] -> [(Int, a)]
zipIndex _ [] = []
zipIndex i (x: xs) = (i, x) : zipIndex (i + 1) xs

echoChoices :: DisplayMenuItem a =>  (Int, a) -> Cmd 
echoChoices (i, x) = echo "    " (show i) "-" (displayMenuItem x) 

askSelectRepeatedly 
    :: ( MonadIO m 
       , DisplayMenuItem a) 
    => NonEmpty a 
    -> m Int
askSelectRepeatedly choices = do
    let orderedChoices = zipIndex 1 (toList choices)
    mapM_ (liftIO . echoChoices) orderedChoices
    printLn "\n> please choose an action (provide the index) : "
    liftIO getLine >>= (
        \case
            Left err -> do
                clearConsole
                printLn $ show err ++ ": Wrong parse input. Please try again."
                askSelectRepeatedly choices
            Right ioIdx -> if ioIdx > length choices || ioIdx <= 0 then do
                clearConsole
                printLn $ show ioIdx ++ ": Number selected is incorrect"
                askSelectRepeatedly choices else return ioIdx) . readEither

askMenu 
    :: ( MonadIO m 
       , DisplayMenuItem a) 
    => NonEmpty a 
    -> m a
askMenu choices = (\idx -> choices !! (idx - 1)) <$> askSelectRepeatedly choices
class DisplayMenuItem a where
    displayMenuItem :: a -> String

    
ask' :: (Read a, MonadIO m) => String -> m a
ask' prompt = do
    print prompt
    liftIO (readEither <$> getLine) >>=
        \case 
            Left err -> do
                print $ show err
                ask' prompt
            Right answer -> return answer

ask :: (Read a, MonadIO m) => String -> (a -> m Bool) -> m a
ask prompt f = do
    answer <- ask' prompt
    f answer >>=
        \case
            True -> return answer
            False -> ask prompt f

askMaybe :: (Read a, MonadIO m) => String -> m (Maybe a)
askMaybe prompt = do
    print prompt
    liftIO (readEither <$> getLine) >>=
        \case 
            Left _ -> do
               return Nothing 
            Right answer -> return (Just answer)
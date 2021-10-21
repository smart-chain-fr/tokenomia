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
    ( askMenu
    , DisplayMenuItem (..)) where

import Data.List.NonEmpty (NonEmpty, toList, (!!))
import Prelude hiding ((!!))
import Text.Read (readEither)
import Shh
import Control.Monad.Reader 

load SearchPath ["echo", "clear", "printf"]

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
    liftIO $ printf "\n> please choose an action (provide the index) : "
    liftIO getLine >>= (
        \case
            Left err -> do
                liftIO clear
                liftIO $ putStrLn  $ show err ++ ": Wrong parse input. Please try again."
                askSelectRepeatedly choices
            Right ioIdx -> if ioIdx > length choices || ioIdx <= 0 then do
                liftIO clear
                liftIO $ putStrLn $ show ioIdx ++ ": Number selected is incorrect"
                askSelectRepeatedly choices else return ioIdx) . readEither


askMenu 
    :: ( MonadIO m 
       , DisplayMenuItem a) 
    => NonEmpty a 
    -> m a
askMenu choices = (\idx -> choices !! (idx - 1)) <$> askSelectRepeatedly choices

class DisplayMenuItem a where
    displayMenuItem :: a -> String 


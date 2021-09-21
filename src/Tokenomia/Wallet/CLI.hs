{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tokenomia.Wallet.CLI (selectWallet) where

import Shh 
import Data.Function ((&))
import Tokenomia.Adapter.Cardano.CardanoCLI as CardanoCLI
import Data.List.NonEmpty as NonEmpty ( NonEmpty )
import Byline.Menu
    ( runBylineT,
      text,
      askWithMenuRepeatedly,
      menu,
      menuSuffix,
      Stylized,
      ToStylizedText(..),
      Menu )
import Data.Text ( Text,pack )
import Data.Maybe ( fromJust )
import Byline.Internal.Stylized ()



load SearchPath ["echo","ssh","cat"]

selectWallet :: IO (Maybe Wallet)
selectWallet = do
    query_registered_wallets
    >>= \case 
        Nothing -> return Nothing 
        Just wallets -> do    
            Just <$> showMenu wallets
            
    




showMenu :: NonEmpty Wallet -> IO Wallet
showMenu wallets = fmap fromJust (runBylineT $ askWithMenuRepeatedly (menuConfig wallets) prompt onError)

menuConfig :: NonEmpty Wallet -> Menu Wallet
menuConfig wallets = menu wallets & menuSuffix "- "

prompt :: Stylized Text
prompt = text "> please choose a wallet (provide the index) : "

onError :: Stylized Text
onError = text "> invalid index provided ! "



instance ToStylizedText Wallet where
  toStylizedText Wallet {..} = text . pack $ name
    

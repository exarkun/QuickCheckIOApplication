{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.QuickCheck
-- import Test.Hspec.Wai.QuickCheck
import Test.Hspec
import Test.Hspec.Wai
import Text.Printf
import Servant
import Servant.API
import Data.Aeson

data Backend = Backend

openBackend :: IO Backend
openBackend = return Backend

data Acknowledgement = Ok Integer

instance ToJSON Acknowledgement where
  toJSON (Ok n) = object [ "value" .= n ]

serveSomeNumber :: Backend -> Integer -> Acknowledgement
serveSomeNumber backend number = Ok number

type TheAPI = Capture "SomeNumber" Integer :> Post '[JSON] Acknowledgement

theServer :: Backend -> Server TheAPI
theServer backend = return . (serveSomeNumber backend)

theAPI :: Proxy TheAPI
theAPI = Proxy

app :: Backend -> Application
app backend = serve theAPI (theServer backend)

prop :: Property
prop = with (app Backend) $ property $ return True

-- property $
--   post (printf "/%d" (5 :: Integer)) `shouldRespondWith` 200

main :: IO ()
main = quickCheck prop

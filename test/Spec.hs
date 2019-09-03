{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Test.Hspec.Wai.QuickCheck as QuickWai
import Test.Hspec
import Test.Hspec.Wai
import Text.Printf
import Servant
import Servant.API
import Data.Aeson
import Data.Text.Encoding
import Data.ByteString.UTF8
  ( fromString
  )

data Backend = Backend

openBackend :: IO Backend
openBackend = do
  print "Opening backend"
  return Backend

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

prop_a :: Property
prop_a = property $ \n -> (n :: Integer) /= 123456

prop_b :: Integer -> Property
prop_b n = monadicIO $
  do
    assert $ n /= 123456

prop_c :: Backend -> Integer -> Property
prop_c backend n = monadicIO $
  do
    assert $ n /= 123456

post' n =
  let
    url = printf "/%d" (n :: Integer)
    encoded = fromString url
  in
    post encoded ""

spec_a :: Spec
spec_a =
  with (return (app Backend)) $
  describe "foo" $
  it "bar" $
  post' 5 `shouldRespondWith` 200

spec_b :: Backend -> Spec
spec_b backend =
  with (return (app backend)) $
  describe "foo" $
  it "bar" $
  post' 5 `shouldRespondWith` 200

spec_c :: Backend -> Spec
spec_c backend =
  with (return (app backend)) $
  describe "foo" $
  it "bar" $
  QuickWai.property $ \n -> post' n `shouldRespondWith` 200

spec_d :: IO Backend -> Spec
spec_d getBackend =
  with (getBackend >>= return . app) $
  describe "foo" $
  it "bar" $
  QuickWai.property $ \n -> post' n `shouldRespondWith` 200

spec_e :: IO Backend -> Spec
spec_e getBackend =
  before (getBackend >>= return . app) $
  describe "foo" $
  it "bar" $
  QuickWai.property $ \n -> post' n `shouldRespondWith` 200

spec_f :: IO Backend -> Spec
spec_f getBackend = do
  backend <- runIO getBackend
  before (return $ app backend) $
    describe "foo" $
    it "bar" $
    QuickWai.property $ \n -> post' n `shouldRespondWith` 200

spec_g :: IO Backend -> Spec
spec_g getBackend =
  describe "foo" $
  it "bar" $ property $ \genN -> monadicIO $ do
  backend <- run getBackend
  n <- run genN
  post' n `shouldRespondWith` 200

main :: IO ()
main = do
  quickCheck prop_a
  quickCheck prop_b
  backend <- openBackend
  quickCheck $ prop_c backend

  hspec spec_a
  hspec $ spec_b backend
  hspec $ spec_c backend
  hspec $ spec_d openBackend
  hspec $ spec_e openBackend
  hspec $ spec_f openBackend
  hspec $ spec_g openBackend

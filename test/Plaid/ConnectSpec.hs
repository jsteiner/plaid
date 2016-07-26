module Plaid.ConnectSpec where

import TestImport

import Plaid.Connect

spec :: Spec
spec = do
  describe "connect" $ do
    it "decodes the response" $ do
        result  <- connect validPlaidCreds validBankCreds
        result `shouldSatisfy` isRight

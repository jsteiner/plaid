module TestImport
    ( module X
    , validPlaidCreds
    , validBankCreds
    , isRight
    ) where

import Test.Hspec as X
import Data.Either (isRight)

import Plaid.Data

validPlaidCreds :: PlaidCreds
validPlaidCreds = PlaidCreds "test_id" "test_secret"

validBankCreds :: BankCreds
validBankCreds = BankCreds "plaid_test" "plaid_good" "wells"

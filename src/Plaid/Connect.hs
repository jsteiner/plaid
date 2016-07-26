module Plaid.Connect
    ( connect
    ) where

import Data.Monoid ((<>))
import Control.Lens ((^.))
import qualified Data.Aeson as A
import qualified Network.Wreq as W

import Plaid.Data

connect :: PlaidCreds -> BankCreds -> IO (Either String ConnectResponse)
connect pc bc = do
    r <- W.postWith W.defaults connectUrl $ A.toJSON creds
    let json = r ^. W.responseBody
    pure $ A.eitherDecode json
  where
    creds = Creds pc bc

connectUrl :: String
connectUrl = baseUrl <> "/connect"

baseUrl :: String
baseUrl = "https://tartan.plaid.com"

module Plaid.Data.Creds
    ( PlaidCreds(..)
    , BankCreds(..)
    , Creds(..)
    ) where

import Data.Aeson
import qualified Data.Text as T

data PlaidCreds = PlaidCreds
    { pcClientId :: T.Text
    , pcSecret :: T.Text
    }

data BankCreds = BankCreds
    { bcUsername :: T.Text
    , bcPassword :: T.Text
    , bcType :: T.Text
    }

data Creds = Creds PlaidCreds BankCreds

instance ToJSON Creds where
    toJSON (Creds pc bc) = object
        [ "client_id" .= pcClientId pc
        , "secret" .= pcSecret pc
        , "username" .= bcUsername bc
        , "password" .= bcPassword bc
        , "type" .= bcType bc
        ]

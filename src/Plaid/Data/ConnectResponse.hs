module Plaid.Data.ConnectResponse
    ( Account(..)
    , Transaction(..)
    , ConnectResponse(..)
    , Location(..)
    , Coordinates(..)
    ) where

import Control.Monad (mzero)
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Aeson
import Data.Aeson.Types

data ConnectResponse = ConnectResponse
    { crAccounts :: [Account]
    , crTransactions :: [Transaction]
    , crAccessToken :: Maybe T.Text
    }
    deriving (Eq, Show)

instance FromJSON ConnectResponse where
    parseJSON = withObject "connect_response" $ \cr ->
        ConnectResponse <$> cr .: "accounts"
                        <*> cr .: "transactions"
                        <*> cr .:? "access_token"

data AccountType
    = Depository DepositoryType
    | Credit CreditType
    | Loan
    | Mortgage
    | Brokerage
    | Other
    deriving (Eq, Show)

data DepositoryType
    = Checking
    | Savings
    | Prepaid
    | DepositoryTypeUnknown
    deriving (Eq, Show)

data CreditType
    = Credit'
    | CreditCard
    | LineOfCredit
    | CreditTypeUnknown
    deriving (Eq, Show)

data Account = Account
    { aAccountId :: T.Text
    , aItemId :: T.Text
    , aUserId :: T.Text
    , aType :: AccountType
    , aCurrentBalance :: Float
    , aAvailableBalance :: Maybe Float
    , aLimit :: Maybe Float
    , aName :: Maybe T.Text
    , aNumber :: Maybe T.Text
    }
    deriving (Eq, Show)

instance FromJSON Account where
    parseJSON = withObject "account" $ \a -> do
        balance <- a .: "balance"
        meta <- a .: "meta"

        Account <$> a .: "_id"
                <*> a .: "_item"
                <*> a .: "_user"
                <*> parseAccountType a
                <*> balance .: "current"
                <*> balance .:? "available"
                <*> meta .:? "limit"
                <*> meta .:? "name"
                <*> meta .:? "number"

parseAccountType :: Object -> Parser AccountType
parseAccountType o = do
    String type' <- o .: "type"
    subtype <- parseAccountSubType <$> o .:? "subtype"

    case (type', subtype) of
      ("depository", Just "checking") -> return $ Depository Checking
      ("depository", Just "savings") -> return $ Depository Savings
      ("depository", Just "prepaid") -> return $ Depository Prepaid
      ("depository", _) -> return $ Depository DepositoryTypeUnknown
      ("credit", Just "credit") -> return $ Credit Credit'
      ("credit", Just "credit card") -> return $ Credit CreditCard
      ("credit", Just "line of credit") -> return $ Credit LineOfCredit
      ("credit", _) -> return $ Credit CreditTypeUnknown
      (_, _) -> return Other

parseAccountSubType :: Maybe Value -> Maybe T.Text
parseAccountSubType (Just (String t)) = Just t
parseAccountSubType _ = Nothing

data Coordinates = Coordinates
    { cLat :: Float
    , cLon :: Float
    }
    deriving (Eq, Show)

instance FromJSON Coordinates where
    parseJSON = withObject "coordinates" $ \t ->
        Coordinates <$> t .: "lat"
                    <*> t .: "lon"

data Location = Location
    { lAddress :: Maybe T.Text
    , lCity :: Maybe T.Text
    , lState :: Maybe T.Text
    , lZip :: Maybe T.Text
    , lCoordinates :: Maybe Coordinates
    }
    deriving (Eq, Show)

instance FromJSON Location where
    parseJSON = withObject "transaction" $ \t ->
        Location <$> t .:? "address"
                 <*> t .:? "city"
                 <*> t .:? "state"
                 <*> t .:? "zip"
                 <*> t .:? "coordinates"

data Transaction = Transaction
    { tTransactionId :: T.Text
    , tAmount :: Float
    , tName :: T.Text
    , tPending :: Bool
    , tCategory :: [T.Text]
    , tDate :: Time.Day
    , tLocation :: Location
    }
    deriving (Eq, Show)

instance FromJSON Transaction where
    parseJSON = withObject "transaction" $ \t -> do
        meta <- t .: "meta"

        Transaction <$> t .: "_id"
                    <*> t .: "amount"
                    <*> t .: "name"
                    <*> t .: "pending"
                    <*> (M.fromMaybe [] <$> t .:? "category")
                    <*> (parseDate =<< (t .: "date"))
                    <*> meta .: "location"

parseDate :: T.Text -> Parser Time.Day
parseDate =
    maybe mzero (return . Time.utctDay) . maybeParseTime . T.unpack
  where
    maybeParseTime = Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d"

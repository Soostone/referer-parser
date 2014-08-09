{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Web.RefererParser.Types where

--------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Data.Aeson          (withText)
import           Data.ByteString     (ByteString)
import           Data.Hashable       (Hashable)
import           Data.Text           (Text)
import           Data.Typeable
import           Data.Yaml
import           GHC.Generics        (Generic)
import           URI.ByteString
--------------------------------------------------


-- | Classification of providers
data Medium = Unknown
            | Email
            | Social
            | Search
            | Display deriving (Show, Eq, Generic)

instance Hashable Medium

mediumBS :: Medium -> ByteString
mediumBS Unknown = "unknown"
mediumBS Email = "email"
mediumBS Social = "social"
mediumBS Search = "search"
mediumBS Display = "display"

-- | Whether this was a paid or organic click
data ClickType = Organic
               | Paid deriving (Show, Eq, Generic)


clickTypeBS :: ClickType -> ByteString
clickTypeBS Organic = "organic"
clickTypeBS Paid = "paid"

-- | Service provider for the referred traffic
newtype Provider = Provider { getProvider :: Text }
    deriving (Show, Eq, Hashable, FromJSON)


-- | User search term parameter if applicable
newtype SearchTerm = SearchTerm { getSearchTerm :: Text }
    deriving (Show, Eq)


-- | Domain of the referer. Note this is not always a bare host.
-- Sometimes, it can include a path like www.google.com/products
newtype Domain = Domain { getDomain :: Text }
    deriving (Show, Eq, Hashable, FromJSON, Typeable)


data RefererMeta = RefererMeta
    { rmMedium     :: Medium
    , rmProvider   :: Provider
    , rmSearchTerm :: Maybe SearchTerm
    , rmClickType  :: Maybe ClickType
    } deriving (Show, Eq)


data Referer = Referer
    { refRawText  :: ByteString
    , refURI      :: URI
    , refParams   :: [(ByteString, ByteString)]
    , refMeta     :: Maybe RefererMeta
    , refChildren :: [(ByteString, Referer)]
    } deriving (Show, Eq)


instance FromJSON Medium where
  parseJSON = withText "Medium" parseMedium
    where
      parseMedium "unknown" = pure Unknown
      parseMedium "email"   = pure Email
      parseMedium "social"  = pure Social
      parseMedium "search"  = pure Search
      parseMedium "display" = pure Display
      parseMedium _         = mzero

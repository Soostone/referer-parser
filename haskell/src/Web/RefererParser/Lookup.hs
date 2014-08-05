{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

module Web.RefererParser.Lookup ( lookupReferer ) where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Data.Aeson              (withArray, withObject)
import           Data.Bifunctor
import           Data.FileEmbed          (embedFile)
import qualified Data.Foldable           as F
import           Data.Hashable           (Hashable)
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HM
import           Data.List               (foldl')
import           Data.List.NonEmpty      (NonEmpty (..))
import qualified Data.List.NonEmpty      as NE
import           Data.Text               (Text, pack)
import           Data.Typeable
import           Data.Yaml
import           Text.URI
-------------------------------------------------------------------------------
import           Web.RefererParser.Types
-------------------------------------------------------------------------------



---- Intermediate representation of the referers file
newtype RefererFile = RefererFile (HashMap Medium ProviderLibrary)


instance FromJSON RefererFile where
  parseJSON v = RefererFile <$> parseRemapped "RefererFile" v


newtype ProviderLibrary = ProviderLibrary (HashMap Provider ProviderDetails)


instance FromJSON ProviderLibrary where
  parseJSON v = ProviderLibrary <$> parseRemapped "ProviderLibrary" v


newtype ParameterName = ParameterName Text deriving (Show, Eq, FromJSON)


data ProviderDetails = ProviderDetails (NonEmpty Domain) [ParameterName]


instance FromJSON ProviderDetails where
  parseJSON = withObject "ProviderDetails" parseProviderDetails
    where
      parseProviderDetails o = ProviderDetails
        <$> (unNonEmptyJSON <$> o .: "domains")
        <*> o .:? "parameters" .!= []


newtype NonEmptyJSON a = NonEmptyJSON { unNonEmptyJSON :: NonEmpty a}


instance (Typeable a, FromJSON a) => FromJSON (NonEmptyJSON a) where
  parseJSON = withArray (showType witness) $ parseNonEmpty <=< mapM parseJSON . F.toList
    where
      witness = undefined :: a
      parseNonEmpty xs = case F.toList xs of
                           x:xs' -> pure . NonEmptyJSON $ x :| xs'
                           _    -> fail "empty list"



---- Final representation of the referers file
type RefererMap = HashMap Domain (Medium, Provider, [ParameterName])


refererMap :: RefererMap
refererMap = either error buildMap $ decodeEither $(embedFile "data/referers.yml")
{-# NOINLINE refererMap #-}


---- Public API
lookupReferer :: URI -> Maybe Referer
lookupReferer u = buildReferer <$> (doLookup =<< expandDomains u)
  where
    doLookup = lookupFirst (`HM.lookup` refererMap)
    buildReferer (domain, (m, p, pns)) = Referer m p domain (lookupTerm u pns)


---- Parsing helpers
parseRemapped :: (FromJSON k, Hashable k, Eq k, FromJSON v) => String -> Value -> Parser (HashMap k v)
parseRemapped ty = withObject ty parseObj
  where
    parseObj o = foldM remap HM.empty $ HM.toList o
    remap acc (k, v) = HM.insert <$> parseJSON (String k) <*> parseJSON v <*> pure acc


buildMap :: RefererFile -> RefererMap
buildMap (RefererFile m) = HM.foldrWithKey go HM.empty m
  where
    go med (ProviderLibrary pl) acc = HM.foldrWithKey (go' med) acc pl
    go' med prov (ProviderDetails ds pns) = insertMany (NE.toList ds) (med, prov, pns)


insertMany :: (Eq k, Hashable k) => [k] -> v -> HashMap k v -> HashMap k v
insertMany ks v h = foldl' (\h' k -> HM.insert k v h') h ks


expandDomains :: URI -> Maybe [Domain]
expandDomains u = do
  host <- uriRegName u
   -- prefer expanded url
  return [packDomain $ host ++ uriPath u, packDomain host]


lookupTerm :: URI -> [ParameterName] -> Maybe Term
lookupTerm u pns = do
  params <- uriParams u
  Term <$> lookupFirstMatch params
  where
    lookupFirstMatch params = msum . map (`lookup` params) $ pns


uriParams :: URI -> Maybe [(ParameterName, Text)]
uriParams u = map packParam . queryToPairs <$> uriQuery u
  where
    packParam = bimap (ParameterName . pack) pack


showType :: Typeable a => a -> String
showType = show . typeOf


lookupFirst :: (Show a, Show b) => (a -> Maybe b) -> [a] -> Maybe (a, b)
lookupFirst finder = msum . map attemptLookup
  where attemptLookup x = (x, ) <$> finder x


packDomain :: String -> Domain
packDomain = Domain . pack

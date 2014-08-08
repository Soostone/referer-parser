{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

module Web.RefererParser.Lookup
  ( mkReferer
  ) where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Data.Aeson              (withArray, withObject)
import           Data.Bifunctor
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as B
import           Data.FileEmbed          (embedFile)
import qualified Data.Foldable           as F
import           Data.Hashable           (Hashable)
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HM
import           Data.List               (foldl')
import           Data.List.NonEmpty      (NonEmpty (..))
import qualified Data.List.NonEmpty      as NE
import           Data.Map                (Map)
import qualified Data.Map                as M
import           Data.Monoid
import           Data.Text               (Text)
import           Data.Text.Encoding
import           Data.Typeable
import           Data.Yaml
import           Snap.Core
import           URI.ByteString
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


------------------------------------------------------------------------------
-- | Public API
mkReferer :: ByteString -> Maybe Referer
mkReferer bs = do
    u <- hush $ parseURI bs
    let params = parseUrlEncoded (uriQuery u)
    let buildReferer (domain, (m, p, pns)) =
          RefererMeta m p domain (lookupTerm (uriParams params) pns) Nothing
        meta = buildReferer <$> (doLookup $ expandDomains u)
        mkChild (k,v) = (k,) <$> mkReferer (B.concat v)
        cs = catMaybes $ map mkChild $ ("", [uriQuery u]) : M.toList params
    return $ Referer bs u (map (second B.concat) $ M.toList params) meta cs
  where
    doLookup = lookupFirst (`HM.lookup` refererMap)


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


expandDomains :: URI -> [Domain]
expandDomains u =
    [packDomain $ host <> uriPath u, packDomain host]
  where
    host = uriAuthority u
   -- prefer expanded url


lookupTerm :: [(ParameterName, Text)] -> [ParameterName] -> Maybe Term
lookupTerm params pns =
    Term <$> lookupFirstMatch params
  where
    lookupFirstMatch ps = msum . map (`lookup` ps) $ pns


uriParams :: Map ByteString [ByteString] -> [(ParameterName, Text)]
uriParams params = map packParam $ M.toList params
  where
    packParam = bimap (ParameterName . decodeUtf8) (decodeUtf8 . B.concat)


showType :: Typeable a => a -> String
showType = show . typeOf


lookupFirst :: (Show a, Show b) => (a -> Maybe b) -> [a] -> Maybe (a, b)
lookupFirst finder = msum . map attemptLookup
  where attemptLookup x = (x, ) <$> finder x


packDomain :: ByteString -> Domain
packDomain = Domain . decodeUtf8

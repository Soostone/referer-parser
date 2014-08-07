{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Error
import           Control.Monad
import           Test.Tasty
import           Test.Tasty.HUnit

import           URI.ByteString

import           Web.RefererParser.Lookup
import           Web.RefererParser.Types

main :: IO ()
main = defaultMain $ testGroup "referer-parser" [lookupTests]

lookupTests = testGroup "Web.RefererParser.Lookup" [lookupRefererTests]

------------------------------------------------------------------------------
lookupRefererTests = testGroup "lookupReferer" [
    testCase "looks up google search" $
      -- Added / before the ? to make the tests pass with the regex parser
      lookupByURI "http://www.google.com/?q=foo" @?=
        Just (Referer Search (Provider "Google") (Domain "www.google.com")
             (Just . Term $ "foo") Nothing)

  , testCase "prefers more specific host matches" $
      lookupByURI "http://www.google.com/products?q=foo" @?=
        Just (Referer Search (Provider "Google Product Search")
                      (Domain "www.google.com/products")
                      (Just . Term $ "foo") Nothing)

  , testCase "fails to parse unknown providers" $
      lookupByURI "http://www.example.com" @?=
        Nothing

  , testCase "omits parameters not in the parameters list" $
      -- Added / before the ? to make the tests pass with the regex parser
      lookupByURI "http://www.google.com/?unrelated=yup" @?=
        Just (Referer Search (Provider "Google") (Domain "www.google.com")
                      Nothing Nothing)
                                               ]


lookupByURI = lookupReferer <=< hush . parseURI

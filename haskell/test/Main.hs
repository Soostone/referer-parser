{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad
import Test.Tasty
import Test.Tasty.HUnit

import Text.URI

import Web.RefererParser.Types
import Web.RefererParser.Lookup

main :: IO ()
main = defaultMain $ testGroup "referer-parser" [lookupTests]

lookupTests = testGroup "Web.RefererParser.Lookup" [lookupRefererTests]

lookupRefererTests = testGroup "lookupReferer" [
    testCase "looks up google search" $
      lookupByURI "http://www.google.com?q=foo" @?=
        Just (Referer Search (Provider "Google") (Domain "www.google.com") (Just . Term $ "foo"))

  , testCase "prefers more specific host matches" $
      lookupByURI "http://www.google.com/products?q=foo" @?=
        Just (Referer Search (Provider "Google Product Search") (Domain "www.google.com/products") (Just . Term $ "foo"))

  , testCase "fails to parse unknown providers" $
      lookupByURI "http://www.example.com" @?=
        Nothing

  , testCase "omits parameters not in the parameters list" $
      lookupByURI "http://www.google.com?unrelated=yup" @?=
        Just (Referer Search (Provider "Google") (Domain "www.google.com") Nothing)
                                               ]


lookupByURI = lookupReferer <=< parseURI

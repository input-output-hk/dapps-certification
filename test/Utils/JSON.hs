{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Utils.JSON where

import Data.Data
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Debug.Trace
import Data.ByteString.Lazy.Char8 hiding (replicate,length)

traceError :: Show a => [Char] -> a -> [Char] -> [Char] -> Bool
traceError propS x err info =
  trace ("ERROR "++ propS ++"\n\t" ++ show x ++ ":\n\t" ++ err ++ "\n" ++ info ++ "\n\n\n") False

testJSON :: (Data a,Show a,Arbitrary a,ToJSON a,FromJSON a,Eq a)=> a -> SpecWith ()
testJSON = testJSONWithEq (==)

testJSONWithEq :: (Data a,Show a,Arbitrary a,ToJSON a,FromJSON a)=> EqF a -> a -> SpecWith ()
testJSONWithEq eq a = prop (fmt a) (jsonT eq a)

testJSON' :: (Show a,Arbitrary a,ToJSON a,FromJSON a,Eq a)=> String -> a -> SpecWith ()
testJSON' = testJSONWithEq' (==)

testJSONWithEq' :: (Show a,Arbitrary a,ToJSON a,FromJSON a)=> EqF a -> String -> a -> SpecWith ()
testJSONWithEq' eq title a = prop (padL 30 title) (jsonT eq a)

fmt :: Data a => a -> String
fmt x = padL 30 (dataTypeName $ dataTypeOf x)

padL :: Int -> String -> String
padL n s
    | length s < n  = s ++ replicate (n - length s) ' '
    | otherwise     = s

type EqF a = a -> a -> Bool

traceEq :: (Show a) => EqF a -> a -> a -> Bool
traceEq eq x y = (x `eq` y) || trace (show x ++ "\n\n" ++ show y) False

jsonT :: (Show a,Arbitrary a,ToJSON a,FromJSON a) => EqF a -> a -> a -> Bool
jsonT eq _ x =
    let bs = encodePretty x
        str = unpack bs
        yE = eitherDecode bs
    in case yE of
           Right y -> traceEq eq x y
           Left err ->
            traceError "jsonT" x err str



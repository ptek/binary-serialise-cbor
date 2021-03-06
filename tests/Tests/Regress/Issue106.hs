module Tests.Regress.Issue106 ( testTree ) where

import qualified Data.Binary.Serialise.CBOR as CBOR
import qualified Data.Binary.Serialise.CBOR.Pretty as CBOR
import           Test.Tasty
import           Test.Tasty.HUnit

repro :: String
repro =
    CBOR.prettyHexEnc $ CBOR.encode (5 :: Word)

testTree :: TestTree
testTree =
    testGroup "Issue 106 - Pretty-printing of Word"
        [ testCase "simple reproduction case"   ("\n05  # word(5)" @=? repro) ]

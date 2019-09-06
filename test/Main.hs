{-# language BangPatterns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language OverloadedStrings #-}
{-# language NumDecimals #-}

import Control.Monad (when)
import Data.Bool (bool)
import Data.Bytes.Types (Bytes(Bytes))
import Data.Char (ord)
import Data.Primitive (ByteArray)
import Data.Scientific (large,small,toWord8)
import Data.Word (Word8)
import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.Tasty.HUnit ((@=?),assertFailure)
import Test.Tasty.QuickCheck (testProperty,(===))
import Data.Fixed (Fixed,E12)

import qualified Data.Scientific as SCI
import qualified Data.Bytes.Parser as P
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts
import qualified Test.Tasty.HUnit as THU
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "Eq"
    [ THU.testCase "A" $ small 300 (-2) @=? small 3 0
    , THU.testCase "B" $ small 300 (-2) @=? large 3e50 (-50)
    , THU.testCase "C" $ large 3e100 (-99) @=? small 30 0
    , THU.testCase "D" $ large 3e5 9999999995 @=? large 3e6 9999999994
    , THU.testCase "E" $ when
        (small 400 maxBound == small 4 (minBound + 1))
        (assertFailure "")
    , THU.testCase "F" $ small 0 (-2) @=? small 0 5
    , THU.testCase "G" $ large 0 (-2) @=? large 0 5
    , testProperty "small" $ \x y ->
        small x y === small x y
    ]
  , testGroup "Word8"
    [ THU.testCase "A" $ Just 30 @=? toWord8 (small 300 (-1))
    , THU.testCase "B" $ Nothing @=? toWord8 (small 300 0)
    , THU.testCase "C" $ Nothing @=? toWord8 (small 1 999999999)
    , THU.testCase "D" $ Just 255 @=? toWord8 (large 255e40 (-40))
    , THU.testCase "E" $ Just 0 @=? toWord8 (large 0 10e30)
    , THU.testCase "F" $ Just 0 @=? toWord8 (small 0 999999999)
    , THU.testCase "G" $ Nothing @=? toWord8 (small (-1) 1)
    ]
  , testGroup "Parser"
    [ testGroup "UTF-8-unsigned"
      [ testProperty "small-integer" $ \i ->
          P.Success (small i 0) 0
          ===
          P.parseBytes (SCI.parserSignedUtf8Bytes ())
            (bytes (show i))
      , testProperty "small-exp" $ \i j b ->
          P.Success (small i j) 0
          ===
          P.parseBytes (SCI.parserSignedUtf8Bytes ())
            (bytes (show i ++ bool "e" "E" b ++ show j))
      , testProperty "fixed-e12-no-exp" $ \(i :: Fixed E12) ->
          QC.counterexample (show i)
          $
          P.Success (SCI.fromFixed i) 0
          ===
          P.parseBytes (SCI.parserSignedUtf8Bytes ())
            (bytes (show i))
      ]
    ]
  -- , testGroup "Word16"
  --   [ THU.testCase "A" $ Just 30 @=? toWord16 (small 300 (-1))
  --   , THU.testCase "B" $ Just 300 @=? toWord16 (small 300 0)
  --   , THU.testCase "C" $ Nothing @=? toWord16 (small 1 999999999)
  --   , THU.testCase "D" $ Just 255 @=? toWord16 (large 255e40 (-40))
  --   , THU.testCase "E" $ Just 0 @=? toWord16 (large 0 10e30)
  --   , THU.testCase "F" $ Just 0 @=? toWord16 (small 0 999999999)
  --   , THU.testCase "G" $ Nothing @=? toWord16 (small (-1) 1)
  --   , THU.testCase "H" $ Just 65535 @=? toWord16 (large 65535e100 (-100))
  --   ]
  ]

bytes :: String -> Bytes
bytes s = let b = pack ('x' : s) in Bytes b 1 (PM.sizeofByteArray b - 1)

pack :: String -> ByteArray
pack = Exts.fromList . map (fromIntegral @Int @Word8 . ord)


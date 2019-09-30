{-# language BangPatterns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language OverloadedStrings #-}
{-# language NumDecimals #-}

import Control.Monad (when,replicateM)
import Data.Bool (bool)
import Data.Bytes.Types (Bytes(Bytes))
import Data.Char (ord)
import Data.Fixed (Fixed,E12)
import Data.Int (Int64)
import Data.Number.Scientific (large,small,toWord8,toWord16,toWord32,toWord64)
import Data.Number.Scientific (toInt64,toInt32)
import Data.Primitive (ByteArray)
import Data.Word (Word8)
import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.Tasty.HUnit ((@=?),assertFailure)
import Test.Tasty.QuickCheck (testProperty,(===))

import qualified Data.Bits as Bits
import qualified Data.Number.Scientific as SCI
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
  , testGroup "Word16"
    [ THU.testCase "A" $ Just 30 @=? toWord16 (small 300 (-1))
    , THU.testCase "B" $ Just 300 @=? toWord16 (small 300 0)
    , THU.testCase "C" $ Nothing @=? toWord16 (small 1 999999999)
    , THU.testCase "D" $ Just 65535 @=? toWord16 (large 65535e40 (-40))
    , THU.testCase "E" $ Just 0 @=? toWord16 (large 0 10e30)
    , THU.testCase "F" $ Just 0 @=? toWord16 (small 0 999999999)
    , THU.testCase "G" $ Nothing @=? toWord16 (small (-1) 1)
    , THU.testCase "H" $ Nothing @=? toWord16 (small 65536 0)
    ]
  , testGroup "Word32"
    [ THU.testCase "A" $ Just 30 @=? toWord32 (small 300 (-1))
    , THU.testCase "B" $ Just 300 @=? toWord32 (small 300 0)
    , THU.testCase "C" $ Nothing @=? toWord32 (small 1 999999999)
    , THU.testCase "D" $ Just 65535 @=? toWord32 (large 65535e40 (-40))
    , THU.testCase "E" $ Just 0 @=? toWord32 (large 0 10e30)
    , THU.testCase "F" $ Just 0 @=? toWord32 (small 0 999999999)
    , THU.testCase "G" $ Nothing @=? toWord32 (small (-1) 1)
    , THU.testCase "H" $ Nothing @=? toWord32 (small 4294967296 0)
    , THU.testCase "I" $ Just 4294967295 @=? toWord32 (large 4294967295e40 (-40))
    , THU.testCase "J" $ Just 4294967295 @=? toWord32 (small 4294967295 0)
    ]
  , testGroup "Word64"
    [ THU.testCase "A" $ Just 30 @=? toWord64 (small 300 (-1))
    , THU.testCase "B" $ Just 300 @=? toWord64 (small 300 0)
    , THU.testCase "C" $ Nothing @=? toWord64 (small 1 999999999)
    , THU.testCase "D" $ Just 65535 @=? toWord64 (large 65535e40 (-40))
    , THU.testCase "E" $ Just 0 @=? toWord64 (large 0 10e30)
    , THU.testCase "F" $ Just 0 @=? toWord64 (small 0 999999999)
    , THU.testCase "G" $ Nothing @=? toWord64 (small (-1) 1)
    , THU.testCase "H" $ Just 4294967296 @=? toWord64 (small 4294967296 0)
    , THU.testCase "I" $ Just 4294967295 @=? toWord64 (large 4294967295e40 (-40))
    , THU.testCase "J" $ Just 4294967295 @=? toWord64 (small 4294967295 0)
    , THU.testCase "K" $ Nothing @=? toWord64 (large (2 ^ (64 :: Int)) 0)
    , THU.testCase "L" $ Just maxBound @=? toWord64 (large ((2 ^ (64 :: Int)) - 1) 0)
    , THU.testCase "M" $ Just (fromIntegral (maxBound :: Int)) @=? toWord64 (small (maxBound :: Int) 0)
    ]
  , testGroup "Int32"
    [ THU.testCase "A" $ Just 30 @=? toInt32 (small 300 (-1))
    , THU.testCase "B" $ Just 300 @=? toInt32 (small 300 0)
    , THU.testCase "C" $ Nothing @=? toInt32 (small 1 999999999)
    , THU.testCase "D" $ Just 65535 @=? toInt32 (large 65535e40 (-40))
    , THU.testCase "E" $ Just 0 @=? toInt32 (large 0 10e30)
    , THU.testCase "F" $ Just 0 @=? toInt32 (small 0 999999999)
    , THU.testCase "G" $ Just (-10) @=? toInt32 (small (-1) 1)
    , THU.testCase "H" $ Just 2147483647 @=? toInt32 (small 2147483647 0)
    , THU.testCase "I" $ Nothing @=? toInt32 (large 4294967295e40 (-40))
    , THU.testCase "J" $ Just (-2147483640)  @=? toInt32 (small (-214748364) 1)
    , THU.testCase "K" $ Just 2147483640 @=? toInt32 (small 214748364 1)
    , THU.testCase "L" $ Nothing @=? toInt32 (small 214748365 1)
    ]
  , testGroup "Int64"
    [ THU.testCase "A" $ Just 30 @=? toInt64 (small 300 (-1))
    , THU.testCase "B" $ Just 300 @=? toInt64 (small 300 0)
    , THU.testCase "C" $ Nothing @=? toInt64 (small 1 999999999)
    , THU.testCase "D" $ Just 65535 @=? toInt64 (large 65535e40 (-40))
    , THU.testCase "E" $ Just 0 @=? toInt64 (large 0 10e30)
    , THU.testCase "F" $ Just 0 @=? toInt64 (small 0 999999999)
    , THU.testCase "G" $ Just (-10) @=? toInt64 (small (-1) 1)
    , THU.testCase "H" $ Just 4294967296 @=? toInt64 (small 4294967296 0)
    , THU.testCase "I" $ Just 4294967295 @=? toInt64 (large 4294967295e40 (-40))
    , THU.testCase "J" $ Just 4294967295 @=? toInt64 (small 4294967295 0)
    , THU.testCase "K" $ Nothing @=? toInt64 (large (2 ^ (64 :: Int)) 0)
    , THU.testCase "L" $ Just maxBound @=? toInt64 (large ((2 ^ (63 :: Int)) - 1) 0)
    , THU.testCase "M" $ Just (fromIntegral (maxBound :: Int)) @=? toInt64 (small (maxBound :: Int) 0)
    , THU.testCase "N" $ Just (fromIntegral (minBound :: Int)) @=? toInt64 (small (minBound :: Int) 0)
    , THU.testCase "O" $ Nothing @=? toInt64 (large (negate (2 ^ (63 :: Int)) - 1) 0)
    , THU.testCase "P" $ Just (minBound :: Int64) @=? toInt64 (large (negate (2 ^ (63 :: Int))) 0)
    , THU.testCase "Q" $ Just 9.2e18 @=? toInt64 (small 92 17)
    , THU.testCase "R" $ Just 9.3e17 @=? toInt64 (small 93 16)
    , THU.testCase "S" $ Nothing @=? toInt64 (small 93 17)
    , THU.testCase "T" $ Nothing @=? toInt64 (large 93 17)
    , THU.testCase "U" $ Just (-9.3e17) @=? toInt64 (small (-93) 16)
    , THU.testCase "V" $ Nothing @=? toInt64 (large 922337203685477581 1)
    ]
  , testGroup "Parser"
    [ testGroup "UTF-8-signed"
      [ testProperty "small-integer" $ \i ->
          let str = show i in
          P.Success (P.Slice (length str + 1) 0 (small i 0))
          ===
          P.parseBytes (SCI.parserSignedUtf8Bytes ()) (bytes str)
      , testProperty "small-exp" $ \i j b ->
          let str = show i ++ bool "e" "E" b ++ show j in
          P.Success (P.Slice (length str + 1) 0 (small i j))
          ===
          P.parseBytes (SCI.parserSignedUtf8Bytes ()) (bytes str)
      , testProperty "fixed-e12-no-exp" $ \(i :: Fixed E12) ->
          let str = show i in
          QC.counterexample str
          $
          P.Success (P.Slice (length str + 1) 0 (SCI.fromFixed i))
          ===
          P.parseBytes (SCI.parserSignedUtf8Bytes ()) (bytes str)
      , testProperty "large-integer" $ \(LargeInteger i) (LargeInteger j) ->
          let str = show (large i j) in
          QC.counterexample str
          $
          P.Success (P.Slice (length str + 1) 0 (large i j))
          ===
          P.parseBytes (SCI.parserSignedUtf8Bytes ()) (bytes str)
      ]
    ]
  ]

bytes :: String -> Bytes
bytes s = let b = pack ('x' : s) in Bytes b 1 (PM.sizeofByteArray b - 1)

pack :: String -> ByteArray
pack = Exts.fromList . map (fromIntegral @Int @Word8 . ord)

-- The Arbitrary instance for Integer that comes with
-- QuickCheck only generates small numbers.
newtype LargeInteger = LargeInteger Integer
  deriving (Eq,Show)

instance QC.Arbitrary LargeInteger where
  arbitrary = do
      n <- QC.choose (1, 17)
      sign <- QC.arbitrary
      r <- (if sign then negate else id) . foldr f 0
        <$> replicateM n QC.arbitrary
      pure (LargeInteger r)
    where
      f :: Word8 -> Integer -> Integer
      f w acc = (acc `Bits.shiftL` 8) + fromIntegral w
  shrink (LargeInteger x)
    | x > 3 =
        [ LargeInteger (div x 2)
        , LargeInteger (div x 3)
        ]
    | otherwise = []


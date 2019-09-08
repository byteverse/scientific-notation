{-# language BangPatterns #-}
{-# language PackageImports #-}
{-# language MagicHash #-}
{-# language ScopedTypeVariables #-}

import Gauge (bgroup,bench,whnf)
import Gauge.Main (defaultMain)
import Data.ByteString.Internal (ByteString(PS))
import Data.Primitive (SmallArray,PrimArray,ByteArray(..))
import Data.Word (Word16)
import Control.Monad.ST (runST)
import Control.Monad.ST.Run (runPrimArrayST)
import GHC.ForeignPtr (ForeignPtrContents(PlainPtr))
import GHC.ForeignPtr (ForeignPtr(ForeignPtr))

import qualified GHC.Exts as Exts
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Parser as P
import qualified Data.Primitive as PM
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.Aeson.Parser as Aeson

import qualified "scientific" Data.Scientific as SlowSci
import qualified "scientific-notation" Data.Number.Scientific as SCI

main :: IO ()
main = defaultMain
  [ bgroup "scientific-notation"
    [ bgroup "parser"
      [ bench "ten-small"
          (whnf (\b -> P.parseByteArray decodeTen b) tenSmall)
      ]
    , bgroup "conversion"
      [ bench "twenty-word16"
        (whnf (\b -> convertArray16 b) twentyFastSci)
      ]
    ]
  , bgroup "scientific"
    [ bench "ten-small"
      (whnf (\b -> Atto.parseOnly (aesonDecodeN 10 []) (fromPinned b)) tenSmall)
    , bgroup "conversion"
      [ bench "twenty-word16"
        (whnf (\b -> convertSlowArray16 b) twentySlowSci)
      ]
    ]
  ]

-- TODO: In the test suite, we should confirm that parsing this
-- actually succeeds. We intentionally avoid leading plus signs
-- here so that we can compare against aeson.
tenSmall :: ByteArray
tenSmall = pin $ Bytes.toByteArray $ Bytes.fromAsciiString $ concat
  [ ",4256"
  , ",-125e14"
  , ",5.000006"
  , ",1e100"
  , ",-13.25E-100"
  , ",-653467618"
  , ",-17e+6"
  , ",9999.001"
  , ",0000.002"
  , ",0000.002E1"
  ]

-- All of these can fit inside a Word16.
twentyPairs :: SmallArray (Int,Int)
twentyPairs = Exts.fromList
  [ (2336,0)
  , (43265,0)
  , (17,0)
  , (24,3)
  , (1,4)
  , (25,0)
  , (0,0)
  , (1900,0)
  , (65,0)
  , (1100,0)
  , (5,3)
  , (0,0)
  , (1600,0)
  , (1500,0)
  , (2000,0)
  , (62,2)
  , (500,0)
  , (670,0)
  , (1100,0)
  , (65500,0)
  ]

twentyFastSci :: SmallArray SCI.Scientific
twentyFastSci = fmap (uncurry SCI.small) twentyPairs

twentySlowSci :: SmallArray SlowSci.Scientific
twentySlowSci = fmap
  (\(x,y) -> SlowSci.scientific (fromIntegral x) y)
  twentyPairs

aesonDecodeN :: Int -> [SlowSci.Scientific] -> Atto.Parser [SlowSci.Scientific]
aesonDecodeN !ix !acc = if ix > 0
  then do
    _ <- Atto.char ','
    !num <- Aeson.scientific
    aesonDecodeN (ix - 1) (num : acc)
  else pure acc

decodeTen :: P.Parser () s (SmallArray SCI.Scientific)
decodeTen = do
  arr <- P.effect (PM.newSmallArray 10 errorThunk)
  let go !ix = if ix >= 0
        then do
          P.ascii () ',' 
          !num <- SCI.parserSignedUtf8Bytes ()
          P.effect (PM.writeSmallArray arr ix num)
          go (ix - 1)
        else P.effect (PM.unsafeFreezeSmallArray arr)
  go 9

convertArray16 ::
     SmallArray SCI.Scientific
  -> PrimArray Word16
convertArray16 xs = runPrimArrayST $ do
  let len = PM.sizeofSmallArray xs
  ws <- PM.newPrimArray len
  let go !ix = if ix >= 0
        then case SCI.toWord16 (PM.indexSmallArray xs ix) of
          Nothing -> error "convertArray16: bad number"
          Just (r :: Word16) -> do
            PM.writePrimArray ws ix r
            go (ix - 1)
        else PM.unsafeFreezePrimArray ws
  go (len - 1)

convertSlowArray16 ::
     SmallArray SlowSci.Scientific
  -> PrimArray Word16
convertSlowArray16 xs = runPrimArrayST $ do
  let len = PM.sizeofSmallArray xs
  ws <- PM.newPrimArray len
  let go !ix = if ix >= 0
        then case SlowSci.toBoundedInteger (PM.indexSmallArray xs ix) of
          Nothing -> error "convertArray16: bad number"
          Just (r :: Word16) -> do
            PM.writePrimArray ws ix r
            go (ix - 1)
        else PM.unsafeFreezePrimArray ws
  go (len - 1)

errorThunk :: a
{-# noinline errorThunk #-}
errorThunk = error "scientific:benchmark error"

-- Convert a pinned immutable byte array to a bytestring.
fromPinned :: ByteArray -> ByteString
{-# inline fromPinned #-}
fromPinned (ByteArray arr# ) = PS
  (ForeignPtr (Exts.byteArrayContents# arr# ) (PlainPtr (Exts.unsafeCoerce# arr#)))
  0 (Exts.I# (Exts.sizeofByteArray# arr# ))

pin :: ByteArray -> ByteArray
pin src = runST $ do
  let len = PM.sizeofByteArray src
  dst <- PM.newByteArray len
  PM.copyByteArray dst 0 src 0 len
  PM.unsafeFreezeByteArray dst

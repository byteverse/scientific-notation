{-# language BangPatterns #-}
{-# language PackageImports #-}
{-# language MagicHash #-}

import Gauge (bgroup,bench,whnf)
import Gauge.Main (defaultMain)
import Data.ByteString.Internal (ByteString(PS))
import Data.Primitive (SmallArray,ByteArray(..))
import Control.Monad.ST (runST)
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
  [ bgroup "parsing"
    [ bench "ten-small" (whnf (\b -> P.parseByteArray decodeTen b) tenSmall)
    ]
  , bgroup "aeson"
    [ bench "ten-small"
      (whnf (\b -> Atto.parseOnly (aesonDecodeN 10 []) (fromPinned b)) tenSmall)
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


{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language TypeApplications #-}
{-# language MultiWayIf #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Data.Number.Scientific
  ( Scientific
  , Scientific#
    -- * Produce
  , small
  , large
  , fromFixed
    -- * Consume
  , toWord
  , toWord8
  , toWord16
  , toWord32
  , toWord64
  , toInt
  , toInt32
  , toInt64
  , withExposed
    -- * Compare
  , greaterThanInt64
    -- * Decode
  , parserSignedUtf8Bytes
  , parserTrailingUtf8Bytes
  , parserUnsignedUtf8Bytes
  , parserNegatedUtf8Bytes
  , parserNegatedTrailingUtf8Bytes
  , parserSignedUtf8Bytes#
  , parserTrailingUtf8Bytes#
  , parserUnsignedUtf8Bytes#
  , parserNegatedUtf8Bytes#
  , parserNegatedTrailingUtf8Bytes#
    -- * Encode
  , builderUtf8
  ) where

import Prelude hiding (negate)

import GHC.Exts (Int#,Word#,Int(I#),(+#))
import GHC.Word (Word(W#),Word8(W8#),Word16(W16#),Word32(W32#),Word64(W64#))
import GHC.Int (Int64(I64#),Int32(I32#))
import Data.Bytes.Builder (Builder)
import Data.Bytes.Parser.Unsafe (Parser(..))
import Data.Fixed (Fixed(MkFixed),HasResolution)

import qualified Arithmetic.Nat as Nat
import qualified Data.Fixed as Fixed
import qualified Data.Bytes.Builder as Builder
import qualified Data.Bytes.Builder.Bounded as BB
import qualified Data.Bytes.Parser as Parser
import qualified Data.Bytes.Parser.Latin as Latin
import qualified Data.Bytes.Parser.Unsafe as Unsafe
import qualified GHC.Exts as Exts
import qualified Prelude as Prelude

-- Implementation Notes
--
-- When consuming a Scientific, we are always careful to avoid
-- forcing the LargeScientific. In situations involving small
-- numbers, this field is not used, so we do not want to waste time
-- evaluating it.

data Scientific = Scientific
  {-# UNPACK #-} !Int -- coefficient
  {-# UNPACK #-} !Int -- base-10 exponent, minBound means use unlimited-precision field
  LargeScientific

type Scientific# = (# Int#, Int#, LargeScientific #)

instance Show Scientific where
  showsPrec _ (Scientific coeff e largeNum) = if e /= minBound
    then showsPrec 0 coeff . showChar 'e' . showsPrec 0 e
    else case largeNum of
      LargeScientific coeffLarge eLarge ->
        showsPrec 0 coeffLarge . showChar 'e' . showsPrec 0 eLarge

instance Eq Scientific where
  Scientific coeffA eA largeA == Scientific coeffB eB largeB
    | eA == minBound && eB == minBound = eqLargeScientific largeA largeB
    | eA == minBound = eqLargeScientific largeA (LargeScientific (fromIntegral coeffB) (fromIntegral eB))
    | eB == minBound = eqLargeScientific (LargeScientific (fromIntegral coeffA) (fromIntegral eA)) largeB
    | eA >= maxBound - padding || eB >= maxBound - padding = eqLargeScientific
        (LargeScientific (fromIntegral coeffA) (fromIntegral eA))
        (LargeScientific (fromIntegral coeffA) (fromIntegral eB))
    | otherwise = eqSmall coeffA eA coeffB eB

data LargeScientific = LargeScientific
  !Integer -- coefficent
  !Integer -- exponent

-- Padding just needs to be any number larger than the number of decimal
-- digits that could represent a 64-bit integer. Normalization of scientific
-- numbers using the small representation is only sound when we know that we
-- are not going to trigger an overflow.
padding :: Int
padding = 50

eqSmall :: Int -> Int -> Int -> Int -> Bool
eqSmall cA0 eA0 cB0 eB0 =
  let (cA,eA) = smallNormalize cA0 eA0
      (cB,eB) = smallNormalize cB0 eB0
   in cA == cB && eA == eB

eqLargeScientific :: LargeScientific -> LargeScientific -> Bool
eqLargeScientific a b =
  let LargeScientific cA eA = largeNormalize a 
      LargeScientific cB eB = largeNormalize b
   in cA == cB && eA == eB

zeroLarge :: LargeScientific
{-# noinline zeroLarge #-}
zeroLarge = LargeScientific 0 0

-- | Construct a 'Scientific' from a coefficient and exponent
-- that fit in a machine word.
small ::
     Int -- ^ Coefficient
  -> Int -- ^ Exponent
  -> Scientific
small !coeff !e = if e /= minBound
  then Scientific coeff e zeroLarge
  else large (fromIntegral coeff) (fromIntegral e)

-- | Construct a 'Scientific' from a coefficient and exponent
-- of arbitrary size.
large ::
     Integer -- ^ Coefficient
  -> Integer -- ^ Exponent
  -> Scientific
large coeff e =
  let !b = LargeScientific coeff e
   in Scientific 0 minBound b

-- | Construct a 'Scientific' from a fixed-precision number.
-- This does not perform well and is only included for convenience.
fromFixed :: HasResolution e => Fixed e -> Scientific
fromFixed n@(MkFixed coeff) =
  let !b = LargeScientific coeff
        (fromIntegral (Prelude.negate (logBase10 0 (Fixed.resolution n))))
   in Scientific 0 minBound b

toWord8 :: Scientific -> Maybe Word8
{-# inline toWord8 #-}
toWord8 (Scientific (I# coeff) (I# e) largeNum) = case toWord8# coeff e largeNum of
  (# (# #) | #) -> Nothing
  (# | w #) -> Just (W8# w)

toWord16 :: Scientific -> Maybe Word16
{-# inline toWord16 #-}
toWord16 (Scientific (I# coeff) (I# e) largeNum) = case toWord16# coeff e largeNum of
  (# (# #) | #) -> Nothing
  (# | w #) -> Just (W16# w)

toWord32 :: Scientific -> Maybe Word32
{-# inline toWord32 #-}
toWord32 (Scientific (I# coeff) (I# e) largeNum) = case toWord32# coeff e largeNum of
  (# (# #) | #) -> Nothing
  (# | w #) -> Just (W32# w)

toInt32 :: Scientific -> Maybe Int32
{-# inline toInt32 #-}
toInt32 (Scientific (I# coeff) (I# e) largeNum) = case toInt32# coeff e largeNum of
  (# (# #) | #) -> Nothing
  (# | w #) -> Just (I32# w)

toWord64 :: Scientific -> Maybe Word64
{-# inline toWord64 #-}
toWord64 (Scientific (I# coeff) (I# e) largeNum) = case toWord# coeff e largeNum of
  (# (# #) | #) -> Nothing
  (# | w #) -> Just (W64# w)

toWord :: Scientific -> Maybe Word
{-# inline toWord #-}
toWord (Scientific (I# coeff) (I# e) largeNum) = case toWord# coeff e largeNum of
  (# (# #) | #) -> Nothing
  (# | w #) -> Just (W# w)

toInt :: Scientific -> Maybe Int
{-# inline toInt #-}
toInt (Scientific (I# coeff) (I# e) largeNum) = case toInt# coeff e largeNum of
  (# (# #) | #) -> Nothing
  (# | i #) -> Just (I# i)

toInt64 :: Scientific -> Maybe Int64
{-# inline toInt64 #-}
toInt64 (Scientific (I# coeff) (I# e) largeNum) = case toInt# coeff e largeNum of
  (# (# #) | #) -> Nothing
  (# | i #) -> Just (I64# i)

-- | Is the number represented in scientific notation greater than the
-- 64-bit integer argument?
greaterThanInt64 :: Scientific -> Int64 -> Bool
greaterThanInt64 (Scientific coeff0@(I# coeff0# ) e0 large0) tgt@(I64# tgt# )
  | e0 == minBound = largeGreaterThanInt64 large0 tgt
  | coeff0 == 0 = 0 > tgt
  | e0 == 0 = I64# coeff0# > tgt
  | coeff0 > 0 =
      if | tgt <= 0 -> True
         | e0 > 0 -> case smallToInt coeff0 e0 of
             (# (# #) | #) -> True
             (# | i# #) -> I64# i# > tgt
           -- In last case, e0 is less than zero.
         | otherwise -> case posIntExp10 (I# tgt#) (Prelude.negate e0) of
             (# (# #) | #) -> False
             (# | i# #) -> I64# coeff0# > I64# i#
  | otherwise = -- Coefficent is negative
      if | tgt >= 0 -> False
         | e0 > 0 -> case smallToInt coeff0 e0 of
             (# (# #) | #) -> False
             (# | i# #) -> I64# i# > tgt
           -- In last case, e0 is less than zero.
         | otherwise -> case negIntExp10 (I# tgt#) (Prelude.negate e0) of
             (# (# #) | #) -> True
             (# | i# #) -> I64# coeff0# > I64# i#

largeGreaterThanInt64 :: LargeScientific -> Int64 -> Bool
largeGreaterThanInt64 large0@(LargeScientific coeff e) !tgt
  | coeff == 0 = 0 > tgt
  | e == 0 = coeff > fromIntegral @Int64 @Integer tgt
  | coeff > 0 =
      if | tgt <= 0 -> True
         | e > 0 -> case largeToInt large0 of
             (# (# #) | #) -> True
             (# | i# #) -> I64# i# > tgt
         | otherwise -> case posSciLowerBound False coeff e of
             Exactly n -> n > fromIntegral @Int64 @Integer tgt
             LowerBoundedMagnitude n -> (n+1) > fromIntegral @Int64 @Integer tgt
  | otherwise = -- Coefficent is negative
      if | tgt >= 0 -> False
         | e > 0 -> case largeToInt large0 of
             (# (# #) | #) -> False
             (# | i# #) -> I64# i# > tgt
         | otherwise -> case posSciLowerBound False coeff e of
             Exactly n -> n > fromIntegral @Int64 @Integer tgt
             LowerBoundedMagnitude n -> n > fromIntegral @Int64 @Integer tgt
             
-- | Expose the non-normalized exponent and coefficient.
withExposed ::
     (Int -> Int -> a)
     -- ^ Called when coefficient and exponent are small
  -> (Integer -> Integer -> a)
     -- ^ Called when coefficient and exponent are large
  -> Scientific
  -> a
withExposed f g (Scientific coeff theExp big) = if theExp /= minBound
  then f coeff theExp
  else case big of
    LargeScientific coeff' theExp' -> g coeff' theExp'

toSmallHelper ::
     (Int -> Int -> (# (# #) | Word# #) ) -- small
  -> (LargeScientific -> (# (# #) | Word# #) ) -- large
  -> Int#
  -> Int#
  -> LargeScientific
  -> (# (# #) | Word# #)
{-# inline toSmallHelper #-}
toSmallHelper fromSmall fromLarge coefficient0# exponent0# large0 =
  if exponent0 /= minBound
    then fromSmall coefficient0 exponent0
    else fromLarge large0
  where
  coefficient0 = I# coefficient0#
  exponent0 = I# exponent0#

toSmallIntHelper ::
     (Int -> Int -> (# (# #) | Int# #) ) -- small
  -> (LargeScientific -> (# (# #) | Int# #) ) -- large
  -> Int#
  -> Int#
  -> LargeScientific
  -> (# (# #) | Int# #)
{-# inline toSmallIntHelper #-}
toSmallIntHelper fromSmall fromLarge coefficient0# exponent0# large0 =
  if exponent0 /= minBound
    then fromSmall coefficient0 exponent0
    else fromLarge large0
  where
  coefficient0 = I# coefficient0#
  exponent0 = I# exponent0#


toWord8# :: Int# -> Int# -> LargeScientific -> (# (# #) | Word# #)
{-# noinline toWord8# #-}
toWord8# coefficient0# exponent0# large0 = 
  toSmallHelper smallToWord8 largeToWord8
    coefficient0# exponent0# large0

toWord16# :: Int# -> Int# -> LargeScientific -> (# (# #) | Word# #)
{-# noinline toWord16# #-}
toWord16# coefficient0# exponent0# largeNum =
  toSmallHelper smallToWord16 largeToWord16
    coefficient0# exponent0# largeNum

toWord32# :: Int# -> Int# -> LargeScientific -> (# (# #) | Word# #)
{-# noinline toWord32# #-}
toWord32# coefficient0# exponent0# largeNum =
  toSmallHelper smallToWord32 largeToWord32
    coefficient0# exponent0# largeNum

toInt32# :: Int# -> Int# -> LargeScientific -> (# (# #) | Int# #)
{-# noinline toInt32# #-}
toInt32# coefficient0# exponent0# largeNum =
  toSmallIntHelper smallToInt32 largeToInt32
    coefficient0# exponent0# largeNum

toWord# :: Int# -> Int# -> LargeScientific -> (# (# #) | Word# #)
{-# noinline toWord# #-}
toWord# coefficient0# exponent0# largeNum =
  toSmallHelper smallToWord largeToWord
    coefficient0# exponent0# largeNum

toInt# :: Int# -> Int# -> LargeScientific -> (# (# #) | Int# #)
{-# noinline toInt# #-}
toInt# coefficient0# exponent0# largeNum =
  toSmallIntHelper smallToInt largeToInt
    coefficient0# exponent0# largeNum

-- Arguments are non-normalized coefficient and exponent.
-- We cannot use the same trick that we use for Word8 and
-- Word16.
smallToWord32 :: Int -> Int -> (# (# #) | Word# #)
smallToWord32 !coefficient0 !exponent0
  | coefficient0 == 0 = (# | 0## #)
  | (coefficient,expon) <- incrementNegativeExp coefficient0 exponent0
  , expon >= 0, expon < 10, coefficient >= 0, coefficient <= 0xFFFFFFFF
    = word32Exp10 (fromIntegral @Int @Word coefficient) expon
  | otherwise = (# (# #) | #)

-- Arguments are non-normalized coefficient and exponent.
smallToInt32 :: Int -> Int -> (# (# #) | Int# #)
smallToInt32 !coefficient0 !exponent0
  | coefficient0 == 0 = (# | 0# #)
  | (coefficient,expon) <- incrementNegativeExp coefficient0 exponent0
  , expon >= 0, expon < 10
  , coefficient >= fromIntegral @Int32 @Int (minBound :: Int32)
  , coefficient <= fromIntegral @Int32 @Int (maxBound :: Int32)
    = if coefficient >= 0
        then posInt32Exp10 coefficient expon
        else negInt32Exp10 coefficient expon
  | otherwise = (# (# #) | #)

-- Arguments are non-normalized coefficient and exponent.
-- We cannot use the same trick that we use for Word8 and
-- Word16.
smallToWord :: Int -> Int -> (# (# #) | Word# #)
smallToWord !coefficient0 !exponent0
  | coefficient0 == 0 = (# | 0## #)
  | (coefficient,expon) <- incrementNegativeExp coefficient0 exponent0
  , expon >= 0, expon < 30, coefficient >= 0
    = wordExp10 (fromIntegral @Int @Word coefficient) expon
  | otherwise = (# (# #) | #)

-- Arguments are non-normalized coefficient and exponent.
smallToInt :: Int -> Int -> (# (# #) | Int# #)
smallToInt !coefficient0 !exponent0
  | coefficient0 == 0 = (# | 0# #)
  | (coefficient,expon) <- incrementNegativeExp coefficient0 exponent0
  , expon >= 0, expon < 30
    = if coefficient >= 0
        then posIntExp10 coefficient expon
        else negIntExp10 coefficient expon
  | otherwise = (# (# #) | #)

-- Arguments are non-normalized coefficient and exponent
-- With Word16, we can do a neat little trick where we
-- cap the coefficient at 65536 and the exponent at 5. This
-- works because a 32-bit signed int can contain 65535e4.
smallToWord16 :: Int -> Int -> (# (# #) | Word# #)
smallToWord16 !coefficient0 !exponent0
  | coefficient0 == 0 = (# | 0## #)
  | (coefficient,expon) <- incrementNegativeExp coefficient0 exponent0
  , expon >= 0, expon < 5, coefficient >= 0, coefficient < 65536
  , r <- exp10 coefficient expon
  , y@(W16# y# ) <- fromIntegral @Int @Word16 r
  , fromIntegral @Word16 @Int y == r
    = (# | y# #)
  | otherwise = (# (# #) | #)

-- Arguments are non-normalized coefficient and exponent
-- With Word8, we can do a neat little trick where we
-- cap the coefficient at 256 and the exponent at 3. This
-- works because a 32-bit signed int can contain 255e2.
smallToWord8 :: Int -> Int -> (# (# #) | Word# #)
smallToWord8 !coefficient0 !exponent0
  | coefficient0 == 0 = (# | 0## #)
  | (coefficient,expon) <- incrementNegativeExp coefficient0 exponent0
  , expon >= 0, expon < 3, coefficient >= 0, coefficient < 256
  , r <- exp10 coefficient expon
  , y@(W8# y# ) <- fromIntegral @Int @Word8 r
  , fromIntegral @Word8 @Int y == r
    = (# | y# #)
  | otherwise = (# (# #) | #)

-- Arguments are non-normalized
largeToWord8 :: LargeScientific -> (# (# #) | Word# #)
largeToWord8 (LargeScientific coefficient0 exponent0)
  | coefficient0 == 0 = (# | 0## #)
  | (coefficient,expon) <- largeIncrementNegativeExp coefficient0 exponent0
  , expon >= 0, expon < 3, coefficient >= 0, coefficient < 256
  , r <- exp10 (fromIntegral @Integer @Int coefficient) (fromIntegral @Integer @Int expon)
  , y@(W8# y# ) <- fromIntegral @Int @Word8 r
  , fromIntegral @Word8 @Int y == r
    = (# | y# #)
  | otherwise = (# (# #) | #)

-- Arguments are non-normalized
largeToWord16 :: LargeScientific -> (# (# #) | Word# #)
largeToWord16 (LargeScientific coefficient0 exponent0)
  | coefficient0 == 0 = (# | 0## #)
  | (coefficient,expon) <- largeIncrementNegativeExp coefficient0 exponent0
  , expon >= 0, expon < 5, coefficient >= 0, coefficient < 65536
  , r <- exp10 (fromIntegral @Integer @Int coefficient) (fromIntegral @Integer @Int expon)
  , y@(W16# y# ) <- fromIntegral @Int @Word16 r
  , fromIntegral @Word16 @Int y == r
    = (# | y# #)
  | otherwise = (# (# #) | #)

-- Arguments are non-normalized
largeToWord32 :: LargeScientific -> (# (# #) | Word# #)
largeToWord32 (LargeScientific coefficient0 exponent0)
  | coefficient0 == 0 = (# | 0## #)
  | (coefficient,expon) <- largeIncrementNegativeExp coefficient0 exponent0
  , expon >= 0, expon < 10, coefficient >= 0, coefficient <= 0xFFFFFFFF
    = word32Exp10 (fromIntegral @Integer @Word coefficient) (fromIntegral @Integer @Int expon)
  | otherwise = (# (# #) | #)

-- Arguments are non-normalized, this targets the native word size
largeToWord :: LargeScientific -> (# (# #) | Word# #)
largeToWord (LargeScientific coefficient0 exponent0)
  | coefficient0 == 0 = (# | 0## #)
  | (coefficient,expon) <- largeIncrementNegativeExp coefficient0 exponent0
  , expon >= 0, expon < 30, coefficient >= 0, coefficient <= (fromIntegral @Word @Integer maxBound)
    = wordExp10 (fromIntegral @Integer @Word coefficient) (fromIntegral @Integer @Int expon)
  | otherwise = (# (# #) | #)

-- Arguments are non-normalized
largeToInt32 :: LargeScientific -> (# (# #) | Int# #)
largeToInt32 (LargeScientific coefficient0 exponent0)
  | coefficient0 == 0 = (# | 0# #)
  | (coefficient,expon) <- largeIncrementNegativeExp coefficient0 exponent0
  , expon >= 0, expon < 10
  , coefficient >= (fromIntegral @Int32 @Integer minBound)
  , coefficient <= (fromIntegral @Int32 @Integer maxBound)
    = if coefficient >= 0
        then posInt32Exp10 (fromIntegral @Integer @Int coefficient) (fromIntegral @Integer @Int expon)
        else negInt32Exp10 (fromIntegral @Integer @Int coefficient) (fromIntegral @Integer @Int expon)
  | otherwise = (# (# #) | #)

-- Arguments are non-normalized, this targets the native word size
largeToInt :: LargeScientific -> (# (# #) | Int# #)
largeToInt (LargeScientific coefficient0 exponent0)
  | coefficient0 == 0 = (# | 0# #)
  | (coefficient,expon) <- largeIncrementNegativeExp coefficient0 exponent0
  , expon >= 0, expon < 30
  , coefficient >= (fromIntegral @Int @Integer minBound)
  , coefficient <= (fromIntegral @Int @Integer maxBound)
    = if coefficient >= 0
        then posIntExp10 (fromIntegral @Integer @Int coefficient) (fromIntegral @Integer @Int expon)
        else negIntExp10 (fromIntegral @Integer @Int coefficient) (fromIntegral @Integer @Int expon)
  | otherwise = (# (# #) | #)

-- Precondition: the exponent is non-negative. This returns
-- an unboxed Nothing on overflow. This implementation should
-- work even on a 32-bit platform.
word32Exp10 :: Word -> Int -> (# (# #) | Word# #)
word32Exp10 !a@(W# a# ) !e = case e of
  0 -> (# | a# #)
  _ -> let (overflow, a') = timesWord2 a 10 in
    if overflow || (a' > 0xFFFFFFFF)
      then (# (# #) | #)
      else word32Exp10 a' (e - 1)

-- Precondition: the exponent is non-negative, and the
-- coefficient is non-negative. This returns an unboxed
-- Nothing on overflow.
posInt32Exp10 :: Int -> Int -> (# (# #) | Int# #)
posInt32Exp10 !a@(I# a# ) !e = case e of
  0 -> (# | a# #)
  _ -> if a < posInt32PreUpper
    then let a' = a * 10 in
      if a' >= a && a' <= fromIntegral (maxBound :: Int32)
        then posInt32Exp10 a' (e - 1)
        else (# (# #) | #)
    else (# (# #) | #)

-- Precondition: the exponent is non-negative, and the
-- coefficient is non-positive. This returns an unboxed
-- Nothing on overflow.
negInt32Exp10 :: Int -> Int -> (# (# #) | Int# #)
negInt32Exp10 !a@(I# a# ) !e = case e of
  0 -> (# | a# #)
  _ -> if a > negInt32PreLower
    then let a' = a * 10 in
      if a' <= a && a' >= fromIntegral (minBound :: Int32)
        then negInt32Exp10 a' (e - 1)
        else (# (# #) | #)
    else (# (# #) | #)

-- Precondition: the exponent is non-negative. This returns
-- an unboxed Nothing on overflow.
wordExp10 :: Word -> Int -> (# (# #) | Word# #)
wordExp10 !a@(W# a# ) !e = case e of
  0 -> (# | a# #)
  _ -> let (overflow, a') = timesWord2 a 10 in if overflow
    then (# (# #) | #)
    else wordExp10 a' (e - 1)

-- Precondition: The exponent is non-negative, and the
-- coefficient is non-negative. This returns an unboxed
-- Nothing on overflow.
posIntExp10 :: Int -> Int -> (# (# #) | Int# #)
posIntExp10 !a@(I# a# ) !e = case e of
  0 -> (# | a# #)
  _ -> if a < posIntPreUpper
    then let a' = a * 10 in
      if a' >= a
        then posIntExp10 a' (e - 1)
        else (# (# #) | #)
    else (# (# #) | #)

-- Precondition: The exponent is non-negative, and the
-- coefficient is non-positive. This returns an unboxed
-- Nothing on overflow.
negIntExp10 :: Int -> Int -> (# (# #) | Int# #)
negIntExp10 !a@(I# a# ) !e = case e of
  0 -> (# | a# #)
  _ -> if a > negIntPreLower
    then let a' = a * 10 in
      if a' <= a
        then negIntExp10 a' (e - 1)
        else (# (# #) | #)
    else (# (# #) | #)

-- What are these lower and upper bounds? The problem that
-- we are trying to solve is that overflow is tricky to detect
-- when we multiply by ten. By putting an upper (or lower)
-- bound on the thing we are multiplying by ten, we can
-- make overflow detection simple: just test that the
-- accumulator became larger (or smaller when dealing with
-- a negative coefficient) than it previously was.

posIntPreUpper :: Int
posIntPreUpper = div maxBound 10 + 10

negIntPreLower :: Int
negIntPreLower = div minBound 10 - 10

posInt32PreUpper :: Int
posInt32PreUpper = 214748370

negInt32PreLower :: Int
negInt32PreLower = (-214748370)

-- Bool is true if overflow happened
timesWord2 :: Word -> Word -> (Bool, Word)
timesWord2 (W# a) (W# b) =
  let !(# c, r #) = Exts.timesWord2# a b
   in (case c of { 0## -> False; _ -> True}, W# r)

-- Precondition: the exponent is non-negative
exp10 :: Int -> Int -> Int
exp10 !a !e = case e of
  0 -> a
  _ -> exp10 (a * 10) (e - 1)

largeNormalize :: LargeScientific -> LargeScientific
largeNormalize s@(LargeScientific w _) = case w of
  0 -> LargeScientific 0 0
  _ -> largeNormalizeLoop s

-- Precondition: the coefficient is non-zero
largeNormalizeLoop :: LargeScientific -> LargeScientific
largeNormalizeLoop (LargeScientific w e) = case quotRem w 10 of
  (q,r) -> case r of
    0 -> largeNormalizeLoop (LargeScientific q (e + 1))
    _ -> LargeScientific w e

largeIncrementNegativeExp :: Integer -> Integer -> (Integer,Integer)
largeIncrementNegativeExp w e = if e >= 0
  then (w,e)
  else case quotRem w 10 of
    (q,r) -> case r of
      0 -> largeIncrementNegativeExp q (e + 1)
      _ -> (w,e)

smallNormalize :: Int -> Int -> (Int,Int)
smallNormalize (I# w) (I# e) = case w of
  0# -> (0,0)
  _ -> case smallNormalize# w e of
    (# w', e' #) -> (I# w', I# e')

incrementNegativeExp :: Int -> Int -> (Int,Int)
incrementNegativeExp (I# w) (I# e) = case incrementNegativeExp# w e of
  (# w', e' #) -> (I# w', I# e')

-- If the exponent is negative, increase it as long as the
-- coefficient divides ten evenly.
incrementNegativeExp# :: Int# -> Int# -> (# Int#, Int# #)
{-# noinline incrementNegativeExp# #-}
incrementNegativeExp# w# e# = if I# e# >= 0
  then (# w#, e# #)
  else case quotRem (I# w# ) 10 of
    (I# q#,r) -> case r of
      0 -> incrementNegativeExp# q# (e# +# 1# )
      _ -> (# w#, e# #)

-- Precondition: coefficient is not zero. If it is,
-- this will loop.
smallNormalize# :: Int# -> Int# -> (# Int#, Int# #)
{-# noinline smallNormalize# #-}
smallNormalize# w# e# = case quotRem (I# w# ) 10 of
  (I# q#,r) -> case r of
    0 -> smallNormalize# q# (e# +# 1# )
    _ -> (# w#, e# #)

-- | Parse a number that is encoded in UTF-8 and in scientific notation.
-- All of these are accepted:
--
-- * 330e-1
-- * 330e+1
-- * 330e1
-- * 330.0e1
-- * -330.0e1
-- * 12
-- * 00012
-- * 2.05
-- * +2.05
-- * +33.6e+1
parserSignedUtf8Bytes :: e -> Parser e s Scientific
parserSignedUtf8Bytes e = boxScientific (parserSignedUtf8Bytes# e)

-- | Variant of 'parserSignedUtf8Bytes' that rejects strings with
-- a leading plus or minus sign.
parserUnsignedUtf8Bytes :: e -> Parser e s Scientific
parserUnsignedUtf8Bytes e = boxScientific (parserUnsignedUtf8Bytes# e)

-- | Variant of 'parserUnsignedUtf8Bytes' that negates the result.
parserNegatedUtf8Bytes :: e -> Parser e s Scientific
parserNegatedUtf8Bytes e = boxScientific (parserNegatedUtf8Bytes# e)

parserTrailingUtf8Bytes# ::
     e -- ^ Error message
  -> Int# -- ^ Leading digit
  -> Parser e s Scientific#
{-# noinline parserTrailingUtf8Bytes# #-}
parserTrailingUtf8Bytes# e leader =
  mapIntPairToScientific (parseSmallTrailing# leader)
  `orElseScientific`
  upcastLargeScientific (parseLargeTrailing e (I# leader))

parserNegatedTrailingUtf8Bytes# ::
     e -- ^ Error message
  -> Int# -- ^ Leading digit
  -> Parser e s Scientific#
{-# noinline parserNegatedTrailingUtf8Bytes# #-}
parserNegatedTrailingUtf8Bytes# e leader =
  mapNegateIntPairToScientific (parseSmallTrailing# leader)
  `orElseScientific`
  upcastNegatedLargeScientific (parseLargeTrailing e (I# leader))

parserSignedUtf8Bytes# ::
     e -- ^ Error message
  -> Parser e s Scientific#
parserSignedUtf8Bytes# e = Latin.any e `bindToScientific` \c -> case c of
  '+' -> parserUnsignedUtf8Bytes# e
  '-' -> parserNegatedUtf8Bytes# e
  _ -> Unsafe.unconsume 1 `bindToScientific` \_ ->
    parserUnsignedUtf8Bytes# e

-- | Variant of 'parseUnsignedUtf8Bytes' where all arguments are
-- unboxed.
parserUnsignedUtf8Bytes# ::
     e -- ^ Error message
  -> Parser e s Scientific#
parserUnsignedUtf8Bytes# e =
  mapIntPairToScientific parseSmall#
  `orElseScientific`
  upcastLargeScientific (parseLarge e)

-- Negates the result after parsing the bytes.
parserNegatedUtf8Bytes# ::
     e -- ^ Error message
  -> Parser e s Scientific#
parserNegatedUtf8Bytes# e =
  mapNegateIntPairToScientific parseSmall#
  `orElseScientific`
  upcastNegatedLargeScientific (parseLarge e)

parserTrailingUtf8Bytes ::
     e -- ^ Error message
  -> Int -- ^ Leading digit, should be between @-9@ and @9@.
  -> Parser e s Scientific
parserTrailingUtf8Bytes e (I# leader) =
  boxScientific (parserTrailingUtf8Bytes# e leader)

parserNegatedTrailingUtf8Bytes ::
     e -- ^ Error message
  -> Int -- ^ Leading digit, should be between @-9@ and @9@.
  -> Parser e s Scientific
parserNegatedTrailingUtf8Bytes e (I# leader) =
  boxScientific (parserNegatedTrailingUtf8Bytes# e leader)
-- 
-- parserTrailingUtf8Bytes# ::
--      e -- Error message
--   -> Parser e s Scientific#
-- parserTrailingUtf8Bytes# !leader e =
--   parseSmall# leader
--   `orElseScientific`
--   unboxScientific (P.fail e)

parseLarge :: e -> Parser e s LargeScientific
parseLarge e = do
  coeff <- Latin.decUnsignedInteger e
  parseLargeCommon e coeff

parseLargeTrailing :: e -> Int -> Parser e s LargeScientific
parseLargeTrailing e !leader = do
  coeff <- Latin.decTrailingInteger leader
  parseLargeCommon e coeff

parseLargeCommon :: e -> Integer -> Parser e s LargeScientific
{-# noinline parseLargeCommon #-}
parseLargeCommon e coeff = do
  Latin.trySatisfyThen (pure (LargeScientific coeff 0)) $ \c -> case c of
    '.' -> Just $ do
      !start <- Unsafe.cursor
      afterDot <- Latin.decUnsignedInteger e
      !end <- Unsafe.cursor
      let !logDenom = end - start
          !coeffFinal = (integerTenExp coeff logDenom) + afterDot
      Latin.trySatisfy (\ch -> ch == 'e' || ch == 'E') >>= \case
        True -> attemptLargeExp e coeffFinal (unI (Prelude.negate logDenom))
        False -> pure $! LargeScientific coeffFinal $! fromIntegral $! Prelude.negate logDenom
    'e' -> Just (attemptLargeExp e coeff 0# )
    'E' -> Just (attemptLargeExp e coeff 0# )
    _ -> Nothing

-- handles unsigned small numbers
parseSmall# :: Parser () s (# Int#, Int# #)
parseSmall# =
  Latin.decUnsignedInt# () `Parser.bindFromIntToIntPair` \coeff# ->
  parseSmallCommon# coeff#

parseSmallTrailing# :: Int# -> Parser () s (# Int#, Int# #)
parseSmallTrailing# leader =
  Latin.decTrailingInt# () leader `Parser.bindFromIntToIntPair` \coeff# ->
  parseSmallCommon# coeff#

parseSmallCommon# :: Int# -> Parser () s (# Int#, Int# #)
{-# noinline parseSmallCommon# #-}
parseSmallCommon# coeff# =
  Latin.trySatisfyThen (Parser.pureIntPair (# coeff#, 0# #)) $ \c -> case c of
    '.' -> Just $
      Unsafe.cursor `Parser.bindFromLiftedToIntPair` \start ->
      Latin.decUnsignedInt# () `Parser.bindFromIntToIntPair` \afterDot# ->
      Unsafe.cursor `Parser.bindFromLiftedToIntPair` \end ->
      let !logDenom = end - start
          goCoeff !coeffShifted !expon = case expon of
            0 ->
              let !(I# coeffShifted# ) = coeffShifted
                  !(# coeffFinal, overflowed #) =
                    Exts.addIntC# coeffShifted# afterDot#
               in case overflowed of
                0# -> Latin.trySatisfy (\ch -> ch == 'e' || ch == 'E') `Parser.bindFromLiftedToIntPair` \b -> case b of
                  True -> attemptSmallExp coeffFinal (unI (Prelude.negate logDenom))
                  False -> Parser.pureIntPair (# coeffFinal, unI (Prelude.negate logDenom) #)
                _ -> Parser.failIntPair ()
            _ ->
              let coeffShifted' = coeffShifted * 10
               in if coeffShifted' >= coeffShifted
                    then goCoeff coeffShifted' (expon - 1)
                    -- If we overflow, fail so that the parser
                    -- for large number will handle it instead.
                    else Parser.failIntPair ()
       in goCoeff (I# coeff# ) logDenom
    'e' -> Just (attemptSmallExp coeff# 0#)
    'E' -> Just (attemptSmallExp coeff# 0#)
    _ -> Nothing


-- The delta passed to this is only ever a negative integer.
attemptLargeExp ::
     e
  -> Integer
  -> Int#
  -> Parser e s LargeScientific
{-# noinline attemptLargeExp #-}
attemptLargeExp e signedCoeff !deltaExp# = do
  expon <- Latin.decSignedInteger e
  let !exponent' = expon + fromIntegral (I# deltaExp# )
  pure (LargeScientific signedCoeff exponent')

-- The delta passed to this is only ever a negative integer.
-- It is also between -21 and -1. (Or maybe -22 or -20, not sure).
attemptSmallExp :: Int# -> Int# -> Parser () s (# Int#, Int# #)
{-# noinline attemptSmallExp #-}
attemptSmallExp !signedCoeff# !deltaExp# = Parser.unboxIntPair $ do
  e <- Latin.decSignedInt ()
  -- I give this a little extra padding just to be safe.
  if e > (minBound + padding)
    then pure (signedCoeff, e + deltaExp)
    else Parser.fail ()
  where
  signedCoeff = I# signedCoeff#
  deltaExp = I# deltaExp#

-- | Convert a 'Word#' parser to a 'Word32' parser. Precondition:
-- the argument parser only returns words less than 4294967296.
boxScientific :: Parser s e Scientific# -> Parser s e Scientific
boxScientific (Parser f) = Parser
  (\x s0 -> case f x s0 of
    (# s1, r #) -> case r of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# (# w, y, z #), b, c #) #) -> (# s1, (# | (# Scientific (I# w) (I# y) z, b, c #) #) #)
  )

unI :: Int -> Int#
unI (I# i) = i

orElseScientific :: Parser x s Scientific# -> Parser e s Scientific# -> Parser e s Scientific#
{-# inline orElseScientific #-}
orElseScientific (Parser f) (Parser g) = Parser
  (\x s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# _ | #) -> g x s1
      (# | r #) -> (# s1, (# | r #) #)
  )

-- Precondition: argument is non-negative
-- If the argument is r and the exponent is e, the result
-- is described as: r * 10^e
integerTenExp :: Integer -> Int -> Integer
integerTenExp !r !e = case e of
  0 -> r
  1 -> r * 10
  2 -> r * 100
  3 -> r * 1000
  4 -> r * 10000
  5 -> r * 100000
  6 -> r * 1000000
  7 -> r * 10000000
  8 -> r * 100000000
  _ -> integerTenExp (r * 1000000000) (e - 9)

data Estimate
  = Exactly !Integer
  | LowerBoundedMagnitude !Integer
    -- For positive N, LowerBoundedMagnitude N means that x > N and x < N+1.
    -- For negative N, LowerBoundedMagnitude N means that x < N and x > N-1.

-- Precondition: Exponent is non-positive. Coefficient is non-zero.
-- When calling this from elsewhere, set wasTruncated to False.
posSciLowerBound :: Bool -> Integer -> Integer -> Estimate
posSciLowerBound !wasTruncated !coeff !e
  | e == 0 = case wasTruncated of
      True -> LowerBoundedMagnitude coeff
      False -> Exactly coeff
  | otherwise = let (q,r) = quotRem coeff 10 in
      case q of
        0 -> LowerBoundedMagnitude 0
        _ -> posSciLowerBound (wasTruncated || r /= 0) q (e + 1)

-- This only works if the number is a power of ten.
-- It is only intended to be used by fromFixed.
-- Precondition: the Integer is not zero.
logBase10 :: Int -> Integer -> Int
logBase10 !acc i = if i == 1
  then acc
  else logBase10 (acc + 1) (div i 10)

upcastLargeScientific ::
     Parser e s LargeScientific
  -> Parser e s Scientific#
upcastLargeScientific (Parser g) = Parser
  (\x s0 -> case g x s0 of
    (# s1, r #) -> case r of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# a, b, c #) #) -> (# s1, (# | (# (# 0#, unI minBound, a #), b, c #) #) #)
  )

upcastNegatedLargeScientific ::
     Parser e s LargeScientific
  -> Parser e s Scientific#
upcastNegatedLargeScientific (Parser g) = Parser
  (\x s0 -> case g x s0 of
    (# s1, r #) -> case r of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# LargeScientific w y, b, c #) #) -> (# s1, (# | (# (# 0#, unI minBound, LargeScientific (Prelude.negate w) y #), b, c #) #) #)
  )

mapIntPairToScientific ::
     Parser e s (# Int#, Int# #)
  -> Parser e s Scientific#
mapIntPairToScientific (Parser g) = Parser
  (\x s0 -> case g x s0 of
    (# s1, r #) -> case r of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# (# y, z #), b, c #) #) -> (# s1, (# | (# (# y, z, zeroLarge #), b, c #) #) #)
  )

-- We do not check to see if exponent==minBound since this is called
-- on the result of an unsigned parser. Fortunately, signed fixed-width
-- integers always have one extra number on the low end that is not the
-- negation of anything on the high end.
mapNegateIntPairToScientific ::
     Parser e s (# Int#, Int# #)
  -> Parser e s Scientific#
mapNegateIntPairToScientific (Parser g) = Parser
  (\x s0 -> case g x s0 of
    (# s1, r #) -> case r of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# (# y, z #), b, c #) #) -> (# s1, (# | (# (# Exts.negateInt# y, z, zeroLarge #), b, c #) #) #)
  )

bindToScientific :: Parser s e a -> (a -> Parser s e Scientific#) -> Parser s e Scientific#
{-# inline bindToScientific #-}
bindToScientific (Parser f) g = Parser
  (\x@(# arr, _, _ #) s0 -> case f x s0 of
    (# s1, r0 #) -> case r0 of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# y, b, c #) #) ->
        runParser (g y) (# arr, b, c #) s1
  )

builderUtf8 :: Scientific -> Builder
builderUtf8 (Scientific coeff e big)
  | e == 0 = Builder.intDec coeff
  | e == minBound = let LargeScientific coeff' e' = big in
      Builder.integerDec coeff'
      <>
      Builder.ascii 'e'
      <>
      Builder.integerDec e'
  | otherwise = Builder.fromBounded Nat.constant $
      BB.intDec coeff
      `BB.append`
      BB.ascii 'e'
      `BB.append`
      BB.intDec e

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
  , toWord8
    -- * Decode
  , parserSignedUtf8Bytes
  , parserSignedUtf8Bytes#
  , parserUnsignedUtf8Bytes#
  , parserNegatedUnsignedUtf8Bytes#
  ) where

import Prelude hiding (negate)

import GHC.Exts (Int#,Word#,Int(I#),(+#),(*#))
import GHC.Word (Word(W#),Word8(W8#))
import Data.Bytes.Parser (Parser(..))
import Data.Fixed (Fixed(MkFixed),HasResolution)

import qualified Data.Fixed as Fixed
import qualified Data.Bytes.Parser as P
import qualified GHC.Exts as Exts
import qualified Prelude as Prelude

data Scientific = Scientific
  {-# UNPACK #-} !Int -- coefficient
  {-# UNPACK #-} !Int -- base-10 exponent, minBound means use unlimited-precision field
  LargeScientific

type Scientific# = (# Int#, Int#, LargeScientific #)

instance Show Scientific where
  showsPrec _ (Scientific coeff e large) = if e /= minBound
    then showsPrec 0 coeff . showChar 'e' . showsPrec 0 e
    else case large of
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

data LargeScientific = LargeScientific !Integer !Integer

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
toWord8 (Scientific (I# coeff) (I# e) large) = case toWord8# coeff e large of
  (# (# #) | #) -> Nothing
  (# | w #) -> Just (W8# w)

toWord8# :: Int# -> Int# -> LargeScientific -> (# (# #) | Word# #)
{-# noinline toWord8# #-}
toWord8# coefficient0# exponent0# large = if exponent0 /= minBound
  then smallToWord8 coefficient0 exponent0
  else largeToWord8 large
  where
  coefficient0 = I# coefficient0#
  exponent0 = I# exponent0#

-- Arguments are non-normalized coefficient and exponent
smallToWord8 :: Int -> Int -> (# (# #) | Word# #)
smallToWord8 !coefficient0 !exponent0
  | coefficient0 == 0 = (# | 0## #)
  | (coefficient,exponent) <- incrementNegativeExp coefficient0 exponent0
  , exponent >= 0, exponent < 3, coefficient >= 0, coefficient < 256
  , r <- exp10 coefficient exponent
  , y@(W8# y# ) <- fromIntegral @Int @Word8 r
  , fromIntegral @Word8 @Int y == r
    = (# | y# #)
  | otherwise = (# (# #) | #)

-- Arguments are non-normalized
largeToWord8 :: LargeScientific -> (# (# #) | Word# #)
largeToWord8 (LargeScientific coefficient0 exponent0)
  | coefficient0 == 0 = (# | 0## #)
  | (coefficient,exponent) <- largeIncrementNegativeExp coefficient0 exponent0
  , exponent >= 0, exponent < 3, coefficient >= 0, coefficient < 256
  , r <- exp10 (fromIntegral @Integer @Int coefficient) (fromIntegral @Integer @Int exponent)
  , y@(W8# y# ) <- fromIntegral @Int @Word8 r
  , fromIntegral @Word8 @Int y == r
    = (# | y# #)
  | otherwise = (# (# #) | #)

-- Precondition: the exponent is non-negative
exp10 :: Int -> Int -> Int
exp10 !a !e = case e of
  0 -> a
  _ -> exp10 (a * 10) (e - 1)

largeNormalize :: LargeScientific -> LargeScientific
largeNormalize s@(LargeScientific w e) = case w of
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
-- All of these would be accepted:
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

parserSignedUtf8Bytes# ::
     e -- ^ Error message
  -> Parser e s Scientific#
parserSignedUtf8Bytes# e = P.any e `bindToScientific` \c -> case c of
  43 -> -- plus sign
    parserUnsignedUtf8Bytes# e
  45 -> -- minus sign
    parserNegatedUnsignedUtf8Bytes# e
  _ -> P.unconsume 1 `bindToScientific` \_ ->
    parserUnsignedUtf8Bytes# e

-- | Variant of 'parseUnsignedUtf8Bytes' where all arguments are
-- unboxed.
parserUnsignedUtf8Bytes# ::
     e -- ^ Error message
  -> Parser e s Scientific#
parserUnsignedUtf8Bytes# e =
  mapIntPairToScientific parseSmall#
  `orElseScientific`
  unboxScientific (P.fail e)

-- Negates the result after parsing the bytes.
parserNegatedUnsignedUtf8Bytes# ::
     e -- ^ Error message
  -> Parser e s Scientific#
parserNegatedUnsignedUtf8Bytes# e =
  mapNegateIntPairToScientific parseSmall#
  `orElseScientific`
  unboxScientific (P.fail e)

-- parserTrailingUtf8Bytes ::
--      e -- ^ Error message
--   -> Int -- ^ Leading digit, should be between @-9@ and @9@.
--   -> Parser e s Scientific
-- parserTrailingUtf8Bytes e (I# leader) =
--   boxScientific (parserTrailingUtf8Bytes# leader e)
-- 
-- parserTrailingUtf8Bytes# ::
--      e -- Error message
--   -> Parser e s Scientific#
-- parserTrailingUtf8Bytes# !leader e =
--   parseSmall# leader
--   `orElseScientific`
--   unboxScientific (P.fail e)

-- handles unsigned small numbers
parseSmall# :: Parser () s (# Int#, Int# #)
{-# noinline parseSmall# #-}
parseSmall# =
  P.decUnsignedInt# () `P.bindIntToIntPair` \coeff# ->
  P.isEndOfInput `P.bindToIntPair` \case
    True -> P.pureIntPair (# coeff#, 0# #)
    -- Hmmm... anyAscii cannot actually fail. There must be
    -- a better way.
    False -> P.anyAscii# () `P.bindCharToIntPair` \c -> case c of
      '.'# ->
        P.cursor `P.bindToIntPair` \start ->
        P.decUnsignedInt# () `P.bindIntToIntPair` \afterDot# ->
        P.cursor `P.bindToIntPair` \end ->
        let !logDenom = end - start
            goCoeff !coeffShifted !exponent = case exponent of
              0 ->
                let !(I# coeffShifted# ) = coeffShifted
                    !(# coeffFinal, overflowed #) =
                      Exts.addIntC# coeffShifted# afterDot#
                 in case overflowed of
                  0# -> P.isEndOfInput `P.bindToIntPair` \case
                    True -> P.pureIntPair (# coeffFinal, unI (Prelude.negate logDenom) #)
                    -- Again, there must be a better way.
                    -- Also, anyAscii is not technically correct. If a
                    -- non-ASCII codepoint follows the number, we do
                    -- not want to fail.
                    False -> P.anyAscii# () `P.bindCharToIntPair` \x ->
                      (attemptSmallExp x coeffFinal (unI (Prelude.negate logDenom)))
                  _ -> P.failIntPair ()
              _ ->
                let coeffShifted' = coeffShifted * 10
                 in if coeffShifted' >= coeffShifted
                      then goCoeff coeffShifted' (exponent - 1)
                      -- If we overflow, fail so that the parser
                      -- for large number will handle it instead.
                      else P.failIntPair ()
         in goCoeff (I# coeff# ) logDenom
      _ -> attemptSmallExp c coeff# 0#

-- The delta passed to this is only ever a non-positive integer.
-- It is also between -21 and 0. (Or maybe -22 or -20, not sure).
attemptSmallExp :: Exts.Char# -> Int# -> Int# -> Parser () s (# Int#, Int# #)
{-# noinline attemptSmallExp #-}
attemptSmallExp !c# !signedCoeff# !deltaExp# = P.unboxIntPair $ case c# of
  'e'# -> do
    e <- P.decSignedInt ()
    -- I give this a little extra padding just to be safe.
    if e > (minBound + padding)
      then pure (signedCoeff, e + deltaExp)
      else P.fail ()
  'E'# -> do
    e <- P.decSignedInt ()
    if e > (minBound + padding)
      then pure (signedCoeff, e + deltaExp)
      else P.fail ()
  _ -> do
    P.unconsume 1
    pure (signedCoeff, deltaExp)
  where
  signedCoeff = I# signedCoeff#
  deltaExp = I# deltaExp#

unboxScientific :: Parser s e Scientific -> Parser s e Scientific#
unboxScientific (Parser f) = Parser
  (\x s0 -> case f x s0 of
    (# s1, r #) -> case r of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# Scientific (I# x) (I# y) z, b, c #) #) -> (# s1, (# | (# (# x, y, z #), b, c #) #) #)
  )

-- | Convert a 'Word#' parser to a 'Word32' parser. Precondition:
-- the argument parser only returns words less than 4294967296.
boxScientific :: Parser s e Scientific# -> Parser s e Scientific
boxScientific (Parser f) = Parser
  (\x s0 -> case f x s0 of
    (# s1, r #) -> case r of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# (# x, y, z #), b, c #) #) -> (# s1, (# | (# Scientific (I# x) (I# y) z, b, c #) #) #)
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

-- This only works if the number is a power of ten.
-- It is only intended to be used by fromFixed.
-- Precondition: the Integer is not zero.
logBase10 :: Int -> Integer -> Int
logBase10 !acc i = if i == 1
  then acc
  else logBase10 (acc + 1) (div i 10)

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

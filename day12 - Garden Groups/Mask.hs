#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci

module Mask where

-------------
-- Imports --
-------------
import Data.Bits
import GHC.Num

-- Each obj has a shape encoded as bits of an Integer.
type Point = (Int,Int)
type Mask = Integer

-- Converts a 2D point to a 1D index
pointToIndex :: Int -> Point -> Int
pointToIndex width (x, y) = y * width + x

indexToPoint :: Integral a => a -> a -> (a, a)
indexToPoint width i = i `divMod` width

pointToMask :: Int -> Point -> Mask
pointToMask width (x,y) = moveMask width (x,y) 1

moveMask :: Int -> (Int,Int) -> Mask -> Mask
moveMask width (dx,dy) pts = pts `shift` pointToIndex width (dx,dy)

movePoint :: Int -> (Int,Int) -> (Int,Int) -> (Int,Int)
movePoint width (dx,dy) (x,y) = (x+dx,y+dy)


-- isOverlapping n n
-- time:  O(n) (approx.) [0 < n < 10000000000, 0 < secs < 7.97]
-- space: O(n) (approx.) [0 < n < 10000000000, 0 < bytes < 30000141704]
-- https://nux-pc/svn/Nux-SVN/My Programming/Scratchings/Advent of Code 2022/day17 (bits).hs/?r=1569
isOverlapping :: Mask -> Mask -> Bool
isOverlapping ps1 ps2 = (ps1 .&. ps2) /= zeroBits

bitwiseSubtract :: Mask -> Mask -> Mask
bitwiseSubtract ps1 ps2 = (ps1 .&. complement ps2)

bitwiseAnd :: Mask -> Mask -> Mask
bitwiseAnd ps1 ps2 = (ps1 .&. ps2)

bitwiseOr :: Mask -> Mask -> Mask
bitwiseOr ps1 ps2 = (ps1 .|. ps2)

bitwiseXor :: Mask -> Mask -> Mask
bitwiseXor ps1 ps2 = (ps1 `xor` ps2)

-- Warning: fromIntegral may truncate an exceptionally large Integer (larger than max Int)
msbIndex :: Integer -> Int
msbIndex n = if n <= 0 then error "msb requires a positive integer input" else fromIntegral (integerLog2 n)

-- Warning: fromIntegral may truncate an exceptionally large Integer (larger than max Int)
removeMsbNTimes :: Int -> Integer -> Integer
removeMsbNTimes 0 n = n  -- Base case: No removals left
removeMsbNTimes _ 0 = 0  -- No MSB to remove
removeMsbNTimes k n
  | k <= 0    = n
  | otherwise = removeMsbNTimes (k - 1) (n `xor` bit (fromIntegral (integerLog2 n)))

-- Warning: fromIntegral may truncate an exceptionally large Integer (larger than max Int)
middleIndex :: Integer -> Int
middleIndex n = fromIntegral . msbIndex $ (removeMsbNTimes halfActiveBits n)
  where halfActiveBits = popCount n `div` 2

msbPoint :: Int -> Integer -> Point
msbPoint width n = indexToPoint width (msbIndex n)

middlePoint :: Int -> Integer -> Point
middlePoint width n = indexToPoint width (middleIndex n)

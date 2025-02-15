#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 --package QuickCheck-2.14.3 --package ansi-terminal-0.11.5 --package linear --package array

----------------------------------
----------------------------------
----  Day 15: Warehouse Woes  ----
----------------------------------
----------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 --package QuickCheck-2.14.3 --package ansi-terminal-0.11.5 --package linear --package array -- '.\day15.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day15part1
-- 

-- *Main> day15part2
-- 

module Main (main) where

import Data.Maybe

import AsciiWorld
import WalkableWorld

data KeyType = Original | Part Int deriving (Show, Eq, Ord)
keyTypeToMaybe Original   = Nothing
keyTypeToMaybe (Part x) = Just x
maybeToKeyType Nothing  = Original
maybeToKeyType (Just x) = Part x
isOriginal = isNothing . keyTypeToMaybe
isPart = isJust . keyTypeToMaybe
getPartNum = fromJust . keyTypeToMaybe

data Key k = Key {keyType :: KeyType, keyValue :: k} deriving (Show, Eq, Ord)

main :: IO ()
main = do
    contents <- readFile "input/day15 (data).csv"
    
    let initWorld :: WalkableWorld (Key Char) (Key Char)
        initWorld = readWorld (Just . WKMask . Key Original) contents
        height = wwHeight initWorld
        asciiWorld = wwRawAsciiWorld initWorld
    
    let
        bgChar :: Char
        bgChar = '.'
        
        maskToChar :: (Key Char) -> Char
        maskToChar (Key Original c) = c
        maskToChar (Key (Part n) c) = c
        
        pointsToChar :: (Key Char) -> Char
        pointsToChar (Key Original c) = c
        pointsToChar (Key (Part n) c) = c
        
        nameZOrder :: WorldKey (Key Char) (Key Char) -> WorldKey (Key Char) (Key Char) -> Ordering
        nameZOrder = compare
        
     in printWorld bgChar maskToChar pointsToChar nameZOrder initWorld

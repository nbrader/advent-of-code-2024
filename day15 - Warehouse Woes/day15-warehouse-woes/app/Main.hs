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
import Data.Tuple
import Data.Function
import Data.Bifunctor
import Data.List
import Data.List.Split
import qualified Data.Map as M

import AsciiWorld
import WalkableWorld

data MaskType  = Box | Wall | InternalMaskType
    deriving (Show, Eq, Ord)

data PointsType = Robot | InternalPointsType
    deriving (Show, Eq, Ord)

main :: IO ()
main = do
    contents <- readFile "input/day15 (example).csv"
    
    let [worldStr,instructionsStr] = map unlines . splitOn [[]] . lines $ contents
        
        charAssoc :: [(Char, WorldKey MaskType PointsType)]
        charAssoc =
            [   ('@', PointsKey Robot),
                ('O', MaskKey Box),
                ('#', MaskKey Wall)
            ]
        
        fromCharMap = M.fromList charAssoc
        toCharMap = M.fromList (map swap charAssoc)
        
        fromChar :: Char -> Maybe (WorldKey MaskType PointsType)
        fromChar = flip M.lookup fromCharMap
        
        toChar :: WorldKey MaskType PointsType -> Char
        toChar = fromMaybe 'I' . flip M.lookup toCharMap
        
        initWorld :: WalkableWorld MaskType PointsType
        initWorld = readWorld fromChar worldStr
        
        bgChar :: Char
        bgChar = '.'
        
        nameZOrder :: WorldKey MaskType PointsType -> WorldKey MaskType PointsType -> Ordering
        nameZOrder = compare
        
        nameZOrder' :: WorldKey (Ext_Int MaskType WWMaskKey) (Ext_Int PointsType WWPointsKey) -> WorldKey (Ext_Int MaskType WWMaskKey) (Ext_Int PointsType WWPointsKey) -> Ordering
        nameZOrder' = nameZOrder `on` conversion
        
        conversion :: WorldKey (Ext_Int MaskType WWMaskKey) (Ext_Int PointsType WWPointsKey) -> WorldKey MaskType PointsType
        conversion = toWorldKey . bimap fromExternal1 fromExternal2 . fromWorldKey
    
    print initWorld
    printWorld bgChar (toChar . MaskKey) (toChar . PointsKey) nameZOrder initWorld
    printRawAsciiWorld bgChar (toChar . MaskKey . fromExternal1) (toChar . PointsKey . fromExternal2) nameZOrder' initWorld
    
    let instrs = concat . lines $ instructionsStr
    putStrLn instrs
    
    let resultingWorlds = foldl' (\ws@(w:_) c -> (moveBotByCharInWorld c w):ws) [initWorld] instrs
    
    mapM_ (printWorld bgChar (toChar . MaskKey) (toChar . PointsKey) nameZOrder) (reverse resultingWorlds)

moveBotByCharInWorld :: Char -> WalkableWorld MaskType PointsType -> WalkableWorld MaskType PointsType
moveBotByCharInWorld c world = movePointsKeyByVecInWWUnlessNewWorldSatisfiesPred Robot (moveVecFromChar c) world hitWall
  where hitWall world' = inWorldIsPointsKeyOverlappingMaskKey (wwRawAsciiWorld world') (External Robot) (External Wall)

moveVecFromChar '>' = (1,0)
moveVecFromChar '<' = (-1,0)
moveVecFromChar '^' = (0,1)
moveVecFromChar 'v' = (0,-1)

fromExternal1 :: Ext_Int MaskType WWMaskKey -> MaskType
fromExternal1 (External x) = x
fromExternal1 (Internal y) = InternalMaskType

fromExternal2 :: Ext_Int PointsType WWPointsKey -> PointsType
fromExternal2 (External x) = x
fromExternal2 (Internal y) = InternalPointsType
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


-------------
-- Imports --
-------------
import Data.List.Split
import Data.Maybe (fromJust, isNothing, isJust, catMaybes)
import Linear hiding (trace)
import Data.List as L (foldl', transpose, findIndex)
import Data.Array as A
import qualified Data.Map as M
import Data.Ord
import Data.Bits
import Data.Function
import Data.Char
import qualified Data.Map as M

import Util ( lrduDirs )
import Mask ( Mask
            , Point
            , bitwiseSubtract
            , bitwiseOr
            , bitwiseXor
            , bitwiseAnd
            , msbIndex
            , middleIndex
            , msbPoint
            , middlePoint
            , pointToMask )

import AsciiWorld as AW ( AsciiWorld(..)
                        , WorldKey(..)
                        , fromWorldKey
                        , toWorldKey
                        , readAsciiWorld
                        , showAsciiWorld
                        , printAsciiWorld
                        , setWidth
                        , changeWidthBy
                        , mapKeyForMasks
                        , mapKeyForPoints
                        , msbPointOfMask
                        , middlePointOfMask
                        , combineTwoAsciiWorlds
                        , combineAsciiWorlds
                        , isNamedPoint
                        , isInNamedMask
                        , isNamedPointOrInNamedMask
                        , moveMaskOfNameBy
                        , movePointsOfNameBy
                        , addMask
                        , deleteMask
                        , filterMaskKeys
                        , filterMasks
                        , lookupMask
                        , adjustMask
                        , updateMask
                        , alterMask
                        , copyMask
                        , applyMask
                        , setPoint
                        , deletePoints
                        , insertMaskFromPoints
                        , insertMaskFromNamedPoints
                        , isOverlappingMasks )


-------------
-- Program --
-------------
main = day15part2

readBots inStr =
    inStr
        & filter (\c -> c `elem` [',', ' ', '-', '\n'] || isDigit c)
        & splitOn "\n"
        & map (splitOn " ")
        & map (map (splitOn ","))
        & map (map (map (read :: String -> Int)))
        & map (\[[px,py],[vx,vy]] -> (V2 vx vy, V2 px py))

run (v,p) = (v, p ^+^ v)

runNTimes 0 (v,p) = (v,p)
runNTimes n (v,p) = runNTimes (n-1) (run (v,p))

modWalls width height (v, V2 px py) = (v, V2 (px `mod` width) (py `mod` height))

filterIntoQuadrants :: Int -> Int -> [V2 Int] -> ([V2 Int], [V2 Int], [V2 Int], [V2 Int])
filterIntoQuadrants width height vs = (filter (inQuad q1) vs, filter (inQuad q2) vs, filter (inQuad q3) vs, filter (inQuad q4) vs)
  where (x0,x1,x2,x3) = (0, (width  `div` 2) - 1, width  - (width  `div` 2), width  - 1)
        (y0,y1,y2,y3) = (0, (height `div` 2) - 1, height - (height `div` 2), height - 1)
        
        q1minX = x0
        q1maxX = x1
        q1minY = y0
        q1maxY = y1
        
        q2minX = x2
        q2maxX = x3
        q2minY = y0
        q2maxY = y1
        
        q3minX = x0
        q3maxX = x1
        q3minY = y2
        q3maxY = y3
        
        q4minX = x2
        q4maxX = x3
        q4minY = y2
        q4maxY = y3
        
        q1 = (q1minX, q1maxX, q1minY, q1maxY)
        q2 = (q2minX, q2maxX, q2minY, q2maxY)
        q3 = (q3minX, q3maxX, q3minY, q3maxY)
        q4 = (q4minX, q4maxX, q4minY, q4maxY)
        
        inQuad (qminX, qmaxX, qminY, qmaxY) (V2 x y) = qminX <= x && x <= qmaxX && qminY <= y && y <= qmaxY

-- determinant [x00, x01, x10, x11] = x00 * x11 - x01 * x10

-- mult [x00, x01, x10, x11] [y00, y01, y10, y11] = [x00 * y00 + x01 * y10, x00 * y01 + x01 * y11, x10 * y00 + x11 * y10, x10 * y01 + x11 * y11]

-- multVec [x00, x01, x10, x11] [y00, y10] = [x00 * y00 + x01 * y10, x10 * y00 + x11 * y10]

-- timesPressedAAndB :: [[Int]] -> Maybe [Int]
-- timesPressedAAndB [[ax,ay],[bx,by],[px,py]]
    -- | det == 0 = Nothing
    -- | (sx_det `mod` det) /= 0 || (sy_det `mod` det) /= 0 = Nothing
    -- | otherwise = Just [sx_det `div` det, sy_det `div` det]
  -- where x@[x00,x01,x10,x11] = [ax,bx,ay,by]
        -- det = determinant x
        -- sx_det = x11 * px - x01 * py
        -- sy_det = -x10 * px + x00 * py

-- tokensFromTimes [a,b] = 3*a+b

-- correct [[ax,ay],[bx,by],[px,py]] = [[ax,ay],[bx,by],[10000000000000+px,10000000000000+py]]

-- solve machine = do
    -- times <- timesPressedAAndB machine
    -- return $ tokensFromTimes times

toPoint (V2 x y) = (x,y)

day15part1 = do
    contents <- readFile "day15 (data).csv"
    let (width, height) = (101,103)
    
    let initBots = readBots contents
    mapM_ print initBots
    
    let asciiWorld = AsciiWorld
            { asciiWorldMasks = M.empty
            , asciiWorldPoints = M.fromList [("Bots", map (toPoint . snd) initBots)]
            , asciiWorldWidth = width }
        
        bgChar = '.'
        maskToChar = id
        pointsToChar = const 'B'
        nameZOrder = compare
    
    printAsciiWorld height bgChar maskToChar pointsToChar nameZOrder asciiWorld
    putStrLn ""
    
    let after100 = map (runNTimes 100) initBots
    mapM_ print after100
    putStrLn ""
    
    let afterMod = map (modWalls width height) after100
    mapM_ print afterMod
    putStrLn ""
    
    let asciiWorld = AsciiWorld
            { asciiWorldMasks = M.empty
            , asciiWorldPoints = M.fromList [("Bots", map (toPoint . snd) afterMod)]
            , asciiWorldWidth = width }
        
        bgChar = '.'
        maskToChar = id
        pointsToChar = const 'B'
        nameZOrder = compare
    
    printAsciiWorld height bgChar maskToChar pointsToChar nameZOrder asciiWorld
    putStrLn ""
    
    let (q1Ps, q2Ps, q3Ps, q4Ps) = filterIntoQuadrants width height (map snd afterMod)
    mapM_ print q1Ps
    putStrLn ""
    mapM_ print q2Ps
    putStrLn ""
    mapM_ print q3Ps
    putStrLn ""
    mapM_ print q4Ps
    putStrLn ""
    
    let safetyScore = product $ map length [q1Ps, q2Ps, q3Ps, q4Ps]
    print safetyScore

-- 13
-- 50+20 = 70
-- 100+16 = 116

normaliseStr = unlines . lines

day15part2 = do
    contents <- readFile "day15 (data).csv"
    world3PatternStr <- readFile "world3.txt"
    treePatternStr <- readFile "tree.txt"
    let (width, height) = (101,103)
    
    let initBots = readBots contents
    -- mapM_ print initBots
    
    let showWorld bots = do
            showAsciiWorld height bgChar maskToChar pointsToChar nameZOrder asciiWorld
          where asciiWorld = AsciiWorld
                    { asciiWorldMasks = M.empty
                    , asciiWorldPoints = M.fromList [("Bots", map (toPoint . snd) bots)]
                    , asciiWorldWidth = width }
                
                bgChar = '.'
                maskToChar = id
                pointsToChar = const 'B'
                nameZOrder = compare
    
    let futureBots = iterate (map (modWalls width height . run)) initBots
    
    let Just step = findIndex (\w -> normaliseStr (showWorld w) == normaliseStr (treePatternStr)) (map head $ chunksOf 103 $ drop 12 $ futureBots)
    
    print (12 + step*103)
    
    -- print $ findIndex (\w -> normaliseStr (showWorld w) == normaliseStr world3PatternStr) futureBots
    -- putStrLn $ showWorld (futureBots !! 3)
    -- putStrLn $ showWorld (futureBots !! (12+79*103))
    -- printWorld (futureBots !! 69)
    -- printWorld (futureBots !! 115)
    -- printWorld (futureBots !! 126)
    -- printWorld (futureBots !! 172)
    -- printWorld (futureBots !! 218)
    -- printWorld (futureBots !! 321)
    
    -- mapM_ printWorld futureBots

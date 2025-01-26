#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 --package QuickCheck-2.14.3

-- To Do:
-- Make partitioner which gives every disconnected part of a layer it's own name by tacking on the next number not already existing in the bit mask map
--      Have the disconnected portions found by a flood fill algorithm using bit mask operations:
--          Loop until all points in world have been sufficiently checked to account for all part of all layers (need a nice way of checking this)
--              find an active point from the layer not so far a member of any partition part
--              Loop until latest found points are empty
--                  find new points by 'and'ing the latest found points in shifted up, down, left and right positions with the "visited" bit mask and 'or'ing them together
--                  xor these points (to subtract them) from the "visited" bit mask and make them the new "latest found points"
-- Make "WalkableWorld" with max walk distance fed in at construction to then add that much margin and so be able to detect reachability effects up to that distance.
-- Make non-zero bit with highest vertical position component tracked by data structure.

module BWorld ( BWorld(..)
              , emptyBWorld
              , readBWorld
              , showBWorld
              , printBWorld
              , combineTwoBWorlds
              , combineBWorlds
              , hasPoint
              , moveBitMaskInBWorld
              , movePointInBWorld
              , subtractBitMask
              , andBitMask
              , orBitMask
              , xorBitMask
              , setPoint
              , insertBitMaskAtPoint
              , isOverlappingBitMasks ) where

-------------
-- Imports --
-------------
import Data.List (sortBy, groupBy, delete, find, transpose)
import Data.List.Split (chunksOf)
import qualified Data.Map as M
import Data.Maybe (fromJust, catMaybes, fromMaybe)
import Data.Ord
import Data.Function
import Data.Bits
import Control.Monad (guard, join)
import Data.Monoid
import Data.Foldable
import Safe (atMay)

import Util (replace)
import BitMask (Point, BitMask, pointToIndex, pointToBitMask, moveBitMask, movePoint, isOverlapping, bitwiseSubtract, bitwiseAnd, bitwiseOr, bitwiseXor, up, dn, lt, rt, allDirs)

-- Each obj has a shape encoded as bits of an Integer.

data BWorld
    = BWorld { bWorldBG :: Char
             , bWorldBitMasks :: M.Map String BitMask
             , bWorldPoints :: M.Map String Point
             , bWorldWidth :: Int } deriving (Show)

emptyBWorld :: Char -> Int -> BWorld
emptyBWorld bgChar width = BWorld bgChar mempty mempty width


-- Assumes all rows have equal length
readBWorld :: Char -> [Char] -> String -> (Int,BWorld)
readBWorld bgChar singularChars inStr
    = ( height
      , BWorld { bWorldBG = bgChar,
                 bWorldBitMasks = foldr addToBitMask M.empty $ filter (\(char,_) -> not (char `elem` singularChars)) char2Ds,
                 bWorldPoints = singularPoints,
                 bWorldWidth = width } )
       
  where rows = lines inStr
        height = length rows
        width
          | height == 0 = 0
          | otherwise   = length $ head rows
        char2Ds = readChar2DsFromRows rows
        singularPoints = M.fromList . concat
                                    . map (\(c, positions) -> map (\(i,pos) -> (c : show i, pos)) (zip [0..] positions))
                                    . map (\c -> (c,) . map snd . filter (\(c', (x,y)) -> c' == c) $ char2Ds)
                                    $ singularChars
        
        readChar2DsFromRows :: [String] -> [(Char, (Int,Int))]
        readChar2DsFromRows rows = do
            (y',row) <- zip [0..] rows
            (x,char) <- zip [0..] row
            
            let y = height - 1 - y'
            
            guard $ char /= bgChar
            
            return (char, (x, y))

        addToBitMask :: (Char, (Int, Int)) -> M.Map String BitMask -> M.Map String BitMask
        addToBitMask (char, (x, y)) = M.alter (setBitInBitMask (x, y)) [char]

        setBitInBitMask :: (Int, Int) -> Maybe BitMask -> Maybe BitMask
        setBitInBitMask (x, y) maybeBitMask = Just $ setBit (fromMaybe 0 maybeBitMask) (y * width + x)


showBWorld :: Int -> (String -> String -> Ordering) -> BWorld -> String
showBWorld height labelZOrder bWorld = unlines . reverse . take height . chunksOf width . map (fromMaybe bgChar) $ listOfMaybeCharsFromBitMasksAndPoints
  where (BWorld bgChar layers points width) = bWorld
        listsOfMaybeCharsFromBitMasks = prioritize labelZOrder $ M.mapWithKey (\label n -> layerToMaybeChars label n) layers
        listOfMaybeCharsFromBitMasks = combineMaybeCharLists listsOfMaybeCharsFromBitMasks
        labelsAndPoints = map head . groupBy ((==) `on` snd) . sortBy (\(aLabel,aPos) (bLabel,bPos) -> compare aPos bPos <> compare aLabel bLabel) . M.toList $ points
        labelsAndIndices = map (fmap (pointToIndex width)) labelsAndPoints
        
        labelToChar [] = ' '
        labelToChar (x:xs) = x
        
        listOfMaybeCharsFromBitMasksAndPoints = foldr (\(label,i) acc -> let maybeOld = join (acc `atMay` i) in replace acc (i, Just $ minimum $ catMaybes [maybeOld, Just (labelToChar label)])) listOfMaybeCharsFromBitMasks labelsAndIndices
        
        combineMaybeCharLists :: [[Maybe a]] -> [Maybe a]
        combineMaybeCharLists = map (getFirst . fold . map First) . transpose
        
        prioritize :: Ord a1 => (a1 -> a1 -> Ordering) -> M.Map a1 a2 -> [a2]
        prioritize labelZOrder = (\m -> map (fromJust . flip M.lookup m) (sortBy labelZOrder $ M.keys m))
        
        layerToMaybeChars :: String -> Integer -> [Maybe Char]
        layerToMaybeChars label n = map (\i -> if n `testBit` i then Just (labelToChar label) else Nothing) [0..]

printBWorld :: Int -> (String -> String -> Ordering) -> BWorld -> IO ()
printBWorld height labelZOrder bWorld = putStrLn $ showBWorld height labelZOrder bWorld


-- Testing
exampleBWorld1 :: BWorld
exampleBWorld1 = BWorld '.' (M.fromList [("U",3)]) (M.fromList [("U",(7,7))]) 10

exampleBWorld2 :: BWorld
exampleBWorld2 = BWorld '.' (M.fromList [("U",96)]) (M.fromList [("U",(0,6))]) 10

exampleBWorld3 :: BWorld
exampleBWorld3 = exampleBWorld1 `combineTwoBWorlds` exampleBWorld2

exampleBWorld4 :: BWorld
exampleBWorld4 = movePointInBWorld "U" (1,1) $ exampleBWorld3

examplePrint1 = printBWorld 10 (comparing id) exampleBWorld1
examplePrint2 = printBWorld 10 (comparing id) exampleBWorld2
examplePrint3 = printBWorld 10 (comparing id) exampleBWorld3
examplePrint4 = printBWorld 10 (comparing id) exampleBWorld4


-- Assumes bWorlds are same size
-- Left-biased such that the background character and any singular points they share are taken from the left
combineTwoBWorlds :: BWorld -> BWorld -> BWorld
combineTwoBWorlds w1 w2
    = w1 { bWorldBitMasks = M.unionWith combineBitMasks (bWorldBitMasks w1) (bWorldBitMasks w2),
           bWorldPoints = M.unionWith combinePoints (bWorldPoints w1) (bWorldPoints w2) }
  where combineBitMasks :: BitMask -> BitMask -> BitMask
        combineBitMasks points1 points2 = points1 .|. points2
        
        combinePoints :: Point -> Point -> Point
        combinePoints point1 _ = point1

combineBWorlds :: [BWorld] -> BWorld
combineBWorlds = foldr1 combineTwoBWorlds

hasPoint :: String -> Point -> BWorld -> Bool
hasPoint label point bWorld = inPoints || inBitMasks
  where
    inPoints = case M.lookup label (bWorldPoints bWorld) of
        Just p -> p == point
        Nothing -> False

    inBitMasks = case M.lookup label (bWorldBitMasks bWorld) of
        Just bits -> testBit bits (pointToIndex (bWorldWidth bWorld) point)
        Nothing -> False

moveBitMaskInBWorld :: String -> (Int,Int) -> BWorld -> BWorld
moveBitMaskInBWorld label (dx,dy) w = w {bWorldBitMasks = M.update (\pts -> Just $ moveBitMask width (dx,dy) pts) label (bWorldBitMasks w)}
  where width = bWorldWidth w

movePointInBWorld :: String -> (Int,Int) -> BWorld -> BWorld
movePointInBWorld label (dx,dy) w = w {bWorldPoints = M.update (\pt -> Just $ movePoint width (dx,dy) pt) label (bWorldPoints w)}
  where width = bWorldWidth w

copyBitMaskInBWorld :: String -> String -> BWorld -> BWorld
copyBitMaskInBWorld srcLabel destLabel w = fromMaybe w $ do
    bitMask <- M.lookup srcLabel (bWorldBitMasks w)
    return $ w {bWorldBitMasks = M.insert destLabel bitMask (bWorldBitMasks w)}

modifyBitMaskWithBitMask :: (BitMask -> BitMask -> BitMask) -> String -> String -> BWorld -> BWorld
modifyBitMaskWithBitMask op label1 label2 w
    |   label1 `M.member` bWorldBitMasks w
     && label2 `M.member` bWorldBitMasks w = w {bWorldBitMasks = M.insert label1 newBitMask (bWorldBitMasks w)}
    | otherwise = w
  where bitMask1 = fromJust $ M.lookup label1  (bWorldBitMasks w)
        bitMask2 = fromJust $ M.lookup label2 (bWorldBitMasks w)
        newBitMask = bitMask1 `op` bitMask2

subtractBitMask = modifyBitMaskWithBitMask bitwiseSubtract
andBitMask      = modifyBitMaskWithBitMask bitwiseAnd
orBitMask       = modifyBitMaskWithBitMask bitwiseOr
xorBitMask      = modifyBitMaskWithBitMask bitwiseXor

exampleOfBitMaskOperation1 = (\(height,world) -> printBWorld height (comparing id) world)                             $ fmap (moveBitMaskInBWorld "+" (0,3) . xorBitMask "+" "*" . moveBitMaskInBWorld "+" (0,1) . copyBitMaskInBWorld "*" "+") $ readBWorld '.' [] (unlines [".....",".....",".....",".....",".***.","..*..","....."])
exampleOfBitMaskOperation2 = (\(height,world) -> print . popCount . fromJust . M.lookup "+" . bWorldBitMasks $ world) $ fmap (moveBitMaskInBWorld "+" (0,3) . xorBitMask "+" "*" . moveBitMaskInBWorld "+" (0,1) . copyBitMaskInBWorld "*" "+") $ readBWorld '.' [] (unlines [".....",".....",".....",".....",".***.","..*..","....."])

exampleWorld5 = readBWorld '.' "X" "XXXYYIYYYYRRRRRRRRRRRRJJJJJJJJBBBBBBBYYYYYYYYYYYYYEEEEERRRRRRCCCCCCCCCCCCCCCCCAAAAAAAAAAAAGGGGGGGGGGGGGGDDDHHHHHHHHHGRGFFGGVGGMMMMBBBBBBBBBB\nXYXYYYYYYYRRRRRRRRRRRRRRJJJJJJBBBBBBBBYYBYYYYYYYYYYEEEEEERRRRRCCCCCCCCCCCCCCAAAAAAAAAAAAAAGGGGGGGGGGGGGGDDDDHHHHHHGGGRGFGGGGGMGMBMBBBBBBBBBB\nXYYYYYYYYYYRRRRRRRRRRRRJJJJJJBBBBBBBBBYYBBYYYYYYYYYYEEAAARRRRCCCCCCCCCCCCCCCAAAAAAAAAAAAAGGGGGGGGGGGGGGGIDDDHHHHGGGGGGGGGGGGGGGGBBBBBBBBBBBB\nIKKYYIYYYYRRRRRRRRRRRRRJJJJJJBBBBBBBBBBBBBBYYYYYYYYEEEEAARRRRRCCCCCCCCCCCCCCCCAAAAAAAAAAAGGGGGGGGGGGIIIIIDDIHHHGGGGGGGGGGGGGGGGGBBBBBBBBBBBB\nIIKKIIYYYYYRRRRRRRQRRRJJJJJJJBBBBBBBBBBBBBBBYYYYYYYEEEERRRRRRCCCCCCCCVCCCVVICCAAAAAAAAAAAGGGGGGGGGGIIIIIIIIIIIHHGGGGGGGGGGGGGGGGBBBBBBBBBBBB\nIIKKIIYYYYYYRRRRRRRRJJJFFJJJJJJBBBBBBBBBVVBBYYYYYYYEEERRRRRRCCCCCCCCCVVCCVVVVAAAAAAAAAAAAAGGGNGGGGGGIIIIIIIIIIHGGGGGGGGGGGGGGGGBBBBBBBBBBBBB\nIIIIIIYYYYYYYRRRRRJJJJJFFFFJLJJJJBBBBBBBBVBBYYYYYYYYEERRRRRRRCCCCCCCPPVVNVVVWAAAAAAAAAAAAAGGGGGGGNGGGIIIIIIIIHHGGGGGGGGGGGGGGGBBBBBBWBBBBBBB\nIIIIIIIYYYYYYYRRRRJJFFFFFFLLLJJJJJBBBBBBBBGGGYYYYYYYGGRRRRRRRRCICCVVPVVVVVVVVAAAAAAAAAAAAAAGGGGGGNNNNNIIIIIIIIHHHGGGFEEEGGGGGGJJBBBWWBBBBJBB\nIIIIIYIYYYYYYYRRGRJFFFFFFFLSLLLLJJBBBBBGBBBGGGYYYYYYGGRRRRRRRRRICCFVVVVVVVVVAAAAAAAAAAAAAYAYYYYNUNNNNNIMIIIIIIIIHHHGFFFEEGGGGGJJJJJSSBBBBBIB\nIIIIIYYYYYYYYYGRGRJFFFFFFLLLLLLLLJSSSFBGBBBGGGYYYYYGGGFRRRRBRRRRRVVVVVVVVVVVAAAAAAAAAAAAYYYYYYNNNNNNNNIIIBIIIIIHHHHFFEEEGGGEJJJJJSSSMMSBBMSS\nIIIIIIYYYYYYYYYRGRFFFFFFLLLLLLLLSSSSSSGGGGGGGGYYGYGGGGGGRWRRRRRRRVVVVVVVVVVVVVAAMAAAAAAAAYYYYYNNNNNNNNNIIIIIIIIHHHHFFFEEEEGEJJJJJSSSSSSSSSSS\nIIIIIIYYYYYYYTYRFFFFFFFFFFFLLLLRRRSSSSGGGGGGYYYYGGGGGGGGGRRRRRRRVVVVVVVVVVVVGGAAAAYAAAAAAYYYYYNNNNNNNNNNNIIIIIOHHHHFFEEEEEEEJJJJJJSUSSSSSSSS\nIIIIIIIYYYYYYTTTFFFFFFFFFFFLLRLRRRJSSSGGGGGGGYGGGGGGGGGGAAARRRRRRVVVVVVVVVVIYYYYYYYAYYYAAYYYYNNNNNNNNNNNNIIIIIHHFHFFFEEEEEEEETJJJJSUTSSSSSSS\nIIIIIIIYYYYYYYTTFFFFFFFFFFFLFRRRRJJSGGGGGGGGXGGGGGGGGGGGAAAAARRVVVVVVVVVVVVIYYYYYYYYYYYYYYYYYNNNNNNNNNNNNIIIFFFFFFFFEEEEEEEETTTTTTTUTSSSSSSS\nIIIIIIIIYYYYYTTTFFFFFFFFFFFFFFRRRRJGGGGGGGGXXXXGGGGGGGGGAAAAQQQQQQQGGEEVVVVVYYOYYYYYYYYYYYYYYYNNNNNNNNNNNNNIFNNNFFFVVGGEEEEETTTTTTTTTTTSSSSS\nIIIIIIIIYYYYYTTTFFFFFFFFFFFFFJJJJJJJGGGGGXXXXXXXGGGGGGGGGAAQQQQQQQQGGEEAVVVVVOOOYYYYYYYYYYYYEYEEANNNNNNNNNNNNNNNFFFVVVGEEEEEEETTTTTTPTSSSSSS\nIIIIIIIIYYYTTTFFFFFQQFFFFFFFJJJJJJGGGGGXXXXXXXXGGGGGGGGDQQQQQQQQQQQQQEEEEOOOOOOYYYYYYYYYYYYYEEEEESNNNNNNNNNNNNNNFFFVVVVVVEEEEKTVTTTTTTSSSSSS\nIIIIIIIYYYYYYYQQIFSQQFFFFFFJJJJJJJJGGGGGXXXXXXXGGGGGGGGDMQQQQQQQQQQQEEEEEOOOOOOOOYYYYYYYEEEEEEEEEENNNNNNNNNNNNNNFFFVVVVVVVDEEVQVTTTTSSSSSSSS\nIIIIIIYYYYKYQZQQQQQQQQQFFJJJJJJJJJJJJGGGXXXXXXXGGGSSSSGDDQQQQQQQQQQEEEEEEOOOOOOYYYMMMEEEEEEEEEEEEEFFFNNNNNNNNNNFFFVVVVVVVVVRRVVVTTTTTXSXXXXS\nIIIIUUUYUQQQQQQQQQQQQQFFJJJJJJJJJJJJJGGJXXXXXXXGGGGGDDDDDQQQQQQQQQLVEEEEEOOOOJJJYYYMMEEEEEEEEEEEEEFFFFFFFNNNNFFFFVVVVVVVVVVVVVVVTTDTXXXXXXXX\nIIUUUUUYUUQQQQQQQQQQQFFFJFJYYJYJJJJJJJJJJXXXXXXGGGGGDDDDDDQQQQQQQLLEEEEEEEOOOJJJZZZMMEEEEEEEEEEEEEEEEFFFFFNNFFFFFVFVVVVVVVVVVVVVTTDXXXXXXXXX\nIIUUUUUUUFIIIQQQQQQQQQFFFFPYYYYJJJJJJJJJJVXNNNNNGGGCDDDDDDLDDWWWQLLEEEEEEEOOOOJZZZZMMMEEEEEEEEEEEEEEIZZZZFNFFFFFFFFFVVVVVVVVVVVVQXXXXXXXXXXX\nIUUUUUUUUFFIIIQQQQQQQQQFFYYYYYYJJJJJJJJJJVDDNNNNGGTCDDDDDDDDDDWQQLLLLEEEEEOOOOOKKZZZMMEEEEEEEEEEEEEWZZZZZFFFFFFFGFFFFVVVVVVVVVVVXXXXXXXXXXXX\nUUUUUUUUFFIIIIIQQQQQQQIFFFYYYYZJJJJJJJJJZDDDDNDNNCCCDDDDDDDWWWWWWLLLLLFEEEZZZOZZZZMMMMMEMEEEEEEEIIEWZZZZZZFFFFFGGFFFVVVVVVVVVVVVXXXXXXXXXXXX\nUUUUUUUUUFIIIQQQQQQQIIIIIIIYYYZZJZJJJJJJZDDDDDDCCCCCCDDDDDWWWXXXWLLLLLFFEEZZZZZZZZZMMMMMMELEEEEIIIZZZZZZZZFFFFFFGGGGVVVVVVVVVVVVXXXXXXXXXXXX\nUUUUUUUUBBBIIQQQQTTTIIIIIIIYYYZZJZJJJJJZZZDDDDDDDCCCCCDDDDDWXXXXLLLLLLLFXXZZZZZZZZZMMMMMMEEEEEEIIIZZZZZZZZZZZZGGGGGGVVVVVVVVVVVVXXXXXXXXXXXX\nUUUUUUUUBBBBBLQQQTTTTIIIIIYYYYYZZZZZJJJZZZDDDDDDDCCCCCDCDDDDXXXXLLLLLLLFXXXZZZZZZZZMMMMMMEEEEEEIIZZZZZZZZZZZZZJJGGGGGVVVVVVVVVVMMXXXXXXXXXXX\nUUUUUUUUUUBBBBQQWTTWTIJIIYYYYYYZZLZZJJZZZZDDDDDDDDCCCCCCCDDDXXXXXLLLLLLFXZZZZZZZZZZMMMMMMMMEEEIIIZZZZZZZZZZZZZJJJJGGJVVHUVVVVVVMMXXXXXXXXXXX\nUUUUUUUUUHBBBBJWWWTWWIJJIYYYYYYZZZZZZZZZZZDDDDDDDDCCCCCCCCDDXXXXXXXLLLXXXXXZZZZPPPPPPPPPPPPPPIIIIIIZZZZZZZZZZJJJJJJJJVJUUVVVVMMMMXXXXXXXXXXX\nUUUUUUUUUHHHOWWWWWWWJJJJJJYYYYYZZZZZZZZZZZDDDDDCCCCCCCCCCCDDXXXXXXXLXXXXXXXXZZZPPPPPPPPPPPPPPMIIIEEAZZZZZZZZZZZZJJJJJJJUUMMMMMMMXXXMMXXXXXXX\nUUUUUUUUUHHHWWWWWWWWWWJJJJJYYYZZZZZZZZZZDDDDDDDCCKCCCCCCCCDDXXXXXXXXXXXXXXXXZZZPPPPPPPPPPPPPPPPPPPPPPZZZZZZZZZZZJJJJUUUUUMMMMMMMMMMMMMMMXMME\nUUUUUUUUHHHHWWWWWWWWWEEEEEEEEYPZZZZZZZZZDDDDDDDDDDCCCCCCCCDDXXXXXXXXXXXXXXXXXZZPPPPPPPPPPPPPPPPPPPPPPZZZZZZZZZZZJJJJUUUUUUMMMMMMMMMMMMMMMMMM\nBUUUUUUUUHHHWWWWWWWWEEEEEEEEPPPZZZZZZZZZZDDDDDDDDDDCMMMCJMMDMXXXXXXXXXXXXXXXEZZPPPPPPPPPPPPPPPPPPPPPPEEEZZZZZZZZJJJUUUUUUUMMMMMMMMMMMMMMMMMF\nBBBUUUUUUHHHWWHWWDWWEEEEEEEEPPPZZZZZZZZZDDDDDDDDDDDMMMMMMMMDMMXXXXXXXCXXXXXXEZZPPPPPPPPPPPPPPPPPPPPPPEEEZZZZZZZZZZJJUUUUUWMMMMMMMMMMMEEMMMMF\nBBBGTBBBBBBBBHHWDDEWEEEEEEEEPPPZZZZZZZVVDDDDDDDDDDMMMMMMMMMMMMXXXXXCXCCCCXXXEZZPPPPPPPPPPPPPPEEEEEEEEEEEEESZZZZZZZJJJUUUUWMMMMMMMMMMMEEEMMMF\nGGGGGBBBBBBBBHLWDDEWEEEEEEEEPZZZZZZZZIDDDDDDDDDDDMMMMMMMMMMEMYYYXXXCCCCCCXCZZZZPPPPPPPPPPPPPPPEEEEEEEEEEESSSZZZZZJJJJUUUUWWWMMMMMXXXEEEEEEEF\nGGGGGBBBBBBBBHHDDEEEEEEEEEEEEZZZZZZZZZCCDDDDDDDDMMMMMMMMMMMEYYXXXXXCCCCCCCCCZZWPPPPPPPPPPPPPPPEEEEEEEEEESSOOOZZJJJJJJJUUUUMMMMMMXXXXEEEEAAAA\nGGGGGBBBBBBBBHEEEEEEEEEEEEEEDDDDZZZZZZCCCCDDDDDDDMMLLMDMMMJYYYYJJXXCCCCCCCWWWWWWWZZPPPPPPPPPPPEEEEEEEEEEESOOIIJJJJJJJJUUJUMMMMMMXXXXXEEEEEEA\nGGGGGBBBBBBBBEEEEEEEEEEEEEDDDDDDZZZZZICCCCCFDDDFDMLLLLMMMJJJJJJJJXXCCCCCWWWWWWWWZZZPPPPPPPPPPPMMEEEEEEESSSOOIIIJJJRJJJJJJJMMVMMXXXXXEEEEEEAA\nGGGGGBBBBBBBBREEEEEEEEEEEEDDDDDDZZCCCCCCCCCFFSSFMMLLLLLMMMMMJJJJJXXCCCCCCWWWWQQQQQZPPPPPPPPPPPZZZEEESSSSOOOOOOOJJJRJJJJJJJVVVVMXXXXEEEEEEEAA\nGGGGGGGGBBBBCRREEEEEEEDDEEDDDDDDDZZOCCCCCCCFFFFFMMLLLFMMMMKKJJJJJCCCCCCCCWWWQQQQQQZPPPPPPPPPPPZZZZSSSSSOOOOOOOOOJJRRJJJJJVVVVVVVVXEEEEEEEEEA\nGGGGGGAABBBBRRRRRRRRDDDDDDDDDDDDDCCCCCCCCCCFFFFFFFFFFFJMMMKKKKJJJJCCCCCCWWQQQQQQQQQPPPPPPPPPPPZZVVVVSSOOOOOOOORRRRRRRRJJJVVVVVVVVBEEEEEEEEAA\nGGGAAAAABBBBRRRRRRDDDDDDDDDDDDDDDCCCCCCCCCCCCFFFFFFFFQQQMKKKJJJJJJJJCCCCWQQQQQQQQQQZZZZZZZZPPPZZTVVSSOOOOOOOOORRRRRRRRJJJVAAVVVVVBEEEEEEEEEA\nGGAAAAAAAAAARRRRRDDDDDDDDDZZZZDZZZCCCCCCCCCCCFFFFFFQQQQQQKKKJJJJJJJJCWCWWQQQQQQQQQQQZQVVZZZPPPVVVVVOOOOOOOOOOORRRRRRRRRRJVVVVVVVVVMEEEEEEAAA\nGGAAAAAAAARRRRRRRRDDDDDDDZZZZZZZZCCCZCCCCCCCCFFFFFFFQQQQQKKKJJJJJJJJWWWWHHQQQQQQQQQQQQQQZHQPPPZVVVVVKOOOOOOOOOORRRRRRRRRVVVVVVVVVVVVEEEEEAAA\nGAAAAAAAAARRRRRRRRDDDDDDDZZZZZZZZZZZZCCCCCCCFFFFFFFFFQQQQQKKJWJJJJJJJWWHHHQQQQQQQQQQQQCQQQQQZGZQQVVVVOOOOOOOOOORRRRRRRRRVVVVVVVVVVVVVEEEAAAA\nGAAAAAAAAARRRRRRRRDDDDDDZZZZZZZZZZZEZCCCCCCFFFFFFFFFFQQTTTWWZWJWJWJWWWWHHHQQQQQQQQQQQQCQQQQQZZZQVVVVVVOOOOOORRRRRRRRRRRVVVVVVVVVVVVAAAAEAAAA\nGGAGAAAAAARRVVRRRRDDDDDDZZZZZZZZZZZECCBCCCCCFFFFFFFFJQQQWWWWWWWWWWWWWWWHHHHHQQQQQQQQQCCQQQQQQQQQQQVVVVVOOOORRRRRRRRRRRRRVVVVVVVVVVVAAAAAAAAA\nGGGGAAAAAAAAAVRIDDDDDDDDZZZZZZZZZZZBQBBBCCFFFTFFSFFFJQKKKWWWWWWWWWWWWWHHHHBQQQQQQQQQQQQQQQQQQQQQQQQVVVVOOOOOOOORRRRRRRXRRRVVVVVVVVAAAAAAAAAA\nGGWWAAAAAAAAAAIILDDDDDDDJZZZZZZZZZZBBBBBCCFFFFFFSEEEEEKKKKWWWWWWWWWWWHHHHHHQQQQQQQQQQQQQQQQQQQQQQQQQSVVOOIOIOOOWRRRRRRXRRVVVVVVVVVVAAAAAAAAA\nWWWWAAAAAAAAAIIILLDDDDDAAXZZZZZZZZBBBBBBBCCCCFCCEEEEEKKKKKWWWWWWWWWWWHHHHHHHQQQQQQBQQQQQQQQQQQQQQQQQQVOOOIIIIOWWRUXXXRXRRVVVVVVVVVVVVAAAAAAA\nWWWAAAAAAHAAAFFFFFFFFAAAAAZZZZZZZBBBBBCCCCCCCCCCEEEEEEKKKKWWKKRWWRRRHHHHHHHHQQQQQQBQQQQQQQQQQQQQQQQAQOOOOIIIOOWWWUXXXXXXXVVVVVVVVVVAAAAAAAAA\nWWNNFAAAAHHHHFFFFFFFFAAAAAZZZZZZZBBBBBBCCCCCCCCCEEEEEEKEKKWKKKRRRRRRRPPCCHHCCCCCQBBBHHQQQQQQQQQQQQOODOOOOIIIWWWWWXXXXXXXXVVVVVVAAAAAAAAAAAAA\nNNNNNAAAAXHHHFFFFFFFFAAAAAAZZZZZZZZZBBBCCCCCCCECZEEEEEEEKKKKKKRRRRRRRPPCCCCCCCCCCBBBHHHQQQQQQKQQQMOOOOOOOOWWWWWWWXXXXXXXXVVAAVVAAAAAALAAALLA\nNNQNNAAAASSSSFFFFFFFFFFFFFAAAAZZZZBBBBBCCCCECEEEEEEEEEEEEKRRRRRRRRRRRPPCCCCCCCCCCBBBBHHHHHQQQQQMMMMMMOOOOWWWWWWWWXXXXXXXXXVAAAAAAAUALLAAALLL\nNNNNNQNNNSSSIFFFFFFFFFFFFFAAAAZZZZZBBBBCCCCEEEEEEEEEEEEEEKRRRRRRRRRRRPPPCCCCCCCCCHBBHHJHJJJMQMMMMMMMMOOYOYWWWWWXXXXXXXXXXXAAAAAAAAALLLLLLLLL\nNNNNNNNNNYFFFFFFFFFFFFFFFFAAAAZZZZBBLBBCCEEEEEEEEEEEEEEKKKRRRRRRRRRRQQQQQCCCCCCCHHBBHHJJJJMMMMMMMYYMMMYYYYYYWWWWXXXXXXXXXXAAAAAAAAALLLLLLLLL\nNNNNNNNNNNFFFFFFFFFFFFFFFFAAAAAAAZZMLLMCCCEEEEEEEEEEEEEEKKRRRRRRRROOQQQQQCGCCCHHHHHHHHHJJJMMMMMMMYYYYYYYYYYYWWWWXXXXXXXXXLLLLLLLLLLLLLLLLLLL\nNNNNNNNNNYFFFFFFFFFFFFFFFFAAAAAAAAAMMMMEEEEEEEEEEEEEEKKKKRRRRRRRRRQOQQQQQQGCCCCHHHHHHJHJJJMMMMMMMYYYYYYYYLLYJNWWWXXXXXXXXLLLLLLLLLLLLLLLLLLL\nNNNNNNNNNNFFFFFFFFFFFFFFFFAAAAAAAIMMMMMMBEEPEEEEEEEEEEKKKKRRRRRRRXQQQQQQQGGCCCSSHHHHJJJJJJJMMMMMMMYYYYYYYYYNNNNWXXXXWXWWWLLLLLLLLLLLLLLLLLLL\nOOONNNNNYYFFFFFFFFFFFFFFFFAAAAAMMMMMMMMMMBBBEEEEEEEEEEKKKKRRRRRQQQQQQQQQQCCCCCSSHHHHHJJJJJJJMMMMMMYYYYYYYYNNNNNWWWWXWWWWWLLLLLLLLLLLLLLLLLLL\nOOONNNNYYYFFFFFFFFFFFFFFFFAMMMMMMMMMMMMMBBBBBEEBEEBEEEKKKKRRRRRRRRQOQQQQQQCCPCPSHHHHHJUJJJJJJMMMMMYYYYYYYYNNNNWWWWWWWWWWWLLLLLLLLLLLLLLLLRRL\nOOOONNJJYYFFFFFFFFFFFFFFFFAMMMMMMMMMMMMMBBBBBBBBEBBBEEKKKKKRWWWRRRROOOSSSSCCPPPSSHHHHJJJJJJJJJMMMMMYFYYYYNNNNNNWWWWWWWWWWLLLLLLLLLLLLLLLRRRL\nOOOONNNJJJDDDDDDFFFFFFFFFFFFFDDMMMMMMMMMMMBBBBBBEBEBEEKKKKWWWWWWOORROOSVSSPPPPSSSSSSSJVJJJSRRMMMMMMYYYYYNNNNNNNNNWWWWWWWWLLLLLLLLLLLLLLLRRHH\nNNNNNNNDDDDDDDDDDDDDLLLLFFFFFDDMMMMMMMMMMMBBBBBBBBEEEEEKKWWWWWWWOOOOOOSSSSSPSSSSSYSSSVVJJJSRRRMRMRRNNNYNNNNNNNNNWWWWWWWNNLLLLLLLLLLLLLLHHHHH\nNNNNNNNDDDDDDDDDDDDDDDDDFFFFFDDMMMMMMMMMMMMMMMMBBBBBBBEEKWWWWWWWOOOSSSSSSSSSSSSSSSKSSVVVJSSRRRRRRRRNNNNNNNNNNNNNWWWWWNWNNLLLLLLLLLLLLLHHHHJH\nNNNNNNNDDDDDDDDDDDDDDDDDFFFFFDDRRMMMMMMMMMMMMMMBBBBBBBBEWWWWWWWWXXSSSSSSSSSSASSSSSSSSRRVVRRRRRRRRRRXNNNNNNNNNNNNWWWNNNNNNNNLLLLLLLLLLLHHHHJH\nNNNNNJJJJJJJJJJJJDDDDDDDFFFFFDDRRMMMMMMXXXXXXMMMXXXBBBBBWWWWWCCWXXSSSSSSSSSSSSSSSSSSSRRVVRRRRRRRRRRRNNNNNNNNNNNNWWWWNNNNNNNLLLLLLNNNNNHHHHHN\nNNNNJJJJJJJJJJJJJDDDDDDDFFFFFDDYRRMSMMMXXXXXXBMMCXXBXXBBWYWWWCCXXXCSSSSSSSSSUUUUUSSSSRRRRRRRRRRRRRRNNNNNNNNNNNNNWWWWNNNNNNNLLLLLLNNNNNNHHHHH\nJJJJJJJJJJJJJJRJJDDDDFFFFFFFFDDYRRMMMMMXXXXXXXXXXXXBXXBXYYWWCCCXXXCCSSSSSSSSUUUUUSSSSRRRRRRRRRRRRRRNNNNNNNNNNNNWWWNNNNNNNNNLLLLLLPPPPPNNHHFH\nJWWWWWWWWJJJJRRRJDDDDFFFFFFFFDDYRRRMMMMXXXXXXXXXXXXXXXXXXXCCCCCCXXXCSSSSSSSSUUUUUSSSSSRRRRRRRRRRRRNNNNNNNNNNNNNNNWWNONNNNNNLLLLLLPPQQHHHHHFF\nJWWWWWWWWJJJRRRRRLYYYFFFFDDDDDDYRRRRRRXXXXXXXXXXXXXXXXXDXXCCCCCXXXCCCSSSSSSSUUUUUSSSSDKKKRRRRRRQRRRDNNNNNNNNNNNNWWWWNNNNNNNLLLLLLPPPQQFHHHHF\nJJWWWWWWWWWJRRRRRRYYYFFFFFDDDDDRROOOOOOOOOXXXXXXXXXXXXXXXXCCCCXXXXCCCCSRRRSSUUUUUSSSSDKRRRRRRRRDDDDDDNNNNNNNNNNSWWSWNRNRNNNLLLLLLPPSFFFHFFFF\nJWWWWWWWWWRRRRRRRRRRYFFFFFDDDDDYYOOOOOOOOOXXXXXXXXXXXXXXXXCCCCCCCCCCCCCFFRSKUUUUUDDDDDDDDRRRRRRDDDDDDDDDDDNNSSSSSSSSSRRRNNNNPPPPPPPSFFFHFFFF\nJWWWWWWWWWRRRRRRRRRRYFFFFFDDDDDYROOOOOOOOOXXXXXXXXXXXXXCCCCCCCCCCCCFFFFFFRRSUUUUUDDDDDDDDDDDDRRDDDDDDDDDDDDDDDSSRRRRRRNNNNNWWPPPPPPPFFFFFFFF\nWWWWWWWWRRRRRRRRRRRYYFFFFFDDDDDRROOOOOOOOOXXXXXXXXXXXXXXCCCCCCXCCCCFFFFFFRSSUUUUUMDDOIDDDDDDDDDDDDDDDDDDDDDDDDSSRRRRRRRNNNNWWWPTPPPPPFFFFFFF\nWWWWWWWWWRRRRRRRRRRRYFFFFFDDDDDOROOOOOOOOOXEEVEXXXXXXXXXXCXXXXXXCCCFFFFFFFOSUUUUUMMDIIDDDDDDDDDDDDDDDDDDDDDDDDDPPPRRRRRRRRRTTTTTPPPPPFFFFFFF\nXXWWWWWWWRRRRRRRRRRRYYFFFFDDDDDRROOOOOOOOOXEEVEEEXXXXXXXXCXXXXXCCCCCCFFFFFFGGGSSMMMDIIDDDDDDDDDDDDXDYYDDDDDDPPPPPPRRRRRRRRRTTTTTTPPPPFFFFEFF\nXXWWWWWWWRIIRRRRRRRKJJFFFFFFFFORROOOOOOOOOXEEEEEEXXXXXXXXXXXXXXXXFCFFFFFFFFFGGGMMMMMIIIDIIDDIDDDDDDDYYDDDDYDPPPPPPRRRRRRRRRTTTTTTTPPEFFFEEEE\nXWWWWWWWWIIRRRRRRRRROJFFFFFFFFOORRRRRRRREEEEEEEEEXXXXXXXXXXXXXXFFFFFFFFFFFFFFGMMMMMIIIIIIIDIIDDDDDDFYYYDDDYPPJPPPPRPPPRRRRRRRTTTTPPPEEFEEEEE\nXXDDWWWWWWIRRRRRRRJJJJFFFFFFFFOOOOORRRRREEEEEEEEEEXXXXXXXXXXXXXXXFFFFFFFFFIGGGMMIIIIIIIIIIIIIIDDFFFFYYYYYYYYYJYPPPPPPPPRRRRTTTTTTTTEEEEEEEEE\nDXDDDDWWWWIRRRRRSRJJJJFFFFFFFFOOOOOORRREEEEEEEEEEEHXHXXXXXXXXXXFFFFFFFFFFGGGGGMMIIIIIIIIIIIIIIKKFYYYYYYYYYYYYYYPPPPPPPRRRRTTTTTTTTTEEEEEEEEE\nDDDDDDDWWWRRRRRRRRJJOOFFFFFFFFOOOOMMRKREEEEEEEEEEEEVVVXXXXXXXXXXFFFFFRRRRGGGGGMMMIIIIIIIIIIIIIIKKYYYYYYYYYYYYYPPPPPPPRRRRRRTTTTTTTTEEEEFEEFE\nDDDDDDDWWWRRRRRRRRJJOOOOOOOOOOOOOOMMMEEEEEEEEEEEEVVVVIVXXXXXXXXXXXFFRRRRRRRRRXIIIIIIIIIIIIIIIKKKUYYYYYYYYYYYYYPPPIIORRRRRRRTTTTTTTEEGFFFFFFE\nDDDDDDWWWWWRRRRRRJJJJJOOOOOOOOOOOOONNLLLWWEEEEEEEVVVVVVFXXXXXXXXXXRFRRRRRRRRRXIIIIIIIIIIIIIIIKKKKKYYYYYYYYYYYYYYOOOOORRRDRRDDTTTTTTGGPFFFFFF\nDDDDDDDDWWWWWZZJJJJJJJJJOOOOOOOOOSONNWWWWWWEWEEVVVVVVFVFFFFXXXXXXXRRRRRRRRRRCRRIWIIIIIIIIIIIKKKKKKYYYYYYYYYYYYYYYGGORRRRDDDDDGGTGTTAGGFFFFFF\nDDDDDDDDDWWZZZZZZJJJJJJJOOOOOOOONNNNNNNWNWWWWEVVVVVVVFFFFFFXXXXXXXRRRRRRRRRRRRREIIIEIIIIIIIIKKKKKBBYYYYYYYYYYYYRYSGSSSRDDDDDDGGGGGGGGGFFFFFF\nDDDDDDDDDWZZZZZZZJJJJJJJJIOOOOOOOONNNNNNWWWWWWVVVVVVVFFFFFFFXXFXXXRRRRRRRRRRRREEEEEEIIIIIIKKKKKKKKYYHYYYYYYYYYHSSSSSSSSSDDDDDDGGGGGGGGFFFFFF\nDDDDDDWWWWWWZZZZZZZJJJJJJIOOODOOOOOFNNNNWWWWWWVVVVVVVFFFFFFFFFFXOOOOOOOORRRRRRRREEEIIIIIIIIIKKKKKKYYHYYYYYYYYHHSHSSSSSSSFDDDDDDGGGGGFGFFFFFF\nDDDDDDWWWZZZZZZZZZZZZJJJJIOOOOOFFOOFFNNNWWWWVWVVVVVVVFFFFFFFFFFXOOOOOOOORRRRRREEEEEEEIIIIIIKKKKKKKKKKYYYYYYYYHHHHSSSSSSSDDGDDSSGGGGGFFFFFFFF\nDDDDDDDDWWZZZZZZZZZZZZZIIIOFFFFFFFFFFOOOWWWVVVVVVVVVVFFFFFFFOOOOOOOOOOOOROOORROOOOEEEEIIIIIKKKKKKKKKKYYYYYYYYYHYSSSSSSSSGGGDDNSGGGGGUUFFFFFF\nDDDDDDDWWWZZZZZZZZZZZZBIIIOFFFFFFFOOOOOOZZVVVVVVVVVVVFFFFFFFOOOOOOOOOOOOROOOOOOOOOOEKEIKIIKKKKKKKKKKKYYKYYYYXYYYYSSSSSSGGGGGNNSGJJLLLLLFFFFF\nDDDDDDDWWZZZZZZZZZBBBBBBBBBFFOOOOOOOOOOOVVVVVVVVVVVVFFFFFFFFOOOOOOOOOOOORQQQAOOOOOOOKKKKKKKKKKKKKKNKKKYNYYYXXYYYSSSSSSSGGGGGNNLLLLLLLLLFFFFN\nDDDDDDWWWWWZGGGZZZBBBBBBBOBOOOOOOOOOOOOOOOVVVVVVVVVVFFFFFFFFOOOOOOOQQQQQQQQQAOOOOOOOKKKKKKKKKKKKNNNKKKKNNNNNXXYYXSSSSSSHGGGGNNNNLLLHLLLLGGFF\nDDDDDDDWWWWWGGGZZZBBBBBBBOBOOOOOOOOOOOOOOVVVVVVVVVFFFTTFFFFFOOOOOOOQQQQQQQQQQOZOOOOOOKFKKKKKKKKKKKNNNNNNNNNNXXYEXXXXXSXHHHHGHNLLLLLLLLLLLLFF\nDDDDDDDWWWWWWWGZBBBBBBBBBBBBBOOOOOOOOOOOOVVVVVVVVVVVVTTFFFFFOOOOOOOQQQQQQQQQQZZZOOOZOOKKKKKKKKKKKKNNNNNNNNXXNXXEEXXXXXXHHHHGHNNLLLLLLLLLBBBB\nDDDDDDDDWWWWWWGGBBBBBBBBBBBBBBOOOOOOOOOOOVVVVVVVVVVKFFFFFFFFOOOOOOOQQQQQQQQQQZZZZZZZZOBKKYKKKKKKSKNNNNNNNNNNNXXXEEXXXXHHHHHHHHNNLLLLLLLRRBRB\nDDUUDDDDWWWWWWGGNBBBBBBBBBBBBOOOOOOOOOOOKVVVVVVVVVKKKKALLLLLLAAAQQQQQQQQQQQQQZZZZZZZZUUYYYYKMMMKKKNNNNNNNNNNNXXXXXXXHHHHHHHHHNNNNLLLLLRRRRRR\nNDDDDDDDWWWWWWWWBBBBBBBBBBBBBBBOOOOOOOOOKKVVVVVVVVKKKZFLLLLLLFHHQQQQQQQQQQQQQZZZZZUUUUYYYYYLMMLLNNNNNNNNNNNNXXXXXXXHHHHHHHHHNNNNNLNLRRRRRRRD\nNDFFDDDDDWWWWWWMMBBBBBBBBRBBBBUOOOOOOOOOKKKKVVVVVKKKKKKLLLLLLFHHHQQQQQQQQQQQQZFFZZUUUULLLLYLLLLLLNNNNNNNNNNAXUXXXXXXXXHHHHHNNNNNNNNLRRRRRRDD\nNDMFFDWWWWWMMMMMMMBBBMMBBBBGOOOOOOOOOOOOOKKKKKKVVVKKKKKLLLLLLLLHQQQQQQQQQQQQQQYFUUUUUUFLLLLLLLLLNNNNNNNNNUUUUUUUXXXXXXHHHHHHNNNNNNRLRRRRRRRD\nMMMMFDWWWWWMMMMMMMXXMMMMBKMOOOOOOOOOOOOOOKKKKKKKKKLLLLLLLLLLLLLHQQQQQQQQQQQQUFFFFUUUUQQQLLLLLLLNNNNNNNNNNUUUUUUUUXXXXHHHWWWNNNNNNNRRRRRRRCDD\nMMMMMWWWWWWWMMMMMMMMMMMMMMMEOOOOOOOKKKKKKKKKKKKKSSLLLLLLLLLLLLLHHHHQQQQQQQQQFFFFFFFFLQQBQQLLLLLLLLNNUNNUSUUUUUUUUUXXXHWWWWWNNNNNNNNRRRRRRCCD\nMMMMMMWWWWMMMMMMMMMMMMMMMMMOOOOOXXOOOKKKKKKKKKKKSSLLLLLLLLLLLLLHHHHHQVQCFFQQQQFFFFFFFQQQQQLLLLLLLLNSSSSSSSUUUUPUUUXXXXWWWWWWWNNNNNNNRRRRRRCC\nMMMMMMMMWWWWWMMMMMMMMMMMMMMMOOFOOVVOKKKKKYYYYYYYYYLLLLLLLLLLLLLHHHLLVVLLFFFFQFFFFFFUQQQQQQLLLLLLLLNSSSSSSSUUUUUUUUXXXWWWWWWWNNNNNNNNNNCRRCCC\nMMMMMMMWWWWWWMMMMMMMMMMMMMMMMPPPPVVVKKKKKYYYYYYYYYLLLLLLLLLLLLLHLLLLLLLFFFFFFFFFFFFFQQQQQQQLLLPPLLGSSSSSSSUUUUUUUUVXXXXWWWWWNCNCNNNNCCCCCCCC\nMMMMMMMMWWMMMMMMMMMMMMMMMMMMMMPPPVIUKKKKUYYYYYYYYYLLLLLLLLLLLLLLLLLLLLLFFFFFFFFFFFJJQQQQQQLPPPPPLLUSSSSSSSUUUUUUUUXXXXXZWWWNNCCCNCNCCCCCCCCC\nMMMMMMMMMMSSSMMMMIIIIMMMMMMMMPPPPPUUUUUUUYYYYYYYYYLLLLLLLLLLLLLLLLLLLLLFFFFFFFFFFFQQQQQQQPPPPPPPUUUSSSSSSSUUUUUUGGXXGXZZZZZCCCCCCCNCCCCCCCCC\nMMMMMMMMMMSSMMIIMIIIIMMMMPPMPPPPPUUUUUUUUYYYYYYYYYLLLLLLLLLLLLLLLLLLLLLLLFLFFFFFFQQQQQQQQPPPPPPPUUUSSSSSSSUUUUUGGGXTGGGGZZZZCCCCCCNCCCCCCCCC\nMMMMMMMMMMSSMMIIIIIIIMSSSSSPPPPPPUUUUUUUKYYYYYYYYYLLLLLLLLLLLLLLLLLLLLLLLFLSSFFFQQQQQQBPQPPPPPPPPPPSSSSSSSUUVUGGGGGGGGGGGGZZCCCCCCCCCCCCCCCC\nMMMMMMMMMMIIIIIIIIIIIMSSSSSSSSSSSSHUUUUUUYYYYYYYYYYYYYSSSSSLLLLLLLLLLLLLLLLSFFVFYYQQYYPPPPPPPPPPPPTUUUUUUUUUVUGGGGGGGGGGGGGZZCCCCCCCICCCCCCF\nMMMMMMMMMTTIIIIIIIIIIISSSSSSSSSSSSUUUUUUUYYYYYYYYYYSSCSSSSSLLLLLLLLLLLLXLSSSDDFFYYQYYPPPPPPPPPPTTTTTVVUUUUUUVVGGGGGGGGGGSSGZCCCCCCCCCLCCCCCC\nQMMMMMMMMMIIIIIIIIIIIISSSSSSSSSSSSUUUUUUUYYYYYYYYYYCCCCSSSSLLLLLLLLLLLLLSSSSSSFYYYYYPPPPPPPPPPTTTTTVVVUVVVVVVGGGGGGGGGGGGGGZCCCCCCCCCCCMYYYM\nQMMMMMMMMMMIIIIIIIIIIISSSSSSSSSSSSUUUUUUUYYYYYYYYYYCCCCCSSSZSSSLLLNNNLLLNSSNSHHFFFFFFFFFPPPPPPTTTTTVVVVVQVVVVVGGRRGGGGGGGGCCCCCCCCCCCCMMMYMM\nQMMMMMMMMMMMIIIIIIIIIISSSSSSSSSSSSWUUUUUUUUUUCCCSCCCCCCCSSZZSSSLLLNNLLNNNNNNYYYFFFFFFFFFPPPPPBTTTTTTTVQQQQQQVVVGRRRGGGGGGGGRCCCCCCCRCCJMMMMM\nQQQMMQQQMMQQIIIGIIIIIISSSSSPPPPPPPWUUUUUUUUUUCXCCCCCCCCCCSSSSSSLLNNNLLNNNNNNNFFFFFFFFFFFPPPPTTTTTTTTTTIQQQQQQRGGRRRRRBBBBBBRRCCCCCCRRMMMMMMM\nQQQQQQQQMQQRIIRGIIIIIISSSSSPPPPPPPPPUUUUUUWUCCXXXCCACCCCCCSSSSSLNNNNNNNNNNNNYFFFFFFFFFFFPTTTTTTTTTTTQQQQQQQQQRGGGGRRRBBBBBBRRCCCRMMRRRMMMMMM\nQQQQQQQQQQQRRRRGIGGIIISSSSSPPPPPPPPPPLUUUUXXXXXXCCCACCSCSSSSSIIIIIIIIINNNNNYYFFFFFFFFFFFPTTTTTTTTTTTTTTQQQQQRRPPRRRRRBBBBBBRRCRRRMMRMRRRMMMM\nQQQQQQQQQGGGGRRGGGGKKKKZJJPPPPPPPPCCPUUUUUXXXXXXXXCASSSSSSSSSIIIIIIIIINNEEEYYFFFFFFFFFFFTTTTTTTTTTTTTTPPQQQQRRPPNRRRRBBBBBBRRRRRRRMMMRRRRMMR\nQQQQQQQQQGGGGGGGGGGKKKKZZZMMMPPPPDCCCUUUUUXXXXXXXXAAFFSSSKSSSIIIIIIIIINNNEEECFFFFFFFYNNNTTTTTTTTTTTTTTTPPPPPPPPPPPPRRBBBBBBRRRRRMMMMMMMRRRRR\nPQQQQQQQQQQGGGGGGGGKKKZZZZZMMMPPRDCCCCCCUUMMXXXXXXAAAASSKKSSSIIIIIIIIINNEEEECFFFFFFFYNNNNNTTTTTTTTTXTTTTTPPPPPPPPPGRBBBBBBBBRRRRMMMMMMMRRRRR\nQQQQQQQQQQQQQGGGGGGGKZZZZZZZMMMMDDDDDCDCDMMMMXXXRAAAAAKKKSSVVIIIIIIIIINEEEEEEFFFFFFFYSSSSBTTTTTTTTTXTTTPPPPPPPPPPPRRBBBBBBBBRRRMMMMMMMMMREER\nDQQQQQQQQQQQGGGGGGGGGGZZZZZZMMMMDDDDDDDDDDMMMXXXXAAAAIIIIIIIIIIIIIIIIIEEEEEEEFFFFFFFYSSSTTTTTTTTXXTXPPTPPPPPPPPPPPPPBBBBBBBBRRMMMMMMMMMMRMEE\nDQQQQQQQQQQGGGGGGGGGZZZZZZZZZMMMDDDDDDDDDDMMMMMXXAAAAIIIIIIIIIIIIIIIIIVVEEEEEFFFFFFFSSSSSTTTTTTTXXXXXPPPPPPPPPPPPPPPBBBBBBBBRRMMMMMMMMMMMMEE\nDDDQQQQQQQQGGGGGGGGGZZZZZZZZPUMMDDDDDDDDDDDDMMMMXMMAAIIIIIIIIIIIIIIIIIVVEEEEIFFFFFFFSSSSSSTTTTXXXXXXPPWPPPPPPPPPXPYUBBBBBBBBMMMMMMMMMMMMMMEE\nDDDQQQQQQGGGGGGGGGGGZZZZZZZPPUDDDDDDDDDDDDDDMMMMMMMAAIIIIIIIIIIIIIIIIIVVEEUESSSSSSSSSSSSSSTTKTXXXXXXXPWPPPPPPPPXXUUUUEEEEMMZMMMMMMMMMMMMEEEH\nDDDQQQQQQQGGGGGGGGGGGGGZZZZUUUDDDDDDDDDDDDDDMMMMMMAAAIIIIIIIIIIIIIIIIIVVUEUUUSSSSSSSSSSSSSXXXXXXXXXXXXXXXEEPPXXXXUUUUEMMEMMZMMMMMMMMMMMEEEEH\nDDDDCQCQQGGGGGGGGGGGLMZZZZZUUUUDUDDDDDDDDDDIIIIIIIIIAIIIIIIIIIIOVVVVVVVVUUUUUSSSSSSSSSSSSXXXXXXXXXXXXXLXEEEXXXXXXXUUMMMMMMMMMMMMMMMMMMMMEEHH\nDDDCCCCCCBBBGGGLGGMSMMZZZZZUUUUUUUDDDDDDDHHIIIIIIIIIAIIIIIIIIIIOOOVVVVVVUUUUUSSSSSSSSSSSSXXXXXXXXXXXXXLCEEXXXXXXXXUUUMMMMMMMMMMMUMMMMMMMHHHH\nCDDCCCCCCCBBGGGLGGMMMMMMMMUUUUUUWUUDDDDDDHHIIIIIIIIIAIIIIIIIIIIOOOVVVVVVVUUSSSSSSSSSSKKXXXXXXXXXXXXXXXCCCEEEXXXXXXXXUUMMMMMMMMUUUMMMMMMMHHHH\nCCCCCCCCCCCGGGOMMGMMMMMMYYYYYYWWWDDDDDDHHHHIIIIIIIIIIIIIIIIIIIIOOOOVVVVVVUNSSSSSSSSKKKKXXXXXXXXXXXXXXCCCXXXXXXXXXXXXXUMMMMMMUUUUUMMMMMMMHTHH\nCCCCCCCCCCCFGMMMMMMMMMMYYYYYYYYNWDDDDDDHHHHIIIIIIIIIIIIIIIIIIIIOOOOOVVQQNNNMMSSKKKKKKKKXXXXXXXXXXXXXXXSZZZXXXXXXXXXXXUUUMMMMUUUUUUUUMMMHHTTH\nCKKCCCCCCCCFFFMMMMMMMMMMYYYYYYYYWWDDDDDHHHHIIIIIIIIIIIIIILOOOOOOOOOOONNNNNNNNTSSKFKKKKXXXXGGGGXXXXXXXXSSZZXXXXXXXXXXUUUUUUUMUUUUUNUUQQMHLTTH\nKKKKCCCCCCKRRRRMMMMMMMMMMMYYYOYYYWWWWDHHHHHIIIIIIIIIIIIIILOOOOOOOOOOONNNNNNNNNNNNFKFFKKXXXGGGGXXXXXXXSSSSXXXXXXXXXUUUUUUUUUUUUUUUUUUUQMMLLTH\nKKKKKKKKKCKKRKKMMMMMMMMMMYYYYYYYYWWWWWHHHHHIIIIIIIIIIIIIILOOOOOOOOOOOYNNNNNNNNNNNFFFFFKKFXGGGGGGGXXXSSSSSXXXXXXXXXUUUUUUUUUUUUUUUUUUUULLLLHH\nKKKKKKKKKLLKKKKMMMMMMMMMMYYYYYYYYYYWWWWWHWHWWHHAAAAAAAAALOOOOOOOOOOVVNNNNNNNNNNNFFFFFFFFFFGGGGGGGXOXSSSSSSXXXXXXXUUUUUUUUUUUUUUUUUUUUULLLHHH\nKKKKKKKKLLLKKMMMMMMMMMMMYYYYYYYYYYWWWWWWWWHHWWAAAAAAAAOOOOOOOOOONNNVVVNNNNNNNNNNFFFFFFFFFFFGGGGGGXSSSSSSSSXXXXXXUUUUUUUUUUUUUUUUUUUUUULKLKKK\nKKKKKKKKKKLKKMMMMMMMMMMMYMMYYYYYYYYWWWWWWWWWWWAAAAAAAAOOOOOOOOOONNNNNNNNNNNNNNFFFFFFFFFFFFFGGGGGGSSSSSSSSSSXXXXXUUUUUUUUUUUUUUUUUUUUUUUKKKKK\nKKKKKKKKKKKKKMXXMMMMMMMMMMMMHHYYYWYYWWWWWWWWWWWAAAAAAAOOOOOOOOOOONNNNNNNNNNNNNFFFFFFFFHHHFGGGGGGSSSSSSSSSSSSXXXUUUUUUUUUUUUUUUUUUUUUUWUUKKKK\nKKKKKKKKKKKKKMXMMMMMMMMMMMMMMHYYYWWWWWWWWWWWWWAAAAAAAAAAOOOOOOOOOOONNNNANNNNNNFFFFFHHHHHHFGGGGGGSSSSSSSSSSSSXUUUUUUUUUUUUUUUUUUUUUUUUWWKKKKK"
exampleOfBitMaskOperation3 = (\(height,world) -> printBWorld height (comparing id) world)                             $ fmap (xorBitMask " " "Q" . moveBitMaskInBWorld " " (0,1) . copyBitMaskInBWorld "Q" " ") $ exampleWorld5
exampleOfBitMaskOperation4 = (\(height,world) -> print . popCount . fromJust . M.lookup "+" . bWorldBitMasks $ world) $ fmap (xorBitMask "+" "Q" . moveBitMaskInBWorld "+" (0,1) . copyBitMaskInBWorld "Q" "+") $ exampleWorld5

setPoint :: String -> (Int,Int) -> BWorld -> BWorld
setPoint label (x,y) w = w {bWorldPoints = M.insert label (x,y) (bWorldPoints w)}

insertBitMaskAtPoint :: String -> String -> BWorld -> Maybe BWorld
insertBitMaskAtPoint layerLabel pointLabel w = do
    point <- M.lookup pointLabel (bWorldPoints w)
    let newBitMask = pointToBitMask width point
    return $ w {bWorldBitMasks = M.insert layerLabel newBitMask (bWorldBitMasks w)}
  where width = bWorldWidth w

isOverlappingBitMasks :: String -> String -> BWorld -> Bool
isOverlappingBitMasks label1 label2 w
    = fromMaybe False $ do
        points1 <- M.lookup label1 (bWorldBitMasks w)
        points2 <- M.lookup label2 (bWorldBitMasks w)
        
        return $ points1 `isOverlapping` points2

partitionLayerByReachableLRDU :: String -> BWorld -> BWorld
partitionLayerByReachableLRDU = undefined

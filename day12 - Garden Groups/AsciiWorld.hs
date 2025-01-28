#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 --package QuickCheck-2.14.3

module AsciiWorld ( AsciiWorld(..)
                  , emptyAsciiWorld
                  , readAsciiWorld
                  , showAsciiWorld
                  , printAsciiWorld
                  , combineTwoAsciiWorlds
                  , combineAsciiWorlds
                  , isNamedPointOrInNamedMask
                  , moveNamedMask
                  , moveNamedPoint
                  , copyNamedMask
                  , applyNamedMask
                  , setPoint
                  , insertMaskAtPoint
                  , isOverlappingMasks
                  , prefixMasksAndPoints
                  , dropNCharsFromMasksAndPoints
                  , deleteMask
                  , lookupMask
                  , adjustMask
                  , updateMask
                  , alterMask
                  , msbPointOfMask
                  , middlePointOfMask ) where

-------------
-- Imports --
-------------
import Data.List (sortBy, groupBy, delete, find, transpose, sortOn, isPrefixOf)
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

import Util ( replace )
import Mask ( Point, Mask, pointToIndex, pointToMask, moveMask, movePoint, isOverlapping, bitwiseSubtract, bitwiseAnd, bitwiseOr, bitwiseXor, msbPoint, middlePoint )

-- Each obj has a shape encoded as bits of an Integer.

data AsciiWorld
    = AsciiWorld { asciiWorldBG :: Char
                 , asciiWorldMasks :: M.Map String Mask
                 , asciiWorldPoints :: M.Map String Point
                 , asciiWorldWidth :: Int } deriving (Show)

emptyAsciiWorld :: Char -> Int -> AsciiWorld
emptyAsciiWorld bgChar width = AsciiWorld bgChar mempty mempty width


-- Assumes all rows have equal length
readAsciiWorld :: Char -> [Char] -> String -> (Int,AsciiWorld)
readAsciiWorld bgChar singularChars inStr
    = ( height
      , AsciiWorld { asciiWorldBG = bgChar,
                     asciiWorldMasks = foldr addToMask M.empty $ filter (\(char,_) -> not (char `elem` singularChars)) char2Ds,
                     asciiWorldPoints = singularPoints,
                     asciiWorldWidth = width } )
       
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

        addToMask :: (Char, (Int, Int)) -> M.Map String Mask -> M.Map String Mask
        addToMask (char, (x, y)) = M.alter (setBitInMask (x, y)) [char]

        setBitInMask :: (Int, Int) -> Maybe Mask -> Maybe Mask
        setBitInMask (x, y) maybeMask = Just $ setBit (fromMaybe 0 maybeMask) (y * width + x)


showAsciiWorld :: Int -> (String -> String -> Ordering) -> AsciiWorld -> String
showAsciiWorld height nameZOrder asciiWorld = unlines . reverse . take height . chunksOf width . map (fromMaybe bgChar) $ listOfMaybeCharsFromMasksAndPoints
  where (AsciiWorld bgChar masks points width) = asciiWorld
        listsOfMaybeCharsFromMasks = if M.null masks
                                      then [replicate (height * width) Nothing]
                                      else prioritize nameZOrder $ M.mapWithKey (\name n -> layerToMaybeChars name n) masks
        listOfMaybeCharsFromMasks = combineMaybeCharLists listsOfMaybeCharsFromMasks
        namesAndPoints = map head . groupBy ((==) `on` snd) . sortBy (\(aName,aPos) (bName,bPos) -> compare aPos bPos <> compare aName bName) . M.toList $ points
        namesAndIndices = map (fmap (pointToIndex width)) namesAndPoints
        
        nameToChar [] = ' '
        nameToChar (x:xs) = x
        
        listOfMaybeCharsFromMasksAndPoints = foldr (\(name,i) acc -> let maybeOld = join (acc `atMay` i) in replace acc (i, Just $ minimum $ catMaybes [maybeOld, Just (nameToChar name)])) listOfMaybeCharsFromMasks namesAndIndices
        
        combineMaybeCharLists :: [[Maybe a]] -> [Maybe a]
        combineMaybeCharLists = map (getFirst . fold . map First) . transpose
        
        prioritize :: Ord a1 => (a1 -> a1 -> Ordering) -> M.Map a1 a2 -> [a2]
        prioritize nameZOrder m = map (fromJust . flip M.lookup m) (sortBy nameZOrder $ M.keys m)
        
        layerToMaybeChars :: String -> Integer -> [Maybe Char]
        layerToMaybeChars name n = map (\i -> if n `testBit` i then Just (nameToChar name) else Nothing) [0..]

printAsciiWorld :: Int -> (String -> String -> Ordering) -> AsciiWorld -> IO ()
printAsciiWorld height nameZOrder asciiWorld = putStrLn $ showAsciiWorld height nameZOrder asciiWorld

-- Count how many times the prefix appears at the start of a key
countPrefixOccurrences :: String -> String -> Int
countPrefixOccurrences prefix = length . takeWhile (isPrefixOf prefix) . iterate (drop (length prefix))

-- Transform the keys while ensuring no collisions, with an exclusion list
addPrefixToKeys :: String -> [String] -> M.Map String v -> M.Map String v
addPrefixToKeys prefix exclude m =
  let keysWithCounts = [(key, countPrefixOccurrences prefix key) | key <- M.keys m, key `notElem` exclude]
      sortedKeys = map fst $ sortOn (Down . snd) keysWithCounts  -- Sort descending by count
  in M.fromList $ renameKeys sortedKeys ++ [(k, v) | (k, v) <- M.toList m, k `elem` exclude]
  where
    renameKeys [] = []
    renameKeys (key:remaining) =
      let newKey = prefix ++ key
      in if newKey `M.member` m
         then renameKeys remaining ++ [(newKey, m M.! key)]
         else (newKey, m M.! key) : renameKeys remaining

-- Drop n characters from the start of all keys, with an exclusion list
dropNCharsFromKeys :: Int -> [String] -> M.Map String v -> M.Map String v
dropNCharsFromKeys n exclude m =
  let keysSorted = sortOn (Down . length) [key | key <- M.keys m, key `notElem` exclude]  -- Sort by length (longest first)
  in M.fromList $ renameKeys keysSorted ++ [(k, v) | (k, v) <- M.toList m, k `elem` exclude]
  where
    renameKeys [] = []
    renameKeys (key:remaining) =
      let newKey = drop n key
      in if newKey `M.member` m
         then renameKeys remaining ++ [(newKey, m M.! key)]
         else (newKey, m M.! key) : renameKeys remaining

prefixMasksAndPoints :: String -> [String] -> AsciiWorld -> AsciiWorld
prefixMasksAndPoints p exclude w = 
    w { asciiWorldMasks  = addPrefixToKeys p exclude (asciiWorldMasks w)
      , asciiWorldPoints = addPrefixToKeys p exclude (asciiWorldPoints w) }

dropNCharsFromMasksAndPoints :: Int -> [String] -> AsciiWorld -> AsciiWorld
dropNCharsFromMasksAndPoints n exclude w = 
    w { asciiWorldMasks  = dropNCharsFromKeys n exclude (asciiWorldMasks w)
      , asciiWorldPoints = dropNCharsFromKeys n exclude (asciiWorldPoints w) }

deleteMask :: String -> AsciiWorld -> AsciiWorld
deleteMask maskName w = w { asciiWorldMasks = M.delete maskName (asciiWorldMasks w) }

lookupMask :: String -> AsciiWorld -> Maybe Mask
lookupMask maskName w = M.lookup maskName (asciiWorldMasks w)

adjustMask :: (Mask -> Mask) -> String -> AsciiWorld -> AsciiWorld
adjustMask f maskName w = w { asciiWorldMasks = M.adjust f maskName (asciiWorldMasks w) }

updateMask :: (Mask -> Maybe Mask) -> String -> AsciiWorld -> AsciiWorld
updateMask f maskName w = w { asciiWorldMasks = M.update f maskName (asciiWorldMasks w) }

alterMask :: (Maybe Mask -> Maybe Mask) -> String -> AsciiWorld -> AsciiWorld
alterMask f maskName w = w { asciiWorldMasks = M.alter f maskName (asciiWorldMasks w) }

msbPointOfMask :: String -> AsciiWorld -> Point
msbPointOfMask maskName w = msbPoint width (fromJust (lookupMask maskName w))
  where width = asciiWorldWidth w

middlePointOfMask :: String -> AsciiWorld -> Point
middlePointOfMask maskName w = middlePoint width (fromJust (lookupMask maskName w))
  where width = asciiWorldWidth w

-- Testing
exampleAsciiWorld1 :: AsciiWorld
exampleAsciiWorld1 = AsciiWorld '.' (M.fromList [("U",3)]) (M.fromList [("U",(7,7))]) 10

exampleAsciiWorld2 :: AsciiWorld
exampleAsciiWorld2 = AsciiWorld '.' (M.fromList [("U",96)]) (M.fromList [("U",(0,6))]) 10

exampleAsciiWorld3 :: AsciiWorld
exampleAsciiWorld3 = exampleAsciiWorld1 `combineTwoAsciiWorlds` exampleAsciiWorld2

exampleAsciiWorld4 :: AsciiWorld
exampleAsciiWorld4 = moveNamedPoint "U" (1,1) $ exampleAsciiWorld3

examplePrint1 = printAsciiWorld 10 (comparing id) exampleAsciiWorld1
examplePrint2 = printAsciiWorld 10 (comparing id) exampleAsciiWorld2
examplePrint3 = printAsciiWorld 10 (comparing id) exampleAsciiWorld3
examplePrint4 = printAsciiWorld 10 (comparing id) exampleAsciiWorld4


-- Assumes asciiWorlds are same size
-- Left-biased such that the background character and any singular points they share are taken from the left
combineTwoAsciiWorlds :: AsciiWorld -> AsciiWorld -> AsciiWorld
combineTwoAsciiWorlds w1 w2
    = w1 { asciiWorldMasks = M.unionWith combineMasks (asciiWorldMasks w1) (asciiWorldMasks w2),
           asciiWorldPoints = M.unionWith combinePoints (asciiWorldPoints w1) (asciiWorldPoints w2) }
  where combineMasks :: Mask -> Mask -> Mask
        combineMasks = bitwiseOr
        
        combinePoints :: Point -> Point -> Point
        combinePoints point1 _ = point1

combineAsciiWorlds :: [AsciiWorld] -> AsciiWorld
combineAsciiWorlds = foldr1 combineTwoAsciiWorlds

isNamedPoint :: String -> Point -> AsciiWorld -> Bool
isNamedPoint name point asciiWorld = inPoints
  where
    inPoints = case M.lookup name (asciiWorldPoints asciiWorld) of
        Just p -> p == point
        Nothing -> False

isInNamedMask :: String -> Point -> AsciiWorld -> Bool
isInNamedMask name point asciiWorld = inMasks
  where
    inMasks = case M.lookup name (asciiWorldMasks asciiWorld) of
        Just bits -> testBit bits (pointToIndex (asciiWorldWidth asciiWorld) point)
        Nothing -> False

isNamedPointOrInNamedMask :: String -> Point -> AsciiWorld -> Bool
isNamedPointOrInNamedMask name point asciiWorld = inPoints || inMasks
  where
    inPoints = isNamedPoint name point asciiWorld
    inMasks = isInNamedMask name point asciiWorld

moveNamedMask :: String -> (Int,Int) -> AsciiWorld -> AsciiWorld
moveNamedMask name (dx,dy) w = w {asciiWorldMasks = M.update (\pts -> Just $ moveMask width (dx,dy) pts) name (asciiWorldMasks w)}
  where width = asciiWorldWidth w

moveNamedPoint :: String -> (Int,Int) -> AsciiWorld -> AsciiWorld
moveNamedPoint name (dx,dy) w = w {asciiWorldPoints = M.update (\pt -> Just $ movePoint width (dx,dy) pt) name (asciiWorldPoints w)}
  where width = asciiWorldWidth w

copyNamedMask :: String -> String -> AsciiWorld -> AsciiWorld
copyNamedMask srcName destName w = fromMaybe w $ do
    mask <- M.lookup srcName (asciiWorldMasks w)
    return $ w {asciiWorldMasks = M.insert destName mask (asciiWorldMasks w)}

applyNamedMask :: (Mask -> Mask -> Mask) -> String -> String -> AsciiWorld -> AsciiWorld
applyNamedMask op modifier target w
    | not (modifier `M.member` asciiWorldMasks w) = error $ "applyNamedMask: not (modifier `M.member` asciiWorldMasks w) == False where modifier == " ++ modifier
    | not (target   `M.member` asciiWorldMasks w) = error $ "applyNamedMask: not (target `M.member` asciiWorldMasks w) == False where target == " ++ modifier
    | otherwise = w {asciiWorldMasks = M.insert target newMask (asciiWorldMasks w)}
  where mask1 = fromJust $ M.lookup modifier (asciiWorldMasks w)
        mask2 = fromJust $ M.lookup target   (asciiWorldMasks w)
        newMask = mask1 `op` mask2

exampleOfMaskOperation1 = (\(height,world) -> printAsciiWorld height (comparing id) world)                      $ fmap (moveNamedMask "+" (0,3) . applyNamedMask bitwiseXor "*" "+" . moveNamedMask "+" (0,1) . copyNamedMask "*" "+") $ readAsciiWorld '.' [] (unlines [".....",".....",".....",".....",".***.","..*..","....."])
exampleOfMaskOperation2 = (\(height,world) -> print . popCount . fromJust . M.lookup "+" . asciiWorldMasks $ world) $ fmap (moveNamedMask "+" (0,3) . applyNamedMask bitwiseXor "*" "+" . moveNamedMask "+" (0,1) . copyNamedMask "*" "+") $ readAsciiWorld '.' [] (unlines [".....",".....",".....",".....",".***.","..*..","....."])

exampleWorld5 = readAsciiWorld '.' "X" "XXXYYIYYYYRRRRRRRRRRRRJJJJJJJJBBBBBBBYYYYYYYYYYYYYEEEEERRRRRRCCCCCCCCCCCCCCCCCAAAAAAAAAAAAGGGGGGGGGGGGGGDDDHHHHHHHHHGRGFFGGVGGMMMMBBBBBBBBBB\nXYXYYYYYYYRRRRRRRRRRRRRRJJJJJJBBBBBBBBYYBYYYYYYYYYYEEEEEERRRRRCCCCCCCCCCCCCCAAAAAAAAAAAAAAGGGGGGGGGGGGGGDDDDHHHHHHGGGRGFGGGGGMGMBMBBBBBBBBBB\nXYYYYYYYYYYRRRRRRRRRRRRJJJJJJBBBBBBBBBYYBBYYYYYYYYYYEEAAARRRRCCCCCCCCCCCCCCCAAAAAAAAAAAAAGGGGGGGGGGGGGGGIDDDHHHHGGGGGGGGGGGGGGGGBBBBBBBBBBBB\nIKKYYIYYYYRRRRRRRRRRRRRJJJJJJBBBBBBBBBBBBBBYYYYYYYYEEEEAARRRRRCCCCCCCCCCCCCCCCAAAAAAAAAAAGGGGGGGGGGGIIIIIDDIHHHGGGGGGGGGGGGGGGGGBBBBBBBBBBBB\nIIKKIIYYYYYRRRRRRRQRRRJJJJJJJBBBBBBBBBBBBBBBYYYYYYYEEEERRRRRRCCCCCCCCVCCCVVICCAAAAAAAAAAAGGGGGGGGGGIIIIIIIIIIIHHGGGGGGGGGGGGGGGGBBBBBBBBBBBB\nIIKKIIYYYYYYRRRRRRRRJJJFFJJJJJJBBBBBBBBBVVBBYYYYYYYEEERRRRRRCCCCCCCCCVVCCVVVVAAAAAAAAAAAAAGGGNGGGGGGIIIIIIIIIIHGGGGGGGGGGGGGGGGBBBBBBBBBBBBB\nIIIIIIYYYYYYYRRRRRJJJJJFFFFJLJJJJBBBBBBBBVBBYYYYYYYYEERRRRRRRCCCCCCCPPVVNVVVWAAAAAAAAAAAAAGGGGGGGNGGGIIIIIIIIHHGGGGGGGGGGGGGGGBBBBBBWBBBBBBB\nIIIIIIIYYYYYYYRRRRJJFFFFFFLLLJJJJJBBBBBBBBGGGYYYYYYYGGRRRRRRRRCICCVVPVVVVVVVVAAAAAAAAAAAAAAGGGGGGNNNNNIIIIIIIIHHHGGGFEEEGGGGGGJJBBBWWBBBBJBB\nIIIIIYIYYYYYYYRRGRJFFFFFFFLSLLLLJJBBBBBGBBBGGGYYYYYYGGRRRRRRRRRICCFVVVVVVVVVAAAAAAAAAAAAAYAYYYYNUNNNNNIMIIIIIIIIHHHGFFFEEGGGGGJJJJJSSBBBBBIB\nIIIIIYYYYYYYYYGRGRJFFFFFFLLLLLLLLJSSSFBGBBBGGGYYYYYGGGFRRRRBRRRRRVVVVVVVVVVVAAAAAAAAAAAAYYYYYYNNNNNNNNIIIBIIIIIHHHHFFEEEGGGEJJJJJSSSMMSBBMSS\nIIIIIIYYYYYYYYYRGRFFFFFFLLLLLLLLSSSSSSGGGGGGGGYYGYGGGGGGRWRRRRRRRVVVVVVVVVVVVVAAMAAAAAAAAYYYYYNNNNNNNNNIIIIIIIIHHHHFFFEEEEGEJJJJJSSSSSSSSSSS\nIIIIIIYYYYYYYTYRFFFFFFFFFFFLLLLRRRSSSSGGGGGGYYYYGGGGGGGGGRRRRRRRVVVVVVVVVVVVGGAAAAYAAAAAAYYYYYNNNNNNNNNNNIIIIIOHHHHFFEEEEEEEJJJJJJSUSSSSSSSS\nIIIIIIIYYYYYYTTTFFFFFFFFFFFLLRLRRRJSSSGGGGGGGYGGGGGGGGGGAAARRRRRRVVVVVVVVVVIYYYYYYYAYYYAAYYYYNNNNNNNNNNNNIIIIIHHFHFFFEEEEEEEETJJJJSUTSSSSSSS\nIIIIIIIYYYYYYYTTFFFFFFFFFFFLFRRRRJJSGGGGGGGGXGGGGGGGGGGGAAAAARRVVVVVVVVVVVVIYYYYYYYYYYYYYYYYYNNNNNNNNNNNNIIIFFFFFFFFEEEEEEEETTTTTTTUTSSSSSSS\nIIIIIIIIYYYYYTTTFFFFFFFFFFFFFFRRRRJGGGGGGGGXXXXGGGGGGGGGAAAAQQQQQQQGGEEVVVVVYYOYYYYYYYYYYYYYYYNNNNNNNNNNNNNIFNNNFFFVVGGEEEEETTTTTTTTTTTSSSSS\nIIIIIIIIYYYYYTTTFFFFFFFFFFFFFJJJJJJJGGGGGXXXXXXXGGGGGGGGGAAQQQQQQQQGGEEAVVVVVOOOYYYYYYYYYYYYEYEEANNNNNNNNNNNNNNNFFFVVVGEEEEEEETTTTTTPTSSSSSS\nIIIIIIIIYYYTTTFFFFFQQFFFFFFFJJJJJJGGGGGXXXXXXXXGGGGGGGGDQQQQQQQQQQQQQEEEEOOOOOOYYYYYYYYYYYYYEEEEESNNNNNNNNNNNNNNFFFVVVVVVEEEEKTVTTTTTTSSSSSS\nIIIIIIIYYYYYYYQQIFSQQFFFFFFJJJJJJJJGGGGGXXXXXXXGGGGGGGGDMQQQQQQQQQQQEEEEEOOOOOOOOYYYYYYYEEEEEEEEEENNNNNNNNNNNNNNFFFVVVVVVVDEEVQVTTTTSSSSSSSS\nIIIIIIYYYYKYQZQQQQQQQQQFFJJJJJJJJJJJJGGGXXXXXXXGGGSSSSGDDQQQQQQQQQQEEEEEEOOOOOOYYYMMMEEEEEEEEEEEEEFFFNNNNNNNNNNFFFVVVVVVVVVRRVVVTTTTTXSXXXXS\nIIIIUUUYUQQQQQQQQQQQQQFFJJJJJJJJJJJJJGGJXXXXXXXGGGGGDDDDDQQQQQQQQQLVEEEEEOOOOJJJYYYMMEEEEEEEEEEEEEFFFFFFFNNNNFFFFVVVVVVVVVVVVVVVTTDTXXXXXXXX\nIIUUUUUYUUQQQQQQQQQQQFFFJFJYYJYJJJJJJJJJJXXXXXXGGGGGDDDDDDQQQQQQQLLEEEEEEEOOOJJJZZZMMEEEEEEEEEEEEEEEEFFFFFNNFFFFFVFVVVVVVVVVVVVVTTDXXXXXXXXX\nIIUUUUUUUFIIIQQQQQQQQQFFFFPYYYYJJJJJJJJJJVXNNNNNGGGCDDDDDDLDDWWWQLLEEEEEEEOOOOJZZZZMMMEEEEEEEEEEEEEEIZZZZFNFFFFFFFFFVVVVVVVVVVVVQXXXXXXXXXXX\nIUUUUUUUUFFIIIQQQQQQQQQFFYYYYYYJJJJJJJJJJVDDNNNNGGTCDDDDDDDDDDWQQLLLLEEEEEOOOOOKKZZZMMEEEEEEEEEEEEEWZZZZZFFFFFFFGFFFFVVVVVVVVVVVXXXXXXXXXXXX\nUUUUUUUUFFIIIIIQQQQQQQIFFFYYYYZJJJJJJJJJZDDDDNDNNCCCDDDDDDDWWWWWWLLLLLFEEEZZZOZZZZMMMMMEMEEEEEEEIIEWZZZZZZFFFFFGGFFFVVVVVVVVVVVVXXXXXXXXXXXX\nUUUUUUUUUFIIIQQQQQQQIIIIIIIYYYZZJZJJJJJJZDDDDDDCCCCCCDDDDDWWWXXXWLLLLLFFEEZZZZZZZZZMMMMMMELEEEEIIIZZZZZZZZFFFFFFGGGGVVVVVVVVVVVVXXXXXXXXXXXX\nUUUUUUUUBBBIIQQQQTTTIIIIIIIYYYZZJZJJJJJZZZDDDDDDDCCCCCDDDDDWXXXXLLLLLLLFXXZZZZZZZZZMMMMMMEEEEEEIIIZZZZZZZZZZZZGGGGGGVVVVVVVVVVVVXXXXXXXXXXXX\nUUUUUUUUBBBBBLQQQTTTTIIIIIYYYYYZZZZZJJJZZZDDDDDDDCCCCCDCDDDDXXXXLLLLLLLFXXXZZZZZZZZMMMMMMEEEEEEIIZZZZZZZZZZZZZJJGGGGGVVVVVVVVVVMMXXXXXXXXXXX\nUUUUUUUUUUBBBBQQWTTWTIJIIYYYYYYZZLZZJJZZZZDDDDDDDDCCCCCCCDDDXXXXXLLLLLLFXZZZZZZZZZZMMMMMMMMEEEIIIZZZZZZZZZZZZZJJJJGGJVVHUVVVVVVMMXXXXXXXXXXX\nUUUUUUUUUHBBBBJWWWTWWIJJIYYYYYYZZZZZZZZZZZDDDDDDDDCCCCCCCCDDXXXXXXXLLLXXXXXZZZZPPPPPPPPPPPPPPIIIIIIZZZZZZZZZZJJJJJJJJVJUUVVVVMMMMXXXXXXXXXXX\nUUUUUUUUUHHHOWWWWWWWJJJJJJYYYYYZZZZZZZZZZZDDDDDCCCCCCCCCCCDDXXXXXXXLXXXXXXXXZZZPPPPPPPPPPPPPPMIIIEEAZZZZZZZZZZZZJJJJJJJUUMMMMMMMXXXMMXXXXXXX\nUUUUUUUUUHHHWWWWWWWWWWJJJJJYYYZZZZZZZZZZDDDDDDDCCKCCCCCCCCDDXXXXXXXXXXXXXXXXZZZPPPPPPPPPPPPPPPPPPPPPPZZZZZZZZZZZJJJJUUUUUMMMMMMMMMMMMMMMXMME\nUUUUUUUUHHHHWWWWWWWWWEEEEEEEEYPZZZZZZZZZDDDDDDDDDDCCCCCCCCDDXXXXXXXXXXXXXXXXXZZPPPPPPPPPPPPPPPPPPPPPPZZZZZZZZZZZJJJJUUUUUUMMMMMMMMMMMMMMMMMM\nBUUUUUUUUHHHWWWWWWWWEEEEEEEEPPPZZZZZZZZZZDDDDDDDDDDCMMMCJMMDMXXXXXXXXXXXXXXXEZZPPPPPPPPPPPPPPPPPPPPPPEEEZZZZZZZZJJJUUUUUUUMMMMMMMMMMMMMMMMMF\nBBBUUUUUUHHHWWHWWDWWEEEEEEEEPPPZZZZZZZZZDDDDDDDDDDDMMMMMMMMDMMXXXXXXXCXXXXXXEZZPPPPPPPPPPPPPPPPPPPPPPEEEZZZZZZZZZZJJUUUUUWMMMMMMMMMMMEEMMMMF\nBBBGTBBBBBBBBHHWDDEWEEEEEEEEPPPZZZZZZZVVDDDDDDDDDDMMMMMMMMMMMMXXXXXCXCCCCXXXEZZPPPPPPPPPPPPPPEEEEEEEEEEEEESZZZZZZZJJJUUUUWMMMMMMMMMMMEEEMMMF\nGGGGGBBBBBBBBHLWDDEWEEEEEEEEPZZZZZZZZIDDDDDDDDDDDMMMMMMMMMMEMYYYXXXCCCCCCXCZZZZPPPPPPPPPPPPPPPEEEEEEEEEEESSSZZZZZJJJJUUUUWWWMMMMMXXXEEEEEEEF\nGGGGGBBBBBBBBHHDDEEEEEEEEEEEEZZZZZZZZZCCDDDDDDDDMMMMMMMMMMMEYYXXXXXCCCCCCCCCZZWPPPPPPPPPPPPPPPEEEEEEEEEESSOOOZZJJJJJJJUUUUMMMMMMXXXXEEEEAAAA\nGGGGGBBBBBBBBHEEEEEEEEEEEEEEDDDDZZZZZZCCCCDDDDDDDMMLLMDMMMJYYYYJJXXCCCCCCCWWWWWWWZZPPPPPPPPPPPEEEEEEEEEEESOOIIJJJJJJJJUUJUMMMMMMXXXXXEEEEEEA\nGGGGGBBBBBBBBEEEEEEEEEEEEEDDDDDDZZZZZICCCCCFDDDFDMLLLLMMMJJJJJJJJXXCCCCCWWWWWWWWZZZPPPPPPPPPPPMMEEEEEEESSSOOIIIJJJRJJJJJJJMMVMMXXXXXEEEEEEAA\nGGGGGBBBBBBBBREEEEEEEEEEEEDDDDDDZZCCCCCCCCCFFSSFMMLLLLLMMMMMJJJJJXXCCCCCCWWWWQQQQQZPPPPPPPPPPPZZZEEESSSSOOOOOOOJJJRJJJJJJJVVVVMXXXXEEEEEEEAA\nGGGGGGGGBBBBCRREEEEEEEDDEEDDDDDDDZZOCCCCCCCFFFFFMMLLLFMMMMKKJJJJJCCCCCCCCWWWQQQQQQZPPPPPPPPPPPZZZZSSSSSOOOOOOOOOJJRRJJJJJVVVVVVVVXEEEEEEEEEA\nGGGGGGAABBBBRRRRRRRRDDDDDDDDDDDDDCCCCCCCCCCFFFFFFFFFFFJMMMKKKKJJJJCCCCCCWWQQQQQQQQQPPPPPPPPPPPZZVVVVSSOOOOOOOORRRRRRRRJJJVVVVVVVVBEEEEEEEEAA\nGGGAAAAABBBBRRRRRRDDDDDDDDDDDDDDDCCCCCCCCCCCCFFFFFFFFQQQMKKKJJJJJJJJCCCCWQQQQQQQQQQZZZZZZZZPPPZZTVVSSOOOOOOOOORRRRRRRRJJJVAAVVVVVBEEEEEEEEEA\nGGAAAAAAAAAARRRRRDDDDDDDDDZZZZDZZZCCCCCCCCCCCFFFFFFQQQQQQKKKJJJJJJJJCWCWWQQQQQQQQQQQZQVVZZZPPPVVVVVOOOOOOOOOOORRRRRRRRRRJVVVVVVVVVMEEEEEEAAA\nGGAAAAAAAARRRRRRRRDDDDDDDZZZZZZZZCCCZCCCCCCCCFFFFFFFQQQQQKKKJJJJJJJJWWWWHHQQQQQQQQQQQQQQZHQPPPZVVVVVKOOOOOOOOOORRRRRRRRRVVVVVVVVVVVVEEEEEAAA\nGAAAAAAAAARRRRRRRRDDDDDDDZZZZZZZZZZZZCCCCCCCFFFFFFFFFQQQQQKKJWJJJJJJJWWHHHQQQQQQQQQQQQCQQQQQZGZQQVVVVOOOOOOOOOORRRRRRRRRVVVVVVVVVVVVVEEEAAAA\nGAAAAAAAAARRRRRRRRDDDDDDZZZZZZZZZZZEZCCCCCCFFFFFFFFFFQQTTTWWZWJWJWJWWWWHHHQQQQQQQQQQQQCQQQQQZZZQVVVVVVOOOOOORRRRRRRRRRRVVVVVVVVVVVVAAAAEAAAA\nGGAGAAAAAARRVVRRRRDDDDDDZZZZZZZZZZZECCBCCCCCFFFFFFFFJQQQWWWWWWWWWWWWWWWHHHHHQQQQQQQQQCCQQQQQQQQQQQVVVVVOOOORRRRRRRRRRRRRVVVVVVVVVVVAAAAAAAAA\nGGGGAAAAAAAAAVRIDDDDDDDDZZZZZZZZZZZBQBBBCCFFFTFFSFFFJQKKKWWWWWWWWWWWWWHHHHBQQQQQQQQQQQQQQQQQQQQQQQQVVVVOOOOOOOORRRRRRRXRRRVVVVVVVVAAAAAAAAAA\nGGWWAAAAAAAAAAIILDDDDDDDJZZZZZZZZZZBBBBBCCFFFFFFSEEEEEKKKKWWWWWWWWWWWHHHHHHQQQQQQQQQQQQQQQQQQQQQQQQQSVVOOIOIOOOWRRRRRRXRRVVVVVVVVVVAAAAAAAAA\nWWWWAAAAAAAAAIIILLDDDDDAAXZZZZZZZZBBBBBBBCCCCFCCEEEEEKKKKKWWWWWWWWWWWHHHHHHHQQQQQQBQQQQQQQQQQQQQQQQQQVOOOIIIIOWWRUXXXRXRRVVVVVVVVVVVVAAAAAAA\nWWWAAAAAAHAAAFFFFFFFFAAAAAZZZZZZZBBBBBCCCCCCCCCCEEEEEEKKKKWWKKRWWRRRHHHHHHHHQQQQQQBQQQQQQQQQQQQQQQQAQOOOOIIIOOWWWUXXXXXXXVVVVVVVVVVAAAAAAAAA\nWWNNFAAAAHHHHFFFFFFFFAAAAAZZZZZZZBBBBBBCCCCCCCCCEEEEEEKEKKWKKKRRRRRRRPPCCHHCCCCCQBBBHHQQQQQQQQQQQQOODOOOOIIIWWWWWXXXXXXXXVVVVVVAAAAAAAAAAAAA\nNNNNNAAAAXHHHFFFFFFFFAAAAAAZZZZZZZZZBBBCCCCCCCECZEEEEEEEKKKKKKRRRRRRRPPCCCCCCCCCCBBBHHHQQQQQQKQQQMOOOOOOOOWWWWWWWXXXXXXXXVVAAVVAAAAAALAAALLA\nNNQNNAAAASSSSFFFFFFFFFFFFFAAAAZZZZBBBBBCCCCECEEEEEEEEEEEEKRRRRRRRRRRRPPCCCCCCCCCCBBBBHHHHHQQQQQMMMMMMOOOOWWWWWWWWXXXXXXXXXVAAAAAAAUALLAAALLL\nNNNNNQNNNSSSIFFFFFFFFFFFFFAAAAZZZZZBBBBCCCCEEEEEEEEEEEEEEKRRRRRRRRRRRPPPCCCCCCCCCHBBHHJHJJJMQMMMMMMMMOOYOYWWWWWXXXXXXXXXXXAAAAAAAAALLLLLLLLL\nNNNNNNNNNYFFFFFFFFFFFFFFFFAAAAZZZZBBLBBCCEEEEEEEEEEEEEEKKKRRRRRRRRRRQQQQQCCCCCCCHHBBHHJJJJMMMMMMMYYMMMYYYYYYWWWWXXXXXXXXXXAAAAAAAAALLLLLLLLL\nNNNNNNNNNNFFFFFFFFFFFFFFFFAAAAAAAZZMLLMCCCEEEEEEEEEEEEEEKKRRRRRRRROOQQQQQCGCCCHHHHHHHHHJJJMMMMMMMYYYYYYYYYYYWWWWXXXXXXXXXLLLLLLLLLLLLLLLLLLL\nNNNNNNNNNYFFFFFFFFFFFFFFFFAAAAAAAAAMMMMEEEEEEEEEEEEEEKKKKRRRRRRRRRQOQQQQQQGCCCCHHHHHHJHJJJMMMMMMMYYYYYYYYLLYJNWWWXXXXXXXXLLLLLLLLLLLLLLLLLLL\nNNNNNNNNNNFFFFFFFFFFFFFFFFAAAAAAAIMMMMMMBEEPEEEEEEEEEEKKKKRRRRRRRXQQQQQQQGGCCCSSHHHHJJJJJJJMMMMMMMYYYYYYYYYNNNNWXXXXWXWWWLLLLLLLLLLLLLLLLLLL\nOOONNNNNYYFFFFFFFFFFFFFFFFAAAAAMMMMMMMMMMBBBEEEEEEEEEEKKKKRRRRRQQQQQQQQQQCCCCCSSHHHHHJJJJJJJMMMMMMYYYYYYYYNNNNNWWWWXWWWWWLLLLLLLLLLLLLLLLLLL\nOOONNNNYYYFFFFFFFFFFFFFFFFAMMMMMMMMMMMMMBBBBBEEBEEBEEEKKKKRRRRRRRRQOQQQQQQCCPCPSHHHHHJUJJJJJJMMMMMYYYYYYYYNNNNWWWWWWWWWWWLLLLLLLLLLLLLLLLRRL\nOOOONNJJYYFFFFFFFFFFFFFFFFAMMMMMMMMMMMMMBBBBBBBBEBBBEEKKKKKRWWWRRRROOOSSSSCCPPPSSHHHHJJJJJJJJJMMMMMYFYYYYNNNNNNWWWWWWWWWWLLLLLLLLLLLLLLLRRRL\nOOOONNNJJJDDDDDDFFFFFFFFFFFFFDDMMMMMMMMMMMBBBBBBEBEBEEKKKKWWWWWWOORROOSVSSPPPPSSSSSSSJVJJJSRRMMMMMMYYYYYNNNNNNNNNWWWWWWWWLLLLLLLLLLLLLLLRRHH\nNNNNNNNDDDDDDDDDDDDDLLLLFFFFFDDMMMMMMMMMMMBBBBBBBBEEEEEKKWWWWWWWOOOOOOSSSSSPSSSSSYSSSVVJJJSRRRMRMRRNNNYNNNNNNNNNWWWWWWWNNLLLLLLLLLLLLLLHHHHH\nNNNNNNNDDDDDDDDDDDDDDDDDFFFFFDDMMMMMMMMMMMMMMMMBBBBBBBEEKWWWWWWWOOOSSSSSSSSSSSSSSSKSSVVVJSSRRRRRRRRNNNNNNNNNNNNNWWWWWNWNNLLLLLLLLLLLLLHHHHJH\nNNNNNNNDDDDDDDDDDDDDDDDDFFFFFDDRRMMMMMMMMMMMMMMBBBBBBBBEWWWWWWWWXXSSSSSSSSSSASSSSSSSSRRVVRRRRRRRRRRXNNNNNNNNNNNNWWWNNNNNNNNLLLLLLLLLLLHHHHJH\nNNNNNJJJJJJJJJJJJDDDDDDDFFFFFDDRRMMMMMMXXXXXXMMMXXXBBBBBWWWWWCCWXXSSSSSSSSSSSSSSSSSSSRRVVRRRRRRRRRRRNNNNNNNNNNNNWWWWNNNNNNNLLLLLLNNNNNHHHHHN\nNNNNJJJJJJJJJJJJJDDDDDDDFFFFFDDYRRMSMMMXXXXXXBMMCXXBXXBBWYWWWCCXXXCSSSSSSSSSUUUUUSSSSRRRRRRRRRRRRRRNNNNNNNNNNNNNWWWWNNNNNNNLLLLLLNNNNNNHHHHH\nJJJJJJJJJJJJJJRJJDDDDFFFFFFFFDDYRRMMMMMXXXXXXXXXXXXBXXBXYYWWCCCXXXCCSSSSSSSSUUUUUSSSSRRRRRRRRRRRRRRNNNNNNNNNNNNWWWNNNNNNNNNLLLLLLPPPPPNNHHFH\nJWWWWWWWWJJJJRRRJDDDDFFFFFFFFDDYRRRMMMMXXXXXXXXXXXXXXXXXXXCCCCCCXXXCSSSSSSSSUUUUUSSSSSRRRRRRRRRRRRNNNNNNNNNNNNNNNWWNONNNNNNLLLLLLPPQQHHHHHFF\nJWWWWWWWWJJJRRRRRLYYYFFFFDDDDDDYRRRRRRXXXXXXXXXXXXXXXXXDXXCCCCCXXXCCCSSSSSSSUUUUUSSSSDKKKRRRRRRQRRRDNNNNNNNNNNNNWWWWNNNNNNNLLLLLLPPPQQFHHHHF\nJJWWWWWWWWWJRRRRRRYYYFFFFFDDDDDRROOOOOOOOOXXXXXXXXXXXXXXXXCCCCXXXXCCCCSRRRSSUUUUUSSSSDKRRRRRRRRDDDDDDNNNNNNNNNNSWWSWNRNRNNNLLLLLLPPSFFFHFFFF\nJWWWWWWWWWRRRRRRRRRRYFFFFFDDDDDYYOOOOOOOOOXXXXXXXXXXXXXXXXCCCCCCCCCCCCCFFRSKUUUUUDDDDDDDDRRRRRRDDDDDDDDDDDNNSSSSSSSSSRRRNNNNPPPPPPPSFFFHFFFF\nJWWWWWWWWWRRRRRRRRRRYFFFFFDDDDDYROOOOOOOOOXXXXXXXXXXXXXCCCCCCCCCCCCFFFFFFRRSUUUUUDDDDDDDDDDDDRRDDDDDDDDDDDDDDDSSRRRRRRNNNNNWWPPPPPPPFFFFFFFF\nWWWWWWWWRRRRRRRRRRRYYFFFFFDDDDDRROOOOOOOOOXXXXXXXXXXXXXXCCCCCCXCCCCFFFFFFRSSUUUUUMDDOIDDDDDDDDDDDDDDDDDDDDDDDDSSRRRRRRRNNNNWWWPTPPPPPFFFFFFF\nWWWWWWWWWRRRRRRRRRRRYFFFFFDDDDDOROOOOOOOOOXEEVEXXXXXXXXXXCXXXXXXCCCFFFFFFFOSUUUUUMMDIIDDDDDDDDDDDDDDDDDDDDDDDDDPPPRRRRRRRRRTTTTTPPPPPFFFFFFF\nXXWWWWWWWRRRRRRRRRRRYYFFFFDDDDDRROOOOOOOOOXEEVEEEXXXXXXXXCXXXXXCCCCCCFFFFFFGGGSSMMMDIIDDDDDDDDDDDDXDYYDDDDDDPPPPPPRRRRRRRRRTTTTTTPPPPFFFFEFF\nXXWWWWWWWRIIRRRRRRRKJJFFFFFFFFORROOOOOOOOOXEEEEEEXXXXXXXXXXXXXXXXFCFFFFFFFFFGGGMMMMMIIIDIIDDIDDDDDDDYYDDDDYDPPPPPPRRRRRRRRRTTTTTTTPPEFFFEEEE\nXWWWWWWWWIIRRRRRRRRROJFFFFFFFFOORRRRRRRREEEEEEEEEXXXXXXXXXXXXXXFFFFFFFFFFFFFFGMMMMMIIIIIIIDIIDDDDDDFYYYDDDYPPJPPPPRPPPRRRRRRRTTTTPPPEEFEEEEE\nXXDDWWWWWWIRRRRRRRJJJJFFFFFFFFOOOOORRRRREEEEEEEEEEXXXXXXXXXXXXXXXFFFFFFFFFIGGGMMIIIIIIIIIIIIIIDDFFFFYYYYYYYYYJYPPPPPPPPRRRRTTTTTTTTEEEEEEEEE\nDXDDDDWWWWIRRRRRSRJJJJFFFFFFFFOOOOOORRREEEEEEEEEEEHXHXXXXXXXXXXFFFFFFFFFFGGGGGMMIIIIIIIIIIIIIIKKFYYYYYYYYYYYYYYPPPPPPPRRRRTTTTTTTTTEEEEEEEEE\nDDDDDDDWWWRRRRRRRRJJOOFFFFFFFFOOOOMMRKREEEEEEEEEEEEVVVXXXXXXXXXXFFFFFRRRRGGGGGMMMIIIIIIIIIIIIIIKKYYYYYYYYYYYYYPPPPPPPRRRRRRTTTTTTTTEEEEFEEFE\nDDDDDDDWWWRRRRRRRRJJOOOOOOOOOOOOOOMMMEEEEEEEEEEEEVVVVIVXXXXXXXXXXXFFRRRRRRRRRXIIIIIIIIIIIIIIIKKKUYYYYYYYYYYYYYPPPIIORRRRRRRTTTTTTTEEGFFFFFFE\nDDDDDDWWWWWRRRRRRJJJJJOOOOOOOOOOOOONNLLLWWEEEEEEEVVVVVVFXXXXXXXXXXRFRRRRRRRRRXIIIIIIIIIIIIIIIKKKKKYYYYYYYYYYYYYYOOOOORRRDRRDDTTTTTTGGPFFFFFF\nDDDDDDDDWWWWWZZJJJJJJJJJOOOOOOOOOSONNWWWWWWEWEEVVVVVVFVFFFFXXXXXXXRRRRRRRRRRCRRIWIIIIIIIIIIIKKKKKKYYYYYYYYYYYYYYYGGORRRRDDDDDGGTGTTAGGFFFFFF\nDDDDDDDDDWWZZZZZZJJJJJJJOOOOOOOONNNNNNNWNWWWWEVVVVVVVFFFFFFXXXXXXXRRRRRRRRRRRRREIIIEIIIIIIIIKKKKKBBYYYYYYYYYYYYRYSGSSSRDDDDDDGGGGGGGGGFFFFFF\nDDDDDDDDDWZZZZZZZJJJJJJJJIOOOOOOOONNNNNNWWWWWWVVVVVVVFFFFFFFXXFXXXRRRRRRRRRRRREEEEEEIIIIIIKKKKKKKKYYHYYYYYYYYYHSSSSSSSSSDDDDDDGGGGGGGGFFFFFF\nDDDDDDWWWWWWZZZZZZZJJJJJJIOOODOOOOOFNNNNWWWWWWVVVVVVVFFFFFFFFFFXOOOOOOOORRRRRRRREEEIIIIIIIIIKKKKKKYYHYYYYYYYYHHSHSSSSSSSFDDDDDDGGGGGFGFFFFFF\nDDDDDDWWWZZZZZZZZZZZZJJJJIOOOOOFFOOFFNNNWWWWVWVVVVVVVFFFFFFFFFFXOOOOOOOORRRRRREEEEEEEIIIIIIKKKKKKKKKKYYYYYYYYHHHHSSSSSSSDDGDDSSGGGGGFFFFFFFF\nDDDDDDDDWWZZZZZZZZZZZZZIIIOFFFFFFFFFFOOOWWWVVVVVVVVVVFFFFFFFOOOOOOOOOOOOROOORROOOOEEEEIIIIIKKKKKKKKKKYYYYYYYYYHYSSSSSSSSGGGDDNSGGGGGUUFFFFFF\nDDDDDDDWWWZZZZZZZZZZZZBIIIOFFFFFFFOOOOOOZZVVVVVVVVVVVFFFFFFFOOOOOOOOOOOOROOOOOOOOOOEKEIKIIKKKKKKKKKKKYYKYYYYXYYYYSSSSSSGGGGGNNSGJJLLLLLFFFFF\nDDDDDDDWWZZZZZZZZZBBBBBBBBBFFOOOOOOOOOOOVVVVVVVVVVVVFFFFFFFFOOOOOOOOOOOORQQQAOOOOOOOKKKKKKKKKKKKKKNKKKYNYYYXXYYYSSSSSSSGGGGGNNLLLLLLLLLFFFFN\nDDDDDDWWWWWZGGGZZZBBBBBBBOBOOOOOOOOOOOOOOOVVVVVVVVVVFFFFFFFFOOOOOOOQQQQQQQQQAOOOOOOOKKKKKKKKKKKKNNNKKKKNNNNNXXYYXSSSSSSHGGGGNNNNLLLHLLLLGGFF\nDDDDDDDWWWWWGGGZZZBBBBBBBOBOOOOOOOOOOOOOOVVVVVVVVVFFFTTFFFFFOOOOOOOQQQQQQQQQQOZOOOOOOKFKKKKKKKKKKKNNNNNNNNNNXXYEXXXXXSXHHHHGHNLLLLLLLLLLLLFF\nDDDDDDDWWWWWWWGZBBBBBBBBBBBBBOOOOOOOOOOOOVVVVVVVVVVVVTTFFFFFOOOOOOOQQQQQQQQQQZZZOOOZOOKKKKKKKKKKKKNNNNNNNNXXNXXEEXXXXXXHHHHGHNNLLLLLLLLLBBBB\nDDDDDDDDWWWWWWGGBBBBBBBBBBBBBBOOOOOOOOOOOVVVVVVVVVVKFFFFFFFFOOOOOOOQQQQQQQQQQZZZZZZZZOBKKYKKKKKKSKNNNNNNNNNNNXXXEEXXXXHHHHHHHHNNLLLLLLLRRBRB\nDDUUDDDDWWWWWWGGNBBBBBBBBBBBBOOOOOOOOOOOKVVVVVVVVVKKKKALLLLLLAAAQQQQQQQQQQQQQZZZZZZZZUUYYYYKMMMKKKNNNNNNNNNNNXXXXXXXHHHHHHHHHNNNNLLLLLRRRRRR\nNDDDDDDDWWWWWWWWBBBBBBBBBBBBBBBOOOOOOOOOKKVVVVVVVVKKKZFLLLLLLFHHQQQQQQQQQQQQQZZZZZUUUUYYYYYLMMLLNNNNNNNNNNNNXXXXXXXHHHHHHHHHNNNNNLNLRRRRRRRD\nNDFFDDDDDWWWWWWMMBBBBBBBBRBBBBUOOOOOOOOOKKKKVVVVVKKKKKKLLLLLLFHHHQQQQQQQQQQQQZFFZZUUUULLLLYLLLLLLNNNNNNNNNNAXUXXXXXXXXHHHHHNNNNNNNNLRRRRRRDD\nNDMFFDWWWWWMMMMMMMBBBMMBBBBGOOOOOOOOOOOOOKKKKKKVVVKKKKKLLLLLLLLHQQQQQQQQQQQQQQYFUUUUUUFLLLLLLLLLNNNNNNNNNUUUUUUUXXXXXXHHHHHHNNNNNNRLRRRRRRRD\nMMMMFDWWWWWMMMMMMMXXMMMMBKMOOOOOOOOOOOOOOKKKKKKKKKLLLLLLLLLLLLLHQQQQQQQQQQQQUFFFFUUUUQQQLLLLLLLNNNNNNNNNNUUUUUUUUXXXXHHHWWWNNNNNNNRRRRRRRCDD\nMMMMMWWWWWWWMMMMMMMMMMMMMMMEOOOOOOOKKKKKKKKKKKKKSSLLLLLLLLLLLLLHHHHQQQQQQQQQFFFFFFFFLQQBQQLLLLLLLLNNUNNUSUUUUUUUUUXXXHWWWWWNNNNNNNNRRRRRRCCD\nMMMMMMWWWWMMMMMMMMMMMMMMMMMOOOOOXXOOOKKKKKKKKKKKSSLLLLLLLLLLLLLHHHHHQVQCFFQQQQFFFFFFFQQQQQLLLLLLLLNSSSSSSSUUUUPUUUXXXXWWWWWWWNNNNNNNRRRRRRCC\nMMMMMMMMWWWWWMMMMMMMMMMMMMMMOOFOOVVOKKKKKYYYYYYYYYLLLLLLLLLLLLLHHHLLVVLLFFFFQFFFFFFUQQQQQQLLLLLLLLNSSSSSSSUUUUUUUUXXXWWWWWWWNNNNNNNNNNCRRCCC\nMMMMMMMWWWWWWMMMMMMMMMMMMMMMMPPPPVVVKKKKKYYYYYYYYYLLLLLLLLLLLLLHLLLLLLLFFFFFFFFFFFFFQQQQQQQLLLPPLLGSSSSSSSUUUUUUUUVXXXXWWWWWNCNCNNNNCCCCCCCC\nMMMMMMMMWWMMMMMMMMMMMMMMMMMMMMPPPVIUKKKKUYYYYYYYYYLLLLLLLLLLLLLLLLLLLLLFFFFFFFFFFFJJQQQQQQLPPPPPLLUSSSSSSSUUUUUUUUXXXXXZWWWNNCCCNCNCCCCCCCCC\nMMMMMMMMMMSSSMMMMIIIIMMMMMMMMPPPPPUUUUUUUYYYYYYYYYLLLLLLLLLLLLLLLLLLLLLFFFFFFFFFFFQQQQQQQPPPPPPPUUUSSSSSSSUUUUUUGGXXGXZZZZZCCCCCCCNCCCCCCCCC\nMMMMMMMMMMSSMMIIMIIIIMMMMPPMPPPPPUUUUUUUUYYYYYYYYYLLLLLLLLLLLLLLLLLLLLLLLFLFFFFFFQQQQQQQQPPPPPPPUUUSSSSSSSUUUUUGGGXTGGGGZZZZCCCCCCNCCCCCCCCC\nMMMMMMMMMMSSMMIIIIIIIMSSSSSPPPPPPUUUUUUUKYYYYYYYYYLLLLLLLLLLLLLLLLLLLLLLLFLSSFFFQQQQQQBPQPPPPPPPPPPSSSSSSSUUVUGGGGGGGGGGGGZZCCCCCCCCCCCCCCCC\nMMMMMMMMMMIIIIIIIIIIIMSSSSSSSSSSSSHUUUUUUYYYYYYYYYYYYYSSSSSLLLLLLLLLLLLLLLLSFFVFYYQQYYPPPPPPPPPPPPTUUUUUUUUUVUGGGGGGGGGGGGGZZCCCCCCCICCCCCCF\nMMMMMMMMMTTIIIIIIIIIIISSSSSSSSSSSSUUUUUUUYYYYYYYYYYSSCSSSSSLLLLLLLLLLLLXLSSSDDFFYYQYYPPPPPPPPPPTTTTTVVUUUUUUVVGGGGGGGGGGSSGZCCCCCCCCCLCCCCCC\nQMMMMMMMMMIIIIIIIIIIIISSSSSSSSSSSSUUUUUUUYYYYYYYYYYCCCCSSSSLLLLLLLLLLLLLSSSSSSFYYYYYPPPPPPPPPPTTTTTVVVUVVVVVVGGGGGGGGGGGGGGZCCCCCCCCCCCMYYYM\nQMMMMMMMMMMIIIIIIIIIIISSSSSSSSSSSSUUUUUUUYYYYYYYYYYCCCCCSSSZSSSLLLNNNLLLNSSNSHHFFFFFFFFFPPPPPPTTTTTVVVVVQVVVVVGGRRGGGGGGGGCCCCCCCCCCCCMMMYMM\nQMMMMMMMMMMMIIIIIIIIIISSSSSSSSSSSSWUUUUUUUUUUCCCSCCCCCCCSSZZSSSLLLNNLLNNNNNNYYYFFFFFFFFFPPPPPBTTTTTTTVQQQQQQVVVGRRRGGGGGGGGRCCCCCCCRCCJMMMMM\nQQQMMQQQMMQQIIIGIIIIIISSSSSPPPPPPPWUUUUUUUUUUCXCCCCCCCCCCSSSSSSLLNNNLLNNNNNNNFFFFFFFFFFFPPPPTTTTTTTTTTIQQQQQQRGGRRRRRBBBBBBRRCCCCCCRRMMMMMMM\nQQQQQQQQMQQRIIRGIIIIIISSSSSPPPPPPPPPUUUUUUWUCCXXXCCACCCCCCSSSSSLNNNNNNNNNNNNYFFFFFFFFFFFPTTTTTTTTTTTQQQQQQQQQRGGGGRRRBBBBBBRRCCCRMMRRRMMMMMM\nQQQQQQQQQQQRRRRGIGGIIISSSSSPPPPPPPPPPLUUUUXXXXXXCCCACCSCSSSSSIIIIIIIIINNNNNYYFFFFFFFFFFFPTTTTTTTTTTTTTTQQQQQRRPPRRRRRBBBBBBRRCRRRMMRMRRRMMMM\nQQQQQQQQQGGGGRRGGGGKKKKZJJPPPPPPPPCCPUUUUUXXXXXXXXCASSSSSSSSSIIIIIIIIINNEEEYYFFFFFFFFFFFTTTTTTTTTTTTTTPPQQQQRRPPNRRRRBBBBBBRRRRRRRMMMRRRRMMR\nQQQQQQQQQGGGGGGGGGGKKKKZZZMMMPPPPDCCCUUUUUXXXXXXXXAAFFSSSKSSSIIIIIIIIINNNEEECFFFFFFFYNNNTTTTTTTTTTTTTTTPPPPPPPPPPPPRRBBBBBBRRRRRMMMMMMMRRRRR\nPQQQQQQQQQQGGGGGGGGKKKZZZZZMMMPPRDCCCCCCUUMMXXXXXXAAAASSKKSSSIIIIIIIIINNEEEECFFFFFFFYNNNNNTTTTTTTTTXTTTTTPPPPPPPPPGRBBBBBBBBRRRRMMMMMMMRRRRR\nQQQQQQQQQQQQQGGGGGGGKZZZZZZZMMMMDDDDDCDCDMMMMXXXRAAAAAKKKSSVVIIIIIIIIINEEEEEEFFFFFFFYSSSSBTTTTTTTTTXTTTPPPPPPPPPPPRRBBBBBBBBRRRMMMMMMMMMREER\nDQQQQQQQQQQQGGGGGGGGGGZZZZZZMMMMDDDDDDDDDDMMMXXXXAAAAIIIIIIIIIIIIIIIIIEEEEEEEFFFFFFFYSSSTTTTTTTTXXTXPPTPPPPPPPPPPPPPBBBBBBBBRRMMMMMMMMMMRMEE\nDQQQQQQQQQQGGGGGGGGGZZZZZZZZZMMMDDDDDDDDDDMMMMMXXAAAAIIIIIIIIIIIIIIIIIVVEEEEEFFFFFFFSSSSSTTTTTTTXXXXXPPPPPPPPPPPPPPPBBBBBBBBRRMMMMMMMMMMMMEE\nDDDQQQQQQQQGGGGGGGGGZZZZZZZZPUMMDDDDDDDDDDDDMMMMXMMAAIIIIIIIIIIIIIIIIIVVEEEEIFFFFFFFSSSSSSTTTTXXXXXXPPWPPPPPPPPPXPYUBBBBBBBBMMMMMMMMMMMMMMEE\nDDDQQQQQQGGGGGGGGGGGZZZZZZZPPUDDDDDDDDDDDDDDMMMMMMMAAIIIIIIIIIIIIIIIIIVVEEUESSSSSSSSSSSSSSTTKTXXXXXXXPWPPPPPPPPXXUUUUEEEEMMZMMMMMMMMMMMMEEEH\nDDDQQQQQQQGGGGGGGGGGGGGZZZZUUUDDDDDDDDDDDDDDMMMMMMAAAIIIIIIIIIIIIIIIIIVVUEUUUSSSSSSSSSSSSSXXXXXXXXXXXXXXXEEPPXXXXUUUUEMMEMMZMMMMMMMMMMMEEEEH\nDDDDCQCQQGGGGGGGGGGGLMZZZZZUUUUDUDDDDDDDDDDIIIIIIIIIAIIIIIIIIIIOVVVVVVVVUUUUUSSSSSSSSSSSSXXXXXXXXXXXXXLXEEEXXXXXXXUUMMMMMMMMMMMMMMMMMMMMEEHH\nDDDCCCCCCBBBGGGLGGMSMMZZZZZUUUUUUUDDDDDDDHHIIIIIIIIIAIIIIIIIIIIOOOVVVVVVUUUUUSSSSSSSSSSSSXXXXXXXXXXXXXLCEEXXXXXXXXUUUMMMMMMMMMMMUMMMMMMMHHHH\nCDDCCCCCCCBBGGGLGGMMMMMMMMUUUUUUWUUDDDDDDHHIIIIIIIIIAIIIIIIIIIIOOOVVVVVVVUUSSSSSSSSSSKKXXXXXXXXXXXXXXXCCCEEEXXXXXXXXUUMMMMMMMMUUUMMMMMMMHHHH\nCCCCCCCCCCCGGGOMMGMMMMMMYYYYYYWWWDDDDDDHHHHIIIIIIIIIIIIIIIIIIIIOOOOVVVVVVUNSSSSSSSSKKKKXXXXXXXXXXXXXXCCCXXXXXXXXXXXXXUMMMMMMUUUUUMMMMMMMHTHH\nCCCCCCCCCCCFGMMMMMMMMMMYYYYYYYYNWDDDDDDHHHHIIIIIIIIIIIIIIIIIIIIOOOOOVVQQNNNMMSSKKKKKKKKXXXXXXXXXXXXXXXSZZZXXXXXXXXXXXUUUMMMMUUUUUUUUMMMHHTTH\nCKKCCCCCCCCFFFMMMMMMMMMMYYYYYYYYWWDDDDDHHHHIIIIIIIIIIIIIILOOOOOOOOOOONNNNNNNNTSSKFKKKKXXXXGGGGXXXXXXXXSSZZXXXXXXXXXXUUUUUUUMUUUUUNUUQQMHLTTH\nKKKKCCCCCCKRRRRMMMMMMMMMMMYYYOYYYWWWWDHHHHHIIIIIIIIIIIIIILOOOOOOOOOOONNNNNNNNNNNNFKFFKKXXXGGGGXXXXXXXSSSSXXXXXXXXXUUUUUUUUUUUUUUUUUUUQMMLLTH\nKKKKKKKKKCKKRKKMMMMMMMMMMYYYYYYYYWWWWWHHHHHIIIIIIIIIIIIIILOOOOOOOOOOOYNNNNNNNNNNNFFFFFKKFXGGGGGGGXXXSSSSSXXXXXXXXXUUUUUUUUUUUUUUUUUUUULLLLHH\nKKKKKKKKKLLKKKKMMMMMMMMMMYYYYYYYYYYWWWWWHWHWWHHAAAAAAAAALOOOOOOOOOOVVNNNNNNNNNNNFFFFFFFFFFGGGGGGGXOXSSSSSSXXXXXXXUUUUUUUUUUUUUUUUUUUUULLLHHH\nKKKKKKKKLLLKKMMMMMMMMMMMYYYYYYYYYYWWWWWWWWHHWWAAAAAAAAOOOOOOOOOONNNVVVNNNNNNNNNNFFFFFFFFFFFGGGGGGXSSSSSSSSXXXXXXUUUUUUUUUUUUUUUUUUUUUULKLKKK\nKKKKKKKKKKLKKMMMMMMMMMMMYMMYYYYYYYYWWWWWWWWWWWAAAAAAAAOOOOOOOOOONNNNNNNNNNNNNNFFFFFFFFFFFFFGGGGGGSSSSSSSSSSXXXXXUUUUUUUUUUUUUUUUUUUUUUUKKKKK\nKKKKKKKKKKKKKMXXMMMMMMMMMMMMHHYYYWYYWWWWWWWWWWWAAAAAAAOOOOOOOOOOONNNNNNNNNNNNNFFFFFFFFHHHFGGGGGGSSSSSSSSSSSSXXXUUUUUUUUUUUUUUUUUUUUUUWUUKKKK\nKKKKKKKKKKKKKMXMMMMMMMMMMMMMMHYYYWWWWWWWWWWWWWAAAAAAAAAAOOOOOOOOOOONNNNANNNNNNFFFFFHHHHHHFGGGGGGSSSSSSSSSSSSXUUUUUUUUUUUUUUUUUUUUUUUUWWKKKKK"
exampleOfMaskOperation3 = (\(height,world) -> printAsciiWorld height (comparing id) world)                      $ fmap (applyNamedMask bitwiseXor " " "Q" . moveNamedMask " " (0,1) . copyNamedMask "Q" " ") $ exampleWorld5
exampleOfMaskOperation4 = (\(height,world) -> print . popCount . fromJust . M.lookup "+" . asciiWorldMasks $ world) $ fmap (applyNamedMask bitwiseXor "+" "Q" . moveNamedMask "+" (0,1) . copyNamedMask "Q" "+") $ exampleWorld5

setPoint :: String -> (Int,Int) -> AsciiWorld -> AsciiWorld
setPoint name (x,y) w = w {asciiWorldPoints = M.insert name (x,y) (asciiWorldPoints w)}

insertMaskAtPoint :: String -> String -> AsciiWorld -> Maybe AsciiWorld
insertMaskAtPoint layerName pointName w = do
    point <- M.lookup pointName (asciiWorldPoints w)
    let newMask = pointToMask width point
    return $ w {asciiWorldMasks = M.insert layerName newMask (asciiWorldMasks w)}
  where width = asciiWorldWidth w

isOverlappingMasks :: String -> String -> AsciiWorld -> Bool
isOverlappingMasks name1 name2 w
    = fromMaybe False $ do
        points1 <- M.lookup name1 (asciiWorldMasks w)
        points2 <- M.lookup name2 (asciiWorldMasks w)
        
        return $ points1 `isOverlapping` points2

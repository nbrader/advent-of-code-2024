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

import AsciiWorld (AsciiWorld(), emptyAsciiWorld)

main :: IO ()
main = do
  print $ (emptyAsciiWorld 10 :: AsciiWorld Int Int)
  putStrLn "hello world"

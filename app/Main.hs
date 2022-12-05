{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified One.One as One
import qualified Two.Two as Two
import System.IO
import qualified Data.String as String
import qualified Data.List as List
import Data.Maybe
import Data.Ix ( Ix(range) )
import Data.Function ( on )
import qualified Data.Bifunctor as Bifunctor

main :: IO ()
main = do
-- 4-2
    handle <- openFile "src/four/four_input.txt" ReadMode  
    file <- hGetContents' handle
    let lines = String.lines file
        splitLines :: [((Int, Int), (Int, Int))] = map
            (Bifunctor.bimap 
            (Bifunctor.bimap read (read . filter (/= '-')) . List.break ('-' ==)) 
            (Bifunctor.bimap read (read . filter (/= '-')) . List.break ('-' ==) . filter (/= ','))
                . List.break (',' ==))
                    lines 
        overlaps = filter id $ map (\(a , b) -> not . null $ List.intersect (range a) (range b)) splitLines
        total = length overlaps
    print total
    hClose handle

-- 4-1
    -- handle <- openFile "src/four/four_input.txt" ReadMode  
    -- file <- hGetContents' handle
    -- let lines = String.lines file
    --     splitLines :: [((Int, Int), (Int, Int))] = map
    --         (Bifunctor.bimap 
    --         (Bifunctor.bimap read (read . filter (/= '-')) . List.break ('-' ==)) 
    --         (Bifunctor.bimap read (read . filter (/= '-')) . List.break ('-' ==) . filter (/= ','))
    --             . List.break (',' ==))
    --                 lines 
    --     overlaps = filter id $ map (\(a , b) -> List.isSubsequenceOf (range a) (range b) || List.isSubsequenceOf (range b) (range a)) splitLines
    --     total = length overlaps
    -- print total
    -- hClose handle 

-- 3-2
    -- handle <- openFile "src/three/three_input.txt" ReadMode  
    -- file <- hGetContents' handle
    -- let indexed = String.lines file
    --     grouped = filter (/= []) . snd $ 
    --         foldr (\curr (old, new) -> (drop 3 old, take 3 old : new)) (indexed, []) indexed 
    --     priorities = zip ['a' .. 'z'] [1 .. 26] ++ zip ['A' .. 'Z'] [27 .. 52]
    --     commons = map (\case 
    --         [a, b, c] -> head $ List.filter (\y -> elem y $ List.filter (`elem` b) a) c
    --         _ -> ' ' ) grouped
    --     total = sum $ mapMaybe (`lookup` priorities) commons
    -- print total
    -- hClose handle 
    
-- 3-1
    -- handle <- openFile "src/three/three_input.txt" ReadMode  
    -- file <- hGetContents' handle
    -- let splitLines = map (\x -> splitAt (div (length x) 2) x) $ String.lines file
    --     priorities = zip ['a' .. 'z'] [1 .. 26] ++ zip ['A' .. 'Z'] [27 .. 52]
    --     commons = map (head . uncurry List.intersect) splitLines 
    --     total = sum $ mapMaybe (`lookup` priorities) commons
    -- print total
    -- hClose handle  

-- 2-2
    -- handle <- openFile "src/two/two_input.txt" ReadMode  
    -- file <- hGetContents' handle
    -- let lines = String.lines file
    --     rounds = map Two.parseRoundTrue lines
    --     totalScore = sum $ map (\(Two.Round i _ _) -> i) rounds
    -- print totalScore
    -- hClose handle  

-- 2-1
    -- handle <- openFile "src/two/two_input.txt" ReadMode  
    -- file <- hGetContents' handle
    -- let lines = String.lines file
    --     rounds = map Two.parseRound lines
    --     totalScore = sum $ map (\(Two.Round i _ _) -> i) rounds
    -- print totalScore
    -- hClose handle  
    
-- 1-2
    -- handle <- openFile "src/one/one_input.txt" ReadMode  
    -- file <- hGetContents' handle
    -- let lines = String.lines file
    --     groups = List.groupBy (\x y -> x /= "" && y /= "") lines
    --     totals = map (sum . map read . filter ("" /=)) groups
    --     sorted = reverse $ List.sort totals
    -- putStr . show . sum $ List.take 3 sorted 
    -- hClose handle  

-- 1-1
    -- handle <- openFile "src/one/one_input.txt" ReadMode  
    -- file <- hGetContents' handle
    -- let lines = String.lines file
    --     groups = List.groupBy (\x y -> x /= "" && y /= "") lines
    --     totals = map (sum . map read . filter ("" /=)) groups
    --     max = maximum totals
    -- putStr $ show max
    -- hClose handle  

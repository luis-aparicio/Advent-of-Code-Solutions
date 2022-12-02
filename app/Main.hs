module Main where

import qualified One.One as One
import qualified Two.Two as Two
import System.IO
import qualified Data.String as String
import qualified Data.List as List

main :: IO ()
main = do
    handle <- openFile "src/two/two_input.txt" ReadMode  
    file <- hGetContents' handle
    let lines = String.lines file
        rounds = map Two.parseRound lines
        totalScore = sum $ map (\(Two.Round i _ _) -> i) rounds
    print totalScore
    hClose handle  
    
-- 2-2
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

-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified One.One as One
import qualified Two.Two as Two
import qualified Five.Five as Five
import qualified Data.List as List
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Char as Char
import qualified Data.String as String
import qualified Debug.Trace as Debug
import System.IO
import Data.Ix ( Ix(range) )
import Data.Char (digitToInt)
import Data.Maybe 
import Data.Function ( on )

main :: IO ()
main = do
-- 6-2
    handle <- openFile "src/six/six_input.txt" ReadMode  
    file <- hGetContents' handle
    let (_, _, final) = foldr (\curr (acc, i, final) -> let
            next = curr : take 13 acc
            size = length next 
            in  if size >= 14 && length (List.nub next) == size && final == 0 
                then (curr : acc, succ i, succ i ) 
                else (curr : acc, succ i, final)) ([], 0, 0) (reverse file)
    print final
    hClose handle

-- 6-1
    -- handle <- openFile "src/six/six_input.txt" ReadMode  
    -- file <- hGetContents' handle
    -- let (_, _, final) = foldr (\curr (acc, i, final) -> let
    --         next = curr : take 3 acc
    --         size = length next 
    --         in  if size >= 4 && length (List.nub next) == size && final == 0 
    --             then (curr : acc, succ i, succ i ) 
    --             else (curr : acc, succ i, final)) ([], 0, 0) (reverse file)
    -- print final
    -- hClose handle

-- 5-2
    -- handle <- openFile "src/five/five_input.txt" ReadMode  
    -- file <- hGetContents' handle
    -- let lines = String.lines file
    --     initStackStrings = take 8 lines
    --     -- 2, 6, 10, 14, 18, 22, 26, 30, 34 are the letter indexes for a line [T] [L] [Z] [R] [C] [Q] [V] [P] [H]
    --     emptyStacksMap = map (\x -> (x, Five.Stack [])) [2, 6, 10, 14, 18, 22, 26, 30, 34]
    --     stacks = foldr (Five.mkStack 1) emptyStacksMap initStackStrings
    --     -- the same as stacks but using (1, 2, 3, 4, 5 ...) instead of (2, 6, 10, 14, 18 ... )
    --     indexedStacks = snd $ foldr (\(_, curr) (i, acc) -> (pred i, (pred i, curr) : acc) ) (10, []) stacks 
    --     instructions = drop 8 lines
    --     final = foldr (\ curr acc -> do 
    --                     let numbers = map read . words $ filter (\x -> Char.isNumber x || Char.isSpace x) curr
    --                     case numbers of 
    --                         [num, from, to] ->  
    --                             let groupToMove = maybe (error "groupToMove not found") (\(Five.Stack s) -> take num s) $ lookup from acc 
    --                             in  map (\(ix, Five.Stack curr) -> if from == ix 
    --                                     then (ix, Five.Stack $ drop num curr) 
    --                                     else if to == ix 
    --                                         then (ix, Five.Stack $ groupToMove ++ curr) else (ix, Five.Stack curr)) acc
    --                         _ -> acc 
    --                     ) indexedStacks (reverse instructions)
    -- print (concatMap (take 1 . Five.stackToList . snd) final)
    -- hClose handle

-- 5-1
    -- handle <- openFile "src/five/five_input.txt" ReadMode  
    -- file <- hGetContents' handle
    -- let lines = String.lines file
    --     initStackStrings = take 8 lines
    --     -- 2, 6, 10, 14, 18, 22, 26, 30, 34 are the letter indexes for a line [T] [L] [Z] [R] [C] [Q] [V] [P] [H]
    --     emptyStacksMap = map (\x -> (x, Five.Stack [])) [2, 6, 10, 14, 18, 22, 26, 30, 34]
    --     stacks = foldr (Five.mkStack 1) emptyStacksMap initStackStrings
    --     -- the same as stacks but using (1, 2, 3, 4, 5 ...) instead of (2, 6, 10, 14, 18 ... )
    --     indexedStacks = snd $ foldr (\(_, curr) (i, acc) -> (pred i, (pred i, curr) : acc) ) (10, []) stacks 
    --     instructions = drop 8 lines
    --     final = foldr (\ curr acc -> do 
    --                     let numbers = map read . words $ filter (\x -> Char.isNumber x || Char.isSpace x) curr
    --                     case numbers of 
    --                         [num, from, to] ->  
    --                             let groupToMove = reverse . maybe (error "groupToMove not found") (\(Five.Stack s) -> take num s) $ lookup from acc 
    --                             in  map (\(ix, Five.Stack curr) -> if from == ix 
    --                                     then (ix, Five.Stack $ drop num curr) 
    --                                     else if to == ix 
    --                                         then (ix, Five.Stack $ groupToMove ++ curr) else (ix, Five.Stack curr)) acc
    --                         _ -> acc 
    --                     ) indexedStacks (reverse instructions)
    -- print (concatMap (take 1 . Five.stackToList . snd) final)
    -- hClose handle

-- 4-2
    -- handle <- openFile "src/four/four_input.txt" ReadMode  
    -- file <- hGetContents' handle
    -- let lines = String.lines file
    --     splitLines :: [((Int, Int), (Int, Int))] = map
    --         (Bifunctor.bimap 
    --         (Bifunctor.bimap read (read . filter (/= '-')) . List.break ('-' ==)) 
    --         (Bifunctor.bimap read (read . filter (/= '-')) . List.break ('-' ==) . filter (/= ','))
    --             . List.break (',' ==))
    --                 lines 
    --     overlaps = filter id $ map (\(a , b) -> not . null $ List.intersect (range a) (range b)) splitLines
    --     total = length overlaps
    -- print total
    -- hClose handle

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

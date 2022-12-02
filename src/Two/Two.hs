module Two.Two where 

data Round = Round Int MyShape TheirShape

type MyShape = Shape

type TheirShape = Shape

data Shape = Rock Int | Paper Int | Scissors Int

instance Eq Shape where 
    (==) (Rock _) (Rock _) = True
    (==) (Paper _) (Paper _) = True
    (==) (Scissors _) (Scissors _) = True
    (==) _ _ = False

charToShape :: Char -> Shape
charToShape c 
        | c == 'A' || c == 'X' = Rock 1
        | c == 'B' || c == 'Y' = Paper 2
        | c == 'C' || c == 'Z' = Scissors 3
        | otherwise = Rock 0

shapeToScore :: Shape -> Int
shapeToScore (Rock s) = s
shapeToScore (Paper s) = s
shapeToScore (Scissors s) = s

didIWin :: MyShape -> TheirShape -> Bool
didIWin (Rock _) (Scissors _) = True  
didIWin (Paper _) (Rock _) = True 
didIWin (Scissors _) (Paper _) = True  
didIWin _ _ = False

parseRound :: String -> Round
parseRound s = case s of 
    [a, _, b]-> let
            theirShape = charToShape a
            myShape = charToShape b 
        in if myShape == theirShape then Round (3 + shapeToScore myShape) myShape theirShape 
            else if didIWin myShape theirShape then Round (6 + shapeToScore myShape) myShape theirShape 
                else Round (shapeToScore myShape) myShape theirShape 
    _ -> Round 0 (Rock 0) (Rock 0)
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

charToShape_ :: Char -> Shape
charToShape_ c 
        | c == 'A' = Rock 1
        | c == 'B' = Paper 2
        | c == 'C' = Scissors 3
        | otherwise = Rock 0
        
data Result = Win | Lose | Draw

charToResult :: Char -> Maybe Result
charToResult c
    | c == 'X' = Just Lose
    | c == 'Y' = Just Draw
    | c == 'Z' = Just Win
    | otherwise = Nothing

shapeToScore :: Shape -> Int
shapeToScore (Rock s) = s
shapeToScore (Paper s) = s
shapeToScore (Scissors s) = s

didIWin :: MyShape -> TheirShape -> Bool
didIWin (Rock _) (Scissors _) = True  
didIWin (Paper _) (Rock _) = True 
didIWin (Scissors _) (Paper _) = True  
didIWin _ _ = False

determineShape :: TheirShape -> Result -> MyShape
determineShape t r = case (t, r) of 
    (Rock _, Win ) -> Paper 2
    (Rock _, Draw ) -> Rock 1
    (Rock _, Lose ) -> Scissors 3
    (Paper _, Win ) -> Scissors 3
    (Paper _, Draw ) -> Paper 2
    (Paper _, Lose ) -> Rock 1
    (Scissors _, Win ) -> Rock 1
    (Scissors _, Draw ) -> Scissors 3
    (Scissors _, Lose ) -> Paper 2

parseRound :: String -> Round
parseRound s = case s of 
    [a, _, b]-> let
            theirShape = charToShape a
            myShape = charToShape b 
        in if myShape == theirShape then Round (3 + shapeToScore myShape) myShape theirShape 
            else if didIWin myShape theirShape then Round (6 + shapeToScore myShape) myShape theirShape 
                else Round (shapeToScore myShape) myShape theirShape 
    _ -> Round 0 (Rock 0) (Rock 0)

parseRoundTrue :: String -> Round
parseRoundTrue s = case s of 
    [a, _, b]-> let
            theirShape = charToShape_ a
            maybeResult = charToResult b 
            myShape = maybe (error "bad char") (determineShape theirShape) maybeResult
        in if myShape == theirShape then Round (3 + shapeToScore myShape) myShape theirShape 
            else if didIWin myShape theirShape then Round (6 + shapeToScore myShape) myShape theirShape 
                else Round (shapeToScore myShape) myShape theirShape 
    _ -> Round 0 (Rock 0) (Rock 0)
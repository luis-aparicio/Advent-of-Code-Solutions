module Five.Five where
import qualified Debug.Trace as Debug
import qualified Data.Char as Char

newtype Stack = Stack [Char]

instance Show Stack where
    show (Stack s) = show s 

stackToList :: Stack -> [Char]
stackToList (Stack l) = l

mkStack :: Int -> String -> [(Int, Stack)] -> [(Int, Stack)]
mkStack i s acc = case s of
    [x] -> acc
    x : xs -> case lookup i acc of
        Just stack -> 
            let newMap = map (\(ix , Stack curr) -> 
                    if i == ix && Char.isLetter x then (ix, Stack $ x : curr) else (ix, Stack curr)) acc
            in mkStack (succ i) xs newMap
        Nothing -> mkStack (succ i) xs acc

-- ***** Functionally Solving Problems

-- ** Reverse Polish Notation  (RPN)
import Data.Char

import System.IO
import Data.List

solveRPN :: String -> [String] -> Int
solveRPN [] [stack] = read stack
solveRPN [ele] stack = doOperation ele [] stack
solveRPN rpn stack
    | isDigit curElem = solveRPN restRPN (element:stack) -- pass the rest and the stack recursively
    | isSymbol curElem = doOperation curElem restRPN stack --(read (stack !! 1)) (read (stack !! 0))
    where (element, rest) = span (/=' ') rpn -- separate the current element from the rest
          curElem = head element -- take the first character incase it has >1 digits
          restRPN = tail rest -- remove the space at the beginning

doOperation :: Char -> String -> [String] -> Int
doOperation operator rest (a:b:stack)
    = solveRPN rest (ans:stack)
    where ans
            | operator == '+' = show (num1 + num2)
            | operator == '-' = show (num1 - num2)
            | operator == '*' = show (num1 * num2)
            -- | operator == '/' = show (num1 / num2)
            | otherwise = "0"
                where num1 = read b
                      num2 = read a
 -- "10 4 3 + 2 * -"
-- solveRPN' :: (Num a, Read a, Fractional a, Floating a) => String -> a  
solveRPN' :: String -> Float  -- 'Float' is an instance of all the above typeclasses
solveRPN' = head . foldl (updateStack) [] . words

-- updateStack :: (Num a, Read a, Fractional a, Floating a) => [a] -> String -> [a]
updateStack :: [Float] -> String -> [Float]
updateStack (a:b:acc) x
    | x == "+" = (b+a):acc
    | x == "-" = (b-a):acc
    | x == "*" = (b*a):acc
    | x == "/" = (b/a):acc -- we need (Fractional a) because of this '/'
    | x == "^" = (b**a):acc -- we need (Floating a) because of this '**'
updateStack (a:acc) "ln" = (log a):acc
updateStack acc "sum" = [sum acc] -- sum of all ements in the stack
updateStack acc x = (read x):acc -- we need (Read a) because of this 'read'

-- ***** Heathrow to London (finding shortest path)
-- 50  10  30  5  90  20 40  2  25  10  8  0 

data Section = Section {getA :: Int, getB:: Int, getC :: Int} deriving (Show)
data Label = A | B | C deriving (Show)
type Path = [(Label, Int)] -- [(B,10),(C,30),(A,5),(C,20),(B,2),(B,8)]  

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) = 
    let priceA = sum $ map snd pathA
        priceB = sum $ map snd pathB
        newPathToA = if forwardPrice <= crossPrice 
            then (A,a):pathA 
            else (C,c):(B,b):pathB
            where forwardPrice = priceA + a
                  crossPrice = priceA + b + c
        newPathToB = if forwardPrice <= crossPrice 
            then (B,b):pathB 
            else (C,c):(A,a):pathA
            where forwardPrice = priceB + b
                  crossPrice = priceB + a + c
    in (newPathToA, newPathToB)

heathrowToLondon :: IO ()
heathrowToLondon = do
    contents <- readFile "path.txt"
    let roads = map read $ lines contents
        threes = chunks 3 roads
        sections = map (\[a,b,c] -> Section a b c) threes
        bestPath = shortestPath sections
        pathString = intercalate " -> " (map (show . fst) bestPath)
        price = show $ sum $ map snd bestPath
    putStrLn $ "The best path is: " ++ pathString
    putStrLn $ "The total time it takes is: " ++ price
    
chunks :: Int -> [Int] -> [[Int]]
chunks _ [] = []
chunks n xs = let (ys, zs) = splitAt n xs in  ys : chunks n zs
    
shortestPath :: [Section] -> Path
shortestPath sections = let (bestPathA, bestPathB) = foldl (roadStep) ([],[]) sections
                        in if (sum $ map snd bestPathA) <= (sum $ map snd bestPathB)
                            then reverse bestPathA
                            else reverse bestPathB
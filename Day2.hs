import Control.Monad (liftM2)
import Data.Bifunctor (bimap)

main :: IO ()
main = do
    input <- readFile "Day2.txt"
    let rounds :: [(Char,Char)]
        rounds = (\s -> (s !! 0,s !! 2)) <$> lines input
    
    print
        $ sum
        $ liftM2 (+) (getResultPoint . calculateResult) (getShapeScore . snd)
            . bimap getElfShape getMyShape <$> rounds

    print
        $ sum
        $ liftM2 (+) (getResultPoint . snd) (getShapeScore . getShapeToPlay)
            . bimap getElfShape getEnd <$> rounds

data Shape = Rock | Paper | Scissors deriving (Eq)
data Result = Lose | Draw | Win deriving (Eq)

getElfShape :: Char -> Shape
getElfShape c = case c of
    'A' -> Rock
    'B' -> Paper
    'C' -> Scissors

getMyShape :: Char -> Shape
getMyShape c = case c of
    'X' -> Rock
    'Y' -> Paper
    'Z' -> Scissors

getShapeScore :: Shape -> Int
getShapeScore shape = case shape of
    Rock     -> 1
    Paper    -> 2
    Scissors -> 3

calculateResult :: (Shape,Shape) -> Result
calculateResult (a,b)
    | a == b = Draw
    | a == Rock && b == Paper || a == Paper && b == Scissors || a == Scissors && b == Rock = Win
    | otherwise = Lose

getResultPoint :: Result -> Int
getResultPoint r = case r of
    Lose -> 0
    Draw -> 3
    Win  -> 6

getEnd :: Char -> Result
getEnd c = case c of
    'X' -> Lose
    'Y' -> Draw
    'Z' -> Win

getShapeToPlay :: (Shape,Result) -> Shape
getShapeToPlay (s,r) = case r of
    Draw -> s
    Win  -> if s == Rock then Paper else
            if s == Paper then Scissors else
            Rock
    Lose -> if s == Rock then Scissors else
            if s == Paper then Rock else
            Paper 

import Data.Maybe (fromJust)
import Control.Applicative ((<|>))

import Data.List as L

main :: IO ()
main = do
    input <- readFile "Day3.txt"

    let rucksacks :: [String]
        rucksacks = lines input

    print
        $ sum
        $ getItemPriority . head . (uncurry intersect) . splitIntoHalf
        <$> rucksacks

    print
        $ sum
        $ getItemPriority . head . foldl1 (intersect) 
        <$> groupElves rucksacks
    
priorities :: [Char]
priorities = ['a'..'z'] ++ ['A'..'Z']

getItemPriority :: Char -> Int
getItemPriority = (+1) . fromJust . (flip L.elemIndex $ priorities)

splitIntoHalf :: [a] -> ([a], [a])
splitIntoHalf xs = let n = length xs `div` 2 in splitAt n xs

groupElves :: [String] -> [[String]]
groupElves = f []
    where f xs [] = xs
          f xs ys = let (a, b) = splitAt 3 ys in f (a:xs) b

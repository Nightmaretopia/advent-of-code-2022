import qualified Data.List as L

main :: IO ()
main = do
    input <- readFile "Day1.txt" 
    let elves :: [Int]
        elves = getElves input

    print $ maximum elves
    print $ sum $ drop (length elves - 3) $ L.sort elves

getElves :: String -> [Int]
getElves = L.foldl' f [0] . lines
    where f xs     "" = 0 : xs
          f (x:xs) s  = x + read s : xs

import Data.Bifunctor (bimap)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

main :: IO ()
main = do
    input <- readFile "Day4.txt"
    let pairs :: [((Int, Int), (Int, Int))]
        pairs = map parsePairs
                $ lines input

    print
        $ length
        $ filter (\((a, b), (x, y)) -> (a >= x && b <= y) || (x >= a && y <= b)) pairs

    print
        $ length
        $ filter (\(ab@(a, b), xy@(x, y)) -> x `within` ab || y `within` ab || a `within` xy || b `within` xy) pairs

within :: Int -> (Int, Int) -> Bool
within n (a, b) = b >= n && n >= a

parsePairs :: String -> ((Int, Int), (Int, Int))
parsePairs = bimap g g . f
    where f         :: String -> (String, String)
          f         = split ',' 
          g         :: String -> (Int, Int)
          g         = bimap read read . split '-'
          split     :: Char -> String -> (String, String)
          split c s = drop 1 <$> splitAt (fromJust $ elemIndex c s) s

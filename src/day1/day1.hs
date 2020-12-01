import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time

findAddendsThatSumTo :: Int -> Map Int Bool -> [Int] -> Maybe (Int, Int)
findAddendsThatSumTo _ _ [] = Nothing
findAddendsThatSumTo total map (x : xs) =
  case Map.lookup (total - x) map of
    Nothing -> findAddendsThatSumTo total (Map.insert x True map) xs
    Just _ -> Just (total - x, x)

findThreeThatSumTo :: Int -> [Int] -> Maybe (Int, Int, Int)
findThreeThatSumTo _ [] = Nothing
findThreeThatSumTo total (x : xs) =
  case findAddendsThatSumTo (total - x) Map.empty xs of
    Nothing -> findThreeThatSumTo total xs
    Just (a, b) -> Just (a, b, x)

main :: IO ()
main = do
  input <- readFile "../../res/input-day1.txt"
  let fileLines = lines input
  let expenses = map read fileLines
  case findAddendsThatSumTo 2020 Map.empty expenses of
    Nothing -> print "Couldn't find a sum to 2020!"
    Just (a, b) -> print (a * b)
  case findThreeThatSumTo 2020 expenses of
    Nothing -> print "Couldn't find a sum to 2020 with 3 addends!"
    Just (a, b, c) -> print (a * b * c)

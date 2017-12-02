import System.IO  
import Data.List

listOfInts :: String -> [[Int]]
listOfInts = map (map (read::String -> Int) . words) . lines

part1 :: String -> Int
part1 = sum . listOfDiffs
  where listOfDiffs = map rowDiff . listOfInts
        rowDiff xs  = maximum xs - minimum xs

part2 :: String -> Int
part2 = sum . rowDiffs
  where rowDiffs         = map (rowDiff . divRow) . listOfInts
        rowDiff xs       = quot (head xs) (xs!!1)
        divRow           = head . head . removeEmpty . map removeEmpty . findDiv
        findDiv xs       = listOfDiv sorted:findDiv (drop 1 sorted)
          where sorted = sortBy (flip compare) xs
        listOfDiv []     = []
        listOfDiv (x:xs) = map (isDivisable x) xs
        isDivisable x y  = if x `rem` y == 0 then [x, y] else []
        removeEmpty      = filter (not . null)

main :: IO ()
main = do  
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print $ part1 contents 
  print $ part2 contents 
  hClose handle


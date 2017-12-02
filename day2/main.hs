import System.IO  
import Control.Monad
import Data.List

listOfInts xs = map (map (read::String -> Int)) $ map words $ lines xs
isDivisable x y = case x `rem` y == 0 of True  -> [ x, y ]
                                         False -> []
part1 xs = sum $ listOfDiffs xs
  where listOfDiffs xs = map rowDiff $ listOfInts xs
        rowDiff xs     = (maximum xs) - (minimum xs)

part2 xs = sum rowDiffs
  where list             = listOfInts xs
        rowDiffs         = map rowDiff $ map divRow list
        rowDiff xs       = quot (xs!!0) (xs!!1)
        divRow xs        = head $ head $ removeEmpty $
                           map removeEmpty $ findDiv $ xs
        findDiv xs       = (listOfDiv sorted):(findDiv (drop 1 sorted))
          where sorted = reverse . sort $ xs
        listOfDiv (x:xs) = map (isDivisable x) $ xs
        removeEmpty xs   = filter (\l -> length l > 0) xs

main = do  
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print $ part1 contents 
  print $ part2 contents 
  hClose handle


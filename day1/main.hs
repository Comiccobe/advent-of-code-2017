import System.IO  
import Data.Char (isSpace, digitToInt)

parts :: (Int -> Int) -> String -> Int
parts f xs = sum . take listLength . zipWith getVal list $ offsetList
  where list       = map digitToInt $ trimEnd xs
        offsetList = drop (f listLength) list ++ list
        listLength = length list
        trimEnd    = reverse . dropWhile isSpace . reverse
        getVal x y = if x == y then x else 0

main :: IO ()
main = do  
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print $ parts (const 1) contents
  print $ parts (`quot` 2) contents
  hClose handle


import System.IO  
import Control.Monad
import Data.Char (isSpace, digitToInt)

evaluate f xs = sum $ take listLength $ zipWith getVal cyclicList $ drop (f listLength) cyclicList
  where list       = toInts xs
        listLength = length list
        cyclicList = cyclic list
        toInts xs  = map digitToInt $ trimEnd xs
        trimEnd    = reverse . dropWhile isSpace . reverse
        cyclic xs  = xs ++ cyclic xs
        getVal x y = case x == y of True  -> x
                                    False -> 0

main = do  
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print $ evaluate (\l -> 1) contents
  print $ evaluate (\l -> quot l 2) contents
  hClose handle


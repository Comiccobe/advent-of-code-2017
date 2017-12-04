import System.IO  
import Data.List
import qualified Data.Set as Set

checkPw :: [String] -> Bool
checkPw xs = length xs == length (unique xs)
  where unique = Set.toList . Set.fromList

checkPws :: [String] -> [Bool]
checkPws [] = []
checkPws (x:xs) = checkPw asWords:checkPws xs
  where asWords = words x

checkPwsEx :: [String] -> [Bool]
checkPwsEx [] = []
checkPwsEx (x:xs) = (not (hasAnagram asWords) && checkPw asWords):checkPwsEx xs
  where asWords = words x
        hasAnagram ys = not (True `notElem` concat (hasAnagrams ys))
          where hasAnagrams [] = []
                hasAnagrams (z:zs) = map (isAnagram z) zs:hasAnagrams zs 
                  where isAnagram a b = sort a == sort b

parts :: ([String] -> [Bool]) -> String -> Int
parts f xs = length $ filter (==True) $ f $ lines xs 

main :: IO ()
main = do  
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print $ parts checkPws contents
  print $ parts checkPwsEx contents
  hClose handle


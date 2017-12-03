
pointsDist :: (Int, Int) -> (Int, Int) -> Int
pointsDist (x, y) (a, b) = (dist x a) + (dist y b)

dist :: Int -> Int -> Int
dist x y = abs (max x y - min x y)

thd :: (a, a, b) -> b
thd (_, _, a) = a

pos :: Int -> (Int, Int)
pos x = walkPath $ take x pattern
  where walkPath xs = (sum [a | (a, _) <- (0, 0):xs],
                       sum [b | (_, b) <- (0, 0):xs])
        pattern = (0, 0):(concat $ tuples (0::Int) $ merge [1,2..] [1,2..])
          where merge [] ys = ys
                merge (y:ys) zs = y:merge zs ys
                tuples _ [] = []
                tuples n (y:ys) = replTuple n y:tuples (n+1) ys
                  where replTuple a z = replicate z (dir a)
                          where dir d = case d `rem` 4 of 0 -> (1, 0)
                                                          1 -> (0, 1)
                                                          2 -> (-1, 0)
                                                          3 -> (0, -1)
                                                          _ -> (0, 0)

part1 :: Int -> Int
part1 x = pointsDist (0, 0) $ pos x

part2 :: Int -> Int
part2 x = eval 0
  where eval n = if currentPoints > x then currentPoints else eval (n+1)
          where currentPoints = thd $ head $ points n
                  where points 0 = [(0, 0, 1)]
                        points 1 = [(0, 0, 1)]
                        points p = new:xs
                          where loc = pos p
                                xs  = points (p-1)
                                new = (fst loc, snd loc, pts)
                                pts = sum
                                  (map thd (filter (closeEnough loc) xs))
                                closeEnough (a, b) (c, d, _) =
                                  dist a c < 2 && dist b d < 2

main :: IO ()
main = do  
  print $ part1 347991
  print $ part2 347991


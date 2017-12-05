import qualified Data.Vector.Mutable as V
import Control.Monad

main :: IO ()
main = do
  d <- (map read . lines) <$> readFile "05.in"
  putStr "Part One: " >> part1 d
  putStr "Part Two: " >> part2 d

part1 = solve (+1)
part2 = solve (\o -> if o >= 3 then o-1 else o+1)

solve f d = do
  v <- V.new l
  forM_ (zip [0..] d) $ uncurry (V.write v)
  go v 0 0
 where
  l = length d
  go v i s
    | i < 0 || i >= l = print s
    | otherwise = do
      o <- V.read v i
      V.write v i (f o)
      go v (i+o) (s+1)

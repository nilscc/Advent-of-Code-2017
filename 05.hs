import qualified Data.Vector.Mutable as V
import Control.Monad

getData :: IO [Int]
getData = (map read . lines) <$> readFile "05.in"

main :: IO ()
main = do
  d <- getData
  part1 d
  part2 d

part1 = solve (const (+1))
part2 = solve (\o -> if o >= 3 then subtract 1 else (+1))

solve f d = do

  let l = length d

  -- copy to mutable vector
  v <- V.new l
  forM_ (zip [0..] d) $ \(i, val) ->
    V.write v i val

  -- solve problem
  let go i s
        | i >= l = print s
        | otherwise = do
          -- get offset
          o <- V.read v i
          V.modify v (f o) i
          go (i+o) (s+1)

  go 0 0

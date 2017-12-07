import qualified Data.Set as S

main = do
  part1 test
  part1 input
  part2 test
  part2 input

test, input :: [Int]
test = [0, 2, 7, 0]
input = [5, 1, 10, 0, 1, 7, 13, 14, 3, 12, 8, 10, 7, 12, 0, 6]

--------------------------------------------------------------------------------
-- Part One

reallocate l = l'
 where
  m = maximum l
  (a,(_:b)) = span (< m) l

  p = m `div` length l
  q = m `mod` length l

  cond b = if b then 1 else 0

  l' = [ x + p + cond (i < q - length b) | (x,i) <- zip a [0..] ]
    ++ [p]
    ++ [ x + p + cond (i < q)            | (x,i) <- zip b [0..] ]

part1 input = print $ go (iterate reallocate input) S.empty 0
 where
  go (first : rest) mem c
    | first `S.member` mem = c
    | otherwise = go rest (first `S.insert` mem) (c+1)

--------------------------------------------------------------------------------
-- Part Two

part2 input = print $ go (iterate reallocate input) S.empty
 where
  go (first : rest) mem
    | first `S.member` mem = (1 +) . length $ takeWhile (first /=) rest
    | otherwise = go rest (first `S.insert` mem)

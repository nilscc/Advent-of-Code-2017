import Control.Monad

-- small unit testing helpers :)
test name b = unless b $ putStrLn $ "Error: '" ++ name ++ "' failed."
isEqual f a b = test
  ("f(" ++ show a ++ ") = " ++ show (f a) ++ " (expected: " ++ show b ++ ")")
  (f a == b)

--------------------------------------------------------------------------------
-- Part One

steps :: Integer -> Integer
steps 1 = 0
steps loc =

  let -- lookup the next power of 2 of current loc
      p = ceiling $ sqrt (fromIntegral loc)
      -- increment to odd power of 2
      q | even p    = p + 1
        | otherwise = p

      -- r is the "inner" distance to 1 => the first part of the result
      r = q `div` 2

      -- magic...
      pos = ((loc - (q-2)^2 - 1) `mod` (q-1)) + 1

      -- s is difference from of the current position to the 'center' position
      -- => second part of result
      s = abs(pos - r)

      -- combine final result
      res = r + s
   in res

part1 :: IO ()
part1 = do

  isEqual steps 1 0
  isEqual steps 2 1
  isEqual steps 3 2
  isEqual steps 4 1
  isEqual steps 5 2
  isEqual steps 6 1
  isEqual steps 7 2
  isEqual steps 8 1
  isEqual steps 9 2
  isEqual steps 10 3
  isEqual steps 11 2
  isEqual steps 12 3
  isEqual steps 13 4
  isEqual steps 14 3
  isEqual steps 15 2
  isEqual steps 16 3
  isEqual steps 17 4
  isEqual steps 23 2

  isEqual steps 1024 31

  -- the final answer is... 419 :)
  isEqual steps 289326 419
  print $ steps 289326

--------------------------------------------------------------------------------
-- Part Two

part2 :: IO ()
part2 = return ()

--------------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  putStrLn "Part One"
  part1
  putStrLn ""
  putStrLn "Part Two"
  part2

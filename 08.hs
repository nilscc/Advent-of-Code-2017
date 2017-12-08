import qualified Data.Map as M
import Data.Maybe

test = "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10"

main = do
  part1 test
  part1 =<< readFile "08.in"
  part2 test
  part2 =<< readFile "08.in"

--------------------------------------------------------------------------------
-- Part One

data Exp = Exp String (Integer -> Integer) Cond

data Cond = Cond String (Integer -> Bool)

-- parsing

parse :: [String] -> Exp
parse (var1:op1:val1:"if":var2:op2:val2:[]) =
  Exp var1
    (parseExp op1 (read val1))
    (Cond var2 (parseCond op2 (read val2)))
parse l = error $ "Invalid expression: " ++ show l

parseExp "inc" val = (+ val)
parseExp "dec" val = (subtract val)

parseCond "==" val = (== val)
parseCond "<"  val = (<  val)
parseCond "<=" val = (<= val)
parseCond ">"  val = (>  val)
parseCond ">=" val = (>= val)
parseCond "!=" val = (/= val)

prepare = map (parse . words) . lines

-- evaluation

eval mem (Exp var op cond)
  | isTrue mem cond = M.alter (Just . op . fromMaybe 0) var mem
  | otherwise       = mem

isTrue mem (Cond var op) = op (M.findWithDefault 0 var mem)

part1 = print . maximum . foldl eval M.empty . prepare

--------------------------------------------------------------------------------
-- Part Two

part2 = print . maximum . map maximum . filter (not . null) . scanl eval M.empty . prepare

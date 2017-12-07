import qualified Data.Set as S

main = do
  part1 =<< getInput "07-test.in"
  part1 =<< getInput "07.in"
  putStrLn ""
  part2 =<< getInput "07-test.in"
  part2 =<< getInput "07.in"
 where
  getInput fp = (map (parse . words) . lines) <$> readFile fp

data InputEntry = InputEntry
  { name :: String
  , weight :: Int
  , above :: [String]
  }
  deriving (Show, Eq)

parse (n:w:[])     = InputEntry n (parseWeight w) []
parse (n:w:"->":r) = InputEntry n (parseWeight w) (parseAbove r)

parseWeight = read . init . tail

parseAbove (x:[]) = [x]
parseAbove (x:r)  = init x : parseAbove r

--------------------------------------------------------------------------------
-- Part One

findBottom :: [InputEntry] -> InputEntry
findBottom entries = head [ x | x <- lowers, not (name x `S.member` aboves) ]
 where
  lowers = filter (not . null . above) entries
  aboves = S.fromList $ concatMap above lowers

part1 = print . findBottom

--------------------------------------------------------------------------------
-- Part Two

data Entry = Entry
  { entryName :: String
  , entryWeight :: Int
  , entryAbove :: [Entry]
  }
  deriving (Show, Eq)

buildTree :: [InputEntry] -> Entry
buildTree entries = go $ findBottom entries
 where
  go (InputEntry n w a) = Entry n w (map go [ x | x <- entries, name x `elem` a ])

calcWeight :: Entry -> Int
calcWeight (Entry _ w a) = w + sum (map calcWeight a)

balanced :: Entry -> Bool
balanced (Entry _ _ [])     = True
balanced (Entry _ _ (x:xs)) = all (calcWeight x ==) (map calcWeight xs)

findUnbalanced :: Entry -> [Entry]
findUnbalanced e@(Entry _ _ a)
  | not (balanced e) && and (map balanced a) = a
  | otherwise = concatMap findUnbalanced a

-- simply show entry weight + calculated tower weight, then figure out the rest
-- manually... :D
solve l = [ (entryWeight x, calcWeight x) | x <- l ]

part2 = print . solve . findUnbalanced . buildTree

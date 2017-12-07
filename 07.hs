import qualified Data.Set as S
getInput fp = (map (parse . words) . lines) <$> readFile fp

main = do
  print . findBottom =<< getInput "07-test.in"
  print . findBottom =<< getInput "07.in"

data Entry = Entry
  { name :: String
  , weight :: Int
  , above :: [String]
  }
  deriving (Show, Eq)

parse (n:w:[])     = Entry n (parseWeight w) []
parse (n:w:"->":r) = Entry n (parseWeight w) (parseAbove r)

parseWeight = read . init . tail
parseAbove (x:[]) = [x]
parseAbove (x:r) = init x : parseAbove r

--------------------------------------------------------------------------------
-- Part One

findBottom :: [Entry] -> Entry
findBottom entries = head [ x | x <- lowers, not (name x `S.member` aboves) ]
 where
  lowers = filter (not . null . above) entries
  aboves = S.fromList $ concatMap above lowers

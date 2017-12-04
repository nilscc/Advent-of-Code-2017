import Prelude hiding (words)
import Control.Arrow
import Data.Char
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T

getLines = T.lines <$> T.readFile "04.in"
words = T.split isSpace

valid = uncurry (==) . ((L.nub . L.sort) &&& L.sort)

main = print . length . filter (valid . words) =<< getLines

import Prelude hiding (words, lines, length, filter, readFile)
import Control.Arrow
import Control.Applicative
import Data.Char
import Data.List hiding (words, lines)
import Data.Text hiding (length, filter)
import Data.Text.IO

valid = uncurry (==) . ((nub . sort) &&& sort)

main = print . length . filter (valid . words) . lines =<< readFile "04.in"

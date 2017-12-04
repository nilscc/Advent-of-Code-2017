import Control.Arrow
import Data.List

valid = uncurry (==) . ((nub.sort) &&& sort)

main =                            print
 .(                         length *** length                           )
 .(           filter (valid.words) &&& filter (valid.map sort.words)    )
 .                                lines
 =<< readFile                    "04.in"

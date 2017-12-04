---------------------------------------------------------------------------
--
--       2017        * A D V E N T * O F * C O D E *            DAY4
--
--                                starring
--
import                         Control.Arrow
import                            Data.List
--
-------------------- P A R T  O N E ----- P A R T  T W O ------------------

main =                              print
 .(                           length *** length                           )
 .(             filter (valid.words) &&& filter (valid.map sort.words)    )
 .                                  lines
 =<< readFile                      "04.in"

valid = uncurry (==) .   ((nub.sort) &&& sort)

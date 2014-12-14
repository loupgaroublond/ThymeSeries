import Prelude
import Data.Array.IArray
import Data.List
import Data.Series
import Data.Division
import Data.Record
import Data.Time hiding (months)
import Control.Applicative
import Control.Comonad

import qualified Text.Show.Pretty as PR

pp :: Show a => a -> IO ()
pp = putStrLn . PR.ppShow

ss = Series 1 $ listArray (0,3) [ 0.25, -0.1, 0.5, -0.3 ]

s1 :: Series (DividedYearData Int Month) Int
s1 = Series (DYDM 2015 Mar) $ listArray (DYDM 2015 Jan, DYDM 2015 Dec) $ range (1,13)

s1r :: Series (DividedYearData Int Quarter) Int
s1r = rollUp s1


main = do
  pp ss
  pp s1
  pp s1r

module Data.Series where

import Data.Array.IArray
import Control.Applicative
import Control.Comonad


data Series t e = Series { curTime :: t
                         , unSeries :: Array t e
                         }
                  deriving Show

minTime :: Ix t => Series t e -> t
minTime = fst . bounds . unSeries

predSeries :: (Enum t, Ix t) => Series t e -> Series t e
predSeries = Series <$> (pred . curTime) <*> unSeries

peekSeries :: Ix t => Series t e -> e
peekSeries = (!) <$> unSeries <*> curTime

seriesHist :: Ix t => Series t e -> [e]
seriesHist = map <$> ((!) . unSeries) <*> range . pastToPresentTime where
    pastToPresentTime = (,) <$> minTime <*> curTime
 
instance Ix t => Functor (Series t) where
  fmap f s = Series (curTime s) $ fmap f (unSeries s)

sumOfSeries :: (Enum t, Num e, Ix t) => Series t e -> e
sumOfSeries = sum . seriesHist

diffOfSeries :: (Enum t, Num e, Ix t) => Series t e -> e
diffOfSeries = (-) <$> peekSeries <*> peekBack 0


peekBack e s | curTime s <= minTime s = e
             | otherwise = peekSeries $ predSeries s


instance Ix t => Comonad (Series t) where
  extract = peekSeries
  duplicate = Series <$> curTime <*> arrayOfSeries where
    arrayOfSeries = array <$> bounds . unSeries <*> repeatSeries
    repeatSeries = zip <$> ixs <*> allPositions
    allPositions = fmap <$> flip Series . unSeries <*> ixs
    ixs = range . bounds . unSeries

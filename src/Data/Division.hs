{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Division where

import Prelude
import Data.Ix

{-# INLINE enumRange #-}
enumRange :: Enum a => (a, a) -> [a]
enumRange (l,r) = map toEnum $ range (fromEnum l, fromEnum r)

{-# INLINE enumIndex #-}
enumIndex :: Enum a => (a, a) -> a -> Int
enumIndex (l,r) i = index (fromEnum l, fromEnum r) (fromEnum i)

{-# INLINE enumInRange #-}
enumInRange :: Enum a => (a, a) -> a -> Bool
enumInRange (l,r) i = inRange (fromEnum l, fromEnum r) (fromEnum i)

{-# INLINE enumRangeSize #-}
enumRangeSize :: Enum a => (a, a) -> Int
enumRangeSize (l,r) = rangeSize (fromEnum l, fromEnum r)

data Month = Jan | Feb | Mar | Apr
           | May | Jun | Jul | Aug
           | Sep | Oct | Nov | Dec
             deriving (Show, Enum, Bounded, Eq, Ord, Ix)

data Quarter = Q1 | Q2 | Q3 | Q4
               deriving (Show, Enum, Bounded, Eq, Ord, Ix)


allBoundedIxs :: (Ix a, Bounded a) => [a]
allBoundedIxs = range (minBound, maxBound)

months :: Int
months = length (allBoundedIxs::[Month])

quarters :: Int
quarters = length (allBoundedIxs::[Quarter])

class (Ix s, Ix s', Bounded s, Bounded s') => SubdivisionTransition s s' where
  rollUpSubdivision :: s -> s'
  rollDownSubdivision :: s' -> [s]
  
-- There is probably some church numeral way to do this but not going down that road tonight
instance SubdivisionTransition Month Quarter where
  rollUpSubdivision m = toEnum $ div (fromEnum m) $ months `div` quarters
  rollDownSubdivision q = enumFromTo (toEnum $ (fromEnum q) * divisions) (toEnum $ (fromEnum q) * divisions + divisions - 1) where 
    divisions = months `div` quarters


class (Enum y, Enum d) => DividedYear y d where
  data DividedYearData y d :: *
  yearDivision :: y -> d -> DividedYearData y d
  year :: DividedYearData y d -> y
  division :: DividedYearData y d -> d
  
instance DividedYear Int Month where
  data DividedYearData Int Month = DYDM Int Month
  yearDivision = DYDM
  year (DYDM y _ ) = y
  division (DYDM _ m ) = m

instance DividedYear Int Quarter where
  data DividedYearData Int Quarter = DYDQ Int Quarter
  yearDivision = DYDQ
  year (DYDQ y _ ) = y
  division (DYDQ _ q ) = q

deriving instance Show (DividedYearData Int Month)
deriving instance Show (DividedYearData Int Quarter)
deriving instance Eq (DividedYearData Int Month)
deriving instance Eq (DividedYearData Int Quarter)

instance Enum (DividedYearData Int Month) where
  fromEnum dy = (year dy) * months + (fromEnum $ division dy)
  toEnum i = let (year, division) = divMod i months in
    yearDivision year (toEnum division)

instance Enum (DividedYearData Int Quarter) where
  fromEnum dy = (year dy) * quarters + (fromEnum $ division dy)
  toEnum i = let (year, division) = divMod i quarters in
    yearDivision year (toEnum division)

instance (Eq (DividedYearData y d), Enum (DividedYearData y d)) => Ord (DividedYearData y d) where
    compare a b = compare (fromEnum a) (fromEnum b)

instance (Eq (DividedYearData y d), Enum (DividedYearData y d)) => Ix (DividedYearData y d) where
    range           = enumRange
    index           = enumIndex
    inRange         = enumInRange
    rangeSize       = enumRangeSize

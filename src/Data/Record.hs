{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Record where

import Control.Applicative
import Data.Array.IArray
import Data.Series
import Data.Division

class Record t where
  data RecordData t :: * -> *
  time :: RecordData t e -> t
  element :: RecordData t e -> e
  
instance Record (DividedYearData Int Month) where
  data RecordData (DividedYearData Int Month) e = RecordMY (DividedYearData Int Month) e
  time (RecordMY t _) = t
  element (RecordMY _ e) = e

recordToAssoc :: Record t => RecordData t e -> (t, e)
recordToAssoc = (,) <$> time <*> element

monthToQuarter = id
monthToYear = id
quarterToYear = id
yearToQuarter = (/) 4
yearToMonth = (/) 12
quarterToMonth = (/) 3

class Num e => RollUp t t' e where
    rollUp :: Series t e -> Series t' e
    
class (Record r, Record r') => RecordTransition r r' e where
  rollUpTime :: r -> r'
  rollDownTime :: r' -> r
  elementBase :: e
  rollUpElement :: e -> e
  rollUpRecord :: RecordData r e -> RecordData r' e
  rollDownRecord :: RecordData r' e -> [RecordData r e]
  
-- instance RecordTransition MonthYear QuarterYear Int where
--   rollUpTime (MonthYear y Jan) = QuarterYear y Q1
--   rollUpTime (MonthYear y Jan) = QuarterYear y Q1
--   rollUpTime (MonthYear y Jan) = QuarterYear y Q1
--   rollUpTime (MonthYear y Jan) = QuarterYear y Q1
--   rollUpTime (MonthYear y Jan) = QuarterYear y Q1
--   rollUpTime (MonthYear y Jan) = QuarterYear y Q1
--   rollUpTime (MonthYear y Jan) = QuarterYear y Q1
--   rollUpTime (MonthYear y Jan) = QuarterYear y Q1
--   rollUpTime (MonthYear y Jan) = QuarterYear y Q1
--   rollUpTime (MonthYear y Jan) = QuarterYear y Q1
--   rollUpTime (MonthYear y Jan) = QuarterYear y Q1
--   rollUpTime (MonthYear y Jan) = QuarterYear y Q1
  

instance Num e => RollUp (DividedYearData Int Month) (DividedYearData Int Quarter) e where
  rollUp = Series <$> mtq . curTime <*> rollUpMTQ . unSeries where
    mtq = DYDQ <$> year <*> rollUpSubdivision . division
    rollUpMTQ = accumArray (+) 0 <$> rollUpBounds <*> map rollUpAssocs . assocs
    rollUpAssocs = (,) <$> mtq . fst <*> monthToQuarter . snd
    rollUpBounds = (,) <$> mtq . fst . bounds <*> mtq . snd . bounds
                      

module Module(Year
             , year
             , YearType(..)
             , ModuleType(..)
             , SuffixType(..)
             ) where

import Control.Exception (assert)

data Year = Year Int deriving (Show,Eq,Ord)

instance Bounded Year where
    minBound = Year 1
    maxBound = Year 6

instance Enum Year where
    succ all@(Year num)
      | all < maxBound = Year $ num+1
      | otherwise = error "At maximum"
    pred all@(Year num)
      | all > minBound = Year $ num-1
      | otherwise = error "At minimum"
    toEnum num = Year num
    fromEnum (Year n) = n
    enumFromTo (Year from) (Year to) = map year [from..to]
    enumFrom from = enumFromTo from maxBound
    enumFromThenTo (Year fst) (Year snd) (Year to) = map year [fst, snd..to]
    enumFromThen fst snd
      | fst <= snd = enumFromThenTo fst snd maxBound
      | fst > snd = enumFromThenTo fst snd minBound



year :: Int -> Year
year x =  assert (x>=1 && x<=6) $ Year x

data YearType = Foundation | Intermediate | Advanced deriving (Show, Read, Eq, Ord, Enum)
data ModuleType = Elective | Enrichment | Core | Honor deriving (Show, Read, Eq)
data SuffixType = Preclusion | CorePrereq | MTinLieu | External deriving (Show, Read, Eq)


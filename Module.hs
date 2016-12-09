module Module(Year
             , year
             , YearType(..)
             , ModuleType(..)
             , SuffixType(..)
             , asInt
             ) where

import Control.Exception (assert)
import Common

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
    enumFromTo = defaultEnumFromTo
    enumFrom = boundedEnumFrom
    enumFromThenTo = defaultEnumFromThenTo
    enumFromThen = boundedEnumFromThen


year :: Int -> Year
year x =  assert (x>=1 && x<=6) $ Year x

asInt :: Year -> Int
asInt (Year n) = n

data YearType = Foundation | Intermediate | Advanced deriving (Show, Read, Eq, Ord, Enum)
data ModuleType = Elective | Enrichment | Core | Honor deriving (Show, Read, Eq)
data SuffixType = EmptySuffix | Preclusion | CorePrereq | MTinLieu | External deriving (Show, Read, Eq)


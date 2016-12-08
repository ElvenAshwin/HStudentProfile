module Common (
    boundedEnumFrom
  , boundedEnumFromThen
  , defaultEnumFromTo
  , defaultEnumFromThenTo 
  , Stringable(..))
      where

class Stringable c where
    stringify :: [c] -> String

instance Stringable Char where
    stringify str = str
--how to trick Haskell into accepting [Char] as an instance of something

boundedEnumFrom :: (Bounded a, Enum a) => a -> [a]
boundedEnumFrom from = enumFromTo from maxBound

boundedEnumFromThen:: (Bounded a, Enum a, Ord a) => a->a->[a]
boundedEnumFromThen fst snd
  | fst<=snd = enumFromThenTo fst snd maxBound
  | otherwise = enumFromThenTo fst snd minBound

defaultEnumFromTo :: (Enum a) => a -> a -> [a]
defaultEnumFromTo from to = map toEnum [fromEnum from .. fromEnum to]

defaultEnumFromThenTo :: (Enum a) => a -> a -> a -> [a]
defaultEnumFromThenTo fst snd to = map toEnum [fromEnum fst, fromEnum snd .. fromEnum to]

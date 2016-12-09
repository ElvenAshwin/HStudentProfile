module Common ( boundedEnumFrom
              , boundedEnumFromThen
              , defaultEnumFromTo
              , defaultEnumFromThenTo
              , subList
              , quintupletMap
              , showQuintuplet
              , quintupletToList
              , Stringable(..)) where

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

subList :: (Integral b) => [a] -> b -> b -> [a]
subList ls start end = drop s (take e ls)
    where s = fromIntegral start
          e = fromIntegral end

quintupletMap :: ((a->a1),(b->b1),(c->c1),(d->d1),(e->e1)) -> (a,b,c,d,e) -> (a1,b1,c1,d1,e1)
quintupletMap (af,bf,cf,df,ef) (a,b,c,d,e) = (af a, bf b, cf c, df d, ef e)

showQuintuplet :: (Show a, Show b, Show c, Show d, Show e) => (a,b,c,d,e) -> (String, String, String, String, String)
showQuintuplet quint = quintupletMap (show,show,show,show,show) quint

quintupletToList :: (a,a,a,a,a) -> [a]
quintupletToList (fst,snd,trd,frt,fft) = [fst,snd,trd,frt,fft]


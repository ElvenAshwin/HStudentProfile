module Grading (
    EnrichmentGrade(..)
  , DVGrade(..)
  , LetterGrade
  , lGrade) where

import Control.Exception (assert)
import Data.List (elemIndex,find)

data EnrichmentGrade = Fail | Pass 
                     | ENRMerit | Distinction deriving (Show, Read, Eq, Ord, Enum, Bounded)
data DVGrade = Unsatisfactory | Satisfactory 
             | DVMerit | Excellent deriving (Show, Read, Eq, Ord, Enum, Bounded)

type Score = Double
data LetterGrade = LetterGrade String Score deriving (Eq)

class GradeRepresentation a where
    index' :: a -> Maybe Int
    index :: a -> Int 
    index repr = extract $ index' repr
        where extract (Just num) = num
              extract Nothing = error "No such index"

instance GradeRepresentation [Char] where
    index' str = str `elemIndex` letters

instance GradeRepresentation Char where
    index' char = [char] `elemIndex` letters

instance GradeRepresentation Double where
    index' num = num `elemIndex` scores

instance GradeRepresentation LetterGrade where
    index' (LetterGrade repr score) = repr `elemIndex` letters
letters = ["F","D","D+","C","C+","B-","B","B+","A-","A","A+"]
scores = [0.0, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.0]

letter_score_assoc = zip letters scores
grades_assoc = map (\(letter,score) -> (LetterGrade letter score)) letter_score_assoc

instance Show LetterGrade where
    show (LetterGrade repr _) = repr


instance Bounded LetterGrade where
    minBound = LetterGrade "F" 0.0
    maxBound = LetterGrade "A+" 5.0

instance Ord LetterGrade where
    grade1<=grade2 = (index grade1) <= (index grade2)

instance Enum LetterGrade where
    succ grade
      | grade < maxBound = grades_assoc !! (index grade + 1)
      | otherwise = error "At max bound"
    
    pred grade
      | grade>minBound = grades_assoc !! (index grade - 1)
      | otherwise = error "At minimum bound"
    
    toEnum idx = LetterGrade (letters !! idx) (scores !! idx)
    fromEnum = index
    enumFromTo from to = map toEnum [fromEnum from.. fromEnum to]
    enumFrom from = enumFromTo from maxBound
    enumFromThenTo fst snd to = map toEnum [fromEnum fst, fromEnum snd .. fromEnum to]
    enumFromThen fst snd 
      | fst <= snd = enumFromThenTo fst snd maxBound
      | fst > snd = enumFromThenTo fst snd minBound
    
lGrade :: (GradeRepresentation a) => a-> LetterGrade
lGrade repr = grades_assoc !! (index repr)



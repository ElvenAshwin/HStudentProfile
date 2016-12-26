module Grading (
    EnrichmentGrade(..)
  , DVGrade(..)
  , LetterGrade
  , grade
      , score) where

import Control.Exception (assert)
import Data.List (elemIndex,find)
import Common

data EnrichmentGrade = Fail | Pass 
                     | ENRMerit | Distinction deriving (Show, Read, Eq, Ord, Enum, Bounded)
data DVGrade = Unsatisfactory | Satisfactory 
             | DVMerit | Excellent deriving (Show, Read, Eq, Ord, Enum, Bounded)

type Score = Double
data LetterGrade = LetterGrade String Score deriving (Eq)

--index is very strict, and triggers an error on failure - should ONLY be used internally
--Any part of this module exposed (like grade) should use index', which returns a Maybe
class GradeRepresentation a where
    index' :: a -> Maybe Int
    index :: a -> Int 
    index repr = extract $ index' repr
        where extract (Just num) = num
              extract Nothing = error "No such index"


instance (Stringable c) => GradeRepresentation [c] where
    index' str = (stringify str) `elemIndex` letters

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
    enumFromTo = defaultEnumFromTo
    enumFrom = boundedEnumFrom
    enumFromThenTo = defaultEnumFromThenTo
    enumFromThen = boundedEnumFromThen
    
grade :: (GradeRepresentation a) => a-> Maybe LetterGrade
grade repr = (!!) <$> (pure grades_assoc) <*> (index' repr)

score :: LetterGrade -> Score
score (LetterGrade _ scr) = scr

import ModuleAttributes
import Data.Ratio

type Score = Double
type Credits = Int
type Transcript = [ModuleReport]

data CAP = CAP {totalScore::Score,
                totalCredits::Credits} deriving (Read)

data Module = Module ModuleCode Credits deriving (Show, Eq, Read)
data ModuleReport = ScoreReport LetterGrade Year Semester
                  | EnrichmentReport EnrichmentGrade Year Semester
                  | DVReport DVGrade Year Semester deriving (Show,Read,Eq)


capScore :: CAP -> Double
capScore (CAP score credits) = score/(fromIntegral credits)

capRatio :: CAP -> Ratio Int
capRatio (CAP score credits) = (numerator $ toRatio score) % (credits*(denominator $ toRatio score))

addCAP :: CAP -> CAP -> CAP
addCAP (CAP aScore aCredit) (CAP bScore bCredit) = (CAP (aScore+bScore) (aCredit+bCredit))

--A score is either an integer, or an integer with a half
--So this works for all scores
toRatio :: (Integral a) => Score -> Ratio a
toRatio score = (round $ score*2)%2

cap :: Score -> Credits -> Maybe CAP
cap 0 0 = mempty
cap a b
  | (CAP a b) >= minBound && (CAP a b) <= maxBound = Just $ CAP a b
  | otherwise = Nothing


instance Eq CAP where
    --Implemented like this to avoid the horrors of floating point comparisons
    a == b = (capRatio a) == (capRatio b)

instance Show CAP where
    show = show.capScore

instance Ord CAP where
    a <= b = (capRatio a) <= (capRatio b)

instance Bounded CAP where
    minBound = CAP 0 1
    maxBound = CAP 5 1

instance Monoid CAP where
    mempty = CAP 0 0
    mappend = addCAP

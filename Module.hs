import ModuleAttributes
import Grading
import Data.Ratio

type Score = Double
type Credits = Int
type Transcript = [ModuleReport]

data CAP = CAP {totalScore::Score,
                totalCredits::Credits} deriving (Read)

data Module = Module {codeOfModule::ModuleCode,
                      creditsOfModule::Credits} deriving (Show, Eq)
data ModuleReport = ScoreReport LetterGrade Module Year Semester
                  | EnrichmentReport EnrichmentGrade Module Year Semester
                  | DVReport DVGrade Module Year Semester deriving (Show,Eq)

class SimpleAttribute a where
    is :: Module -> a -> Bool
    fis:: Maybe Module -> a -> Maybe Bool
    fis m attr = fmap ((flip is) $ attr) m
instance SimpleAttribute ModuleType where
    (Module code _) `is` mType = moduleType code == mType

instance SimpleAttribute YearType where
    (Module code _) `is` yType = (yearType . acadLevel) code == yType

instance SimpleAttribute SuffixType where
    (Module code _) `is` sType = suffix code == sType

class GettableAttribute a where
    getAttr::Module -> a

isSubject :: CSStatus -> Module -> Subject -> Bool
isSubject status m subj = (subjectOfModule status m) == subj

isMT :: Module -> Bool
isMT m = extract $ (isSubject Ambiguous m) <$> subject "Mother Tongue"
    where extract Nothing = False
          extract (Just x) = x

subjectOfModule :: CSStatus -> Module -> Subject
subjectOfModule status (Module code _) = (subjectCodeToSubject . subjectCode) code status

capScore :: CAP -> Double
capScore (CAP score credits) = score/(fromIntegral credits)

capRatio :: CAP -> Ratio Int
capRatio (CAP score credits) = (numerator $ toRatio score) % (credits*(denominator $ toRatio score))

addCAP :: CAP -> CAP -> CAP
addCAP (CAP aScore aCredit) (CAP bScore bCredit) = (CAP (aScore+bScore) (aCredit+bCredit))

--Returns the empty CAP for da vinci and enrichment modules
moduleCAP :: ModuleReport -> CAP
moduleCAP (ScoreReport grade m _ _) = 
    CAP (score grade * (fromIntegral $ creditsOfModule m)) (creditsOfModule m)

moduleCAP _ = mempty

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

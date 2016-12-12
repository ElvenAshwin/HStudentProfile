module ModuleAttributes(Year
  , ModuleCode (..)
  , moduleCode
  , year
  , YearType(..)
  , ModuleType(..)
  , SuffixType(..)
  , asInt
  , moduleTypeToInt
  , intToModuleType
  , suffixToString
  , stringToSuffix
  , charToSuffix
    ) where


import Common
import Data.Char (isAlpha, isDigit)
import Data.List (groupBy)
import Text.Read (readMaybe)

type ModuleNum = String
type CodeString = String

data SubjectCode = AE|CL|EL|GM|MA|PE|AR|CM|EN|HD|TH|BG|CS|FR|HY|ML|TL|BL|DV|GE|IH|MU
                 | UD|CH|EC|GJ|JP|PC deriving (Show, Read, Eq)
data ModuleCode = Code {subjectCode::SubjectCode
                       ,acadLevel::Year 
                       ,moduleType::ModuleType
                       ,moduleNum::ModuleNum
                       ,suffix::SuffixType} deriving (Eq)

data Year = Year Int deriving (Show,Eq,Ord)
data YearType = Foundation | Intermediate | Advanced deriving (Show, Read, Eq, Ord, Enum)
data ModuleType = Elective | Enrichment | Core | Honor deriving (Show, Read, Eq)
data SuffixType = EmptySuffix | Preclusion 
                | CorePrereq | MTinLieu | External deriving (Show, Read, Eq)

year :: Int -> Maybe Year
year x
  | x>=1 && x<=6 = Just $ Year x
  | otherwise = Nothing

asInt :: Year -> Int
asInt (Year n) = n

moduleTypeToInt :: ModuleType -> Int
moduleTypeToInt Core = 1
moduleTypeToInt Elective = 2
moduleTypeToInt Enrichment = 3
moduleTypeToInt Honor = 4

intToModuleType :: Int -> Maybe ModuleType
intToModuleType 1 = Just Core
intToModuleType 2 = Just Elective
intToModuleType 3 = Just Enrichment
intToModuleType 4 = Just Honor
intToModuleType x = Nothing

suffixToString :: SuffixType -> String
suffixToString EmptySuffix = ""
suffixToString Preclusion = "A"
suffixToString CorePrereq = "C"
suffixToString MTinLieu = "M"
suffixToString External = "V"

stringToSuffix :: String -> Maybe SuffixType
stringToSuffix "" = Just EmptySuffix
stringToSuffix "A" = Just Preclusion
stringToSuffix "C" = Just CorePrereq
stringToSuffix "M" = Just MTinLieu
stringToSuffix "V" = Just External
stringToSuffix x = Nothing

charToSuffix :: Char -> Maybe SuffixType
charToSuffix c = stringToSuffix [c]


--These statements define the structure of the module code string
--(start, end), not inclusive of end
subjectCodeIdx = (0,2)
yearIdx = (2,3)
moduleTypeIdx = (3,4)
moduleNumIdx = (4,6)
suffixIdx = (6,7)

getSuffix:: CodeString -> Maybe SuffixType
getSuffix code = extractAttribute code suffixIdx stringToSuffix

getYear:: CodeString -> Maybe Year
getYear code = extractAttribute code yearIdx (\str -> readMaybe str >>= year)

getModuleType :: CodeString -> Maybe ModuleType
getModuleType code = extractAttribute code moduleTypeIdx (\str -> readMaybe str >>= intToModuleType)

getModuleNum :: CodeString -> Maybe ModuleNum
getModuleNum code = extractAttribute code moduleNumIdx moduleNumChecker
    where gap (a,b) = b-a
          moduleNumChecker str
            | (length str == gap moduleNumIdx)  && (all isDigit str) = Just str
            | otherwise = Nothing

getSubjectCode :: CodeString -> Maybe SubjectCode
getSubjectCode code = extractAttribute code subjectCodeIdx readMaybe

extractAttribute :: (Integral b) => CodeString -> (b, b) -> (String -> a) -> a
extractAttribute code (start,end) func = func (subList code start end)

moduleCode :: CodeString -> Maybe ModuleCode
moduleCode str = Code <$> subjectcode <*> year <*> moduletype <*> modulenum <*> suffix
    where subjectcode = getSubjectCode str
          year = getYear str
          moduletype = getModuleType str
          modulenum = getModuleNum str
          suffix = getSuffix str

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

instance Show ModuleCode where
    show (Code subj year mtype num suffix) = 
        concat.quintupletToList.quintupletMap (show,show,show,id,id) $ (subj, asInt(year), moduleTypeToInt mtype, num, suffixToString suffix)


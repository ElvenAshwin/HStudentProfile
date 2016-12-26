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
  , Subject
  , subject
  , CSStatus(..)
  , subject'
  , subjectCodeToSubject
  , subjectCodesOf
  , SubjectCode(..)
  , yearType
  , Semester) where


import Common
import Data.Char (isAlpha, isDigit)
import Data.List (groupBy)
import Text.Read (readMaybe)

type ModuleNum = String
type CodeString = String
type SubjectName = String

data SubjectCode = AE|CL|EL|GM|MA|PE|AR|CM|EN|HD|TH|BG|CS|FR|HY|ML|MH|TL|BL|DV|GE|IH|MU
                 | UD|CH|EC|GJ|JP|PC deriving (Show, Read, Eq)

data Subject = Subject SubjectName [SubjectCode] deriving (Eq)

data ModuleCode = Code {subjectCode::SubjectCode
                       ,acadLevel::Year 
                       ,moduleType::ModuleType
                       ,moduleNum::ModuleNum
                       ,suffix::SuffixType} deriving (Eq)

data Year = Year Int deriving (Show,Eq,Ord)
data Semester = Sem1 | Sem2 deriving (Show,Eq,Ord,Enum)
data YearType = Foundation | Intermediate | Advanced deriving (Show, Read, Eq, Ord, Enum)
data ModuleType = Elective | Enrichment | Core | Honor deriving (Show, Read, Eq)
data SuffixType = EmptySuffix | Preclusion 
                | CorePrereq | MTinLieu | External deriving (Show, Read, Eq)


--CSisMath implies CS is part of the "Mathematics" subject
--Otherwise, CS gets its own "Computer Science" subject
--Ambiguous implies the outcome does not matter (behavior for calling Math or CS
--with Ambiguous is undefined)
data CSStatus = CSisMath | CSisSubject | Ambiguous deriving (Show, Read, Eq)

--apply list to a CSStatus in order to retrieve appropriate list of subjects
subjects = map (\str -> (\status -> subject' status str)) subjectnames
    where subjectnames = ["Mathematics", "Mother Tongue", "Physics", "Humanities", "English"
            , "Chemistry", "Biology", "Computing Studies"]

--If you are dealing with CS or Math, explicitly tell if CS should be part of math
--This function serves as the definition for subjects - all other functions must
--be compatible with changes to this function
subject' :: CSStatus -> SubjectName -> Maybe Subject
subject' status str = case str of
                       "Mathematics" -> Just $ Subject "Mathematics " (if status==CSisMath
                                                                   then [MA,CS]
                                                                   else [MA])
                       
                       "Mother Tongue" -> Just $ Subject "Mother Tongue" [GM,HD,MH
                                                                         ,TH,TL,BG
                                                                         ,ML,CL,FR
                                                                         ,UD,CH,GJ
                                                                         ,JP]

                       "Physics" -> Just $ Subject "Physics" [PC]
                       "Humanities" -> Just $ Subject "Humanities" [EN, AR, HY, GE, IH, MU, EC]
                       "English" -> Just $ Subject "English" [EL]
                       "Chemistry" -> Just $ Subject "Chemistry" [CM]
                       "Biology" -> Just $ Subject "Biology" [BL]
                       "Math" -> subject' status "Mathematics"
                       "Computing Studies" -> if status == CSisSubject
                                                 then Just $ Subject "Computing Studies" [CS]
                                                 else Nothing
                       x -> Nothing

-- If you are not dealing with CS or Math, this function will save spaces
-- But do NOT call if dealing with CS or Math - ambiguous to reader, undefined
-- behavior
subject :: SubjectName -> Maybe Subject
subject = subject' Ambiguous

subjectCodesOf :: Subject -> [SubjectCode]
subjectCodesOf (Subject name ls) = ls

--This is admittedly a pure use of a monad, but it works so well!
--This function is guaranteed to respond to alterations in the definition in subject'
subjectCodeToSubject :: SubjectCode -> CSStatus -> Subject
subjectCodeToSubject code status = head (do
    Just subj <- subjects <*> (pure status)
    if code `elem` (subjectCodesOf subj) then [subj] else [])


year :: Int -> Maybe Year
year x
  | x>=1 && x<=6 = Just $ Year x
  | otherwise = Nothing

yearType :: Year -> YearType
yearType (Year n)
  | n `elem` [1,2] = Foundation
  | n `elem` [3,4] = Intermediate
  | n `elem` [5,6] = Advanced

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

instance Show Subject where
    show (Subject name _) = name


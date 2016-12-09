import Module
import Common
import Data.Char (isAlpha)
import Data.List (groupBy)

type ModuleNum = String

data SubjectCode = AE|CL|EL|GM|MA|PE|AR|CM|EN|HD|TH|BG|CS|FR|HY|ML|TL|BL|DV|GE|IH|MU
                 | UD|CH|EC|GJ|JP|PC deriving (Show, Read, Eq)
data ModuleCode = Code SubjectCode Year ModuleType ModuleNum SuffixType deriving (Eq)

moduleTypeToInt :: ModuleType -> Int
moduleTypeToInt Core = 1
moduleTypeToInt Elective = 2
moduleTypeToInt Enrichment = 3
moduleTypeToInt Honor = 4

intToModuleType :: Int -> ModuleType
intToModuleType 1 = Core
intToModuleType 2 = Elective
intToModuleType 3 = Enrichment
intToModuleType 4 = Honor

suffixToString :: SuffixType -> String
suffixToString EmptySuffix = ""
suffixToString Preclusion = "A"
suffixToString CorePrereq = "C"
suffixToString MTinLieu = "M"
suffixToString External = "V"

stringToSuffix :: String -> SuffixType
stringToSuffix "" = EmptySuffix
stringToSuffix "A" = Preclusion
stringToSuffix "C" = CorePrereq
stringToSuffix "M" = MTinLieu
stringToSuffix "V" = External

charToSuffix :: Char -> SuffixType
charToSuffix c = stringToSuffix [c]

type CodeString = String

--These statements define the structure of the module code string
--(start, end), not inclusive of end
subjectCodeIdx = (0,2)
yearIdx = (2,3)
moduleTypeIdx = (3,4)
moduleNumIdx = (4,6)
suffixIdx = (6,7)

getSuffix:: CodeString -> SuffixType
getSuffix code = extractAttribute code suffixIdx stringToSuffix

getYear:: CodeString -> Year
getYear code = extractAttribute code yearIdx (year.read)

getModuleType :: CodeString -> ModuleType
getModuleType code = extractAttribute code moduleTypeIdx (intToModuleType.read)

getModuleNum :: CodeString -> ModuleNum
getModuleNum code = extractAttribute code moduleNumIdx id

getSubjectCode :: CodeString -> SubjectCode
getSubjectCode code = extractAttribute code subjectCodeIdx read

extractAttribute :: (Integral b) => CodeString -> (b, b) -> (String -> a) -> a
extractAttribute code (start,end) func = func (subList code start end)

moduleCode :: CodeString -> ModuleCode
moduleCode str = Code subjectcode year moduletype modulenum suffix
    where subjectcode = getSubjectCode str
          year = getYear str
          moduletype = getModuleType str
          modulenum = getModuleNum str
          suffix = getSuffix str


instance Show ModuleCode where
    show (Code subj year mtype num suffix) = 
        concat.quintupletToList.quintupletMap (show,show,show,id,id) $ (subj, asInt(year), moduleTypeToInt mtype, num, suffixToString suffix)



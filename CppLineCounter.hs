import qualified Data.ByteString.Char8 as C
import System.Environment (getArgs, getProgName)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Data.Char (isSpace)
import Control.Monad (forM)
import Data.List (isSuffixOf)

data Counter = Counter {
    codeLine :: Int,
    commentLine :: Int,
    blankLine :: Int,
    totalLine :: Int,

    isCodeLine :: Bool,
    isBlankLine :: Bool,
    lexComment :: Bool,
    isMultiLineComment :: Bool
} deriving(Eq)

instance Show Counter where
    show (Counter code comment blank total _ _ _ _) =
        "code: " ++ show code ++
        "\ncomment: " ++ show comment ++
        "\nblank: " ++ show blank ++
        "\ntotal: " ++ show total

emptyCounter :: Counter
emptyCounter = Counter {
    codeLine = 0, commentLine = 0, blankLine = 0, totalLine = 0,
    isCodeLine = False, isBlankLine = True, lexComment = False, isMultiLineComment = False
}

plusCounter :: Counter -> Counter -> Counter
plusCounter c1 c2 = emptyCounter {
    codeLine = codeLine c1 + codeLine c2,
    commentLine = commentLine c1 + commentLine c2,
    blankLine = blankLine c1 + blankLine c2,
    totalLine = totalLine c1 + totalLine c2
}

incCommentLine :: Counter -> Counter
incCommentLine counter =
    if isMultiLineComment counter
    then counter { commentLine = (commentLine counter) + 1 }
    else counter { lexComment = False }

incBlankLine :: Counter -> Counter
incBlankLine counter =
    if isBlankLine counter
    then counter { blankLine = (blankLine counter) + 1 }
    else counter { isBlankLine = True }

incCodeLine :: Counter -> Counter
incCodeLine counter =
    if isCodeLine counter
    then counter { codeLine = (codeLine counter) + 1, isCodeLine = False }
    else counter

incTotalLine :: Counter -> Counter
incTotalLine counter = counter { totalLine = (totalLine counter) + 1 }

incLine :: Counter -> Counter
incLine counter = incCommentLine $ incBlankLine $ incCodeLine $ incTotalLine counter

startComment :: Counter -> Bool -> Counter
startComment counter multiLine = 
    if lexComment counter
    then counter
    else counter {
        lexComment = True, isMultiLineComment = multiLine, commentLine = (commentLine counter) + 1
    }

endComment :: Counter -> Counter
endComment counter = counter {
    lexComment = False, isMultiLineComment = False
}

countLine :: String -> Counter -> Counter

-- count new line
countLine ('\n':xs) counter =
    case xs of
        ('\r':xs') -> countLine xs' (incLine counter)
        otherwise -> countLine xs (incLine counter)

countLine ('\r':xs) counter =
    case xs of
        ('\n':xs') -> countLine xs' (incLine counter)
        otherwise -> countLine xs (incLine counter)

-- count comment
countLine ('/':xs) counter =
    case xs of
        ('/':xs') -> countLine xs' (startComment counter { isBlankLine = False } False)
        ('*':xs'') -> countLine xs'' (startComment counter { isBlankLine = False } True)
        otherwise -> countLine xs counter { isBlankLine = False }

countLine ('*':xs) counter =
    case xs of
        ('/':xs') -> countLine xs' (endComment counter { isBlankLine = False })
        otherwise -> countLine xs counter { isBlankLine = False }

-- count other
countLine (c:xs) counter =
    if lexComment counter
    then countLine xs counter { isBlankLine = False }
    else if isSpace c
         then countLine xs counter
         else countLine xs counter { isCodeLine = True, isBlankLine = False }

countLine [] counter = counter

cppFileExt :: [String]
cppFileExt = [".c", ".C", ".cpp", ".CPP", ".cc", ".CC", ".cxx", ".CXX", ".h", ".hpp", "hxx"]

printCounter :: Counter -> IO ()
printCounter counter = do
    putStrLn (show counter)

getAllCppFiles :: FilePath -> IO [FilePath]
getAllCppFiles dir = do
    names <- getDirectoryContents dir
    let files = filter (`notElem` [".", ".."]) names
    paths <- forM files $ \name -> do
        let path = dir </> name
        isDir <- doesDirectoryExist path
        if isDir
        then getAllCppFiles path
        else if any (`isSuffixOf` path) cppFileExt
             then return [path]
             else return []
    return (concat paths)

count :: FilePath -> IO ()
count dir = do
    files <- getAllCppFiles dir
    counters <- forM files $ \file -> do
        context <- C.readFile file
        return (countLine (C.unpack context) emptyCounter)
    printCounter $ foldr plusCounter emptyCounter counters

main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    if null args
    then putStrLn $ "usage: " ++ progName ++ " dir"
    else count $ head args

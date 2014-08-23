{-# LANGUAGE BangPatterns #-}

import qualified Data.ByteString.Char8 as C
import System.Environment (getArgs, getProgName)
import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Data.Char (isSpace)
import Control.Monad (forM)
import Control.Arrow (first)
import Data.List (isSuffixOf, foldl')

data Counter = Counter {
    codeLine :: !Int,
    commentLine :: !Int,
    blankLine :: !Int,
    totalLine :: !Int,

    isCodeLine :: !Bool,
    isBlankLine :: !Bool,
    lexingComment :: !Bool,
    isMultiLineComment :: !Bool
} deriving(Eq)

instance Show Counter where
    show (Counter code comment blank total _ _ _ _) =
        "code: " ++ show code ++
        "\ncomment: " ++ show comment ++
        "\nblank: " ++ show blank ++
        "\ntotal: " ++ show total

emptyCounter :: Counter
emptyCounter = Counter 0 0 0 0 False True False False

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
    else counter { lexingComment = False }

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
    if lexingComment counter
    then counter
    else counter {
        lexingComment = True, isMultiLineComment = multiLine,
        commentLine = (commentLine counter) + 1
    }

endMultiLineComment :: Counter -> Counter
endMultiLineComment counter =
    if isMultiLineComment counter
    then counter { lexingComment = False, isMultiLineComment = False }
    else counter

notBlankLine :: Counter -> Counter
notBlankLine counter = counter { isBlankLine = False }

takeTwo :: String -> C.ByteString -> (String, C.ByteString)
takeTwo s@(_:[]) bs = first ((s ++ ) . C.unpack) $ C.splitAt 1 bs
takeTwo [] bs = first C.unpack $ C.splitAt 2 bs

countLine :: (String, C.ByteString) -> Counter -> Counter
countLine (('\n':xs), bs) counter =
    case xs of
        ('\r':ys) -> countLine (takeTwo ys bs) $ incLine counter
        otherwise -> countLine (takeTwo xs bs) $ incLine counter

countLine (('\r':xs), bs) counter =
    case xs of
        ('\n':ys) -> countLine (takeTwo ys bs) $ incLine counter
        otherwise -> countLine (takeTwo xs bs) $ incLine counter

countLine (('/':xs), bs) counter =
    case xs of
        ('/':ys) -> countLine (takeTwo ys bs) $ startComment (notBlankLine counter) False
        ('*':ys) -> countLine (takeTwo ys bs) $ startComment (notBlankLine counter) True
        otherwise -> countLine (takeTwo xs bs) $ notBlankLine counter

countLine (('*':xs), bs) counter =
    case xs of
        ('/':ys) -> countLine (takeTwo ys bs) $ endMultiLineComment $ notBlankLine counter
        otherwise -> countLine (takeTwo xs bs) $ notBlankLine counter

countLine ((c:xs), bs) counter =
    if lexingComment counter
    then countLine (takeTwo xs bs) $ notBlankLine counter
    else if isSpace c
         then countLine (takeTwo xs bs) counter
         else countLine (takeTwo xs bs) $ notBlankLine counter { isCodeLine = True }

countLine ([], bs) counter = incLine counter

cppFileExt :: [String]
cppFileExt = [".c", ".C", ".cpp", ".CPP", ".cc", ".CC", ".cxx", ".CXX", ".h", ".hpp", ".hxx"]

printCounter :: Counter -> IO ()
printCounter counter = do
    putStrLn (show counter)

getAllCppFiles :: FilePath -> IO [FilePath]
getAllCppFiles dir = do
    names <- getDirectoryContents dir
    let files = filter (`notElem` [".", ".."]) names
    paths <- forM files $ \name -> do
        let !path = dir </> name
        isDir <- doesDirectoryExist path
        if isDir
        then getAllCppFiles path
        else if any (`isSuffixOf` path) cppFileExt
             then return [path]
             else return []
    return $! concat paths

getFiles :: FilePath -> IO [FilePath]
getFiles path = do
    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path
    if isFile
    then return [path]
    else if isDir
         then getAllCppFiles path
         else return []

isPathExist :: FilePath -> IO Bool
isPathExist path = do
    isFile <- doesFileExist path
    if isFile
    then return True
    else doesDirectoryExist path

countAll :: FilePath -> IO ()
countAll path = do
    files <- getFiles path
    counters <- forM files $ \file -> do
        context <- C.readFile file
        if C.null context
        then return emptyCounter
        else return $! countLine (takeTwo [] context) emptyCounter
    printCounter $ foldl' plusCounter emptyCounter counters

count :: FilePath -> IO ()
count path = do
    isExist <- isPathExist path
    if isExist
    then countAll path
    else putStrLn $ path ++ " is not exist"

main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    if null args
    then putStrLn $ "usage: " ++ progName ++ " dir"
    else count $ head args

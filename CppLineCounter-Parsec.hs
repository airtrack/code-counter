{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import qualified Data.ByteString.Char8 as B
import System.Environment (getArgs, getProgName)
import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Data.Char (isSpace)
import Data.List (isSuffixOf, foldl')
import Control.Monad (forM)
import Control.Monad.State

data Context = Context {
    line :: !Int,
    code :: !Int,
    comment :: !Int,
    blank :: !Int,
    lexingComment :: !Bool,
    isMultiLineComment :: !Bool,
    isCodeLine :: !Bool
} deriving (Eq)

instance Show Context where
    show (Context line code comment blank _ _ _) =
        "code: " ++ show code ++ "\n" ++
        "comment: " ++ show comment ++ "\n" ++
        "blank: " ++ show blank ++ "\n" ++
        "total: " ++ show line

emptyContext = Context 0 0 0 0 False False False

plusContext (Context line1 code1 comment1 blank1 _ _ _) (Context line2 code2 comment2 blank2 _ _ _) =
    Context (line1 + line2) (code1 + code2) (comment1 + comment2) (blank1 + blank2) False False False

incCodeLine context =
    if isCodeLine context
    then context { code = (code context) + 1, isCodeLine = False }
    else context

incCommentLine context =
    if isMultiLineComment context
    then context { comment = (comment context) + 1 }
    else context { lexingComment = False }

incTotalLine context = context { line = (line context) + 1 }

incBlankLine context = context { blank = (blank context) + 1 }

incLine context = incCommentLine $ incCodeLine $ incTotalLine context

startComment multiLineComment context =
    if lexingComment context
    then context
    else context {
        lexingComment = True, isMultiLineComment = multiLineComment,
        comment = (comment context) + 1
    }

endMultiLineComment context =
    if isMultiLineComment context
    then context { isMultiLineComment = False, lexingComment = False }
    else context

newtype Parser a = Parser {
    runP :: State (B.ByteString, Context) a
} deriving (Monad, MonadState (B.ByteString, Context))

runParse :: Parser a -> B.ByteString -> (a, (B.ByteString, Context))
runParse p b = runState (runP p) $ (b, emptyContext)

repeatP :: Parser Bool -> Parser Bool
repeatP p = do
    ok <- p
    if ok
    then repeatP p
    else return True

try :: Parser Bool -> Parser Bool
try p = do
    cc <- get
    ok <- p
    if ok
    then return True
    else (put cc) >> return False

optional :: Parser Bool -> Parser Bool
optional p = try p >> return True

notP :: Parser Bool -> Parser Bool
notP p = do
    cc <- get
    ok <- p
    if ok
    then (put cc) >> return True
    else do
        ok' <- matchAny
        if ok'
        then notP p
        else return False

(<||>) :: Parser Bool -> Parser Bool -> Parser Bool
(<||>) p1 p2 = do
    p1OK <- p1
    if p1OK
    then return True
    else do
        p2OK <- p2
        if p2OK
        then return True
        else return False

match :: (Char -> Bool) -> (Context -> Context) -> Parser Bool
match f fc = do
    (bs, context) <- get
    case B.uncons bs of
        Nothing -> return False
        Just (c, s)
            | f c -> (put (s, fc context)) >> return True
            | otherwise -> return False

matchAny :: Parser Bool
matchAny = match (\_ -> True) id

char :: Char -> (Context -> Context) -> Parser Bool
char c fc = match (\c' -> c == c') fc

string :: String -> (Context -> Context) -> Parser Bool
string s fc = try $ string_ s fc

string_ :: String -> (Context -> Context) -> Parser Bool
string_ (c:xs) fc = do
    ok <- char c id
    if ok
    then string_ xs fc
    else return False

string_ [] fc = do
    (bs, context) <- get
    put (bs, fc context)
    return True

newLine = (string "\r\n" incLine) <||>
          (string "\n\r" incLine) <||>
          (char '\r' incLine) <||>
          (char '\n' incLine) <||>
          (return False)

space = match isBlank id
    where isBlank c =
            if c /= '\r' && c /= '\n'
            then isSpace c
            else False

spaceLine = do
    repeatP space
    ok <- try newLine
    (bs, context) <- get
    if ok
    then (put (bs, incBlankLine context)) >> return True
    else if B.null bs
         then (put (bs, incTotalLine $ incBlankLine context)) >> return False
         else return False

lineComment = string "//" $ startComment False

multiCommentBegin = string "/*" $ startComment True

multiCommentEnd = string "*/" endMultiLineComment

otherChars = do
    (bs, _) <- get
    notP $ space <||> newLine <||> lineComment <||> multiCommentBegin <||> multiCommentEnd
    (bs', context) <- get
    if B.length bs /= B.length bs'
    then (put (bs', setCodeLine context)) >> return True
    else return False
    where
    setCodeLine context =
        if lexingComment context
        then context
        else context { isCodeLine = True }

normalLine = do
    (bs, _) <- get
    if B.null bs
    then return False
    else normalLine'
    where
    normalLine' = do
        repeatP $ space <||> otherChars <||> lineComment <||> multiCommentBegin <||> multiCommentEnd
        ok <- try newLine
        (bs, context) <- get
        if ok
        then return True
        else if B.null bs
             then (put (bs, incLine context)) >> return False
             else return False

file = repeatP $ spaceLine <||> normalLine

countFile bs = snd . snd $ runParse file bs

cppFileExt :: [String]
cppFileExt = [".c", ".C", ".cpp", ".CPP", ".cc", ".CC", ".cxx", ".CXX", ".h", ".hpp", "hxx"]

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

printContext :: Context -> IO ()
printContext context = do
    putStrLn (show context)

countAll :: FilePath -> IO ()
countAll path = do
    files <- getFiles path
    contexts <- forM files $ \file -> do
        bs <- B.readFile file
        return $! countFile bs
    printContext $ foldl' plusContext emptyContext contexts

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
    then putStrLn $ "usage: " ++ progName ++ " path"
    else count $ head args

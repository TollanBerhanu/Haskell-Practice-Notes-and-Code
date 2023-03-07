-- Pure code can throw exceptions, but they can only be caught in the I/O part of our code (when we're inside
-- do block that goes into main). That's because you don't know when (or if) anything will be evaluated in pure code,
-- and doesn't have a well-defined order of execution (because it is lazy), whereas I/O code does.

-- It's usually preferred not to mix exceptions and pure code. It's better to take advantage of Haskell's powerful type
-- system and use types like 'Either' and Ma'ybe to represent results that may have failed.

-- *** This is a program that opens a file whose name is given to it as a command line argument and tells us how many lines
-- it has. It throws an exception if the file doesn't exist.

{- import System.Environment (getArgs)
import System.IO
import System.Directory (doesFileExist)

main = do
    [fileName] <- getArgs
    -- contents <- readFile fileName -- if the file doesn't exits, GHC throws this error, and crashes:
                                     -- Exceptions.hs: ./something.txt: openFile: does not exist (No such file or directory)
    -- We can overcome this error by using the 'doesFileExist' function from System.Directory.
    fileExists <- doesFileExist fileName -- doesFileExist :: FilePath -> IO Bool ( I/O action that returns a boolean value)
    -- We can't just use 'doesFileExist' in an if expression directly.
    if fileExists
        then do contents <- readFile fileName
                putStr $ "The file has " ++ (show $ length $ lines contents) ++ " lines."
        else do putStr "The file does not exist!" -}

-- To deal with this using exceptions, we're going to take advantage of the 'catch' function from 'System.IO.Error'
-- catch :: IO a -> (IOError -> IO a) -> IO a ... It takes two parameters: ('I/O action (try)' and 'handler (catch)')
-- If the I/O action (opening a file) passed to catch throws an I/O exception, that exception gets passed to the handler,
-- which then decides what to do. The handler takes a value of type IOError which  carries information regarding
-- the type of the exception thrown

-- *** Let's do the same thing using 'catch'
import System.IO
import System.Environment (getArgs)
import System.IO.Error (catchIOError, isDoesNotExistError, ioeGetFileName)
import Control.Exception (catch)

main = {- do -} toTry `catch` handler --  or `catchIOError`

toTry :: IO()
toTry = do 
    [fileName] <- getArgs
    contents <- readFile fileName
    putStr $ "The file has " ++ (show $ length $ lines contents) ++ " lines."

{- handler :: IOError -> IO()
handler err = do putStr "Whoops! something went wrong" -}
-- This would just print the text if any error is thrown, we can catch specific errors as shown below:

{- handler :: IOError -> IO ()  
handler e  
    | isDoesNotExistError e = putStrLn "The file does not exist!" -- isDoesNotExistError :: IOError -> Bool  
    | isFullError e = putStrLn "Free some space"  -- putStrLn :: String -> IO ()
    | isIllegalOperation e = putStrLn "Call the cops!" -- You can also call any IO actions/functions you define here  
    | otherwise = ioError e   -}-- ioError :: IOException -> IO a (we re-throw the exception for any other unknown error)
-- isAlreadyExistsError, isDoesNotExistError, isAlreadyInUseError, isFullError, isEOFError, isIllegalOperation
-- isPermissionError, isUserError
-- (the function 'userError' is used for making exceptions from our code and equipping them with a string)
-- E.g., (ioError $ userError "remote computer unplugged!"), but it's prefered you use types like 'Either' and 'Maybe'
-- to express possible failure instead of throwing exceptions yourself with 'userError'

handler :: IOError -> IO()
handler err                         --ioeGetFileName :: IOError -> Maybe FilePath(String)
    | isDoesNotExistError err = case ioeGetFileName err of -- ioeGetLocation, ioeGetHandle, ioeGetErrorType, ...
                                    Just pathName -> putStrLn $ "Whoops! File does not exist at: " ++ pathName
                                    Nothing -> putStrLn "Whoops! File does not exist at unknown location!"
    | otherwise = ioError err
    -- We can't print the 'fileName' we get from 'getArgs', because only the 'IOError' is passed to 'handler'

-- You don't have to use one handler to catch exceptions in your whole I/O part
{- main = do try1 `catch` handler1 -- any caught error won't stop execution
             
             try2 `catch` handler2 -- this line is executed unless an error is thrown (but not caught) in the last line
             
             putStr "End of IO operations"  -} -- same here

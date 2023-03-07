-- Pure code can throw exceptions, but they can only be caught in the I/O part of our code (when we're inside
-- do block that goes into main). That's because you don't know when (or if) anything will be evaluated in pure code,
-- and doesn't have a well-defined order of execution (because it is lazy), whereas I/O code does.

-- It's usually preferred not to mix exceptions and pure code. It's better to take advantage of Haskell's powerful type
-- system and use types like 'Either' and Ma'ybe to represent results that may have failed.

-- This is a program that opens a file whose name is given to it as a command line argument and tells us how many lines
-- it has. It throws an exception if the file doesn't exist.

import System.Environment (getArgs)
import System.IO
import System.Directory (doesFileExist)

main = do
    [fileName] <- getArgs
    -- contents <- readFile fileName -- if the file doesn't exits, GHC throws this error, and crashes:
                                     -- Exceptions.hs: ./something.txt: openFile: does not exist (No such file or directory)
    fileExists <- doesFileExist fileName -- doesFileExist :: FilePath -> IO Bool ( I/O action that returns a boolean value)
    -- We can't just use 'doesFileExist' in an if expression directly.
    if fileExists
        then do contents <- readFile fileName
                putStr $ "The file has " ++ (show $ length $ lines contents) ++ " lines."
        else do putStr "The file does not exist!"
-- We can overcome this error by using the 'doesFileExist' function from System.Directory.



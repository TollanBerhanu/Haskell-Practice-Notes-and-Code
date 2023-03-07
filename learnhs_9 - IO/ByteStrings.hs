-- Bytestrings are like lists, only each element is one byte (or 8 bits) in size.They come in two flavors: strict and lazy
-- Strict bytestrings reside in Data.ByteString. There are no promises involved (you can't have infinite strict bytestrings)
-- There's less overhead in strict bytestrnigs, but they fill up memory

-- Lazy bytestrings reside in Data.ByteString.Lazy. Unlike lists, lazy bytestrings are stored in chunks, each of size 64K.
-- If you evaluate a byte in a lazy bytestring, the first chunk (64K) will be evaluated. They are like lists of strict
-- bytestrings with size of 64K. A lot of functions in Data.ByteString.Lazy have the same name as functions in Data.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL -- we mostly use this
import System.Environment (getArgs)
-- pack :: [Word8] -> ByteString ...  it takes a list of bytes of type Word8 and returns a ByteString
-- Word8 is like Int, but with smaller range (0-255) ... represents an 8-bit number (2^8)

smallLetters = BL.pack [97,98,99] -- "abc"
allCaps = BL.pack[65..90] -- "A...Z"
allCapsByteStrings = BL.unpack allCaps -- [65..90] ... unpack does the reverse

-- convert from 'a list of strict bytestrings' to 'a lazy bytestring' and vice versa
strictToLazy = BL.fromChunks [ BS.pack [40,41,42], BS.pack [43,44,45], BS.pack [46,47,48] ] -- "()*+,-./0"
lazyToStrict = BL.toChunks strictToLazy -- ["()*","+,-","./0"]

-- The bytestring version of ':' is called 'cons'. It takes a byte and a bytestring and puts it at the beginning
-- It makes a new chunk in the bytestring even if the first chunk isn't full. That's why the strict version of cons,
-- namely (cons') is better if you're going to be inserting a lot of bytes at the beginning of a bytestring.
consExample = BL.cons 85 $ BL.pack [80,81,82,84] -- "UPQRT" ... U is a new chunk
cons'Example = foldr BL.cons' BL.empty [48..57] -- "0123456789" ... 'BL.empty' makes an empty bytestring.

-- Data.ByteString modules have lots of functions that are analogous to those in Data.List, including, but not limited to:
-- head, tail, init, null, length, map, reverse, foldl, foldr, concat, takeWhile, filter

-- It also has functions like the ones found in System.IO., only 'String' is replaced with 'ByteString'
-- E.g., readFile :: FilePath -> IO String .... readFile :: FilePath -> IO ByteString

-- **** Program that takes two filenames as command-line args and copies the first file into the second.

-- ** These modules are required and are imported at the beginning of the file

-- import System.Environment (getArgs)
-- import qualified Data.ByteString.Lazy as BL (readFile, writeFile)
--the strict version will read the file into memory at once!

main = do
    (fileName1:fileName2:_) <- getArgs
    copyFile fileName1 fileName2 -- we could have used 'System.Directory.copyFile' instead of implementing it below

copyFile :: FilePath -> FilePath -> IO() -- 'FilePath' is just a type synonym for 'String'
copyFile source dest = do
            contents <- BL.readFile source
            BL.writeFile dest contents

-- runhaskell .\ByteStrings.hs file.txt file1.txt      

-- ** Whenever you need better performance in a program that reads a lot of data into strings, try using bytestrings,
-- you'll probably get some good performance boosts.
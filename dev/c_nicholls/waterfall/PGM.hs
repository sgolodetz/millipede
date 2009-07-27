{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

-- | "Graphics.Pgm" is a pure Haskell library to read and write PGM images.  It
--   properly supports both 8 bit and 16 bit pixels, and multiple PGMs per
--   file.  The PGM is the lowest common denominator of useful image file
--   formats.  It consists of a header of the form
--
--   @P5 width height maxVal@
--
--   followed by a single whitespace character, usually a newline, where
--   @width@, @height@, and @maxVal@ are positive integers consisting of digits
--   only giving the number of columns, number of rows, and the highest grey
--   level in the image to follow.
--
--   If @maxVal@ < 256, then the format uses 1 byte per pixel; otherwise it
--   uses 2.  The routines in this library properly handle both, including
--   automatically determining which to write when writing an array to disk.
--
--   The header can also contain comments, starting with @#@ on a new line, and
--   continuing to the end of the line.  These are read out and returned as a
--   'String' with newlines kept intact (except for the last newline of the
--   last comment line, which is removed).  Comments from anywhere between the
--   header fields are concatenated into the same document.  'pgmToArray'
--   ignores comments; 'pgmToArrayWithComments' reads them.
--
--   After the header, the pixel data is written in big-endian binary form,
--   most significant byte first for 16 bit pixels.  The pixels are a single
--   row-major raster through the image.
--
--   To put multiple PGMs in a file, append them.  This module allows you to
--   put white space between them, though this might choke other
--   implementations.
--
--   All arrays returned by this library from PGMs have pixel type 'Int', since
--   this is simply more useful for most purposes.  If you want to write a PGM
--   back out, you must first coerce your pixel type to 'Word16'!  There are
--   too many possible ways of handling negative values, larger depths, or
--   other things beyond the comprehension of 'Word16' to handle with a simple
--   wrapper function.  If you know you have positive values less than 2^16,
--   then you can coerce an array @arr@ to 'Word16' with
--
--   > amap (fromIntegral :: Int -> Word16) arr
--
--   The array's indices (of the form (row,column)) start at (0,0) and run to
--   (@height@-1,@width@-1).

module PGM
  ( pgmToArray,
    pgmsToArrays,
    pgmToArrayWithComments, pgmsToArraysWithComments,
    arrayToPgmWithComment,
    pgmsFromFile, pgmsFromHandle,
    arrayToPgm, arrayToFile, arrayToHandle, arraysToHandle,
    arraysToFile,
    amap
  )
  where

import Control.Monad            (liftM)
import Data.Array.Unboxed       (IArray, Ix, UArray, amap, bounds, elems, listArray)
import Data.ByteString.Internal (c2w)
import Data.List                (intercalate)
import Data.Word                (Word8, Word16)
import System.IO                (Handle, IOMode(ReadMode, WriteMode), hClose, openFile)
import Text.Parsec              (ParseError, (<?>), choice, getInput, many1, manyTill, parse, setInput, try)
import Text.Parsec              (anyChar, char, digit, newline, space, spaces)
import Text.Parsec.ByteString   (Parser)
import Text.Printf              (printf)
import qualified Data.ByteString as B

magicNumber :: Parser ()
magicNumber = do { char 'P'; char '5'; return () }

integer :: Parser Int
integer = do { s <- many1 digit; return $ read s }

width :: Parser Int
width = integer

height :: Parser Int
height = integer

maxVal :: Parser Int
maxVal = do { i <- integer; return $ min 65536 i }

comment :: Parser String
comment = do { char '#'; c <- manyTill anyChar (try newline); return $ c ++ "\n" }

commentAwareWhiteSpace :: Parser String
commentAwareWhiteSpace = liftM concat $ many1 (choice [comment,do { many1 space; return "" }])

pgmHeader :: Parser (Int,Int,Int,String)
pgmHeader = do magicNumber <?> "magic number"
               hVal0 <- commentAwareWhiteSpace
               cols <- width <?> "width"
               hVal1 <- commentAwareWhiteSpace
               rows <- height <?> "height"
               hVal2 <- commentAwareWhiteSpace
               m <- maxVal <?> "maximum grey value"
               space
               let q = hVal0 ++ hVal1 ++ hVal2
               return (rows,cols,m, Prelude.init q)

pgmWithComments :: (IArray UArray a, Integral a) => Parser (UArray (Int,Int) a, String)
pgmWithComments = do (rows,cols,m,comments) <- pgmHeader
                     let d = if m < 256 then 1 else 2
                     ip <- getInput
                     let body = B.take (rows*cols*d) ip
                     setInput $ B.drop (rows*cols*d) ip
                     let arr = readArray (if d == 1 then Depth8 else Depth16) rows cols body
                     return (arr,comments)

pgmsWithComments :: (IArray UArray a, Integral a) => Parser [(UArray (Int,Int) a, String)]
pgmsWithComments = many1 (do { h <- pgmWithComments ; spaces ; return h })


pgm :: (IArray UArray a, Integral a) => Parser (UArray (Int,Int) a)
pgm = do (rows,cols,m,_) <- pgmHeader
         let d = if m < 256 then 1 else 2
         ip <- getInput
         let body = B.take (rows*cols*d) ip
         setInput $ B.drop (rows*cols*d) ip
         let arr = readArray (if d == 1 then Depth8 else Depth16) rows cols body
         return arr

pgms :: (IArray UArray a, Integral a) => Parser [UArray (Int,Int) a]
pgms = many1 (do { h <- pgm ; spaces ; return h })


-- | Parse the first (and possible only) PGM in a 'ByteString' into an array.
--   If the parsing succeeds, you will still need to match on the 'Right'
--   constructor to get the array.
pgmToArray :: (Integral a, IArray UArray a) => B.ByteString -> Either ParseError (UArray (Int,Int) a)
pgmToArray = parse pgm "Failed to parse PGM."

-- | The same as 'pgmToArray', but taking also returning the comments in the
--   PGM file as a 'String'.
pgmToArrayWithComments :: (Integral a, IArray UArray a) => B.ByteString -> Either ParseError (UArray (Int,Int) a, String)
pgmToArrayWithComments = parse pgmWithComments "Failed to parse PGM."

-- | Precisely the same as 'pgmToArray', but this time fetches all the PGMs in
--   the file, and returns them as a list of arrays.
pgmsToArrays :: (Integral a, IArray UArray a) => B.ByteString -> Either ParseError [UArray (Int,Int) a]
pgmsToArrays = parse pgms "Failed to parse PGMs."

-- | Same as 'pgmsToArrays', but again returning comments.
pgmsToArraysWithComments :: (Integral a, IArray UArray a) => B.ByteString -> Either ParseError [(UArray (Int,Int) a, String)]
pgmsToArraysWithComments = parse pgmsWithComments "Failed to parse PGMs."


-- | A wrapper around 'pgmsFromHandle' which also opens the file to read from.
pgmsFromFile :: String -> IO (Either ParseError [UArray (Int,Int) Int])
pgmsFromFile fname = do h <- openFile fname ReadMode
                        s <- pgmsFromHandle h
                        hClose h
                        return s

-- | Parse all PGMs in the contents of a handle, and return them as a list of
--   arrays.
pgmsFromHandle :: Handle -> IO (Either ParseError [UArray (Int,Int) Int])
pgmsFromHandle = liftM pgmsToArrays . B.hGetContents


readArray8 :: Int -> Int -> B.ByteString -> UArray (Int,Int) Word8
readArray8 rows cols src = listArray ((0,0), (rows-1,cols-1)) (B.unpack src)

readArray16 :: Int -> Int -> B.ByteString -> UArray (Int,Int) Word16
readArray16 rows cols src = listArray ((0,0), (rows-1,cols-1)) src'
    where raw = B.unpack src
          src' = pairWith f raw
          f a b = 256 * fromIntegral a + fromIntegral b

data Depth = Depth8 | Depth16

readArray :: (IArray UArray a, Integral a) => Depth -> Int -> Int -> B.ByteString -> UArray (Int,Int) a
readArray Depth8  rows cols src = amap fromIntegral $ readArray8  rows cols src
readArray Depth16 rows cols src = amap fromIntegral $ readArray16 rows cols src

pair :: [a] -> [(a,a)]
pair [] = []
pair (_:[]) = []
pair (a:b:ls) = (a,b) : pair ls

pairWith :: (a -> a -> b) -> [a] -> [b]
pairWith f ls = Prelude.map (uncurry f) $ pair ls

pgmHeaderString :: Int -> Int -> Word16 -> String -> B.ByteString
pgmHeaderString rows cols mVal comm = B.pack $ Prelude.map c2w $
                                           printf "P5\n#%s\n%d %d %d\n" (format comm)
                                                      (cols+1) (rows+1) mVal
    where format = Data.List.intercalate "\n#" . lines

-- | Takes an array (which must already be coerced to have element type
--   'Word16') and produces a 'ByteString' encoding of that array as a PGM.
arrayToPgm :: IArray m Word16 => m (Int,Int) Word16 -> B.ByteString
arrayToPgm arr = pgmHeaderString rows cols mVal "" `B.append`
                   listToByteString mVal (elems arr)
    where (rows,cols) = (xmax-xmin,ymax-ymin)
          ((xmin,ymin),(xmax,ymax)) = bounds arr
          mVal = arrayLift max arr

-- | Precisely the same as 'arrayToPgm', but takes a 'String' to encode into
--   the file header as a comment after the magic number but before the width
--   field.
arrayToPgmWithComment :: IArray m Word16 => m (Int,Int) Word16 -> String -> B.ByteString
arrayToPgmWithComment arr cm = pgmHeaderString rows cols mVal cm `B.append`
                   listToByteString mVal (elems arr)
    where (rows,cols) = (xmax-xmin,ymax-ymin)
          ((xmin,ymin),(xmax,ymax)) = bounds arr
          mVal = arrayLift max arr

arrayLift :: (Ix i, IArray m a) => (a -> a -> a) -> m i a -> a
arrayLift f arr = Prelude.foldl f (head q) q
    where q = elems arr

listToByteString :: Word16 -> [Word16] -> B.ByteString
listToByteString d vs
    | d < 256 = B.pack ((Prelude.map fromIntegral vs)::[Word8])
    | otherwise = B.pack $ concatMap (\x -> [fromIntegral (x `div` 256),
                                           fromIntegral (x `rem` 256)]) vs

-- | Write a single array to a given handle.
arrayToHandle :: IArray m Word16 => Handle -> m (Int,Int) Word16 -> IO ()
arrayToHandle h arr = B.hPutStr h (arrayToPgm arr)

-- | A wrapper around 'arrayToHandle' which opens the file to write to, then
--   closes it afterwards.
arrayToFile :: IArray m Word16 => String -> m (Int,Int) Word16 -> IO ()
arrayToFile fname arr = do h <- openFile fname WriteMode
                           arrayToHandle h arr
                           hClose h

-- | Writes a list of arrays to a given handle.  Note that most implementations
--   of PGM will ignore all but the first when they read this file.
arraysToHandle :: IArray m Word16 => Handle -> [m (Int,Int) Word16] -> IO ()
arraysToHandle = mapM_ . arrayToHandle

-- | A wrapper around 'arraysToHandle' which opens and closes the file to write
--   to.
arraysToFile :: IArray m Word16 => String -> [m (Int,Int) Word16] -> IO ()
arraysToFile fname arrs = do h <- openFile fname WriteMode
                             arraysToHandle h arrs
                             hClose h

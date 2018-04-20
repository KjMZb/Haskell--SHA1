module Crypto
( leftRotate,
  toBits,
  toHex,
  chunker
) where

import Data.Bits
import Data.Char


-- bit string, size, number to rotate, resulting bit string
leftRotate :: Integer -> Int -> Int -> Integer 
leftRotate bits size spaces =   (shiftR (bits .&. (shiftL ((shiftL 1 spaces)-1) (size - spaces))) (size - spaces)) .|.
                                (shiftL (bits .&. ((shiftL 1 (size-spaces))-1)) spaces)

-- Message, number of characters, results bit string
toBits :: String -> Int -> Integer
toBits [] _       = 0
toBits _ 0        = 0
toBits (x:xs) len = shiftL ( toInteger $ ord x ) ( ( len - 1 ) * 8)
                    .|. (toBits xs ( len - 1 ) ) 

toHex :: Integer -> String
toHex 0                             = ""
toHex val
   | val .&. ((shiftL 1 4)-1) == 0  = (toHex (shiftR val 4))++"0" 
   | val .&. ((shiftL 1 4)-1) == 1  = (toHex (shiftR val 4))++"1"
   | val .&. ((shiftL 1 4)-1) == 2  = (toHex (shiftR val 4))++"2"
   | val .&. ((shiftL 1 4)-1) == 3  = (toHex (shiftR val 4))++"3"
   | val .&. ((shiftL 1 4)-1) == 4  = (toHex (shiftR val 4))++"4"
   | val .&. ((shiftL 1 4)-1) == 5  = (toHex (shiftR val 4))++"5"
   | val .&. ((shiftL 1 4)-1) == 6  = (toHex (shiftR val 4))++"6"
   | val .&. ((shiftL 1 4)-1) == 7  = (toHex (shiftR val 4))++"7"
   | val .&. ((shiftL 1 4)-1) == 8  = (toHex (shiftR val 4))++"8"
   | val .&. ((shiftL 1 4)-1) == 9  = (toHex (shiftR val 4))++"9"
   | val .&. ((shiftL 1 4)-1) == 10 = (toHex (shiftR val 4))++"A"
   | val .&. ((shiftL 1 4)-1) == 11 = (toHex (shiftR val 4))++"B"
   | val .&. ((shiftL 1 4)-1) == 12 = (toHex (shiftR val 4))++"C"
   | val .&. ((shiftL 1 4)-1) == 13 = (toHex (shiftR val 4))++"D"
   | val .&. ((shiftL 1 4)-1) == 14 = (toHex (shiftR val 4))++"E"
   | val .&. ((shiftL 1 4)-1) == 15 = (toHex (shiftR val 4))++"F"
   | otherwise                      = ""

-- bit string, number of chunks, size of chunk, list of chunks
chunker :: Integer -> Int -> Int -> [Integer]
chunker bits 0 size   = []
chunker bits len size = (shiftR (bits .&. shiftL ( (shiftL 1 size) - 1 ) ((len-1)*size) ) ((len-1)*size) ) : (chunker bits (len - 1) size)

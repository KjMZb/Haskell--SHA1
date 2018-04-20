import Data.Bits
import Data.Char
import Crypto

functionF :: Integer -> Integer -> Integer -> Integer
functionF b c d = (b .&. c ) .|. ( ( complement b ) .&. d )

functionG :: Integer -> Integer -> Integer -> Integer
functionG b c d = (b .&. c) .|. (b .&. d) .|. (c .&. d)

functionH :: Integer -> Integer -> Integer -> Integer
functionH b c d = xor ( xor b c ) d 

-- bit string, len in bits, padded bit string
pad :: Integer -> Int -> Integer
pad bits len
   | (512-(mod len 512)) < 65 = ( shiftL ((shiftL bits 1) .|. 1) (1023 - (mod len 512)) )
                              .|. (mod (toInteger len) (18446744073709551616::Integer))
   | (mod len 512) == 0       = ( shiftL ((shiftL bits 1) .|. 1) 511 ) 
                              .|. (mod (toInteger len) (18446744073709551616::Integer))
   | otherwise                = ( shiftL ((shiftL bits 1) .|. 1) (511 - (mod len 512)) )
                              .|. (mod (toInteger len) (18446744073709551616::Integer))

-- a0, b0, c0, d0, hash
combine :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
combine a b c d e = (shiftL a 128) .|. (shiftL b 96) .|. (shiftL c 64) .|. (shiftL d 32) .|. e

expander :: [Integer] -> Int -> [Integer]
expander st 80 = st
expander st i  = expander ( st ++ [leftRotate ((st!!(i-3)) `xor` (st!!(i-8)) `xor` (st!!(i-14)) `xor` (st!!(i-16))) 32 1]) (i+1)

-- a0, b0, c0, d0, e0, 512 bit chunks, s array
outer :: Integer -> Integer -> Integer -> Integer -> Integer -> [Integer] -> (Integer, Integer, Integer, Integer, Integer)
outer a b c d e []         = (a, b, c, d, e)
outer a b c d e (x:chunks) = let expanded = expander (chunker x 16 32) 16
                                 (a1, b1, c1, d1, e1) = inner a b c d e expanded 0 in
                             outer (mod (a + a1) (2^32)) (mod (b + b1) (2^32)) (mod (c + c1) (2^32)) (mod (d + d1) (2^32)) (mod (e + e1) (2^32)) chunks

-- a0, b0, c0, d0, e0, 512 bit chunks, s array, step
inner :: Integer -> Integer -> Integer -> Integer -> Integer -> [Integer] -> Int -> (Integer, Integer, Integer, Integer, Integer)
inner a b c d e chunks 80     = (a,b,c,d,e)
inner a b c d e chunks step
   | step >= 0  && step <= 19 = let f = mod ( (functionF b c d) + (leftRotate a 32 5) + e + (chunks!!step) + 1518500249 ) (2^32) in
                                    inner f a (leftRotate b 32 30) c d chunks (step+1)                               
   | step >= 20 && step <= 39 = let f = mod ( (functionH b c d) + (leftRotate a 32 5) + e + (chunks!!step) + 1859775393 ) (2^32) in
                                    inner f a (leftRotate b 32 30) c d chunks (step+1)
   | step >= 40 && step <= 59 = let f = mod ( (functionG b c d) + (leftRotate a 32 5) + e + (chunks!!step) + 2400959708 ) (2^32) in
                                    inner f a (leftRotate b 32 30) c d chunks (step+1)
   | step >= 60 && step <= 79 = let f = mod ( (functionH b c d) + (leftRotate a 32 5) + e + (chunks!!step) + 3395469782 ) (2^32) in
                                    inner f a (leftRotate b 32 30) c d chunks (step+1)

sha1  :: String -> String
sha1  message = let len       = length message
                    chunks    = if ((512-(mod (len*8) 512)) < 65) then div ((len*8) + (1024 - (mod (len*8) 512))) 512
                               else ( if ((mod (len*8) 512) == 0 ) then div (len*8) 512 else div ((len*8) + (512 - (mod (len*8) 512))) 512 )   
                    (a,b,c,d,e) = outer (1732584193::Integer) (4023233417::Integer)
                                  (2562383102::Integer) (271733878::Integer) (3285377520::Integer)
                                  (chunker ( pad (toBits message len) (len * 8) ) chunks 512) in
                    toHex $ combine a b c d e
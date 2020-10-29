module Encrypt
  ( encode,
    decode,
    newCipher,
    Cipher (..),
    randPwd,
    seedPwd
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List (sortBy)
import Data.Word (Word8)
import qualified Data.Word as W
import System.Random (newStdGen, mkStdGen)
import System.Random.Shuffle (shuffle')

data Cipher = Cipher {encodePwd :: ByteString, decodePwd :: ByteString} deriving (Show)

randPwd :: IO ByteString
randPwd = B.pack . shuffle' [0 .. 255] 256 <$> newStdGen

seedPwd :: Int -> ByteString
seedPwd = B.pack . shuffle' [0 .. 255] 256 . mkStdGen

get :: Word8 -> ByteString -> Word8
get 0 x = B.head x
get n xs = get (n -1) (B.tail xs)

encode :: Cipher -> ByteString -> ByteString
encode (Cipher c _) = B.map (flip get c)

decode :: Cipher -> ByteString -> ByteString
decode (Cipher _ c) = B.map (flip get c)

newCipher :: ByteString -> Cipher
newCipher pwd = Cipher pwd (go p)
  where
    p = B.unpack pwd
    go = B.pack . map snd . sortBy (\(a, _) (b, _) -> compare a b) . flip zip ([0 .. 255] :: [Word8])

{- test data
pwd = B.pack $ reverse [0..255]
c  = newCipher pwd
a = encode c (B.pack [0,2,3,3])
b = decode c a
 -}

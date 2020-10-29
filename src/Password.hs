module Password (randPwd) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Word (Word8)
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')

randPwd :: IO ByteString
randPwd = do
  rng <- newStdGen
  let xs = [0..255] :: [Word8]
  return $ B.pack $ shuffle' xs (length xs) rng

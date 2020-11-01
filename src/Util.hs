module Util where

import Encrypt ( newCipher, seedPwd, Cipher )
import Data.ByteString (ByteString)

samplePwd :: ByteString
samplePwd = seedPwd 233

sampleCipher :: Cipher
sampleCipher = newCipher samplePwd

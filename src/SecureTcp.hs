module SecureTcp where

import Cipher

data SecureSocket = SecureSocket Cipher String deriving Show


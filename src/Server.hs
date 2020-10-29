module Server where

import qualified Data.ByteString as B
import Network.Simple.TCP hiding (recv, send)
import SecureTcp
import Encrypt (newCipher)

server :: IO ()
server =
  serve (Host "127.0.0.1") "8000" $ \(connectionSocket, remoteAddr) -> do
    x <- decodeCopy (SecureSocket c connectionSocket)
    print (B.unpack <$> x)
    putStrLn $ "TCP connection established from " ++ show remoteAddr

pwd = B.pack $ reverse [0..255]
c  = newCipher pwd
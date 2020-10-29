{-# LANGUAGE OverloadedStrings #-}

module ServerTest where

import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Encrypt (newCipher)
import Network.Simple.TCP
import Network.Socket (SockAddr (SockAddrUnix))
import SecureTcp

server :: IO ()
server =
  serve (Host "127.0.0.1") "8000" $ \(connectionSocket, remoteAddr) -> do
    x <- decodeCopy (SecureSocket c connectionSocket)
    print (B.unpack <$> x)
    putStrLn $ "TCP connection established from " ++ show remoteAddr

serverb :: IO ()
serverb =
  serve (Host "127.0.0.1") "8000" $ \(connectionSocket, remoteAddr) -> do
    x <- recv connectionSocket 64
    print (B.unpack <$> x)

    send connectionSocket (B.pack [0x05, 0x00])
    t <- recv connectionSocket 1024
    print t

    case t of
      Nothing -> return ()
      Just t' -> do
        let (tAddr, tPort) = getADDR t'
        send connectionSocket (B.pack [0x05, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])
        print tAddr
    -- TODO: http request

    putStrLn $ "TCP connection established from " ++ show remoteAddr

getADDR :: ByteString -> (ByteString, ByteString)
getADDR x = bimap B.pack B.pack $ go ([], s)
  where
    s = drop 4 $ B.unpack x
    go (p, [a, b]) = (p, [a, b])
    go (a, (b : bs)) = go (a ++ [b], bs)

pwd = B.pack $ reverse [0 .. 255]

c = newCipher pwd

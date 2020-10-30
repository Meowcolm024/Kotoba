{-# LANGUAGE OverloadedStrings #-}

module ServerTest where

import Control.Monad.IO.Class (MonadIO)
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word (Word8)
import Encrypt (newCipher)
import Network.Simple.TCP
import Network.Socket (SockAddr (SockAddrUnix))
import Network.HTTP.Client.Conduit
import SecureTcp
import Network.HTTP.Simple 

server :: IO ()
server =
  serve (Host "127.0.0.1") "8000" $ \(connectionSocket, remoteAddr) -> do
    x <- decodeCopy (SecureSocket c connectionSocket)
    print (B.unpack <$> x)
    putStrLn $ "TCP connection established from " ++ show remoteAddr

serverb :: IO ()
serverb =
  serve (Host "127.0.0.1") "1080" $ \(connectionSocket, remoteAddr) -> do
    x <- recv connectionSocket 64
    print (B.unpack <$> x)

    send connectionSocket (B.pack [0x05, 0x00])
    t <- recv connectionSocket 1024
    print t

    case t of
      Nothing -> return ()
      Just t' -> do
        let (tAddr, tPort) = getADDR ((B.unpack t') !! 3) t'
        send connectionSocket (B.pack [0x05, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])
        print tAddr
        h <- recv connectionSocket 1024
        putStrLn $ "REQUEST: " ++ show h
        {- case h of
          Nothing -> return ()
          Just h' -> do
            let b = setRequestBody (RequestBodyBS h') defaultRequest -- !
            r <- httpBS b
            let p = getResponseBody r
            sendBack connectionSocket p -}
        let content = "<!doctype html><html><body>Hello World</body></html>"
        send connectionSocket content

    putStrLn $ "TCP connection established from " ++ show remoteAddr

getADDR :: Word8 -> ByteString -> (ByteString, ByteString)
getADDR t x = bimap B.pack B.pack $ go ([], f t s)
  where
    s = B.unpack x
    f x = if t == 0x03 then drop 5 else drop 4
    go (p, [a, b]) = (p, [a, b])
    go (a, (b : bs)) = go (a ++ [b], bs)

sendBack :: MonadIO m => Socket -> ByteString -> m ()
sendBack socket input = helper (B.unpack input)
  where
    helper i = do
      let (first, rest) = splitAt 1024 i
      send socket (B.pack first)
      if null rest
        then return ()
        else helper rest

pwd = B.pack $ reverse [0 .. 255]

c = newCipher pwd

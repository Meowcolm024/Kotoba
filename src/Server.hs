{-# LANGUAGE OverloadedStrings #-}

module Server where

import Control.Monad (forever)
import qualified Data.ByteString as B
import Data.Word (Word8)
import Encrypt (newCipher)
import Network.Simple.TCP
import SecureTcp

local :: IO ()
local = sendbytes [0 .. 3]

sendbytes :: [Word8] -> IO ()
sendbytes xs =
  connect "127.0.0.1" "8000" $ \(connectionSocket, remoteAddr) -> do
    encodeCopy (SecureSocket c connectionSocket) (B.pack xs)
    putStrLn $ "Connection established to " ++ show remoteAddr

runServer :: IO ()
runServer =
  serve (Host "127.0.0.1") "8000" $ \(connectionSocket, remoteAddr) -> do
    x <- decodeCopy (SecureSocket c connectionSocket)
    case x of
      Nothing -> return ()
      Just x' -> do
        -- ! perform HTTP request here and data back
        print x'
        let content = "<!doctype html><html><body>Hello World</body></html>"
        returnLocal c content "127.0.0.1" "4000"

returnLocal ci dt hn hp =
  connect hn hp $ \(connectionSocket, remoteAddr) ->
    encodeCopy (SecureSocket ci connectionSocket) dt

pwd = B.pack $ reverse [0 .. 255]

c = newCipher pwd

{-# LANGUAGE OverloadedStrings #-}

module Server where

import Control.Monad (forever)
import qualified Data.ByteString as B
import Data.Word (Word8)
import Encrypt (newCipher)
import Network.Simple.TCP
import SecureTcp
import Util ( sampleCipher )
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)

runServer :: IO ()
runServer =
  serve (Host "127.0.0.1") "8000" $ \(localSocket, remoteAddr) -> do 
    getLocal localSocket ("127.0.0.1", "1080")
    putStrLn $ "TCP connection established from " ++ show remoteAddr

dialServer ci dt (rhn, rhp) (lhn, lhp) = do
  connect rhn rhp $ \(connectionSocket, _) -> do
    encodeCopy (SecureSocket ci connectionSocket) dt
  listen lhn lhp $ \(connectionSocket, _) -> do
    accept connectionSocket $ \(connectionSocket', _) -> do
      decodeCopy (SecureSocket ci connectionSocket') :: MonadIO m => m (Maybe ByteString)

returnLocal ci dt hn hp =
  connect hn hp $ \(connectionSocket, remoteAddr) ->
    encodeCopy (SecureSocket ci connectionSocket) dt

getLocal local remote = do
  result <- recv local 2048
  case result of
    Nothing -> return ()
    Just r -> do
      x <- dialServer sampleCipher r ("127.0.0.1", "4000") remote
      case x of
        Nothing -> return ()
        Just x' -> send local x'
      getLocal local remote

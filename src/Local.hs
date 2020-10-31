{-# LANGUAGE OverloadedStrings #-}

module Local where

import Control.Monad.IO.Class (MonadIO)
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char (chr)
import Data.Word (Word8)
import Encrypt (Cipher, newCipher)
import Network.Simple.TCP
import Network.Socket (SockAddr (SockAddrUnix))
import SecureTcp

runLocal :: IO ()
runLocal =
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

        case h of
          Nothing -> return ()
          Just h' -> do
            putStrLn $ b2s h'
            
            -- content <- doReq h' (b2s tAddr) (b2s tPort)
            content <- dialServer c h' ("127.0.0.1", "8000") ("127.0.0.1", "4000")
            print content

            case content of
              Nothing -> return ()
              Just content' ->
                send connectionSocket content'

    putStrLn $ "TCP connection established from " ++ show remoteAddr

getADDR :: Word8 -> ByteString -> (ByteString, ByteString)
getADDR t x = bimap B.pack B.pack $ go ([], f t s)
  where
    s = B.unpack x
    f x = if t == 0x03 then drop 5 else drop 4
    go (p, [a, b]) = (p, [a, b])
    go (a, (b : bs)) = go (a ++ [b], bs)

dialServer ci dt (rhn, rhp) (lhn, lhp) = do
  connect rhn rhp $ \(connectionSocket, _) -> do
    encodeCopy (SecureSocket ci connectionSocket) dt
  listen lhn lhp $ \(connectionSocket, _) -> do
    accept connectionSocket $ \(connectionSocket', _) -> do
      decodeCopy (SecureSocket ci connectionSocket') :: MonadIO m => m (Maybe ByteString)

doReq dt dstAddr dstPort = do
  connect dstAddr dstPort $ \(connectionSocket, _) -> do
    send connectionSocket dt
    getBack connectionSocket

getBack :: MonadIO m => Socket -> m (Maybe ByteString)
getBack sSocket = do
  result <- recv sSocket 1024
  if result == Nothing
    then return . return $ B.empty
    else do
      rest <- getBack sSocket
      return $ B.append <$> result <*> rest

pwd = B.pack $ reverse [0 .. 255]
c = newCipher pwd

b2s :: ByteString -> String
b2s = map (chr . fromEnum) . B.unpack

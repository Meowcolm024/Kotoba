{-# LANGUAGE OverloadedStrings #-}

module Local where

import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char (chr)
import Data.Word (Word8)
import Network.Simple.TCP

runLocal :: IO ()
runLocal =
  serve (Host "127.0.0.1") "1080" $ \(connectionSocket, remoteAddr) -> do
    x <- recv connectionSocket 64
    print (B.unpack <$> x)
    -- respond connection
    send connectionSocket (B.pack [0x05, 0x00])
    t <- recv connectionSocket 1024
    case t of
      Nothing -> return ()
      Just t' -> do
        -- get true dst address and port
        let (dstAddr, dstPort) = getADDR ((B.unpack t') !! 3) t'
        -- respond success connection
        send connectionSocket $ B.pack [0x05, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
        print (bs2addr dstAddr, bs2port dstPort)
        -- getting full http request
        request <- recv connectionSocket 2048
        case request of
          Nothing -> return ()
          Just req ->
            -- return result
            doReq req connectionSocket (bs2addr dstAddr) (bs2port dstPort)

    putStrLn $ "TCP connection established from " ++ show remoteAddr

getADDR :: Word8 -> ByteString -> (ByteString, ByteString)
getADDR t x = bimap B.pack B.pack $ go ([], f t s)
  where
    s = B.unpack x
    f x = if t == 0x03 then drop 5 else drop 4
    go (p, [a, b]) = (p, [a, b])
    go (a, (b : bs)) = go (a ++ [b], bs)

doReq :: ByteString -> Socket -> HostName -> ServiceName -> IO ()
doReq requests local dstAddr dstPort =
  connect dstAddr dstPort $ \(dstSocket, _) -> do
    send dstSocket requests
    retrieve dstSocket local

retrieve :: Socket -> Socket -> IO ()
retrieve remote local = do
  result <- recv remote 4096
  case result of
    Nothing -> return ()
    Just r -> send local r >> retrieve remote local

bs2addr :: ByteString -> String
bs2addr = map (chr . fromEnum) . B.unpack

bs2port :: ByteString -> String
bs2port = dropWhile (== '0') . concatMap (show . fromEnum) . B.unpack

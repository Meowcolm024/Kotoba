module Local where

import qualified Data.ByteString as B
import Data.Word (Word8)
import Network.Simple.TCP hiding (recv, send)
import SecureTcp

local :: IO ()
local = sendbytes [0 .. 3]

sendbytes :: [Word8] -> IO ()
sendbytes xs =
  connect "127.0.0.1" "8000" $ \(connectionSocket, remoteAddr) -> do
    -- send connectionSocket (B.pack xs)
    putStrLn $ "Connection established to " ++ show remoteAddr
    
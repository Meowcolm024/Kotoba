module SecureTcp
  ( encodeCopy,
    decodeCopy,
    SecureSocket (..),
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Encrypt ( Cipher, encode, decode )
import Network.Simple.TCP

data SecureSocket = SecureSocket Cipher Socket deriving (Show)

decodeRead :: MonadIO m => SecureSocket -> m (Maybe ByteString)
decodeRead (SecureSocket cipher socket) = do
  result <- recv socket 1024
  return $ decode cipher <$> result

encodeWrite :: MonadIO m => SecureSocket -> ByteString -> m ()
encodeWrite (SecureSocket cipher socket) = send socket . encode cipher

decodeCopy :: MonadIO m => SecureSocket -> m (Maybe ByteString)
decodeCopy sSocket = do
  result <- decodeRead sSocket
  if result == Nothing
    then return . return $ B.empty
    else do
      rest <- decodeCopy sSocket
      return $ B.append <$> result <*> rest

encodeCopy :: MonadIO m => SecureSocket -> ByteString -> m ()
encodeCopy sSocket input = helper sSocket (B.unpack input)
  where
    helper s i = do
      let (first, rest) = splitAt 1024 i
      encodeWrite s (B.pack first)
      if null rest
        then return ()
        else helper sSocket rest

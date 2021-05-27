{-# LANGUAGE OverloadedStrings #-}
module Api.Pastebin where

import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import           Network.Socket
import           Network.Socket.ByteString as NBS

pastebinAddr :: HostName
pastebinAddr = "tcp.st"

pastebinPort :: Integer
pastebinPort = 7777

pasteUrlLength :: Int
pasteUrlLength = 25

paste :: ByteString -> IO ByteString
paste text =
  getAddrInfo Nothing (Just pastebinAddr) (Just $ show pastebinPort) >>= \case
    [] -> error "Could not resolve hostname"
    addr:_ -> do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock (addrAddress addr)
      sendAll sock text
      recvUrl sock ""
  where
    recvUrl sock buf
      | BS.isInfixOf "\n" buf
      , (line, rest) <- BS.breakSubstring "\n" buf
      , (word, param) <- BS.breakSubstring " " line
      = case word of
          "URL"   -> pure $ BS.tail param
          "ERROR" -> error $ show param
          _       -> recvUrl sock (BS.tail rest)
      | otherwise
      = NBS.recv sock 4096 >>= \bs ->
          if BS.null bs
          then error "No URL received"
          else recvUrl sock (BS.append buf bs)

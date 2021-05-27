module JsonUtil where

import Data.Aeson

readJsonFile :: FilePath -> IO Value
readJsonFile file = do result <- eitherDecodeFileStrict' file
                       case result of
                         Left err -> error (show err)
                         Right json -> pure json

writeJsonFile :: FilePath -> Value -> IO ()
writeJsonFile = encodeFile

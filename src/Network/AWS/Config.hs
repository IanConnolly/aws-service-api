{-# LANGUAGE TemplateHaskell, QuasiQuotes, ScopedTypeVariables #-}
module Network.AWS.Config
    ( AWSSetup(..)
    , loadSetupFromFile
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Text.Config
import Text.Parsec (parse)

mkConfig "configParser" [config|
AWSSetup
    accessKey       ByteString
    secretAccessKey ByteString
    region          String
    image           String
    instanceType    String
|]

loadSetupFromFile :: FilePath -> IO AWSSetup
loadSetupFromFile path = do
    str <- BS.readFile path
    case parse configParser "" str of
        Left err   -> fail $ show err
        Right conf -> return conf

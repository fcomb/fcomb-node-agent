{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config (
    loadConf,
    saveConf,
    Configuration(..)
) where

import Data.Yaml
import Data.Maybe
import GHC.Generics
import Globals
import Control.Lens
import System.Directory (doesFileExist)

data Configuration = Configuration {
    nodeId    :: String
  , nodeToken :: String
} deriving Generic

instance ToJSON Configuration
instance FromJSON Configuration


loadConf :: IO (Maybe Configuration)
loadConf = do
    doesFileExist configFilePath >>= \exists ->
        case exists of
            True ->
              decodeFile configFilePath :: IO (Maybe Configuration)
            False ->
              return Nothing


saveConf :: Configuration -> IO ()
saveConf conf = encodeFile configFilePath conf

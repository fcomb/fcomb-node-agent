{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

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
  , agentToken :: String
} deriving Generic

instance ToJSON Configuration
instance FromJSON Configuration


loadConf :: IO (Maybe Configuration)
loadConf = doesFileExist configFilePath >>= \case
    True -> decodeFile configFilePath :: IO (Maybe Configuration)
    _ -> return Nothing


saveConf :: Configuration -> IO ()
saveConf conf = encodeFile configFilePath conf

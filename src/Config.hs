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

data Configuration = Configuration {
    dockerHost :: String
    , fcombHost :: String
    , nodeToken :: String
} deriving Generic

instance ToJSON Configuration
instance FromJSON Configuration


loadConf :: IO Configuration
loadConf = do
    maybeConf <- decodeFile configFilePath :: IO (Maybe Configuration)
    -- todo: error handling
    let conf = fromJust maybeConf
        -- todo: apply lenses
        confWithDockerHost = if null (dockerHost conf)
            then conf { dockerHost = defaultDockerHost}
            else conf
        confWithDockerHostWithFcombHost = if null (fcombHost conf)
            then conf { fcombHost = defaultFcombHost}
            else conf

    return confWithDockerHostWithFcombHost


saveConf :: Configuration -> IO ()
saveConf conf = encodeFile configFilePath conf

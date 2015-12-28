{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config (
    loadConf,
    saveConf,
    defaultConf,
    Configuration(..)
) where

import Data.Yaml
import Data.Maybe
import GHC.Generics
import Globals
import Control.Lens

data Configuration = Configuration {
    certCommonName :: String
    , dockerHost :: String
    , fcombHost :: String
    , fcombToken :: String
    , fcombUUID :: String
    , dockerOpts :: String
} deriving Generic

instance ToJSON Configuration
instance FromJSON Configuration


loadConf :: FilePath -> IO Configuration
loadConf filePath = do
    maybeConf <- decodeFile filePath :: IO (Maybe Configuration)
    -- todo: error handling
    let conf = fromJust maybeConf
        -- todo: apply lenses
        confWithDockerHost = if null (dockerHost conf)
            then conf { dockerHost = defaultDockerHost}
            else conf
        confWithFcombHost = if null (fcombHost conf)
            then conf { fcombHost = defaultFcombHost}
            else conf

    return confWithFcombHost


saveConf :: FilePath -> Configuration -> IO ()
saveConf filePath conf = encodeFile filePath conf


defaultConf :: Configuration
defaultConf = Configuration
        defaultCertCommonName
        defaultDockerHost
        defaultFcombHost
        ""
        ""
        ""

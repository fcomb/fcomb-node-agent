{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Http (
    module Http
) where

import System.Posix.Files
import System.FilePath
import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit as HC
import Network.Wreq
import qualified Data.Conduit as C
import Control.Monad.Trans.Resource (runResourceT)
import GHC.Generics
import Globals
import Data.Maybe
import Data.Aeson
import Data.Text


data RegResponseForm = RegResponseForm {
    userCaCert :: Text
    , fcombUUID :: Text
    , certCommonName :: Text
    , dockerBinaryURL :: Text
    , ngrokBinaryURL :: Text
    , publicIpAddress :: Text
} deriving Generic

instance ToJSON RegResponseForm
instance FromJSON RegResponseForm


data RegPostForm = RegPostForm {
    postVersion :: Text
} deriving Generic

instance ToJSON RegPostForm
instance FromJSON RegPostForm


data RegPatchForm = RegPatchForm {
    publicCert :: Text
    , patchVersion :: Text
} deriving Generic

instance ToJSON RegPatchForm
instance FromJSON RegPatchForm

--
--data RegGetForm = RegGetForm {
--    agentVersion :: Text
--    , dockerUrl :: Text
--    , externalFqdn :: Text
--    , ngrokUrl :: Text
--    , publicCert :: Text
--    , resourceUri :: Text
--    , state :: Text
--    , tunnel :: Text
--    , userCaCert :: Text
--    , uuid :: Text
--    , ngrokHost :: Text
--} deriving Generic
--
--instance ToJSON RegGetForm
--instance FromJSON RegGetForm


download :: String -> FilePath -> IO ()
download url path = do
    request <- parseUrl url
    manager <- newManager tlsManagerSettings
    runResourceT $ do
       response <- http request manager
       HC.responseBody response C.$$+- sinkFile path


postToFcomb :: String -> String -> String -> FilePath -> FilePath -> IO ()
postToFcomb url token uuid caFilePath configFilePath = do
    let form = RegPostForm version
        formData = encode form
    register url "POST" token uuid caFilePath configFilePath formData


patchToFcomb :: String -> String -> String -> FilePath -> FilePath -> FilePath -> IO ()
patchToFcomb url token uuid caFilePath certFilePath configFilePath = do
    cert <- readFile certFilePath
    let form = RegPatchForm {cert, version}
        formData = encode form
    register url "PATCH" token uuid caFilePath configFilePath formData


register :: String -> String -> String -> String -> FilePath -> FilePath -> ByteString -> IO ()
register url method token uuid caFilePath configFilePath formData = do
    let body = sendRegRequest url method token uuid formData
    handleRegResponse body caFilePath configFilePath


sendRegRequest :: String -> String -> String -> String -> ByteString -> IO ()
sendRegRequest url method token uuid formData = do
    let options = defaults  & header "Authorization FcombAgentToken " ++ token
                            & header "Content-Type application/json"
                            & header "User-Agent fcomb-agent/" ++ version

    sendRequest method (combine url uuid) formData options


sendRequest :: String -> String -> ByteString -> Options -> Maybe ByteString
sendRequest method url formData opts = do
    r <- customMethodWith method opts url formData
    --let status = r ^. responseStatus . statusCode
    -- todo: error handling
    fromJust $ r ^? responseBody


handleRegResponse :: ByteString -> FilePath -> FilePath -> IO ()
handleRegResponse body caFilePath configFilePath = do
    let maybeForm = decode body :: Maybe RegResponseForm
        form = fromJust maybeForm

    -- Save user ca cert file
    writeFile caFilePath (userCaCert form)

    -- Update global Conf

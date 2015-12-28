{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Http (
    download,
    register
) where

import System.Posix.Files
import System.FilePath
import Network.Wreq
import GHC.Generics
import Globals
import Data.Maybe
import Data.Aeson
import Data.Text
import Control.Lens
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C

data RegRequest = RegRequest {
    certificationRequest :: String
} deriving Generic

instance ToJSON RegRequest


data NodeResponseForm = NodeResponseForm {
    rootCertificate :: String,
    signedCertificate :: String
} deriving Generic

instance FromJSON NodeResponseForm


download :: String -> FilePath -> IO ()
download url path = do
    r <- get url
    BL.writeFile path (r ^. responseBody)


register :: String -> String -> FilePath -> FilePath-> IO ()
register token cert caFilePath certFilePath = do
    let regUrl = defaultFcombHost ++ regEndpoint
    putStrLn $ "Registering with fcomb: " ++ regUrl

    let regReq = encode $ RegRequest cert
        regReqOpts = defaults  & param "access_token" .~ [pack token]
                               & header "content-Type" .~ ["application/json"]

    regResp <- postWith regReqOpts regUrl regReq
    let nodeLocation = regResp ^. responseHeader "location"
        nodeUrl = defaultFcombHost ++ C.unpack nodeLocation
        nodeToken = regResp ^. responseHeader "authorization"
        [authTerm, authToken] = C.split ' ' nodeToken
        nodeOpts = defaults & header "Authorization" .~ [nodeToken]

    putStrLn $ "Received registration response. Obtaining node: " ++ nodeUrl
    nodeResp <- getWith nodeOpts nodeUrl
    let body = nodeResp ^. responseBody
        maybeRespForm = decode body :: Maybe NodeResponseForm
        respForm = fromJust maybeRespForm

    putStrLn "Received node response with certificates"
    putStrLn $ "Writing root certificate to " ++ caFilePath
    writeFile caFilePath (rootCertificate respForm)
    putStrLn $ "Writing signed certificate to " ++ certFilePath
    writeFile certFilePath (signedCertificate respForm)
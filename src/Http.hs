{-# LANGUAGE OverloadedStrings #-}

module Http (
    download,
    joinNode,
    registerNode
) where

import Globals
import System.Posix.Files
import System.FilePath
import Network.Wreq
import Data.Maybe
import Data.Aeson
import Data.Text
import Control.Lens
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C
import qualified Data.List as L

data JoinRequest = JoinRequest {
    certificationRequest :: String
}

instance ToJSON JoinRequest where
    toJSON (JoinRequest certificationRequest) =
        object ["certificationRequest" Data.Aeson..= certificationRequest]


data EmptyRequest = EmptyRequest

instance ToJSON EmptyRequest where
    toJSON EmptyRequest =
        object []


data NodeResponseForm = NodeResponseForm {
    nodeId :: Int,
    rootCertificate :: String,
    signedCertificate :: String
}

instance FromJSON NodeResponseForm where
    parseJSON (Object v) = NodeResponseForm <$> v .: "id"
                                            <*> v .: "rootCertificate"
                                            <*> v .: "signedCertificate"


download :: String -> FilePath -> IO ()
download url path = do
    r <- get url
    BL.writeFile path (r ^. responseBody)


joinNode :: String -> String -> IO (String, String)
joinNode joinToken cert = do
    let joinUrl = fcombHost ++ nodesEndpoint ++ "join"
    putStrLn $ "Joining with fcomb: " ++ joinUrl

    let joinReq = encode $ JoinRequest cert
        joinReqOpts = defaults & header "content-Type" .~ ["application/json"]
                               & header "Authorization" .~ [C.pack $ "Token " ++ joinToken]

    joinResp <- postWith joinReqOpts joinUrl joinReq
    let nodeLocation = joinResp ^. responseHeader "location"
        nodeUrl = fcombHost ++ C.unpack nodeLocation
        nodeToken = C.unpack . L.last . C.split ' ' $ joinResp ^. responseHeader "authorization"

    getNode nodeUrl nodeToken


getNode :: String -> String -> IO (String, String)
getNode nodeUrl nodeToken = do
    putStrLn $ "Received join response. Obtaining node: " ++ nodeUrl

    let nodeOpts = defaults & header "Authorization" .~ [C.pack $ "Token " ++ nodeToken]

    nodeResp <- getWith nodeOpts nodeUrl
    let body = nodeResp ^. responseBody
        maybeRespForm = decode body :: Maybe NodeResponseForm
        respForm = fromJust maybeRespForm
        regUrl = fcombHost ++ nodesEndpoint ++ show (nodeId respForm) ++ "/register"

    putStrLn $ "Writing root certificate to " ++ caFilePath
    writeFile caFilePath (rootCertificate respForm)
    putStrLn $ "Writing signed certificate to " ++ certFilePath
    writeFile certFilePath (signedCertificate respForm)

    return (show $ nodeId respForm, nodeToken)


registerNode :: String -> String -> IO (Either String Bool)
registerNode nodeId nodeToken = do
    let regUrl = fcombHost ++ nodesEndpoint ++ nodeId ++ "/register"
        nodeOpts = defaults & header "Authorization" .~ [C.pack $ "Token " ++ nodeToken]
    putStrLn $ "Finally registering the node " ++ regUrl
    regResp <- postWith nodeOpts regUrl (encode EmptyRequest)

    let status = regResp ^. responseStatus
        code = status ^. statusCode
        message = show $ status ^. statusMessage
        result = if code >= 200 && code < 300
            then
                Right True
            else
                Left message

    return result
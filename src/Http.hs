{-# LANGUAGE OverloadedStrings #-}

module Http (
    download,
    joinNode
) where

import System.Posix.Files
import System.FilePath
import Network.Wreq
import Data.Maybe
import Data.Aeson
import Data.Text
import Control.Lens
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C

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


joinNode :: String -> String -> String -> String -> FilePath -> FilePath-> IO (String, String)
joinNode fcombHost nodesEndpoint joinToken cert caFilePath certFilePath = do
    let joinUrl = fcombHost ++ nodesEndpoint ++ "join"
    putStrLn $ "Joining with fcomb: " ++ joinUrl

    let joinReq = encode $ JoinRequest cert
        joinReqOpts = defaults & header "content-Type" .~ ["application/json"]

    joinResp <- postWith joinReqOpts joinUrl joinReq
    let nodeLocation = joinResp ^. responseHeader "location"
        nodeUrl = fcombHost ++ C.unpack nodeLocation
        nodeToken = joinResp ^. responseHeader "authorization"
        nodeOpts = defaults & header "Authorization" .~ [C.append "Token " nodeToken]

    putStrLn $ "Received joind response. Obtaining node: " ++ nodeUrl
    nodeResp <- getWith nodeOpts nodeUrl
    let body = nodeResp ^. responseBody
        maybeRespForm = decode body :: Maybe NodeResponseForm
        respForm = fromJust maybeRespForm
        regUrl = fcombHost ++ nodesEndpoint ++ show (nodeId respForm) ++ "/register"

    putStrLn $ "Writing root certificate to " ++ caFilePath
    writeFile caFilePath (rootCertificate respForm)
    putStrLn $ "Writing signed certificate to " ++ certFilePath
    writeFile certFilePath (signedCertificate respForm)

    return $ (show $ nodeId respForm, C.unpack nodeToken)

register :: String -> String -> String -> String -> IO ()
register fcombHost nodesEndpoint nodeId nodeToken = do
    let regUrl = fcombHost ++ nodesEndpoint ++ nodeId ++ "/register"
        nodeOpts = defaults & header "Authorization" .~ [C.pack $ "Token " ++ nodeToken]
    putStrLn $ "Finally registering the node " ++ regUrl
    regResp <- postWith nodeOpts regUrl (encode EmptyRequest)
    return ()

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
import Network.HTTP.Client (HttpException ())
import Data.Maybe
import Data.Aeson
import Data.Text
import Control.Lens
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C
import qualified Data.List as L
import Control.Retry
import Control.Monad.Catch
import Data.Monoid

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


-- 12 attempts starting from 10 seconds delay
httpRetryPolicy = exponentialBackoff 10000000 <> limitRetries 12

-- todo - should handling be limited to server fault exceptions only?
alwaysRetryHttp :: Monad m => HttpException -> m Bool
alwaysRetryHttp e = return True

download :: String -> FilePath -> IO ()
download url path = do
    let req = get url

    putStrLn $ "Downloading docker binary from " ++ url

    r <- recovering
            httpRetryPolicy
            [const $ Handler alwaysRetryHttp]
            (\retryStatus -> do
                putStrLn "Sending api request..."
                req
            )

    BL.writeFile path (r ^. responseBody)


joinNode :: String -> String -> IO (String, String)
joinNode joinToken cert = do
    let joinUrl = fcombHost ++ nodesEndpoint ++ "join"
        joinData = encode $ JoinRequest cert
        joinReqOpts = defaults & header "content-Type" .~ ["application/json"]
                               & header "Authorization" .~ [C.pack $ "Token " ++ joinToken]
        joinReq = postWith joinReqOpts joinUrl joinData

    putStrLn $ "Joining with fcomb: " ++ joinUrl

    joinResp <- recovering
                    httpRetryPolicy
                    [const $ Handler alwaysRetryHttp]
                    (\retryStatus -> do
                        putStrLn "Sending api request..."
                        joinReq
                    )

    let nodeLocation = joinResp ^. responseHeader "location"
        nodeUrl = fcombHost ++ C.unpack nodeLocation
        nodeToken = C.unpack . L.last . C.split ' ' $ joinResp ^. responseHeader "token"

    getNode nodeUrl nodeToken


getNode :: String -> String -> IO (String, String)
getNode nodeUrl nodeToken = do
    let nodeOpts = defaults & header "Authorization" .~ [C.pack $ "Token " ++ nodeToken]
        nodeReq = getWith nodeOpts nodeUrl

    putStrLn $ "Received join response. Obtaining node: " ++ nodeUrl

    nodeResp <- recovering
                    httpRetryPolicy
                    [const $ Handler alwaysRetryHttp]
                    (\retryStatus -> do
                        putStrLn "Sending api request..."
                        nodeReq
                    )

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
        regReq = postWith nodeOpts regUrl (encode EmptyRequest)

    putStrLn $ "Registering the node " ++ regUrl

    regResp <- recovering
                    httpRetryPolicy
                    [const $ Handler alwaysRetryHttp]
                    (\retryStatus -> do
                        putStrLn "Sending api request..."
                        regReq
                    )

    -- todo - verify correctness of this
    let status = regResp ^. responseStatus
        code = status ^. statusCode
        message = show $ status ^. statusMessage
        result = if code >= 200 && code < 300
            then
                Right True
            else
                Left message

    return result
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource (runResourceT)
import           Crypto.Hash
import           Crypto.PubKey.RSA
import qualified Crypto.RSA                   as RSA
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as BC
import qualified Data.Conduit                 as C
import           Data.Conduit.Binary          (sinkFile)
import           Data.PEM
import           Data.X509
import           Data.X509.PKCS10
import           Network.HTTP.Client          (defaultManagerSettings)
import           Network.HTTP.Conduit
import qualified Network.Wreq                 as WQ
-- import Control.Lens
import           Data.Aeson                   (toJSON)
import           Data.Aeson.Lens              (key, nth)
import           Data.Aeson.Types
import           System.Environment

rsaKeySize = 256

publicExponent = 0x10001

newtype JoinRequest =
  JoinRequest { certificationRequest :: String }
  deriving (Show, Eq)

instance ToJSON JoinRequest where
  toJSON (JoinRequest req) =
    object ["certificationRequest" .= req]

main :: IO ()
main = do
  (pubKey, privKey) <- generate rsaKeySize publicExponent
  putStrLn $ show $ RSA.toPEM privKey

  let subjectAttrs = makeX520Attributes [(X520CommonName, "node.fcomb.io"), (X520OrganizationName, "fcomb")]
  let extAttrs = PKCS9Attributes [PKCS9Attribute $ ExtBasicConstraints False Nothing, PKCS9Attribute $ ExtKeyUsage [KeyUsage_digitalSignature,KeyUsage_nonRepudiation,KeyUsage_keyEncipherment]]
  Right req <- generateCSR subjectAttrs extAttrs (KeyPairRSA pubKey privKey) SHA512
  BC.putStrLn . pemWriteBS . toPEM $ req -- export in PEM format
  putStrLn . show $ verify (csrToSigned req) $ PubKeyRSA pubKey -- sign CSR before verify

  let pem = BC.unpack . pemWriteBS . toPEM $ req
  let joinReq = JoinRequest pem
  joinToken <- head <$> getArgs
  let url = "https://api.fcomb.io/v1/agent/nodes/join?access_token=" ++ joinToken
  r <- WQ.post url (toJSON joinReq)
  -- res <- (r ^? responseBody . key "json" . nth 0)
  putStrLn . show $ r

  -- manager <- newManager defaultManagerSettings
  -- request <- liftIO $ parseUrl "http://coreos:2375/containers/ubuntu_tty/archive?path=/"
  -- runResourceT $ do
  --   response <- http request manager
  --   responseBody response C.$$+- sinkFile "/tmp/docker.dump"

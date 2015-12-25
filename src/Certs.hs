{-# LANGUAGE OverloadedStrings #-}

module Certs (
    createCerts
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Crypto.Hash
import           Crypto.PubKey.RSA
import           Data.PEM
import           Data.X509
import           Data.X509.PKCS10
import qualified Network.Wreq as WQ
-- import Control.Lens
import Data.Aeson (toJSON)
import Data.Aeson.Lens (key, nth)
import Data.Aeson.Types

rsaKeySize = 256

publicExponent = 0x10001

newtype JoinRequest =
  JoinRequest { certificationRequest :: String }
  deriving (Show, Eq)

instance ToJSON JoinRequest where
  toJSON (JoinRequest req) = object ["certificationRequest" .= req]

createCerts :: IO ()
createCerts = do
  (pubKey, privKey) <- generate rsaKeySize publicExponent

  let subjectAttrs = makeX520Attributes [(X520CommonName, "node.fcomb.io"), (X520OrganizationName, "fcomb")]
  let extAttrs = PKCS9Attributes [PKCS9Attribute $ ExtBasicConstraints False Nothing, PKCS9Attribute $ ExtKeyUsage [KeyUsage_digitalSignature,KeyUsage_nonRepudiation,KeyUsage_keyEncipherment]]
  Right req <- generateCSR subjectAttrs extAttrs (KeyPairRSA pubKey privKey) SHA512
  BC.putStrLn . pemWriteBS . toPEM $ req -- export in PEM format
  putStrLn . show $ verify (csrToSigned req) $ PubKeyRSA pubKey -- sign CSR before verify

  let pem = BC.unpack . pemWriteBS . toPEM $ req
  let joinReq = JoinRequest pem
  r <- WQ.post "http://api/v1/nodes/join" (toJSON joinReq)
  -- res <- (r ^? responseBody . key "json" . nth 0)
  putStrLn . show $ r


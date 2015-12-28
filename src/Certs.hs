{-# LANGUAGE OverloadedStrings #-}

module Certs (
    createCerts
) where

import qualified Data.ByteString.Char8 as BC
import           Crypto.Hash
import           Crypto.PubKey.RSA
import           Data.PEM
import           Data.X509
import           Data.X509.PKCS10
import qualified Network.Wreq as WQ

rsaKeySize = 256

publicExponent = 0x10001

createCerts :: FilePath -> IO (String)
createCerts keyFilePath = do
    (pubKey, privKey) <- generate rsaKeySize publicExponent

    let subjectAttrs = makeX520Attributes [(X520CommonName, "node.fcomb.io"), (X520OrganizationName, "fcomb")]
    let extAttrs = PKCS9Attributes [PKCS9Attribute $ ExtBasicConstraints False Nothing, PKCS9Attribute $ ExtKeyUsage [KeyUsage_digitalSignature, KeyUsage_nonRepudiation, KeyUsage_keyEncipherment]]

    Right req <- generateCSR subjectAttrs extAttrs (KeyPairRSA pubKey privKey) SHA512
    return $ BC.unpack . pemWriteBS . toPEM $ req

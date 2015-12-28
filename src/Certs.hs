{-# LANGUAGE OverloadedStrings #-}

module Certs (
    createCerts
) where

import qualified Data.ByteString.Char8 as BC
import           Crypto.Hash
import qualified Crypto.RSA as RSA
import           Crypto.PubKey.RSA
import           Data.PEM
import           Data.X509
import           Data.X509.PKCS10

rsaKeySize = 256

publicExponent = 0x10001

createCerts :: FilePath -> IO String
createCerts keyFilePath = do
    (pubKey, privKey) <- generate rsaKeySize publicExponent

    putStrLn $ "Writing private key to " ++ keyFilePath
    BC.writeFile keyFilePath $ pemWriteBS $ RSA.toPEM privKey

    let subjectAttrs = makeX520Attributes [(X520CommonName, "node.fcomb.io"), (X520OrganizationName, "fcomb")]
    let extAttrs = PKCS9Attributes [PKCS9Attribute $ ExtBasicConstraints False Nothing, PKCS9Attribute $ ExtKeyUsage [KeyUsage_digitalSignature, KeyUsage_nonRepudiation, KeyUsage_keyEncipherment]]

    Right req <- generateCSR subjectAttrs extAttrs (KeyPairRSA pubKey privKey) SHA512
    return $ BC.unpack . pemWriteBS . toPEM $ req

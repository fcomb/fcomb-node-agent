module Crypto.RSA
       ( toPEM
       ) where

import qualified Crypto.PubKey.RSA        as RSA
import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding
import           Data.ASN1.Types
import           Data.PEM

toPEM :: RSA.PrivateKey -> PEM
toPEM privKey =
  PEM "RSA PRIVATE KEY" [] bits
  where
    bits = encodeASN1' DER keyASN1
    pubKey = RSA.private_pub privKey
    keyASN1 =
      Start Sequence :
      IntVal 0 :
      IntVal (RSA.public_n pubKey) :
      IntVal (RSA.public_e pubKey) :
      IntVal (RSA.private_d privKey) :
      IntVal (RSA.private_p privKey) :
      IntVal (RSA.private_q privKey) :
      IntVal (RSA.private_dP privKey) :
      IntVal (RSA.private_dQ privKey) :
      IntVal (RSA.private_qinv privKey) :
      End Sequence : []

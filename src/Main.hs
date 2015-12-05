{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Conduit
import Network.HTTP.Client (defaultManagerSettings)

import qualified Data.Conduit as C
import Data.Conduit.Binary (sinkFile)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  request <- liftIO $ parseUrl "http://coreos:2375/containers/ubuntu_tty/archive?path=/"
  runResourceT $ do
    response <- http request manager
    responseBody response C.$$+- sinkFile "/tmp/docker.dump"

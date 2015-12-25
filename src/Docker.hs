module Docker (
    startDocker
) where

import System.Process
import System.FilePath

startDocker :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
startDocker dockerBinPath keyFilePath certFilePath caFilePath = do
    putStrLn "Checking docker version:"
    callCommand "docker -v"
    --let opts = getDockerStartOpt(dockerBinPath, keyFilePath, certFilePath, caFilePath)
    --putStrLn "Starting docker daemon:"
    --putStrLn "Docker daemon has been started"
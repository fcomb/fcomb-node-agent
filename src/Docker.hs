module Docker (
    startDocker
) where

import System.Process
import System.FilePath
import Text.Printf

startDocker :: FilePath -> FilePath -> FilePath -> FilePath -> String -> IO ProcessHandle
startDocker dockerBinPath keyFilePath certFilePath caFilePath dockerHost = do
    putStrLn "Checking docker version:"
    callCommand "docker -v"

    let daemonOpt = " daemon"
        bindOpt = " -H " ++ dockerHost
        certOpt = " --tlscert " ++ certFilePath ++
                  " --tlskey " ++ keyFilePath ++
                  " --tlscacert " ++ caFilePath ++
                  " --tlsverify"
        opts = daemonOpt ++ bindOpt ++ certOpt

    putStrLn $ "Running docker with following options: " ++ opts
    spawnCommand $ dockerBinPath ++ opts

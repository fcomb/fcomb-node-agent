module Docker (
    startDocker
) where

import System.Process


startDocker :: FilePath -> FilePath -> FilePath -> FilePath -> String -> String -> IO ProcessHandle
startDocker dockerBinPath keyFilePath certFilePath caFilePath dockerHost dockerSocket = do
    putStrLn "Checking docker version:"
    callCommand $ dockerBinPath ++ " -v"

    let daemonOpt = " daemon"
        bindOpt = " -H " ++ dockerHost ++ " -H " ++ dockerSocket
        certOpt = " --tlscert " ++ certFilePath ++
                  " --tlskey " ++ keyFilePath ++
                  " --tlscacert " ++ caFilePath ++
                  " --tlsverify"
        opts = daemonOpt ++ bindOpt ++ certOpt

    putStrLn $ "Running docker: " ++ dockerBinPath ++ opts
    spawnCommand $ dockerBinPath ++ opts


getDockerVersion :: FilePath -> IO String
getDockerVersion dockerBinPath =
    readCreateProcess (shell dockerBinPath) ""

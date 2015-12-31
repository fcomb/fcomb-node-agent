module Agent (
    startAgent
) where

import Http
import Certs
import Config
import Globals
import Docker
import System.Environment
import System.Directory
import System.Posix.Files
import System.Process
import Control.Concurrent
import Control.Exception
import Data.Typeable

data AgentException = AgentException String
    deriving (Show, Typeable)

instance Exception AgentException


startAgent :: IO ()
startAgent = do
    putStrLn $ "Running fcomb agent version " ++ agentVersion

    doesDirectoryExist fcombHome >>= \exists -> if exists
        then return ()
        else do
            putStrLn $ "Creating directory for fcomb home " ++ fcombHome
            createDirectoryIfMissing True fcombHome

    doesDirectoryExist dockerDir >>= \exists -> if exists
        then return ()
        else do
            putStrLn $ "Creating directory for docker home " ++ dockerDir
            createDirectoryIfMissing True dockerDir

    doesFileExist dockerNewBinPath >>= \exists -> if exists
        then do
            putStrLn "Updating docker binary"
            copyFile dockerNewBinPath dockerBinPath
            removeFile dockerNewBinPath
        else return ()

    doesFileExist dockerBinPath >>= \exists -> if exists
        then return ()
        else do
            putStrLn "Downloading docker binary..."
            download dockerBinaryURL dockerBinPath

            p <- getPermissions dockerBinPath
            setPermissions dockerBinPath (p {executable = True})

            putStrLn $ "Creating symbolic link to docker at " ++ dockerSymbolicLink
            doesFileExist dockerSymbolicLink >>= \exists -> if exists
                then removeLink dockerSymbolicLink
                else return ()

            createSymbolicLink dockerBinPath dockerSymbolicLink

-- shortcut to the registration procedure
    putStrLn $ "Loading config from " ++ configFilePath
    confOpt <- loadConf
    conf <- case confOpt of
                Nothing -> do
                    (nodeId, nodeToken) <- registerAndSaveToken
                    putStrLn $ "Update config " ++ configFilePath
                    let conf' = Configuration nodeId nodeToken
                    saveConf conf'
                    return conf'
                Just conf' -> do
                    putStrLn $ "Found registered node " ++ (nodeId conf')
                    return conf'

    putStrLn "Checking docker version:"
    getDockerVersion dockerBinPath >>= \version ->
        putStrLn version

    putStrLn "Initializing docker daemon"
    dockerHandle <- startDocker dockerSymbolicLink keyFilePath certFilePath caFilePath dockerHost dockerSocket

    putStrLn "Docker daemon has been started. Entering maintenance loop"
    maintenanceLoop dockerHandle 0

    putStrLn "Over"

    where
        maintenanceLoop :: ProcessHandle -> Int -> IO ()
        maintenanceLoop dockerHandle respawns = do
            threadDelay $ fromInteger heartBeatInterval
            --updateDocker dockerBinPath dockerNewBinPath

            getProcessExitCode dockerHandle >>= \maybeExitCode -> case maybeExitCode of
                Just code -> do
                    if respawns > 3
                        then
                            throw $ AgentException $ "terminating after " ++ (show respawns) ++ " respawns"
                        else
                            return ()

                    putStrLn "Respawning docker daemon"
                    newHandle <- startDocker dockerSymbolicLink keyFilePath certFilePath caFilePath dockerHost dockerSocket
                    maintenanceLoop newHandle (respawns + 1)
                _ ->
                    maintenanceLoop dockerHandle respawns


registerAndSaveToken :: IO (String, String)
registerAndSaveToken = do
    args <- getArgs
    token <- case args of
       a1:as | not (null a1) -> do
           putStrLn $ "Provided token: " ++ a1
           return a1
       _ ->
           throw $ AgentException "Token is empty! Provide token as the first argument to the agent"

    putStrLn $ "Removing old certificates: " ++ keyFilePath ++ ", " ++ certFilePath ++ ", " ++ caFilePath
    doesFileExist keyFilePath >>= \exists -> if exists
       then removeFile keyFilePath
       else return ()

    doesFileExist certFilePath >>= \exists -> if exists
       then removeFile certFilePath
       else return ()

    doesFileExist caFilePath >>= \exists -> if exists
       then removeFile caFilePath
       else return ()

    cert <- createCerts keyFilePath
    (nodeId, nodeToken) <- joinNode fcombHost nodesEndpoint token cert caFilePath certFilePath

    -- saveConf (conf {nodeId = show nodeId})

    return (nodeId, nodeToken)

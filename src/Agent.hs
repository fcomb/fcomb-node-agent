module Agent (
    startAgent,
    AgentException
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
            createDirectory fcombHome

    doesDirectoryExist dockerDir >>= \exists -> if exists
        then return ()
        else do
            putStrLn $ "Creating directory for docker home " ++ dockerDir
            createDirectory dockerDir

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

    doesFileExist configFilePath >>= \exists -> if exists
        then return ()
        else do
            putStrLn $ "Config not found, writing defaults to " ++ configFilePath
            saveConf $ Configuration defaultDockerHost defaultFcombHost ""

-- shortcut to the registration procedure
    putStrLn $ "Loading config from " ++ configFilePath
    loadConf >>= \conf -> if null (nodeToken conf)
        then registerAndSaveToken
        else return ()

    putStrLn "Checking docker version:"
    getDockerVersion dockerBinPath >>= \version ->
        putStrLn version

    putStrLn "Initializing docker daemon"
    dockerHandle <- startDocker dockerSymbolicLink keyFilePath certFilePath caFilePath defaultDockerHost defaultDockerSocket

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
                    newHandle <- startDocker dockerSymbolicLink keyFilePath certFilePath caFilePath defaultDockerHost defaultDockerSocket
                    maintenanceLoop newHandle (respawns + 1)
                _ -> return ()

            maintenanceLoop dockerHandle respawns


registerAndSaveToken :: IO ()
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
    nodeToken <- register defaultFcombHost regEndpoint token cert caFilePath certFilePath

    loadConf >>= \conf ->
        saveConf (conf {nodeToken = nodeToken})

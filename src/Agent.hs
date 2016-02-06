{-# LANGUAGE LambdaCase #-}

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
    conf <- loadConf >>= \case
        Just conf | null (agentToken conf) ->
            throw $ AgentException $ "Config exists at " ++ configFilePath ++ " but missing a token. Please use the 'install' command with a new token"
        Just conf | null (nodeId conf) || null (nodeToken conf) -> do
            let token = agentToken conf
            putStrLn $ "Registering node for agent token " ++ token
            (nodeId, nodeToken) <- registerAgent token
            let conf' = Configuration nodeId nodeToken token
            saveConf conf'
            return conf'
        Just conf -> do
            putStrLn $ "Found registered node " ++ (nodeId conf)
            return conf
        _ ->
            throw $ AgentException $ "Config not found at " ++ configFilePath ++ " Please use the 'install' command with your agent token"

    prepareHomes
    prepareDockerBinaries

    putStrLn "Checking docker version:"
    getDockerVersion dockerBinPath >>= putStrLn

    putStrLn "Initializing docker daemon"
    dockerHandle <- startDocker dockerSymbolicLink keyFilePath certFilePath caFilePath dockerHost dockerSocket
    putStrLn "Docker daemon has been started"

    regStatus <- registerNode (nodeId conf) (nodeToken conf)

    case regStatus of
        Right _ -> do
            putStrLn "Registration successful. Entering docker maintenance loop"
            maintenanceLoop dockerHandle 0
        Left message -> do
            putStrLn $ "Failed to register node: " ++ message
            putStrLn "Killing docker instance"
            terminationLoop dockerHandle 0

    putStrLn "Over"

    where
        maintenanceLoop :: ProcessHandle -> Int -> IO ()
        maintenanceLoop dockerHandle respawns = do
            threadDelay $ fromInteger heartBeatInterval
            --updateDocker dockerBinPath dockerNewBinPath

            getProcessExitCode dockerHandle >>= \case
                Just exitCode -> do
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

        terminationLoop :: ProcessHandle -> Int -> IO ()
        terminationLoop dockerHandle attempts = do
            threadDelay $ fromInteger heartBeatInterval

            getProcessExitCode dockerHandle >>= \case
                Just exitCode -> putStrLn $ "Docker successfully exited with code: " ++ (show exitCode)
                _ -> do
                    if attempts > 3
                        then
                            throw $ AgentException $ "Failed to kill docker after " ++ (show attempts) ++ " attempts"
                        else
                            return ()

                    putStrLn "Sending sigterm to docker daemon"
                    terminateProcess dockerHandle
                    terminationLoop dockerHandle (attempts + 1)


prepareHomes :: IO ()
prepareHomes = do
    doesDirectoryExist fcombHome >>= \case
        False -> do
            putStrLn $ "Creating directory for fcomb home " ++ fcombHome
            createDirectoryIfMissing True fcombHome
        _ -> return ()

    doesDirectoryExist dockerHome >>= \case
        False -> do
            putStrLn $ "Creating directory for docker home " ++ dockerHome
            createDirectoryIfMissing True dockerHome
        _ -> return ()


prepareDockerBinaries :: IO ()
prepareDockerBinaries = do
    doesFileExist dockerNewBinPath >>= \case
        True -> do
            putStrLn "Updating docker binary"
            copyFile dockerNewBinPath dockerBinPath
            removeFile dockerNewBinPath
        _ -> return ()

    doesFileExist dockerBinPath >>= \case
        False -> do
            download dockerBinaryURL dockerBinPath

            p <- getPermissions dockerBinPath
            setPermissions dockerBinPath (p {executable = True})

            putStrLn $ "Creating symbolic link to docker at " ++ dockerSymbolicLink
            doesFileExist dockerSymbolicLink >>= \case
                True -> removeLink dockerSymbolicLink
                _ -> return ()

            createSymbolicLink dockerBinPath dockerSymbolicLink
        _ -> return ()


registerAgent :: String -> IO (String, String)
registerAgent agentToken = do
    putStrLn $ "Removing old certificates: " ++ keyFilePath ++ ", " ++ certFilePath ++ ", " ++ caFilePath
    doesFileExist keyFilePath >>= \case
        True -> removeFile keyFilePath
        _ -> return ()

    doesFileExist certFilePath >>= \case
        True -> removeFile certFilePath
        _ -> return ()

    doesFileExist caFilePath >>= \case
        True -> removeFile caFilePath
        _ -> return ()

    cert <- createCerts keyFilePath
    (nodeId, nodeToken) <- joinNode agentToken cert

    return (nodeId, nodeToken)

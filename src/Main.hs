module Main where

import Http
import Certs
import Config
import Globals
import Docker
import System.Environment
import System.Exit
import System.Directory
import System.FilePath
import System.Posix.Files
import System.Posix.Process
import System.Process
import GHC.IO.Handle
import Control.Concurrent

main :: IO ()
main = do
    putStrLn $ "Running fcomb agent version " ++ agentVersion

    args <- getArgs

    token <- case args of
        a1:as | not (null a1) -> do
            putStrLn $ "Provided token: " ++ a1
            return (a1)
        _ -> do
            putStrLn "Token is empty! Provide token as the first argument to the agent"
            exitFailure

    let dockerBinPath = combine dockerDir dockerBinaryName
        --dockerNewBinPath = combine dockerDir dockerNewBinaryName
        --dockerNewBinSigPath = combine dockerDir dockerNewBinarySigName
        --configFilePath = combine fcombHome configFileName
        keyFilePath = combine fcombHome keyFileName
        certFilePath = combine fcombHome certFileName
        caFilePath = combine fcombHome cAFileName

    putStrLn $ "Creating directories for fcomb and docker (if missing): " ++ fcombHome ++ ", " ++ dockerDir
    createDirectoryIfMissing True fcombHome
    createDirectoryIfMissing True dockerDir

    fileExist dockerBinPath >>= \exists -> if exists
        then
            return ()
        else do
            putStrLn "Downloading docker binary..."
            download dockerBinaryURL dockerBinPath

            p <- getPermissions dockerBinPath
            setPermissions dockerBinPath (p {executable = True})

            fileExist dockerSymbolicLink >>= \exists -> if exists
                then removeLink dockerSymbolicLink
                else return ()

            createSymbolicLink dockerBinPath dockerSymbolicLink


    putStrLn $ "Removing old certificates: " ++ keyFilePath ++ ", " ++ certFilePath ++ ", " ++ caFilePath
    fileExist keyFilePath >>= \exists -> if exists
        then removeFile keyFilePath
        else return ()

    fileExist certFilePath >>= \exists -> if exists
        then removeFile certFilePath
        else return ()

    fileExist caFilePath >>= \exists -> if exists
        then removeFile caFilePath
        else return ()

    --createPidFile fcombPidFile

--    putStrLn $ "Checking if config file exists " ++ configFilePath
--    fileExist configFilePath >>= \exists -> if exists
--        then
--            return ()
--        else do
--            putStrLn "Writing default settings to the config file"
--            saveConf configFilePath defaultConf
--
--    putStrLn $ "Loading config from " ++ configFilePath
--    conf <- loadConf configFilePath

    cert <- createCerts keyFilePath
    register token cert caFilePath certFilePath

    putStrLn "Initializing docker daemon"
    dockerHandle <- startDocker dockerSymbolicLink keyFilePath certFilePath caFilePath defaultDockerHost
    putStrLn "Docker daemon has been started"

--    putStrLn "Docker server started. Entering maintenance loop"
--    maintenanceLoop dockerHandle

    putStrLn "Over"


--maintenanceLoop :: ProcessHandle -> IO ()
--maintenanceLoop dockerHandle = do
--    threadDelay $ fromInteger heartBeatInterval
--    updateDocker dockerBinPath dockerNewBinPath dockerNewBinSigPath keyFilePath certFilePath caFilePath handle
--
--    hIsClosed dockerHandle >>= \isClosed -> if isClosed
--        then do
--            threadDelay $ fromInteger $ heartBeatInterval * 1000 * 1000
--            if not scheduleToTerminateDocker
--                then do
--                    putStrLn "Respawning docker daemon"
--                    startDocker
--                else return ()
--        else return ()
--    maintenanceLoop


checkPidFile :: FilePath -> IO ()
checkPidFile pidFile =
    fileExist pidFile >>= \exists -> if exists
        then do
            pid <- readFile pidFile
            let procFile = combine "/proc" pid
            fileExist procFile >>= \exists -> if exists
                then
                    putStrLn $ "Found pid file, make sure that fcomb-agent is not running or remove " ++ pidFile
                else do
                    return ()
        else
            return ()


createPidFile :: FilePath -> IO ()
createPidFile pidFile =
    getProcessID >>= \pid -> do
        checkPidFile pidFile
        writeFile pidFile (show pid)
        putStrLn $ "Created pid file( " ++ pidFile ++ "): " ++ (show pid)

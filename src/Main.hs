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
import Control.Concurrent

main :: IO ()
main = do
    args <- getArgs

    token <- case args of
        a1:as | not (null a1) -> do
            putStrLn $ "Got agent token: " ++ a1
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

    -- create directories with parents and default permissions of 755
    createDirectoryIfMissing True fcombHome
    createDirectoryIfMissing True dockerDir

    -- removing old certificates
    fileExist keyFilePath >>= \exists -> if exists
        then removeFile keyFilePath
        else return ()

    fileExist certFilePath >>= \exists -> if exists
        then removeFile certFilePath
        else return ()

    fileExist caFilePath >>= \exists -> if exists
        then removeFile caFilePath
        else return ()


    putStrLn $ "Running node-agent: version " ++ agentVersion
    --createPidFile fcombPidFile

--    putStrLn "Checking if config file exists"
--    fileExist configFilePath >>= \exists -> if exists
--        then
--            return ()
--        else do
--            putStrLn "Writing a config file with default settings"
--            saveConf configFilePath defaultConf
--
--    putStrLn "Loading Configuration file"
--    conf <- loadConf configFilePath

    cert <- createCerts keyFilePath
    putStrLn $ "Generated certificate: \n" ++ cert
    let regUrl = defaultFcombHost ++ regEndpoint
    putStrLn $ "Registering with fcomb: " ++ regUrl
    register regUrl token cert caFilePath certFilePath

    -- download docker binary if missing
    fileExist dockerBinPath >>= \exists -> if exists
        then
            return ()
        else do
            putStrLn "Downloading docker binary..."
            download dockerBinaryURL dockerBinPath
            --removeLink dockerSymbolicLink
            --createSymbolicLink path dockerSymbolicLink

    putStrLn "Initializing docker daemon"
    --dockerHandle <- startDocker dockerBinPath keyFilePath certFilePath caFilePath defaultDockerHost
    putStrLn "Docker daemon has been started"
    --putStrLn "Verifying the registration with Fcomb"
    --verifyRegistration regUrl

    --putStrLn "Docker server started. Entering maintenance loop"
    --maintenanceLoop

    putStrLn "Over"


--maintenanceLoop = do
--    threadDelay heartBeatInterval
--    updateDocker dockerBinPath dockerNewBinPath dockerNewBinSigPath keyFilePath certFilePath caFilePath handle
--
--    if dockerProcess == nil
--        threadDelay heartBeatInterval * 1000 * 1000
--        if dockerProcess == nil ScheduleToTerminateDocker == false
--            putStrLn "Respawning docker daemon"
--            startDocker
--
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

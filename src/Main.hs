module Main where

import Http
import Certs
import Config
import Globals
import Docker
import System.Directory
import System.FilePath
import System.Posix.Files
import System.Posix.Process
import Control.Concurrent

main :: IO ()
main = do
    let dockerBinPath = combine dockerDir dockerBinaryName
        dockerNewBinPath = combine dockerDir dockerNewBinaryName
        dockerNewBinSigPath = combine dockerDir dockerNewBinarySigName
        configFilePath = combine fcombHome configFileName
        keyFilePath = combine fcombHome keyFileName
        certFilePath = combine fcombHome certFileName
        caFilePath = combine fcombHome cAFileName

    -- create directories with parents and default permissions of 755
    createDirectoryIfMissing True fcombHome
    createDirectoryIfMissing True dockerDir
    createDirectoryIfMissing True logDir

    putStrLn "Running node-agent: version" + agentVersion
    createPidFile fcombPidFile

	putStrLn "Checking if config file exists"
	fileExist configFilePath >>= \exists -> if exists
        then
            return ()
        else do
            -- todo: weird logic
            let conf = loadDefaultConf
            saveConf configFilePath conf

	putStrLn "Loading Configuration file"
	conf <- loadConf configFilePath

	-- todo: update config with args
    -- setConfigFile configFilePath

    if null (fcombToken conf)
        then do
            putStrLn "Token is empty!"
            removeFile fcombPidFile
        else
            return ()

    let regUrl = combine (fcombHost conf) regEndpoint
    if null (fcombUUID conf)
        then do
            removeFile keyFilePath
            removeFile certFilePath
            removeFile caFilePath
            putStrLn "Registering in Fcomb via POST: " ++ regUrl
            postToFcomb regUrl caFilePath configFilePath
        else
            return ()

    --createCerts keyFilePath certFilePath (certCommonName conf)

    --putStrLn "Registering in Fcomb via PATCH: " ++ regUrl ++ (fcombUUID conf)
    --patchToFcomb regUrl caFilePath certFilePath configFilePath

    -- saving conf after patching
    saveConf configFilePath conf

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
    startDocker dockerBinPath keyFilePath certFilePath caFilePath

   -- putStrLn "Verifying the registration with Fcomb"
    --verifyRegistration regUrl

    --putStrLn "Docker server started. Entering maintenance loop"
    --maintenanceLoop

    putStrLn "Over"


--maintenanceLoop = do
--    threadDelay heartBeatInterval
--    updateDocker dockerBinPath dockerNewBinPath dockerNewBinSigPath keyFilePath certFilePath caFilePath
--
--    if dockerProcess == nil
--        threadDelay heartBeatInterval * 1000 * 1000
--        if dockerProcess == nil ScheduleToTerminateDocker == false
--            putStrLn "Respawning docker daemon"
--            startDocker
--
--    maintenanceLoop


checkPidFile :: FilePath -> IO ()
checkPidFile pidFile = do
	fileExist pidFile >>= \exists -> if exists
        then do
            pid <- readFile pidFile
            let procFile = combine "/proc" pid
	        fileExist procFile >>= \exists -> if exists
                then
                    putStrLn "Found pid file, make sure that fcomb-agent is not running or remove " ++ pidFile
                else do
                    return ()
        else do
            return ()


createPidFile :: FilePath -> IO ()
createPidFile pidFile = do
    checkPidFile pidFile
    getProcessID >>= \pid -> do
        writeFile pidFile pid
        putStrLn "Created pid file( " ++ pidFIle ++ "): " + pid

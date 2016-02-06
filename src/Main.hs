{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment
import System.Exit
import System.Directory
import System.Posix.Process
import Control.Exception
import Agent
import Globals (fcombPidFile, agentVersion)
import Config


main :: IO ()
main = do
    putStrLn $ "Running fcomb agent version " ++ agentVersion

    getArgs >>= \case
        "install" : tokenArg : [] -> do
            putStrLn $ "Provided agent token: " ++ tokenArg
            saveConf $ Configuration "" "" tokenArg
            exitSuccess

        "start" : [] -> do
            createPidFile fcombPidFile
            catchAny startAgent $ \e -> do
                putStrLn $ "Got an exception: " ++ show e
                removeFile fcombPidFile
                putStrLn $ "Removed pid file(" ++ fcombPidFile ++ ")"
                exitFailure

        _ -> do
            putStrLn "Usage: "
            putStrLn "install [AgentToken]      | to prepare agent configuration with the given token"
            putStrLn "start                     | to start the agent"
            exitFailure


catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch


checkPidFile :: FilePath -> IO ()
checkPidFile pidFile =
    doesFileExist pidFile >>= \case
        True -> do
            pid <- readFile pidFile
            let procFile = "/proc/" ++ pid
            doesFileExist procFile >>= \exists -> if exists
                then
                    putStrLn $ "Found pid file, make sure that fcomb-agent is not running or remove " ++ pidFile
                else do
                    return ()
        _ -> return ()


createPidFile :: FilePath -> IO ()
createPidFile pidFile =
    getProcessID >>= \pid -> do
        checkPidFile pidFile
        writeFile pidFile (show pid)
        putStrLn $ "Created pid file(" ++ pidFile ++ "): " ++ (show pid)

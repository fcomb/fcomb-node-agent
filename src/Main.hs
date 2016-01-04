{-# LANGUAGE LambdaCase #-}

module Main where

import System.Exit
import System.Directory
import System.Posix.Process
import Control.Exception
import Agent
import Globals (fcombPidFile)


main :: IO ()
main = do
    createPidFile fcombPidFile
    catchAny startAgent $ \e -> do
        putStrLn $ "Got an exception: " ++ show e
        removeFile fcombPidFile
        putStrLn $ "Removed pid file(" ++ fcombPidFile ++ ")"
        exitFailure
    exitSuccess


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

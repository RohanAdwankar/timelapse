{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Process (readCreateProcess, readCreateProcessWithExitCode, shell, CreateProcess(cwd))
import System.Exit (ExitCode(..))
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Control.Monad (forM_, unless, when)
import Control.Exception (bracket, catch, SomeException)
import Control.Concurrent (threadDelay)
import Data.List (sort)
import qualified Data.ByteString.Lazy as B
import Data.Aeson ((.:), decode, Object)
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    putStrLn "Enter the GitHub repository URL:"
    repoUrl <- getLine
    putStrLn "Enter the number of commits to process:"
    n <- readLn :: IO Int
    
    withSystemTempDirectory "timelapse" $ \tmpDir -> do
        putStrLn $ "Cloning repository to " ++ tmpDir
        cloneRepo tmpDir repoUrl
        commits <- getCommits tmpDir
        let selectedCommits = selectCommits n commits
        putStrLn "Selected commits, their package.json scripts, and dev script output:"
        forM_ selectedCommits $ \commit -> do
            putStrLn $ "Commit: " ++ commit
            scripts <- getPackageJsonScripts tmpDir commit
            case scripts of
                Just s  -> do
                    putStrLn $ "Scripts:\n" ++ s
                    runDevScript tmpDir
                Nothing -> putStrLn "No package.json or no scripts found for this commit."
            putStrLn ""  

cloneRepo :: FilePath -> String -> IO ()
cloneRepo dir url = do
    putStrLn $ "Cloning " ++ url ++ " into " ++ dir
    _ <- readCreateProcess (shell $ "git clone " ++ url ++ " " ++ dir) ""
    return ()

getCommits :: FilePath -> IO [String]
getCommits dir = do
    output <- readCreateProcess (shell $ "cd " ++ dir ++ " && git log --format=%H --reverse") ""
    return $ lines output

selectCommits :: Int -> [String] -> [String]
selectCommits n commits
    | n <= 0 = []
    | n == 1 = [head commits]
    | otherwise = 
        let indices = evenlySpacedIndices (length commits) n
        in map (commits !!) indices

evenlySpacedIndices :: Int -> Int -> [Int]
evenlySpacedIndices total n
    | n <= 1 = [0]
    | n >= total = [0..total-1]
    | otherwise = 
        let step = (total - 1) `div` (n - 1)
            baseIndices = take (n-1) [0, step..]
        in baseIndices ++ [total - 1]

getPackageJsonScripts :: FilePath -> String -> IO (Maybe String)
getPackageJsonScripts dir commit = do
    _ <- readCreateProcess (shell $ "cd " ++ dir ++ " && git checkout " ++ commit) ""
    
    let packageJsonPath = dir </> "package.json"
    exists <- doesFileExist packageJsonPath
    
    if exists
        then do
            content <- B.readFile packageJsonPath
            case decode content of
                Just obj -> return $ formatScripts $ parseMaybe (.: "scripts") obj
                Nothing  -> return $ Just "Error: Could not parse package.json"
        else return Nothing

formatScripts :: Maybe Object -> Maybe String
formatScripts Nothing = Nothing
formatScripts (Just scripts) = Just $ unlines $ map formatScript $ KM.toList scripts
  where
    formatScript (name, value) = "  " ++ show name ++ ": " ++ show value

runDevScript :: FilePath -> IO ()
runDevScript dir = do
    putStrLn "Running 'npm install'..."
    installResult <- runCommandWithOutput dir "npm install" `catch` handleException
    case installResult of
        Just output -> do
            putStrLn "npm install output:"
            putStrLn output
            putStrLn "Running 'npm run dev'..."
            devResult <- runCommandWithOutput dir "npm run dev" `catch` handleException
            case devResult of
                Just devOutput -> do
                    putStrLn "Initial server output:"
                    putStrLn $ unlines $ take 10 $ lines devOutput
                Nothing -> putStrLn "Failed to run 'npm run dev'"
        Nothing -> putStrLn "Failed to run 'npm install'"

runCommandWithOutput :: FilePath -> String -> IO (Maybe String)
runCommandWithOutput dir cmd = do
    (exitCode, stdout, stderr) <- readCreateProcessWithExitCode (shell cmd) { cwd = Just dir } ""
    case exitCode of
        ExitSuccess -> return $ Just stdout
        ExitFailure _ -> do
            putStrLn $ "Command failed: " ++ cmd
            putStrLn "Error output:"
            putStrLn stderr
            return Nothing

handleException :: SomeException -> IO (Maybe a)
handleException e = do
    putStrLn $ "An exception occurred: " ++ show e
    return Nothing
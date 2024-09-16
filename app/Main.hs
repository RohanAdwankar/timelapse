module Main (main) where

import System.Process (readCreateProcess, shell)
import System.IO.Temp (withSystemTempDirectory)
import Control.Monad (forM_)

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
        putStrLn "Selected commits:"
        forM_ selectedCommits putStrLn

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
    | n == 2 = [head commits, last commits]
    | otherwise = 
        let middleCommits = selectMiddleCommits (n - 2) (tail $ init commits)
        in head commits : middleCommits ++ [last commits]

selectMiddleCommits :: Int -> [String] -> [String]
selectMiddleCommits n commits
    | n <= 0 = []
    | null commits = []
    | otherwise = 
        let mid = length commits `div` 2
            (left, m:right) = splitAt mid commits
        in m : selectMiddleCommits (n - 1) (left ++ right)
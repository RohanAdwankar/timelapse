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
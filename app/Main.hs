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
    output <- readCreateProcess (shell $ "cd " ++ dir ++ " && git log --format=%H") ""
    return $ lines output

selectCommits :: Int -> [String] -> [String]
selectCommits n commits = take n $ binaryTreeTraversal commits

binaryTreeTraversal :: [a] -> [a]
binaryTreeTraversal [] = []
binaryTreeTraversal [x] = [x]
binaryTreeTraversal xs = 
    let mid = length xs `div` 2
        (left, m:right) = splitAt mid xs
    in m : binaryTreeTraversal left ++ binaryTreeTraversal right
module HsFp1.FilesRemoving where
    
import System.Directory
import Data.List

main :: IO ()
main = do 
    putStr "Substring: "
    line <- getLine
    if line == "" then return () else do
        dirContent <- getDirectoryContents "."
        let toRemove = filter (isSubsequenceOf line) dirContent
        sequence_ (fmap rmFile toRemove)
    
rmFile :: FilePath ->  IO ()
rmFile p = do 
    putStrLn $ "Removing file: " ++ p
    removeFile p
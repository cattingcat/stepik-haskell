module HsFp1.BasicIoDialog where

main :: IO ()
main = do 
    name <- askName
    putStrLn ("Hi, " ++ name ++ "!") where
        askName = do 
            putStrLn "What is your name?"
            putStr "Name: "
            line <- getLine
            if line == "" then askName else return line
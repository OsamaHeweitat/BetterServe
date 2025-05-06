module Main where
    import Eval
    import System.Directory (getCurrentDirectory)

    main :: IO ()
    main = do
        cwd <- getCurrentDirectory
        putStrLn "Enter the filenfame of the SQL-like script:"
        filename <- getLine
        result <- eval filename
        putStr ""
        --putStrLn "Result:"
        --putStrLn result
        -- putStrLn "Done."
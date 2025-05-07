module Main where
    import Eval
    import System.Directory (getCurrentDirectory)
    import System.Environment (getArgs)

    main :: IO ()
    main = do
        args <- getArgs
        let filename = if null args then error "Please provide a filename as an argument." else head args
        result <- eval filename
        return () --putStr ""
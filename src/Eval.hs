module Eval where
import Grammar
import qualified Data.Map as Map
import System.IO
import Data.List.Split (splitOn)

type Env = Map.Map String Value
type Row = [String]
type Table = (String, [[String]])

data Value
    = TableVal [[String]]
    | StrVal String
    | IntVal Int
    | BoolVal Bool
    deriving (Show, Eq)

-- Evaluate Tables AST into actual Table (name, rows)
evalTables :: Tables -> IO Table
evalTables (LoadTable filename) = do
    contents <- readFile filename
    let rows = map (splitOn ",") (lines contents)
    return (filename, rows)

-- TableOp, TableJoin, TableConc

-- Define evalExp to convert the Table into a Value if needed
evalExp :: Env -> Exp -> IO Value
evalExp env (SelectColumns cols) = do
    let table = getTableFromEnv env
    let result = selectColumns cols table
    return $ TableVal result

getTableFromEnv :: Env -> [[String]]
getTableFromEnv env = case Map.lookup "table" env of
    Just (TableVal t) -> t
    _ -> error "Table not found in environment"

-- Select specific columns
selectColumns :: [Int] -> [[String]] -> [[String]]
selectColumns cols table = map (\row -> [row !! col | col <- cols]) table

-- Row filtering helper
filterRows :: (Row -> Bool) -> [[String]] -> [[String]]
filterRows cond table = filter cond table

-- Project both column selection and filtering
project :: [Int] -> (Row -> Bool) -> [[String]] -> [[String]]
project cols cond table = selectColumns cols (filterRows cond table)

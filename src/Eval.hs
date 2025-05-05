module Eval where
import Grammar

type Table = (String, [[String]]) -- (TableName, Rows)
type ColumnType = (Int, [String]) -- (ColumnIndex, ColumnValues)

data Value
    = TableVal [[String]]
    | StrVal String
    | IntVal Int
    | BoolVal Bool
    deriving (Show, Eq)

evalStmt :: Statement -> IO [String]
evalStmt (SelectStmt selection tabs optionals) = do
    tables <- evalTables tabs
    result <- evalSelection selection tables
    result <- evalOptionals optionals result

evalTables :: Tables -> [Table]
evalTables (LoadTable filename) = do
    contents <- readFile filename
    let rows = map (splitOn ",") (lines contents)
    return [(filename, rows)]

evalSelection :: Selection -> [Table] -> [Column]
evalSelection SelectAll tables = tables
evalSelection (SelectColumns columns) tables = map (evalColumns columns) tables

evalColumns :: [Column] -> [Table] -> [ColumnType]
evalColumns columns tables = do
    let columnIndex = map (getColumnIndex columns) tables
    let columnValues = map (getColumnValues columnIndex) tables
    return (columnIndex, columnValues)

evalOptionals :: [Optional] -> [Column] -> [Table] -> [String]
evalOptionals 

readCSV :: String -> IO Table
readCSV filename = do
    content <- readFile filename
    let filenameWithoutExt = takeWhile (/= '.') filename
    let rows = lines content
    let header = words (head rows)
    let dataRows = map words (tail rows)
    return (filenameWithoutExt, header : dataRows)
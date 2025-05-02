module Eval where
import Grammar

type Table = (String, [[String]]) -- (TableName, Rows)
type Column = (Int, [String]) -- (ColumnIndex, ColumnValues)

evalStmt :: Statement -> IO [String]
evalStmt (SelectStmt selection tabs optionals) = do
    tables <- evalTables tabs
    result <- evalSelection selection tables
    result <- evalOptionals optionals result

evalTables :: [String] -> IO [Table]

evalSelection :: Selection -> [Table] -> [Column]
evalSelection SelectAll tables = tables
evalSelection (SelectColumns columns) tables = map (evalColumns columns) tables

readCSV :: String -> IO Table
readCSV filename = do
    content <- readFile filename
    let filenameWithoutExt = takeWhile (/= '.') filename
    let rows = lines content
    let header = words (head rows)
    let dataRows = map words (tail rows)
    return (filenameWithoutExt, header : dataRows)
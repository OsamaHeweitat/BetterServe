module Eval where
import Grammar
import Tokens
import qualified Data.Map as Map
import System.IO
import Data.List.Split (splitOn)
import Data.List
import Data.Maybe (fromMaybe)
import Data.Char (ord)
import Control.Applicative
import Text.Parsec
import Data.Array (Ix(index))

type Table = (Int, [[String]]) -- (Index, Rows)
type ColumnType = (Int, [String]) -- (ColumnIndex, ColumnValues)

-- Evaluates the program with the given filename
eval :: String -> IO String
eval filename = do
    contents <- readFile filename
    let program = Program (Grammar.parse (alexScanTokens contents))
    evalProgram program "" False

-- Evaluates each line (separated by ";") in the program
-- The bool is just whether to add new line or not (whether its a multi-program or not)
evalProgram :: Program -> String -> Bool -> IO String
evalProgram (Program []) result _ = return result
evalProgram (Program (statement:rest)) result newline = do
    stmtLines <- evalStmt statement
    let formatted = unlines (filter (not . null) (map (toCSVFormat . snd) stmtLines))
    evalProgram (Program rest) (result ++ formatted) True
    where
        toCSVFormat :: [String] -> String
        toCSVFormat = intercalate ","

-- Loads the requested tables
-- Selects the given columns
-- Evaluates any optional tokens
-- Either ENDs or OUTPUTs
evalStmt :: Statement -> IO [ColumnType]
evalStmt (SelectOpt selection tabs optionals end) = do
    tables <- evalTables 0 tabs
    result <- evalSelection selection tables
    evalEnd (evalOptionals optionals result tables) end
evalStmt (SelectStmt selection tabs end) = do -- Version without optionals
    tables <- evalTables 0 tabs
    selectionResults <- evalSelection selection tables
    evalEnd (return selectionResults) end
evalStmt (CommentStmt comment) = return []

-- Creates a list of all requested tables
evalTables :: Int -> [Tables] -> IO [Table]
evalTables _ [] = return []
evalTables tableIndex (table:rest) = do
    t <- evalTable tableIndex table
    ts <- evalTables (tableIndex + 1) rest
    return (t : ts)

-- Takes in either a single requested table, or calculate a table expression like UNION
evalTable :: Int -> Tables -> IO Table
evalTable tableIndex (LoadTable filename) = do
    contents <- readFile (filename ++ ".csv")
    let newContents = if "\n" `isSuffixOf` contents then contents ++ "\n" else contents
    let rows = lines newContents
    let arities = map (length . splitOn ",") rows
    if all (== head arities) arities
        then return ()
        else error "CSV file has inconsistent arity."
    let rows = map (splitOn ",") (lines contents)
    let cleanedRows = map (map (reverse . dropWhile (== ' ') . reverse . dropWhile (== ' '))) rows
    return (tableIndex, cleanedRows)
evalTable tableIndex (TableOp tables1 expr tables2) = do
    evalTables1 <- evalTables tableIndex tables1
    evalTables2 <- evalTables (tableIndex + length tables1) tables2
    let result
            | expr == Cartesian =
                (tableIndex, [row1 ++ row2 | (_, rows1) <- evalTables1, row1 <- rows1, (_, rows2) <- evalTables2, row2 <- rows2])
            | expr == Union =
                (tableIndex, nub (concatMap snd evalTables1 ++ concatMap snd evalTables2))
            | expr == Intersect =
                (tableIndex, [row1 | (_, rows1) <- evalTables1, row1 <- rows1, (_, rows2) <- evalTables2, row2 <- rows2, row1 == row2])
    return result

-- Concatenates 2 tables 
evalTable tableIndex (TableConc tables1 tables2) = do
    evalTables1 <- evalTables tableIndex tables1
    evalTables2 <- evalTables (tableIndex + length tables1) tables2
    let allRows1 = concatMap snd evalTables1
        allRows2 = concatMap snd evalTables2
    let arity1 = map length allRows1
        arity2 = map length allRows2
    if null arity1 || null arity2 || head arity1 == head arity2
        then return (tableIndex, allRows1 ++ allRows2)
        else error "Cannot concatenate tables with mismatched arity."

-- TableJoin for InnerJoin
evalTable tableIndex (TableJoin tables1 InnerJoin tables2 colIndex) = do
    table1 <- evalTable tableIndex (head tables1)
    table2 <- evalTable (tableIndex + 1) (head tables2)
    let (_, rows1) = table1
        (_, rows2) = table2
    let (idx1, idx2) = case colIndex of
            StringComp (SpecNumber i1 _) (SpecNumber i2 _) -> (i1, i2)
            _ -> error "Unsupported comparison type for InnerJoin. Must be SpecNumber == SpecNumber."
    let matchedRows = [r1 ++ r2 | r1 <- rows1, r2 <- rows2, safeIndex r1 idx1 == safeIndex r2 idx2]
    return (tableIndex, matchedRows)
  where
    safeIndex :: [a] -> Int -> a
    safeIndex xs i
      | i >= 0 && i < length xs = xs !! i
      | otherwise = error $ "Column index " ++ show i ++ " out of bounds in join."

-- TableJoin for LeftJoin 
evalTable tableIndex (TableJoin tables1 LJoin tables2 colIndex) = do
    table1 <- evalTable tableIndex (head tables1)
    table2 <- evalTable (tableIndex + 1) (head tables2)
    let (_, rows1) = table1
        (_, rows2) = table2
    let (idx1, idx2) = case colIndex of
            StringComp (SpecNumber i1 _) (SpecNumber i2 _) -> (i1, i2)
            _ -> error "Unsupported comparison type for LJoin. Must be SpecNumber == SpecNumber."
    let matched = [ (r1, r2) | r1 <- rows1, r2 <- rows2, safeIndex r1 idx1 == safeIndex r2 idx2 ]
        groupedMatches = groupBy (\(r1a, _) (r1b, _) -> r1a == r1b) (sortOn fst matched)
    -- Create rows for r1 that had at least one match in r2
    let joinedRows = [ r1 ++ r2 | (r1, r2) <- matched ]
    -- Find all r1s that didn’t match any r2
    let matchedR1s = nub [ r1 | (r1, _) <- matched ]
    let unmatchedR1s = [ r1 | r1 <- rows1, r1 `notElem` matchedR1s ]
    let r2Arity = if null rows2 then 0 else length (head rows2)
    let nullR2 = replicate r2Arity ""
    let resultRows = joinedRows ++ [ r1 ++ nullR2 | r1 <- unmatchedR1s ]
    return (tableIndex, resultRows)
  where
    safeIndex :: [a] -> Int -> a
    safeIndex xs i
      | i >= 0 && i < length xs = xs !! i
      | otherwise = error $ "Column index " ++ show i ++ " out of bounds in join."

-- TableJoin for RightJoin 
evalTable tableIndex (TableJoin tables1 RJoin tables2 colIndex) = do
    table1 <- evalTable tableIndex (head tables1)
    table2 <- evalTable (tableIndex + 1) (head tables2)
    let (_, rows1) = table1
        (_, rows2) = table2
    let (idx1, idx2) = case colIndex of
            StringComp (SpecNumber i1 _) (SpecNumber i2 _) -> (i1, i2)
            _ -> error "Unsupported comparison type for RightJoin. Must be SpecNumber == SpecNumber."
    let matchedRows =
            [r1 ++ r2 | r1 <- rows1, r2 <- rows2, safeIndex r1 idx1 == safeIndex r2 idx2]
        unmatchedRows =
            [ replicate (length (head rows1)) "" ++ r2
            | r2 <- rows2
            , not (any (\r1 -> safeIndex r1 idx1 == safeIndex r2 idx2) rows1)
            ]
    return (tableIndex, matchedRows ++ unmatchedRows)
  where
    safeIndex :: [a] -> Int -> a
    safeIndex xs i
      | i >= 0 && i < length xs = xs !! i
      | otherwise = error $ "Column index " ++ show i ++ " out of bounds in join."

-- TableJoin for FullJoin
evalTable tableIndex (TableJoin tables1 Join tables2 colIndex) = do
    table1 <- evalTable tableIndex (head tables1)
    table2 <- evalTable (tableIndex + 1) (head tables2)
    let (_, rows1) = table1
        (_, rows2) = table2
    let (idx1, idx2) = case colIndex of
            StringComp (SpecNumber i1 _) (SpecNumber i2 _) -> (i1, i2)
            _ -> error "Unsupported comparison type for FullJoin. Must be SpecNumber == SpecNumber."
    let matchedRows =
            [r1 ++ r2 | r1 <- rows1, r2 <- rows2, safeIndex r1 idx1 == safeIndex r2 idx2]
        unmatchedLeft =
            [ r1 ++ replicate (length (head rows2)) ""
            | r1 <- rows1
            , not (any (\r2 -> safeIndex r1 idx1 == safeIndex r2 idx2) rows2)
            ]
        unmatchedRight =
            [ replicate (length (head rows1)) "" ++ r2
            | r2 <- rows2
            , not (any (\r1 -> safeIndex r1 idx1 == safeIndex r2 idx2) rows1)
            ]
    return (tableIndex, matchedRows ++ unmatchedLeft ++ unmatchedRight)
  where
    safeIndex :: [a] -> Int -> a
    safeIndex xs i
      | i >= 0 && i < length xs = xs !! i
      | otherwise = error $ "Column index " ++ show i ++ " out of bounds in join."

-- If the program ends in OUTPUT, it prints the output
evalEnd :: IO [ColumnType] -> End -> IO [ColumnType]
evalEnd final End = final
evalEnd final Output = do
    printedLine <- final
    let output = toOutputForm (columnToRows printedLine)
    let outputTrimmed = reverse (dropWhile (== '\n') (reverse output))
    if not (null printedLine) then putStr (outputTrimmed) else putStr ""
    return printedLine

-- Evaluates whether columns or wildcard is selected
evalSelection :: Selection -> [Table] -> IO [ColumnType]
evalSelection SelectAll tables = do
    let tableArity = case tables of
            ((_, rows):_) -> case rows of
                (firstRow:_) -> length firstRow
                [] -> 0
            [] -> 0
    let this = map ColIndex [0..(tableArity - 1)]
    let strings = evalColumns this tables
    return strings
evalSelection (SelectColumns cols) tables = do
    let strings = evalColumns cols tables
    return (zip [0..] (map snd strings))

-- Returns the list of all requested columns
evalColumns :: [Grammar.Column] -> [Table] -> [ColumnType]
evalColumns [] _ = []
evalColumns (x:xs) tables = evalColumn x tables ++ evalColumns xs tables

-- Returns requested column from table
evalColumn :: Grammar.Column -> [Table] -> [ColumnType]
evalColumn (ColIndex x) tables = [getColumn x tables]
evalColumn (ColIndexTable x tabIndex) tables = [getColWithIndex x tabIndex tables]
evalColumn (IfStmt col1s boolExpr col2s) tables = [ (x, [ if evalBoolean boolExpr tables i
    then col1!!i else col2!!i | i <- [0..rows - 1] ]) | ((x, col1), (_, col2)) <- zip col1Vals col2Vals ]
    where
        col1Vals = evalColumns col1s tables
        col2Vals = evalColumns col2s tables
        rows = case col1Vals of
            (_, vals):_ -> length vals
            _ -> error "No value in column"
        col1 = concatMap snd col1Vals
        col2 = concatMap snd col2Vals

-- Gets column in each table selected (which is usually for a single table)
getColumn :: Int -> [Table] -> ColumnType
getColumn colIndex tables = (colIndex, concatMap (getColValues colIndex) tables)
    where
        getColValues :: Int -> Table -> [String]
        getColValues columnIndex (_, rows) = map (!! columnIndex) rows

-- Gets the column of a specific table (zero indexed)
-- 0.0 refers to Table 0 column 0
getColWithIndex :: Int -> Int -> [Table] -> ColumnType
getColWithIndex tabIndex colIndex tables = (colIndex, concatMap (getColValues colIndex) filteredTables)
    where
        filteredTables = filter (\(tblIndex, _) -> tblIndex == tabIndex) tables
        getColValues :: Int -> Table -> [String]
        getColValues columnIndex (_, rows) = map (!! columnIndex) rows

-- Takes in a boolean expression and returns equivalent bool value
evalBoolean :: Boolean -> [Table] -> Int -> Bool
evalBoolean (BoolExpr b1 BoolAND b2) t i = evalBoolean b1 t i && evalBoolean b2 t i
evalBoolean (BoolExpr b1 BoolOR b2) t i = evalBoolean b1 t i || evalBoolean b2 t i
evalBoolean (BoolExpr b1 BoolXOR b2) t i = evalBoolean b1 t i /= evalBoolean b2 t i
evalBoolean (BoolNOT b) t i = not (evalBoolean b t i)
evalBoolean BoolTrue _ _ = True
evalBoolean BoolFalse _ _ = False
evalBoolean (BoolComp comparison) tables row = evalBoolComp comparison tables row

-- Compares int or string values
evalBoolComp :: Comparison -> [Table] -> Int -> Bool
evalBoolComp (StringComp s1 s2) t row = evalString s1 t row == evalString s2 t row
evalBoolComp (IntEq number1 number2) tables row = evalInt number1 tables row == evalInt number2 tables row
evalBoolComp (IntGT number1 number2) tables row = evalInt number1 tables row > evalInt number2 tables row
evalBoolComp (IntLT number1 number2) tables row = evalInt number1 tables row < evalInt number2 tables row

-- Gets or converts an input into a corresponding string
evalString :: Str -> [Table] -> Int -> String
evalString (Number x) tables row = case tables of
    (table:_) -> if row < rows then cols!!row else ""
        where rows = length cols
              cols = snd (getColumn x [table])
    [] -> error "No tables available to evaluate the number."
evalString (SpecNumber x tabIndex) t row | row >= rows = ""
                                         | otherwise = cols!!row
    where rows = length cols
          cols = snd (getColWithIndex x tabIndex t)
evalString (Name x) _ _ = x
evalString (Quote x) _ _ = filter (/='\"') x

-- Calculates an integer based off an input like LENGTH or NUM
evalInt :: IntCalc -> [Table] -> Int -> Int
evalInt (CountLength str) t row = length (evalString str t row)
evalInt (Digit x) _ _ = x
evalInt (CharOrdOfCol col) t i = ord (head (evalString col t i))
evalInt (IntAdd x1 x2) t i = evalInt x1 t i + evalInt x2 t i
evalInt (IntSub x1 x2) t i = evalInt x1 t i - evalInt x2 t i
evalInt (IntMul x1 x2) t i = evalInt x1 t i * evalInt x2 t i
evalInt (IntDiv x1 x2) t i = evalInt x1 t i `div` evalInt x2 t i
evalInt (IntPow x1 x2) t i = evalInt x1 t i ^ evalInt x2 t i

-- Evaluates the list of optional tokens in sequence
evalOptionals :: [Optional] -> [ColumnType] -> [Table] -> IO [ColumnType]
evalOptionals [] result _ = return result
evalOptionals (x:xs) result tables = do
    newResult <- evalOptional x result tables
    evalOptionals xs newResult tables

-- Processes optional tokens and returns output as IO type
evalOptional :: Optional -> [ColumnType] -> [Table] -> IO [ColumnType]
evalOptional (WhenCondition boolean) columns tables = return (processWhen boolean columns tables)
evalOptional (Store filename) columns _ = do
    _ <- storeFile filename  (columnToRows columns)
    return columns
evalOptional (AsExpr outputMod) columns _ = return (evalAs outputMod columns [])
evalOptional (Transpose) columns _ = return (evalTranspose columns)
evalOptional (OrderAs order) columns _ = return (evalOrder order columns)
evalOptional (GroupAs theGroup) columns _ = return $ concat (groupBy (evalGroup theGroup) columns)

-- For each line in each column, it checks whether it's inlcuded in the output
-- If it is, add it to the list
processWhen :: Boolean -> [ColumnType] -> [Table] -> [ColumnType]
processWhen b cols tables = zip colIndices filterCols
    where colIndices = map fst cols
          colData = map snd cols
          rows = transpose colData -- [[String]]
          keepIndices = [i | (i, row) <- zip [0..] rows, evalBoolean b tables i] -- Rows to keep
          filterRows = [row | (i, row) <- zip [0..] rows, i `elem` keepIndices]
          filterCols = transpose filterRows

columnToRows :: [ColumnType] -> [[String]]
columnToRows columns = transpose [str | (_, str) <- columns]

storeFile :: String -> [[String]] -> IO ()
storeFile filename result = writeFile (filename ++ ".csv") (toOutputForm result)

toOutputForm :: [[String]] -> String
toOutputForm out = intercalate "\n" result
    where
        result = map (intercalate "," . map cleanItem) out
        cleanItem = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

-- Modifies the output by adding a string, or changing the order of your selection
-- acc stores the current output
evalAs :: [Outputs] -> [ColumnType] -> [ColumnType] -> [ColumnType] -- This needs to return [ColumnType]
evalAs [] _ acc = acc
evalAs ((OutputQuote x):rest) r acc = evalAs (OutputString (filter (/='\n') x):rest) r acc
evalAs ((OutputCols number):rest) result acc | number <= length result = evalAs rest result (acc ++ [result!!number])
    | otherwise = error "Index out of bounds"
evalAs ((OutputString str):rest) result acc = evalAs rest result (acc ++ [(0, [str | x <- [0..rows-1]])])
    where rows = length (snd (head result))

-- Transposes output
evalTranspose :: [ColumnType] -> [ColumnType]
evalTranspose columns = zip [0..] (rows)
    where rows = (columnToRows columns)

-- Takes in an order and orders the given columns that way
-- Asc, Desc, or based of an integer calculation like LENGTH
evalOrder :: Order -> [ColumnType] -> [ColumnType]
evalOrder OrderByAsc result = zip (map fst result) (transpose (sort (columnToRows result)))
evalOrder OrderByDesc result = zip (map fst result) (transpose (sortBy (flip compare) (columnToRows result)))
evalOrder (NestedOrder calc order) result = evalOrder order (evalOrder (OrderCalc calc) result) -- Dont know why this was written like this
evalOrder (OrderCalc calc) result = zip (map fst result) (transpose sortedRows)
    where rows = columnToRows result
          sortIndices = sortOn (\i -> evalInt calc [(0, columnToRows result)] i) [0..length rows - 1]
          sortedRows = map (rows !!) sortIndices

-- Takes in a comparison, and returns if the columns are in the same group
evalGroup :: Comparison -> ColumnType -> ColumnType -> Bool
evalGroup theGroup (colIndex1, values1) (colIndex2, values2) =
    evalBoolComp theGroup [] 0 && colIndex1 == colIndex2


--evalGroup :: Comparison -> [Table] -> ColumnType -> ColumnType -> Bool
--evalGroup theGroup tables (colIndex1, values1) (colIndex2, values2) =
--    evalBoolComp theGroup tables 0 && colIndex1 == colIndex2

safeHead :: [a] -> a
safeHead [] = error "Empty list"
safeHead (x:_) = x
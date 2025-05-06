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

eval :: String -> IO String
eval filename = do
    contents <- readFile filename
    let program = Program (Grammar.parse (alexScanTokens contents))
    evalProgram program ""

evalProgram :: Program -> String -> IO String
evalProgram (Program []) result = return result
evalProgram (Program (statement:rest)) result = do
    stmtLines <- evalStmt statement
    --putStrLn $ "Statement: " ++ show stmtLines
    let formatted = unlines (filter (not . null) (map (toCSVFormat . snd) stmtLines))
    --putStrLn $ "Formatted: " ++ formatted
    --putStrLn $ "Result: " ++ result
    -- evalProgram (Program rest) (result ++ "\n" ++ formatted)
    evalProgram (Program rest) (result ++ formatted)
    where
        toCSVFormat :: [String] -> String
        toCSVFormat = intercalate ","

evalStmt :: Statement -> IO [ColumnType]
evalStmt (SelectOpt selection tabs optionals end) = do
    tables <- evalTables 0 tabs
    result <- evalSelection selection tables
    evalEnd (evalOptionals optionals result tables) end
evalStmt (SelectStmt selection tabs end) = do -- Version without optionals
    tables <- evalTables 0 tabs
    selectionResults <- evalSelection selection tables
    evalEnd (return selectionResults) end
evalStmt (CommentStmt comment) = do
    return []

evalTables :: Int -> [Tables] -> IO [Table]
evalTables _ [] = return []
evalTables tableIndex (table:rest) = do
    t <- evalTable tableIndex table
    ts <- evalTables (tableIndex + 1) rest
    return (t : ts)

evalTable :: Int -> Tables -> IO Table
evalTable tableIndex (LoadTable filename) = do
    contents <- readFile (filename ++ ".csv")
    let rows = lines contents
    let arities = map (length . splitOn ",") rows
    if all (== head arities) arities
        then return ()
        else error "CSV file has inconsistent arity."

    let rows = map (splitOn ",") (lines contents)
    return (tableIndex, rows)

evalTable tableIndex (TableOp tables1 expr tables2) = do
    evalTables1 <- evalTables tableIndex tables1
    evalTables2 <- evalTables (tableIndex + length tables1) tables2
    let result
            | expr == Cartesian =
                (tableIndex, [row1 ++ row2 | (_, rows1) <- evalTables1, row1 <- rows1, (_, rows2) <- evalTables2, row2 <- rows2])
            | expr == Union =
                (tableIndex, concatMap snd evalTables1 ++ concatMap snd evalTables2)
            | expr == Intersect =
                (tableIndex, [row1 | (_, rows1) <- evalTables1, row1 <- rows1, (_, rows2) <- evalTables2, row2 <- rows2, row1 == row2])
    return result

evalTable _ (TableConc _ _) = error "TableConc not implemented"
evalTable _ (TableJoin {}) = error "TableJoin not implemented"

evalEnd :: IO [ColumnType] -> End -> IO [ColumnType]
evalEnd final End = final
evalEnd final Output = do
    printedLine <- final
    _ <- putStrLn (toOutputForm (columnToRows printedLine))
    return printedLine

evalSelection :: Selection -> [Table] -> IO [ColumnType]
evalSelection SelectAll tables = do
    let tableArity = length (head (snd (head tables)))
    let this = map ColIndex [0..(tableArity - 1)]
    let strings = evalColumns this tables
    return strings
evalSelection (SelectColumns cols) tables = do
    let strings = evalColumns cols tables
    return (zip [0..] (map snd strings))

evalColumns :: [Grammar.Column] -> [Table] -> [ColumnType]
evalColumns [] _ = []
evalColumns (x:xs) tables = evalColumn x tables ++ evalColumns xs tables

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

getColumn :: Int -> [Table] -> ColumnType
getColumn colIndex tables = (colIndex, concatMap (getColValues colIndex) tables)
    where
        getColValues :: Int -> Table -> [String]
        getColValues columnIndex (_, rows) = map (!! columnIndex) rows

getColWithIndex :: Int -> Int -> [Table] -> ColumnType
getColWithIndex colIndex tabIndex tables = (colIndex, concatMap (getColValues colIndex) filteredTables)
    where
        filteredTables = filter (\(tblIndex, _) -> tblIndex == tabIndex) tables
        getColValues :: Int -> Table -> [String]
        getColValues columnIndex (_, rows) = map (!! columnIndex) rows

evalBoolean :: Boolean -> [Table] -> Int -> Bool
evalBoolean (BoolExpr b1 BoolAND b2) t i = evalBoolean b1 t i && evalBoolean b2 t i
evalBoolean (BoolExpr b1 BoolOR b2) t i = evalBoolean b1 t i || evalBoolean b2 t i
evalBoolean (BoolExpr b1 BoolXOR b2) t i = evalBoolean b1 t i /= evalBoolean b2 t i
evalBoolean (BoolNOT b) t i = not (evalBoolean b t i)
evalBoolean BoolTrue _ _ = True
evalBoolean BoolFalse _ _ = False
evalBoolean (BoolComp comparison) tables row = evalBoolComp comparison tables row

evalBoolComp :: Comparison -> [Table] -> Int -> Bool
evalBoolComp (StringComp s1 s2) t row = evalString s1 t row == evalString s2 t row
evalBoolComp (IntEq number1 number2) tables row = evalInt number1 tables row == evalInt number2 tables row
evalBoolComp (IntGT number1 number2) tables row = evalInt number1 tables row > evalInt number2 tables row
evalBoolComp (IntLT number1 number2) tables row = evalInt number1 tables row < evalInt number2 tables row

evalString :: Str -> [Table] -> Int -> String
evalString (Number x) tables row = case tables of
    (table:_) -> snd (getColumn x [table]) !! row
    [] -> error "No tables available to evaluate the number."
evalString (SpecNumber x tabIndex) t row = snd (getColWithIndex x tabIndex t) !! row
evalString (Name x) _ _ = x
evalString (Quote _) _ _ = error "Quote pattern not implemented in evalString."

evalInt :: IntCalc -> [Table] -> Int -> Int
evalInt (CountLength str) t row = length (evalString str t row) --countLength (evalColumn col)
evalInt (Digit x) _ _ = x
evalInt (CharOrdOfCol col) t i = ord (head (evalString col t i))
evalInt (IntAdd x1 x2) t i = evalInt x1 t i + evalInt x2 t i
evalInt (IntSub x1 x2) t i = evalInt x1 t i - evalInt x2 t i
evalInt (IntMul x1 x2) t i = evalInt x1 t i * evalInt x2 t i
evalInt (IntDiv x1 x2) t i = evalInt x1 t i `div` evalInt x2 t i
evalInt (IntPow x1 x2) t i = evalInt x1 t i ^ evalInt x2 t i

evalOptionals :: [Optional] -> [ColumnType] -> [Table] -> IO [ColumnType]
evalOptionals [] result _ = return result
evalOptionals (x:xs) result tables = do
    newResult <- evalOptional x result tables
    evalOptionals xs newResult tables

evalOptional :: Optional -> [ColumnType] -> [Table] -> IO [ColumnType]
evalOptional (WhenCondition boolean) columns tables = return (processWhen boolean columns tables)
evalOptional (Store filename) columns _ = do
    _ <- storeFile filename  (columnToRows columns)
    return columns
evalOptional (AsExpr outputMod) columns _ = return (evalAs outputMod columns [])
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
    where result = map (intercalate ",") out

-- acc stores the current output
evalAs :: [Outputs] -> [ColumnType] -> [ColumnType] -> [ColumnType] -- This needs to return [ColumnType]
evalAs [] _ acc = acc
evalAs ((OutputQuote _):_) _ _ = error "OutputQuote pattern not implemented in evalAs."
evalAs ((OutputCols number):rest) result acc | number <= length result = evalAs rest result ((result!!(number-1)) : acc)
    | otherwise = error "Index out of bounds"
evalAs ((OutputString str):rest) result acc = evalAs rest result ((0, [str]) : acc)

evalOrder :: Order -> [ColumnType] -> [ColumnType]
evalOrder OrderByAsc result = result -- sort result -- SORT
evalOrder OrderByDesc result = result --reverse (sort result) SORT
evalOrder (NestedOrder calc order) result = result -- TODO
evalOrder (OrderCalc calc) result = result -- TODO

evalGroup :: Comparison -> ColumnType -> ColumnType -> Bool
evalGroup theGroup (colIndex1, values1) (colIndex2, values2) =
    evalBoolComp theGroup [] 0 && colIndex1 == colIndex2
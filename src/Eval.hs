module Eval where
import Grammar
import qualified Data.Map as Map
import System.IO
import Data.List
import Data.Char

type Table = (Int, [[String]])
type ColumnType = (Int, [String])

evalProgram :: Program -> String -> IO String
evalProgram (Program []) result = do return result
evalProgram (Program (statement:rest)) result = do
    lines <- evalStmt statement
    let formatted = unlines (map toCSVFormat lines)
    restResult <- evalProgram (Program rest) (result ++ "\n" ++ formatted)
    return restResult

toOutputForm :: [[String]] -> String
toOutputForm out = intercalate "\n" (intercalate "," out)

toCSVFormat :: [String] -> String
toCSVFormat lines = intercalate "," lines

evalStmt :: Statement -> IO [ColumnType]
evalStmt (SelectOpt selection tabs optionals end) = do
    tables <- evalTables tabs
    result <- evalSelection selection tables
    final <- evalOptionals optionals result  tables
    evalEnd final end
evalStmt (SelectStmt selection tabs end) = do -- Version without optionals
    tables <- evalTables tabs
    result <- evalSelection selection tables
    evalEnd result end


evalEnd :: IO [ColumnType] -> End -> IO [ColumnType]
evalEnd final End = final
evalEnd final Output = do
    _ <- putStrLn (toOutputForm (columnToString final))
    return final


-- Eval Selection

evalSelection :: Selection -> [Table] -> IO [ColumnType]
evalSelection SelectAll tables = return (allTablesToColumns tables)
evalSelection (SelectColumns cols) tables = do
    let strings = evalColumns cols tables
    return (zip [0..] (map (:[]) strings))


-- Eval Tables - Start

evalTables :: [Tables] -> IO [Table]
evalTables [] = return []
evalTables (table:rest) = do
    t <- evalTable table
    ts <- evalTables rest
    return (t : ts)

-- Evaluate Tables AST into actual Table (name, rows)
evalTable :: Tables -> IO Table
evalTable (LoadTable filename) = do
    contents <- readFile filename
    let rows = map (splitOn ",") (lines contents)
    return (filename, rows)

-- Select specific columns
selectColumns :: [Int] -> [[String]] -> [[String]]
selectColumns cols table = map (\row -> [row !! col | col <- cols]) table

-- Row filtering helper
filterRows :: (Row -> Bool) -> [[String]] -> [[String]]
filterRows cond table = filter cond table

-- Project both column selection and filtering
project :: [Int] -> (Row -> Bool) -> [[String]] -> [[String]]
project cols cond table = selectColumns cols (filterRows cond table)


-- Evaluating the columns and returning the string representations
evalColumns :: [Column] -> [Table] -> [ColumnType]
evalColumns [] _ = []
evalColumns (x:xs) tables = (evalColumn x tables) : (evalColumns xs tables)

evalColumn :: Column -> [Table] -> [ColumnType]
evalColumn (ColIndex x) tables = [getColumn x tables]
evalColumn (ColIndexTable x tabIndex) tables = [getColWithName x tabIndex tables]
evalColumn (IfStmt col1s boolExpr col2s) tables = [ (x, [ if (evalBoolean boolExpr tables i) 
    then col1!!i else col2!!i | i <- [0..rows - 1] ]) | ((x, col1), (_, col2)) <- zip col1Vals col2Vals ]
    where
        col1Vals = evalColumns col1s tables
        col2Vals = evalColumns col2s tables
        rows = case col1Vals of
            (_, vals):_ -> length vals
            _ -> error "No value in column"

-- Evaluating the optionals starts here

--evalOptionals optionals result
evalOptionals :: [Optional] -> [ColumnType] -> [Table] -> IO [ColumnType]
evalOptionals [] result _ = return result
evalOptionals (x:xs) result tables = do 
    newResult <- evalOptional x result tables
    evalOptionals xs newResult tables

--evalOptional optional result
evalOptional :: Optional -> [ColumnType] -> [Table] -> IO [ColumnType]
evalOptional (WhenCondition boolean) columns tables = return (processWhen boolean columns tables)
evalOptional (Store filename) columns tables = do 
    _ <- (storeFile filename (columnToString columns))
    return columns
evalOptional (AsExpr outputMod) columns tables = return (evalAs outputMod columns [])
evalOptional (OrderAs order) columns tables = return (evalOrder order columns)
evalOptional (GroupAs group) columns tables = (concat (groupBy (evalGroup group) columns)) -- TODO

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

evalBoolean :: Boolean -> [Table] -> Int -> Bool
evalBoolean (BoolExpr b1 (BoolAND) b2) t i = (evalBoolean b1 t i) && (evalBoolean b2 t i)
evalBoolean (BoolExpr b1 (BoolOR) b2) t i = (evalBoolean b1 t i) || (evalBoolean b2 t i)
evalBoolean (BoolExpr b1 (BoolXOR) b2) t i = (evalBoolean b1 t i) /= (evalBoolean b2 t i)
evalBoolean (BoolNOT b) t i = not (evalBoolean b t i)
evalBoolean BoolTrue _ _ = True
evalBoolean BoolFalse _ _ = False
evalBoolean (BoolComp comparison) tables row = evalBoolComp comparison tables row

evalBoolComp :: Comparison -> [Table] -> Int -> Bool
evalBoolComp (StringComp s1 s2) t row = (evalString s1 t row) == (evalString s2 t row)
evalBoolComp (IntEq number1 number2) _ row = (evalInt number1 row) == (evalInt number2 row)
evalBoolComp (IntGT number1 number2) _ row = (evalInt number1 row) > (evalInt number2 row)
evalBoolComp (IntLT number1 number2) _ row = (evalInt number1 row) < (evalInt number2 row)

evalString :: Str -> [Table] -> Int -> String
evalString (Number x) t row = (snd (getColumn x (t!!0)))!!row -- ASSUME THIS ONE ONLY HAPPENS FOR SINGLE TABLE
evalString (SpecNumber x tabIndex) t row = (snd (getColWithName x tabIndex t))!!row
evalString (Name x) _ _ = x

evalInt :: IntCalc -> [Table] -> Int -> Int
evalInt (CountLength str) t row = length (evalString str t row) --countLength (evalColumn col)
evalInt (Digit x) _ _ = x
--evalInt (CharOrdOfCol Str) t i = 
--    let val = evalColumn col t in
--    ord (head (snd val !! i))
evalInt (IntAdd x1 x2) t i = (evalInt x1 t i) + (evalInt x2 t i)
evalInt (IntSub x1 x2) t i = (evalInt x1 t i) - (evalInt x2 t i)
evalInt (IntMul x1 x2) t i = (evalInt x1 t i) * (evalInt x2 t i)
evalInt (IntDiv x1 x2) t i = (evalInt x1 t i) `div` (evalInt x2 t i)
evalInt (IntPow x1 x2) t i = (evalInt x1 t i) ^ (evalInt x2 t i)

-- out stores the current output
evalAs :: [Outputs] -> [ColumnType] -> [ColumnType] -> [ColumnType] -- This needs to return [ColumnType]
evalAs [] result out = out
evalAs ((OutputCols number):rest) result out | number <= length result = evalAs rest result (out ++ [result!!(number-1)])
    | otherwise = error "Index out of bounds"
evalAs ((OutputString str):rest) result out = evalAs rest result (out ++ [(0, [str])])

evalOrder :: Order -> [ColumnType] -> [ColumnType]
evalOrder OrderByAsc result = result -- sort result -- SORT
evalOrder OrderByDesc result = result --reverse (sort result) SORT
evalOrder (NestedOrder calc order) result = result -- TODO
evalOrder (OrderCalc calc) result = result -- TODO


-- Helper methods are here. 
getColumn :: Int -> Table -> ColumnType
getColumn x table = (x, map (!! x) (snd table))

getColWithName :: Int -> Int -> [Table] -> ColumnType
getColWithName x tabIndex [] = error "Index out of bounds"
getColWithName x 1 (table:rest) = getColumn x table
getColWithName x tabIndex (table:rest) = getColWithName x (tabIndex - 1) rest

storeFile :: String -> [[String]] -> IO [[String]]
storeFile filename result = do
    writeFile filename (toOutputForm result)
    return result

countLength :: String -> Int
countLength result = length result

columnToString :: [ColumnType] -> [[String]]
columnToString columns = transpose [str | (_, str) <- columns]


readCSV :: String -> IO Table
readCSV filename = do
    content <- readFile filename
    let filenameWithoutExt = takeWhile (/= '.') filename
    let rows = lines content
    let header = words (head rows)
    let dataRows = map words (tail rows)
    return (filenameWithoutExt, header : dataRows)

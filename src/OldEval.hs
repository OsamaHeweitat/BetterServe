module Eval where
import Grammar
import qualified Data.Map as Map
import System.IO
import Data.List
import Control.Applicative -- For the `many` combinator

type Table = (Int, [[String]]) -- (Index, Rows)
type ColumnType = (Int, [String]) -- (ColumnIndex, ColumnValues)

data Value
    = TableVal [[String]]
    | StrVal String
    | IntVal Int
    | BoolVal Bool
    deriving (Show, Eq)

eval :: String -> IO String
eval filename = do
    contents <- readFile filename
    let program = parseProgram contents
    result <- evalProgram program ""
    return result

parseProgram :: String -> Program
parseProgram input = case parse programParser input of
    Left err -> error $ "Parse error: " ++ show err
    Right result -> result
    where
        programParser = do
            statements <- many statementParser
            _ <- eof
            return (Program statements)
            
evalProgram :: Program -> String -> IO String
evalProgram (Program []) result = do return result
evalProgram (Program (statement:rest)) result = do
    lines <- evalStmt statement
    let formatted = unlines (map (toCSVFormat . snd) lines)
    evalProgram (Program rest) (result ++ "\n" ++ formatted)

toOutputForm :: [[String]] -> String
toOutputForm out = intercalate "\n" (map (intercalate ",") out)

toCSVFormat :: [String] -> String
toCSVFormat = intercalate ","

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
evalEnd final End = return final
evalEnd final Output = do
    printOut (processColumns final)
    return final


evalSelection :: Selection -> [Table] -> IO [ColumnType]
evalSelection SelectAll tables = return (allTablesToColumns tables)
evalSelection (SelectColumns cols) tables = do
    let strings = evalColumns cols tables
    return (zip [0..] (map (:[]) strings))  -- Make single-column format

-- evalSelection :: Selection -> [Table] -> [Column]
-- evalSelection SelectAll tables = tables
-- evalSelection (SelectColumns columns) tables = map (evalColumns columns) tables

-- evalColumns :: [Column] -> [Table] -> [ColumnType]
-- evalColumns columns tables = do
--     let columnIndex = map (getColumnIndex columns) tables
--     let columnValues = map (getColumnValues columnIndex) tables
--     return (columnIndex, columnValues)


evalTables :: [Tables] -> IO [Table]
evalTables [] = return []
evalTables (table:rest) = do
    t <- evalTable table
    ts <- evalTables rest
    return (t : ts)

evalTable :: Tables -> IO Table
evalTable (LoadTable filename) = do
    contents <- readFile filename
    let rows = map (splitOn ",") (lines contents)
    return (filename, rows)

-- Evaluating the columns and returning the string representations

evalColumns :: [Column] -> [Table] -> [ColumnType]
evalColumns [] _ = []
evalColumns (x:xs) tables = (evalColumn x tables) : (evalColumns xs tables)

evalColumn :: Column -> [Table] -> ColumnType
evalColumn (ColIndex x) tables = getColumn x tables
evalColumn (ColIndexTable x tabIndex) tables = getColWithName x tabIndex tables
evalColumn (IfStmt cols1 boolExpr cols2) tables = if (evalBoolean boolExpr) then (evalColumns cols1 tables) else (evalColumns cols2 tables)

-- Evaluating the optionals starts here

--evalOptionals optionals result
evalOptionals :: [Optional] -> [ColumnType] -> [Table] -> IO [ColumnType]
evalOptionals [] result _ = return result
evalOptionals (x:xs) result tables = evalOptionals xs (evalOptional x result tables) tables

--evalOptional optional result
evalOptional :: Optional -> [ColumnType] -> [Table] -> IO [ColumnType]
evalOptional (WhenCondition boolean) columns tables = return (processWhen boolean columns table)
evalOptional (Store filename) columns tables = do
    _ <- storeFile filename (columnToString columns)
    return columns
evalOptional (AsExpr outputMod) columns tables = return (evalAs outputMod columns [])
evalOptional (OrderAs order) columns tables = evalOrder order columns
evalOptional (GroupAs group) columns tables = concat (groupBy (evalGroup group) columns) -- TODO

-- For each line in each column, it checks whether it's inlcuded in the output
-- If it is, add it to the list
processWhen :: Boolean -> [ColumnType] -> [Table] -> [ColumnType]
processWhen b col tables = map filterColumn col
    where filterColumn (colIndex, x) = (colIndex, [v | (ind, v) <- zip [0..] x, ind `elem` keepIndices])
          keepIndices = [i | i <- [0..len - 1], evalBoolean b tables i] -- Rows to keep
          len = length (snd col)

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
evalInt (CharOrdOfCol col) t i = col -- TODO
evalInt (IntAdd x1 x2) t i = (evalInt x1 t i) + (evalInt x2 t i)
evalInt (IntSub x1 x2) t i = (evalInt x1 t i) - (evalInt x2 t i)
evalInt (IntMul x1 x2) t i = (evalInt x1 t i) * (evalInt x2 t i)
evalInt (IntDiv x1 x2) t i = (evalInt x1 t i) / (evalInt x2 t i)
evalInt (IntPow x1 x2) t i = (evalInt x1 t i) ^ (evalInt x2 t i)

-- out stores the current output
evalAs :: [Outputs] -> [ColumnType] -> [ColumnType] -> [ColumnType] -- This needs to return [ColumnType]
evalAs [] result out = out
evalAs ((OutputCols number):rest) result out | number <= length result = evalAs rest result (out ++ [result!!(number-1)])
    | otherwise = error "Index out of bounds"
--evalAs ((OutputCols []):rest) result out = evalAs rest result
--evalAs ((OutputCols (col:cols)):rest) result out = evalAs (OutputCols cols) result (out ++ (modifyOut col result))
evalAs ((OutputString str):rest) result out = evalAs rest result (out ++ [(0, [str])])

evalOrder :: Order -> [ColumnType] -> IO [ColumnType]
evalOrder OrderByAsc result = return result -- sort result -- SORT
evalOrder OrderByDesc result = return result --reverse (sort result) SORT
evalOrder (NestedOrder calc order) result = return result -- TODO
evalOrder (OrderCalc calc) result = return result -- TODO


-- Helper methods are here. 
-- I expect they'll be used elsewhere so they're separate

-- Type reminder
-- type Table = (Int, [[String]])
-- type ColumnType = (Int, [String])
getColumn :: Int -> Table -> ColumnType
getColumn x null = null
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

module Eval where
import Grammar
import qualified Data.Map as Map
import System.IO
import Data.List.Split (splitOn)

type Env = Map.Map String Value
--type Row = [String]
--type Table = (String, [[String]])
type Table = (Int, [[String]])
type ColumnType = (Int, [String])

data Value
    = TableVal [[String]]
    | StrVal String
    | IntVal Int
    | BoolVal Bool
    deriving (Show, Eq)

-- Start
eval :: Env -> Exp -> Int
eval = undefined

evalProgram :: Program -> String -> IO String
evalProgram (Program []) result = do return result
evalProgram (Program (statement:rest)) result = do
    lines <- evalStmt statement
    let formatted = unlines (map toCSVFormat lines)
    restResult <- evalProgram (Program rest) (result ++ "\n" ++ formatted)
    return restResult
    --let output = map toCSVFormat lines
    --return evalProgram rest 

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
    _ <- printOut (processColumns final)
    return final


-- Eval Selection

evalSelection :: Selection -> [Table] -> IO [ColumnType]
evalSelection SelectAll tables = return (allTablesToColumns tables)
evalSelection (SelectColumns cols) tables = do
    let strings = evalColumns cols tables
    return (zip [0..] (map (:[]) strings))  -- Make single-column format


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

-- Define evalExp to convert the Table into a Value if needed
evalExp :: Env -> Exp -> IO Value
evalExp env (SelectColumns cols) = do
    let table = getTableFromEnv env
    let result = selectColumns cols table
    return $ TableVal result

evalExp env (TableOp cols exp) = do
    val <- evalExp env exp
    case val of
        TableVal table -> return $ TableVal (selectColumns cols table)
        _ -> error "Expected a table in TableOp"

evalExp env (TableJoin e1 e2) = do
    val1 <- evalExp env e1
    val2 <- evalExp env e2
    case (val1, val2) of
        (TableVal t1, TableVal t2) -> return $ TableVal (joinTables t1 t2)
        _ -> error "Expected two tables in TableJoin"

evalExp env (TableConc e1 e2) = do
    val1 <- evalExp env e1
    val2 <- evalExp env e2
    case (val1, val2) of
        (TableVal t1, TableVal t2) -> return $ TableVal (t1 ++ t2)
        _ -> error "Expected two tables in TableConc"

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
evalOptional :: Optional -> [ColumnType] -> [Table] -> [ColumnType]
evalOptional (WhenCondition boolean) columns tables = processWhen boolean columns table
evalOptional (Store filename) columns tables = do 
    _ <- (storeFile filename (columnToString columns))
    columns
evalOptional (AsExpr outputMod) columns tables = evalAs outputMod columns []
evalOptional (OrderAs order) columns tables = evalOrder order columns
evalOptional (GroupAs group) columns tables = (concat (groupBy (evalGroup group) columns)) -- TODO

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
evalInt (IntDiv x1 x2) t i = (evalInt x1 t i) `div` (evalInt x2 t i)
evalInt (IntPow x1 x2) t i = (evalInt x1 t i) ^ (evalInt x2 t i)

-- out stores the current output
evalAs :: [Outputs] -> [ColumnType] -> [ColumnType] -> [ColumnType] -- This needs to return [ColumnType]
evalAs [] result out = out
evalAs ((OutputCols number):rest) result out | number <= length result = evalAs rest result (out ++ [result!!(number-1)])
    | otherwise = error "Index out of bounds"
--evalAs ((OutputCols []):rest) result out = evalAs rest result
--evalAs ((OutputCols (col:cols)):rest) result out = evalAs (OutputCols cols) result (out ++ (modifyOut col result))
evalAs ((OutputString str):rest) result out = evalAs rest result (out ++ [(0, [str])])

evalOrder :: Order -> [ColumnType] -> [ColumnType]
evalOrder OrderByAsc result = result -- sort result -- SORT
evalOrder OrderByDesc result = result --reverse (sort result) SORT
evalOrder (NestedOrder calc order) result = result -- TODO
evalOrder (OrderCalc calc) result = result -- TODO


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


------------------------------------

--evalTables :: Tables -> [Table]
--evalTables (LoadTable filename) = do
--    contents <- readFile filename
--    let rows = map (splitOn ",") (lines contents)
--    return [(filename, rows)]

--evalColumns :: [Column] -> [Table] -> [ColumnType]
--evalColumns columns tables = do
--    let columnIndex = map (getColumnIndex columns) tables
--    let columnValues = map (getColumnValues columnIndex) tables
--    return (columnIndex, columnValues)

readCSV :: String -> IO Table
readCSV filename = do
    content <- readFile filename
    let filenameWithoutExt = takeWhile (/= '.') filename
    let rows = lines content
    let header = words (head rows)
    let dataRows = map words (tail rows)
    return (filenameWithoutExt, header : dataRows)

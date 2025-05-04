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
    let output = map toCSVFormat lines
    return evalProgram rest 

toOutputForm :: [[String]] -> String
toOutputForm out = intercalate "\n" (intercalate "," out)

toCSVFormat :: [String] -> String
toCSVFormat lines = intercalate "," lines

evalStmt :: Statement -> IO [String]
evalStmt (SelectOpt selection tabs optionals end) = do
    tables <- evalTables tabs
    result <- evalSelection selection tables
    result <- evalOptionals optionals result 
evalStmt (SelectStmt selection tabs end) = do -- Version without optionals
    tables <- evalTables tabs
    result <- evalSelection selection tables


-- Eval Tables - Start

-- Evaluate Tables AST into actual Table (name, rows)
evalTables :: Tables -> IO Table
evalTables (LoadTable filename) = do
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

evalColumns :: [Column] -> [Table] -> [String]
evalColumns [] tables = []
evalColumns (x:xs) tables = [(evalColumn x tables)] ++ [(evalColumns xs tables)]

evalColumn :: Column -> [Table] -> String
evalColumn (ColIndex x) tables = getColumn x tables
evalColumn (ColIndexTable x tabIndex) tables = getColWithName x tabIndex tables
evalColumn (IfStmt cols1 boolExpr cols2) tables = if (evalBoolean boolExpr) then (evalColumns cols1 tables) else (evalColumns cols2 tables)

-- Evaluating the optionsals starts here

--evalOptionals optionals result
evalOptionals :: [Optional] -> [String] -> IO [String]
evalOptionals [] result = result
evalOptionals (x:xs) result = (evalOptionals xs (evalOptional x result))

--evalOptional optional result
evalOptional :: Optional -> [ColumnType] -> [Table] -> [ColumnType]
evalOptional (WhenCondition boolean) columns tables = processWhen boolean columns table
evalOptional (Store filename) columns tables = do 
    (storeFile filename columns)
    resultToString columns
evalOptional (AsExpr outputMod) columns tables = evalAs outputMod columns []
evalOptional (OrderAs order) columns tables = evalOrder order columns
evalOptional (GroupAs group) columns tables = (concat (groupBy (evalGroup group) columns))

-- For each line in each column, it checks whether it's inlcuded in the output
-- If it is, add it to the list
processWhen :: Boolean -> [ColumnType] -> [Table] -> [ColumnType]
processWhen b col tables = map filterColumn col
    where filterColumn (colIndex, x) = (colIndex, [v | (ind, v) <- zip [0..] x, ind `elem` keepIndices])
          keepIndices = [i | i <- [0..len - 1], evalBoolean b tables i] -- Rows to keep
          len = length (snd col)

evalBoolean :: Boolean -> [Table] -> Int -> Bool
evalBoolean (BoolExpr b1 (BoolAND) b2) _ _ = b1 (&&) b2
evalBoolean (BoolExpr b1 (BoolOR) b2) _ _ = b1 (||) b2
evalBoolean (BoolExpr b1 (BoolXOR) b2) _ _ = b1 /= b2
evalBoolean (BoolNOT b) _ _ = not (evalBoolean b)
evalBoolean BoolTrue _ _ = True
evalBoolean BoolFalse _ _ = False
evalBoolean (BoolComp comparison) tables row = evalBoolComp comparison tables row

evalBoolComp :: Comparison -> [Table] -> Int -> Bool
evalBoolComp (StringComp s1 s2) t row = (evalString s1 t row) == (evalString s2 t row)
evalBoolComp (IntEq number1 number2) _ _ = (evalInt number1) == (evalInt number2)
evalBoolComp (IntGT number1 number2) _ _ = (evalInt number1) > (evalInt number2)
evalBoolComp (IntLT number1 number2) _ _ = (evalInt number1) < (evalInt number2)

evalString :: Str -> [Table] -> Int -> String
evalString (Number x) t row = (snd (getColumn x (t!!0)))!!row -- ASSUME THIS ONE ONLY HAPPENS FOR SINGLE TABLE
evalString (SpecNumber x tabIndex) t row = (snd (getColWithName x tabIndex t))!!row
evalString (Name x) _ _ = x

evalInt :: IntCalc -> Int
evalInt (CountLength col) = countLength (evalColumn col)
evalInt (Digit x) = x
evalInt (CharOrdOfCol col) = col -- TODO
evalInt (IntAdd x1 x2) = (evalInt x1) + (evalInt x2)
evalInt (IntSub x1 x2) = (evalInt x1) - (evalInt x2)
evalInt (IntMul x1 x2) = (evalInt x1) 'mul' (evalInt x2)
evalInt (IntDiv x1 x2) = (evalInt x1) 'div' (evalInt x2)
evalInt (IntPow x1 x2) = (evalInt x1) ^ (evalInt x2)

-- out stores the current output
evalAs :: [Outputs] -> [ColumnType] -> [ColumnType] -> [ColumnType] -- This needs to return [ColumnType]
evalAs [] result out = out
evalAs ((OutputCols number):rest) result out | number <= length result = evalAs rest result (out ++ [result!!(number-1)])
    | otherwise = error "Index out of bounds"
--evalAs ((OutputCols []):rest) result out = evalAs rest result
--evalAs ((OutputCols (col:cols)):rest) result out = evalAs (OutputCols cols) result (out ++ (modifyOut col result))
evalAs ((OutputString str):rest) result out = evalAs rest result (out ++ (0, [str]))

evalOrder :: Order -> [String] -> [String]
evalOrder OrderByAsc result = sort result
evalOrder OrderByDesc result = reverse (sort result)
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
getColWithName x tabIndex [] = []
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

--processOutput ::
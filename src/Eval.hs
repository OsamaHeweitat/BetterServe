module Eval where
import Grammar
import Tokens
import qualified Data.Map as Map
import System.IO
import Data.List.Split (splitOn)
import Data.List
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
    let formatted = unlines (map (toCSVFormat . snd) stmtLines)
    evalProgram (Program rest) (result ++ "\n" ++ formatted)
    where
        toCSVFormat :: [String] -> String
        toCSVFormat = intercalate ","

evalStmt :: Statement -> IO [ColumnType]
evalStmt (SelectOpt selection tabs optionals end) = do
    tables <- evalTables 1 tabs
    result <- evalSelection selection tables
    final <- evalOptionals optionals result tables
    evalEnd final end
evalStmt (SelectStmt selection tabs end) = do -- Version without optionals
    tables <- evalTables 1 tabs
    result <- evalSelection selection tables
    evalEnd result end
evalStmt (CommentStmt comment) = do
    putStrLn $ "Comment: " ++ comment
    return []

evalTables :: Int -> [Tables] -> IO [Table]
evalTables _ [] = return []
evalTables tableIndex (table:rest) = do
    t <- evalTable tableIndex table
    ts <- evalTables (tableIndex + 1) rest
    return (t : ts)

evalTable :: Int -> Tables -> IO Table
evalTable tableIndex (LoadTable filename) = do
    contents <- readFile filename
    let rows = map (splitOn ",") (lines contents)
    return (tableIndex, rows)
evalTable _ (TableOp {}) = error "TableOp not implemented"
evalTable _ (TableConc _ _) = error "TableConc not implemented"
evalTable _ (TableJoin {}) = error "TableJoin not implemented"

evalEnd :: [ColumnType] -> End -> IO [ColumnType]
evalEnd final End = return final
evalEnd final Output = return final

evalSelection :: Selection -> [Table] -> IO [ColumnType]
evalSelection SelectAll tables = return (allTablesToColumns tables)
    where
        allTablesToColumns :: [Table] -> [ColumnType]
        allTablesToColumns [] = []
        allTablesToColumns ((tableIndex, rows):rest) = (tableIndex, concat rows) : allTablesToColumns rest
evalSelection (SelectColumns cols) tables = do
    let strings = evalColumns cols tables
    return (zip [0..] (map (:[]) (concatMap snd strings)))

evalColumns :: [Grammar.Column] -> [Table] -> [ColumnType]
evalColumns [] _ = []
evalColumns (x:xs) tables = evalColumn x tables : evalColumns xs tables

evalColumn :: Grammar.Column -> [Table] -> ColumnType
evalColumn (ColIndex x) tables = getColumn x tables
evalColumn (ColIndexTable x tabIndex) tables = getColWithIndex x tabIndex tables
evalColumn (IfStmt cols1 boolExpr cols2) tables =
    let columns = if evalBoolean boolExpr tables 0
                  then evalColumns cols1 tables
                  else evalColumns cols2 tables
    in (0, concatMap snd columns)

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
    _ <- storeFile filename (columnToString columns)
    return columns
evalOptional (AsExpr outputMod) columns _ = return (evalAs outputMod columns [])
evalOptional (OrderAs order) columns _ = return (evalOrder order columns)
evalOptional (GroupAs theGroup) columns _ = return $ concat (groupBy (evalGroup theGroup) columns)

processWhen :: Boolean -> [ColumnType] -> [Table] -> [ColumnType]
processWhen b col tables = map filterColumn col
    where filterColumn (colIndex, x) = (colIndex, [v | (ind, v) <- zip [0..] x, ind `elem` keepIndices])
          keepIndices = [i | i <- [0..len - 1], evalBoolean b tables i] -- Rows to keep
          len = if null col then 0 else length (snd (head col))

columnToString :: [ColumnType] -> [[String]]
columnToString columns = transpose [str | (_, str) <- columns]

storeFile :: String -> [[String]] -> IO [[String]]
storeFile filename result = do
    writeFile filename (toOutputForm result)
    return result

toOutputForm :: [[String]] -> String
toOutputForm out = intercalate "\n" (map (intercalate ",") out)

evalAs :: [Outputs] -> [ColumnType] -> [ColumnType] -> [ColumnType] -- This needs to return [ColumnType]
evalAs [] _ out = out
evalAs ((OutputQuote _):_) _ _ = error "OutputQuote pattern not implemented in evalAs."
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

evalGroup :: Comparison -> ColumnType -> ColumnType -> Bool
evalGroup theGroup (colIndex1, values1) (colIndex2, values2) =
    evalBoolComp theGroup [] 0 && colIndex1 == colIndex2
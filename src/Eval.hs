module Eval where 
import Grammar

type Env = [(String, Int)]
type Table = (String, [[String]])
type ColumnType = (Int, [String])

eval :: Env -> Exp -> Int
eval = undefined

evalProgram :: Program -> String -> IO String
evalProgram (Program []) result = do return result
evalProgram (Program (statement:rest)) result = do
    lines <- evalStmt statement
    output = toCSVFormat lines
    return evalProgram rest 

toCSVFormat :: [String] -> String
toCSVFormat lines = intercalate "," lines

evalStmt :: Statement -> IO [String]
evalStmt (SelectOpt selection tabs optionals end) = do
    tables <- evalTables tabs
    result <- evalSelection selection tables
    result <- evalOptionals optionals result 
evalStmt (SelectStmt selection tabs end) = do
    tables <- evalTables tabs
    result <- evalSelection selection tables


evalColumns :: [Column] -> [Table] -> [String]
evalColumns [] tables = []
evalColumns (x:xs) tables = (evalColumn x tables) ++ (evalColumns xs tables)

evalColumn :: Column -> [Table] -> String
evalColumn (ColIndex x) tables = getColumn x tables
evalColumn (ColIndexTable x tabIndex) tables = getColWithName x tabIndex tables
evalColumn (IfStmt cols1 boolExpr cols2) tables = if (evalBoolean boolExpr) then (evalColumns cols1 tables) else (evalColumns cols2 tables)


--evalOptionals optionals result
evalOptionals :: [Optional] -> [String] -> IO [String]
--evalOptionals [] result = result
evalOptionals [Output] result = processOutput result
evalOptionals [End] result = return (result)
evalOptionals (x:xs) result = (evalOptionals xs (evalOptional x result))

--evalOptional optional result
evalOptional :: Optional -> [ColumnType] -> [Table] -> [String]
evalOptional (WhenCondition boolean) columns tables = return (filter (evalBoolean boolean tables) columns)
evalOptional (Store filename) columns tables = do 
    (storeFile filename columns)
    resultToString columns
evalOptional (AsExpr outputMod) columns tables = evalAs outputMod columns tables []
evalOptional (OrderAs order) columns tables = evalOrder order columns
evalOptional (GroupAs group) columns tables = (concat (groupBy (evalGroup group) columns))

evalBoolean :: Boolean -> [Table] -> Bool
evalBoolean (BoolExpr b1 (BoolAND) b2) t = b1 (&&) b2
evalBoolean (BoolExpr b1 (BoolOR) b2) t = b1 (||) b2
evalBoolean (BoolExpr b1 (BoolXOR) b2) t = b1 /= b2
evalBoolean (BoolNOT b) t = not (evalBoolean b)
evalBoolean BoolTrue t = True
evalBoolean BoolFalse t = False
evalBoolean (BoolComp comparison) tables = evalBoolComp comparison tables

evalBoolComp :: Comparison -> [Table] -> Bool
evalBoolComp (StringComp s1 s2) t = (evalString s1 t) == (evalString s2 t)
evalBoolComp (IntEq number1 number2) t = (evalInt number1) == (evalInt number2)
evalBoolComp (IntGT number1 number2) t = (evalInt number1) > (evalInt number2)
evalBoolComp (IntLT number1 number2) t = (evalInt number1) < (evalInt number2)

evalString :: Str -> [Table] -> String
evalString (Number x) t = (snd (getColumn x t))!!x
evalString (SpecNumber x tabIndex) t = (snd (getColWithName x tabIndex t))!!x
evalString (Name x) t = x

evalInt :: IntCalc -> Int
evalInt (CountLength col) = countLength (evalColumn col)
evalInt (Digit x) = x
evalInt (CharOrdOfCol col) = col -- TODO
evalInt (IntAdd x1 x2) = (evalInt x1) + (evalInt x2)
evalInt (IntSub x1 x2) = (evalInt x1) - (evalInt x2)
evalInt (IntMul x1 x2) = (evalInt x1) 'mul' (evalInt x2)
evalInt (IntDiv x1 x2) = (evalInt x1) 'div' (evalInt x2)
evalInt (IntPow x1 x2) = (evalInt x1) ^ (evalInt x2)

evalAs :: [Outputs] -> [String] -> [String] -> [String]
evalAs [] result out = out
evalAs ((OutputCols []):rest) result out = evalAs rest result
evalAs ((OutputCols (col:cols)):rest) result out = evalAs (OutputCols cols) result (out ++ (modifyOut col result))
evalAs ((OutputString str):rest) result out = evalAs rest result (out ++ str)

evalOrder :: Order -> [String] -> [String]
evalOrder OrderByAsc result = sort result
evalOrder OrderByDesc result = reverse (sort result)
evalOrder (NestedOrder calc order) result = result -- TODO
evalOrder (OrderCalc calc) result = result -- TODO



-- type Table = (String, [[String]])
-- type ColumnType = (Int, [String])
getColumn :: Int -> Table -> ColumnType
getColumn x null = null
getColumn x table = (fst table, ) ++ (getColumn x rest) -- TODO
    where lines = snd table

getColumn' :: Int -> [[String]] -> [[String]]
getColumn' col [] = []
getColumn' col xs = [x!!col | x <- xs]

getColWithName :: Int -> Int -> [Table] -> ColumnType
getColWithName x tabIndex [] = []
getColWithName x 1 (table:rest) = getColumn x table
getColWithName x tabIndex (table:rest) = getColWithName x (tabIndex - 1) rest


storeFile :: String -> [String] -> [String]
storeFile filename result = do
    return result -- TODO

countLength :: String -> Int
countLength result = length result


processOutput ::
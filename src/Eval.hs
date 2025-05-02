module Eval where 
import Grammar

type Env = [(String, Int)]
type Table = (String, [[String]])


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
evalStmt (SelectStmt selection tabs optionals end) = do
    tables <- evalTables tabs
    result <- evalSelection selection tables
    result <- evalOptionals optionals result 

evalColumns :: [Columns] -> [Table] -> [String]
evalColumns [] tables = []
evalColumns (x:xs) tables = (evalColumn x tables) ++ (evalColumns xs tables)

evalColumn :: Column -> [Table] -> String
evalColumn (ColIndex x) tables = getColumn x tables
evalColumn (ColIndexTable x name) tables = getColWithName x name tables
evalColumn (IfStmt cols1 boolExpr cols2) tables = if (evalBoolean boolExpr) then (evalColumns cols1 tables) else (evalColumns cols2 tables)


--evalOptionals optionals result
evalOptionals :: [Optional] -> [String] -> IO [String]
--evalOptionals [] result = result
evalOptionals [Output] result = processOutput result
evalOptionals [End] result = return (result)
evalOptionals (x:xs) result = (evalOptionals xs (evalOptional x result))

--evalOptional optional result
evalOptional :: Optional -> [String] -> IO [String]
evalOptional (WhenCondition boolean) result = return (filter (evalBoolean boolean) result)
evalOptional (Store filename) result = do 
    (storeFile filename result)
    return result
evalOptional (AsExpr outputMod) result = return (evalAs outputMod result [])
evalOptional (OrderAs order) result = return (evalOrder order result)
evalOptional (GroupAs group) result = return (concat (groupBy (evalGroup group) result))

evalBoolean :: Boolean -> Bool
evalBoolean (BoolExpr b1 (BoolAND) b2) = b1 (&&) b2
evalBoolean (BoolExpr b1 (BoolOR) b2) = b1 (||) b2
evalBoolean (BoolExpr b1 (BoolXOR) b2) = b1 /= b2
evalBoolean (BoolNOT b) = not (evalBoolean b)
evalBoolean BoolTrue = True
evalBoolean BoolFalse = False
evalBoolean (BoolComp comparison) = evalBoolComp comparison

evalBoolComp :: Comparison -> Bool
evalBoolComp (StringComp s1 s2) = s1 == s2
evalBoolComp (IntEq number1 number2) = (evalInt number1) == (evalInt number2)
evalBoolComp (IntGT number1 number2) = (evalInt number1) > (evalInt number2)
evalBoolComp (IntLT number1 number2) = (evalInt number1) < (evalInt number2)

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
getColumn :: Int -> [Table] -> [Table]
getColumn x [] = []
getColumn x (table:rest) =  ++ (getColumn x rest)
    where 

getColWithName :: Int -> String -> [Table] -> [Table]
getColWithName x name [] = []
getColWithName x name (table:rest) | (fst table) == name = getColumn x [table]
                                   | otherwise = getColWithName x name rest


storeFile :: String -> [String] -> [String]
storeFile filename result = do
    return result -- TODO

countLength :: String -> Int
countLength result = length result


processOutput ::
module Eval where 
import Grammar

type Env = [(String, Int)]

eval :: Env -> Exp -> Int
eval = undefined
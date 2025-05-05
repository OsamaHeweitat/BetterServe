{ 
module Tokens where 
}

%wrapper "posn"

$digit = 0-9
-- digits 
$alpha = [a-zA-Z_]    
-- alphabetic characters
$intComp = [\<\>\=]
-- comparison
$white = [\ \t\n]

tokens :-
$white+         ; 
  ";"           { \p s -> TokenSemicolon p } 
  "GET"         { \p s -> TokenGET p } 
  "FROM"        { \p s -> TokenFROM p } 
  "#" [$alpha $digit \s]*  { \p s -> TokenComment p (read s) } 

  "*"           { \p s -> TokenAll p } 
  ","           { \p s -> TokenComma p } 
  "("           { \p s -> TokenLParen p } 
  "IF"          { \p s -> TokenIf p } 
  "OTHERWISE"   { \p s -> TokenOtherwise p } 
  ")"           { \p s -> TokenRParen p } 
  "."           { \p s -> TokenPeriod p } 

  "LOAD"        { \p s -> TokenLoad p } 
  "ON"          { \p s -> TokenOn p } 
  "CARTESIAN"   { \p s -> TokenCartesian p } 
  "UNION"       { \p s -> TokenUnion p } 
  "INTERSECT"   { \p s -> TokenIntersect p } 
  "INNER"       { \p s -> TokenInner p } 
  "LEFT"        { \p s -> TokenLJoin p } 
  "RIGHT"       { \p s -> TokenRJoin p } 
  "FULL"        { \p s -> TokenJoin p } 

  "OUTPUT"      { \p s -> TokenOutput p } 
  "END"         { \p s -> TokenEnd p } 

  "WHEN"        { \p s -> TokenWhen p } 
  "STORE"       { \p s -> TokenStore p } 
  "AS"          { \p s -> TokenAs p } 
  "ORDER"       { \p s -> TokenOrder p } 
  "GROUPING"    { \p s -> TokenGrouping p } 
  "UP"          { \p s -> TokenUp p } 
  "DOWN"        { \p s -> TokenDown p } 

  "NOT"         { \p s -> TokenNOT p } 
  "TRUE"        { \p s -> TokenTrue p } 
  "FALSE"       { \p s -> TokenFalse p } 
  "AND"         { \p s -> TokenAND p } 
  "OR"          { \p s -> TokenOR p } 
  "XOR"         { \p s -> TokenXOR p } 

  "="           { \p s -> TokenEq p } 
  ">"           { \p s -> TokenGT p }
  "<"           { \p s -> TokenLT p }
  "\""            { \p s -> TokenString p } 

  "LENGTH"      { \p s -> TokenLength p } 
  "ORD_OF"      { \p s -> TokenOrd p } 
  "+"           { \p s -> TokenPlus p }
  "-"           { \p s -> TokenMinus p }
  "*"           { \p s -> TokenMul p }
  "/"           { \p s -> TokenDiv p }
  "^"           { \p s -> TokenPow p }
  "NUM"         { \p s -> TokenNumber p }
  $digit+       { \p s -> TokenDigit p (read s) } 
  $alpha [$alpha $digit]*   { \p s -> TokenVar p s } 

{ 
-- Each action has type :: AlexPosn -> String -> Token 

-- The token types: 
data Token = 
  TokenSemicolon AlexPosn   |    
  TokenGET AlexPosn         |
  TokenFROM AlexPosn        |
  TokenComment AlexPosn String |
  TokenAll AlexPosn         |
  TokenComma AlexPosn       | 
  TokenLParen AlexPosn      |
  TokenIf AlexPosn          |
  TokenOtherwise AlexPosn   |
  TokenRParen AlexPosn      |
  TokenPeriod AlexPosn      |
  TokenLoad AlexPosn        |
  TokenPlus AlexPosn        |
  TokenOn AlexPosn          |
  TokenCartesian AlexPosn   |
  TokenUnion AlexPosn       |
  TokenIntersect AlexPosn   |
  TokenInner AlexPosn       |
  TokenLJoin AlexPosn       |
  TokenRJoin AlexPosn       |
  TokenJoin AlexPosn        |
  TokenOutput AlexPosn      |
  TokenEnd AlexPosn         |
  TokenWhen AlexPosn        |
  TokenStore AlexPosn       |
  TokenAs AlexPosn          |
  TokenOrder AlexPosn       |
  TokenGrouping AlexPosn    |
  TokenUp AlexPosn          |
  TokenDown AlexPosn        |
  TokenNOT AlexPosn         |
  TokenTrue AlexPosn        |
  TokenFalse AlexPosn       |
  TokenAND AlexPosn         |
  TokenOR AlexPosn          |
  TokenXOR AlexPosn         |
  TokenEq AlexPosn          |
  TokenGT AlexPosn          |
  TokenLT AlexPosn          |
  TokenString AlexPosn      |
  TokenLength AlexPosn      |
  TokenOrd AlexPosn         |
  TokenMinus AlexPosn       |
  TokenMul AlexPosn         |
  TokenDiv AlexPosn         |
  TokenPow AlexPosn         |
  TokenNumber AlexPosn      |
  TokenDigit AlexPosn Int   |
  TokenVar AlexPosn String   
  deriving (Eq,Show) 

tokenPosn :: Token -> String
tokenPosn (TokenComment (AlexPn a l c) s) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenDigit (AlexPn a l c) i) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenVar (AlexPn a l c) s) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenSemicolon (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenGET (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenFROM (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenAll (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenComma (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenLParen (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenIf (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenOtherwise (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenRParen (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenPeriod (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenLoad (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenPlus (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenOn (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenCartesian (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenUnion (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenIntersect (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenInner (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenLJoin (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenRJoin (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenJoin (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenOutput (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenEnd (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenWhen (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenStore (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenAs (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenOrder (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenGrouping (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenUp (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenDown (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenNOT (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenTrue (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenFalse (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenAND (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenOR (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenXOR (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenEq (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenGT (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenLT (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenString (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenLength (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenOrd (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenMinus (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenMul (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenDiv (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenPow (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenNumber (AlexPn a l c)) = show (l) ++ ":" ++ show (c)
}
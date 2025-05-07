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
  ";"           { \p _ -> TokenSemicolon p } 
  "GET"         { \p _ -> TokenGET p } 
  "FROM"        { \p _ -> TokenFROM p } 
  "#" [$alpha $digit \s]*  { \p s -> TokenComment p (read s) } 

  "*"           { \p _ -> TokenAll p } 
  ","           { \p _ -> TokenComma p } 
  "("           { \p _ -> TokenLParen p } 
  "IF"          { \p _ -> TokenIf p } 
  "OTHERWISE"   { \p _ -> TokenOtherwise p } 
  ")"           { \p _ -> TokenRParen p } 
  "."           { \p _ -> TokenPeriod p } 

  "LOAD"        { \p _ -> TokenLoad p } 
  "ON"          { \p _ -> TokenOn p } 
  "CARTESIAN"   { \p _ -> TokenCartesian p } 
  "UNION"       { \p _ -> TokenUnion p } 
  "INTERSECT"   { \p _ -> TokenIntersect p } 
  "INNER"       { \p _ -> TokenInner p } 
  "LEFT"        { \p _ -> TokenLJoin p } 
  "RIGHT"       { \p _ -> TokenRJoin p } 
  "FULL"        { \p _ -> TokenJoin p } 

  "OUTPUT"      { \p _ -> TokenOutput p } 
  "END"         { \p _ -> TokenEnd p } 

  "WHEN"        { \p _ -> TokenWhen p } 
  "STORE"       { \p _ -> TokenStore p } 
  "AS"          { \p _ -> TokenAs p } 
  "TRANSPOSE"   { \p _ -> TokenTrans p }
  "ORDER"       { \p _ -> TokenOrder p } 
  "GROUPING"    { \p _ -> TokenGrouping p } 
  "UP"          { \p _ -> TokenUp p } 
  "DOWN"        { \p _ -> TokenDown p } 

  "NOT"         { \p _ -> TokenNOT p } 
  "TRUE"        { \p _ -> TokenTrue p } 
  "FALSE"       { \p _ -> TokenFalse p } 
  "AND"         { \p _ -> TokenAND p } 
  "OR"          { \p _ -> TokenOR p } 
  "XOR"         { \p _ -> TokenXOR p } 

  "="           { \p _ -> TokenEq p } 
  ">"           { \p _ -> TokenGT p }
  "<"           { \p _ -> TokenLT p }
  \"            { \p _ -> TokenQuote p } 

  "LENGTH"      { \p _ -> TokenLength p } 
  "ORD_OF"      { \p _ -> TokenOrd p } 
  "+"           { \p _ -> TokenPlus p }
  "-"           { \p _ -> TokenMinus p }
  "*"           { \p _ -> TokenMul p }
  "/"           { \p _ -> TokenDiv p }
  "^"           { \p _ -> TokenPow p }
  "NUM"         { \p _ -> TokenNumber p }
  \"[^\"]*\"    { \p s -> TokenAny p s }
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
  TokenTrans AlexPosn       |
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
  TokenQuote AlexPosn       |
  TokenLength AlexPosn      |
  TokenOrd AlexPosn         |
  TokenMinus AlexPosn       |
  TokenMul AlexPosn         |
  TokenDiv AlexPosn         |
  TokenPow AlexPosn         |
  TokenNumber AlexPosn      |
  TokenDigit AlexPosn Int   |
  TokenVar AlexPosn String  |
  TokenAny AlexPosn String
  deriving (Eq,Show) 

tokenPosn :: Token -> String
tokenPosn (TokenComment (AlexPn _ l c) _) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenDigit (AlexPn _ l c) _) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenVar (AlexPn _ l c) _) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenAny (AlexPn _ l c) _) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenSemicolon (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenGET (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenFROM (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenAll (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenComma (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenLParen (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenIf (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenOtherwise (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenRParen (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenPeriod (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenLoad (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenPlus (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenOn (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenCartesian (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenUnion (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenIntersect (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenInner (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenLJoin (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenRJoin (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenJoin (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenOutput (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenEnd (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenWhen (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenStore (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenAs (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenTrans (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenOrder (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenGrouping (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenUp (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenDown (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenNOT (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenTrue (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenFalse (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenAND (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenOR (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenXOR (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenEq (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenGT (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenLT (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenQuote (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenLength (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenOrd (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenMinus (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenMul (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenDiv (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenPow (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
tokenPosn (TokenNumber (AlexPn _ l c)) = show (l) ++ ":" ++ show (c)
}
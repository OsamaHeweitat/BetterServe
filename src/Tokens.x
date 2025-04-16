{ 
module Tokens where 
}

%wrapper "basic" 
--%wrapper "posn"

$digit = 0-9
-- digits 
$alpha = [a-zA-Z_]    
-- alphabetic characters
$intComp = [\<\>\=]
-- comparison
$white = [\ \t\n]

tokens :-
$white+         ; 
  ";"           { \s -> TokenSemicolon } 
  "GET"         { \s -> TokenGET } 
  "FROM"        { \s -> TokenFROM } 
  "#" [$alpha $digit \s]*  { \s -> TokenComment (read s) } 

  "*"           { \s -> TokenAll } 
  ","           { \s -> TokenComma } 
  "("           { \s -> TokenLParen } 
  "IF"          { \s -> TokenIf } 
  "OTHERWISE"   { \s -> TokenOtherwise } 
  ")"           { \s -> TokenRParen } 
  "."           { \s -> TokenPeriod } 

  "LOAD"        { \s -> TokenLoad } 
  "ON"          { \s -> TokenOn } 
  "CARTESIAN"   { \s -> TokenCartesian } 
  "UNION"       { \s -> TokenUnion } 
  "INTERSECT"   { \s -> TokenIntersect } 
  "INNER"       { \s -> TokenInner } 
  "LEFT"        { \s -> TokenLJoin } 
  "RIGHT"       { \s -> TokenRJoin } 
  "FULL"        { \s -> TokenJoin } 

  "OUTPUT"      { \s -> TokenOutput } 
  "END"         { \s -> TokenEnd } 

  "WHEN"        { \s -> TokenWhen } 
  "STORE"       { \s -> TokenStore } 
  "AS"          { \s -> TokenAs } 
  "ORDER"       { \s -> TokenOrder } 
  "GROUPING"    { \s -> TokenGrouping } 
  "UP"          { \s -> TokenUp } 
  "DOWN"        { \s -> TokenDown } 

  "NOT"         { \s -> TokenNOT } 
  "TRUE"        { \s -> TokenTrue } 
  "FALSE"       { \s -> TokenFalse } 
  "AND"         { \s -> TokenAND } 
  "OR"          { \s -> TokenOR } 
  "XOR"         { \s -> TokenXOR } 

  "="           { \s -> TokenEq } 
  ">"           { \s -> TokenGT }
  "<"           { \s -> TokenLT }
  \"            { \s -> TokenString } 

  "LENGTH"      { \s -> TokenLength } 
  "ORD_OF"      { \s -> TokenOrd } 
  "+"           { \s -> TokenPlus }
  "-"           { \s -> TokenMinus }
  "*"           { \s -> TokenMul }
  "/"           { \s -> TokenDiv }
  "^"           { \s -> TokenPow }
  $digit+       { \s -> TokenDigit (read s) } 
  $alpha [$alpha $digit]*   { \s -> TokenVar s } 

{ 
-- Each action has type :: AlexPosn -> String -> Token 


--data Token = Token' AlexPosn

-- The token types: 
data Token = 
  TokenSemicolon    |    
  TokenGET          |
  TokenFROM         |
  TokenComment String |
  TokenAll          |
  TokenComma        | 
  TokenLParen       |
  TokenIf           |
  TokenOtherwise    |
  TokenRParen       |
  TokenPeriod       |
  TokenLoad         |
  TokenPlus         |
  TokenOn           |
  TokenCartesian    |
  TokenUnion        |
  TokenIntersect    |
  TokenInner        |
  TokenLJoin        |
  TokenRJoin        |
  TokenJoin         |
  TokenOutput       |
  TokenEnd          |
  TokenWhen         |
  TokenStore        |
  TokenAs           |
  TokenOrder        |
  TokenGrouping     |
  TokenUp           |
  TokenDown         |
  TokenNOT          |
  TokenTrue         |
  TokenFalse        |
  TokenAND          |
  TokenOR           |
  TokenXOR          |
  TokenEq           |
  TokenGT           |
  TokenLT           |
  TokenString       |
  TokenLength       |
  TokenOrd          |
  TokenMinus        |
  TokenMul          |
  TokenDiv          |
  TokenPow          |
  TokenDigit Int    |
  TokenVar String   
  deriving (Eq,Show) 

}
{ 
module Grammar where 
import Tokens 
}

%name parse
%tokentype { Token } 
%error { parseError }
%token 
    SEMICOLON           { TokenSemicolon } 
    GET         { TokenGET } 
    FROM        { TokenFROM } 
    COMMENT     { TokenComment $$ }

    AST           { TokenAll } 
    COMMA           { TokenComma } 
    LPAREN           { TokenLParen } 
    IF          { TokenIf } 
    OTHERWISE   { TokenOtherwise } 
    RPAREN           { TokenRParen } 
    DOT           { TokenPeriod } 

    LOAD        { TokenLoad } 
    PLUS           { TokenPlus } 
    ON          { TokenOn } 
    CARTESIAN   { TokenCartesian } 
    UNION       { TokenUnion } 
    INTERSECT   { TokenIntersect } 
    INNER       { TokenInner } 
    LEFT        { TokenLJoin } 
    RIGHT       { TokenRJoin } 
    FULL        { TokenJoin } 

    OUTPUT      { TokenOutput } 
    END         { TokenEnd } 

    WHEN        { TokenWhen } 
    STORE       { TokenStore } 
    AS          { TokenAs } 
    ORDER       { TokenOrder } 
    GROUPING    { TokenGrouping } 
    UP          { TokenUp } 
    DOWN        { TokenDown } 

    NOT         { TokenNOT } 
    TRUE        { TokenTrue } 
    FALSE       { TokenFalse } 
    AND         { TokenAND } 
    OR          { TokenOR } 
    XOR         { TokenXOR } 

    EQ           { TokenEq } 
    GT      { TokenGT } 
    LT      { TokenLT } 
    QUOTE            { TokenString } 
-- NEXT TASK, SPLIT INTCOMP INTO MULTIPLE OF THEM LIKE GT LT EQ
    LENGTH      { TokenLength } 
    ORD_OF      { TokenOrd } 
    
    MINUS       { TokenMinus }
    TIMES       { TokenMul }
    DIVIDE      { TokenDiv }
    POWER       { TokenPow }
    INT       { TokenDigit $$ } 
    STRING    { TokenVar $$ }

%right in
%left NEG 

%% 

Program : Statement { [ $1 ] }
    | Program SEMICOLON Statement { $1 ++ [ $3 ] }
Statement : GET Selection FROM Tables Optionals { SelectStmt $2 $4 $5 }
    | COMMENT { CommentStmt $1 }

Selection : ColumnList { SelectColumns $1 }
    | AST { SelectAll }
ColumnList : Column { [$1] }
    | ColumnList COMMA Column { $1 ++ [$3] }
    | LPAREN ColumnList IF Boolean OTHERWISE ColumnList RPAREN { [IfStmt $2 $4 $6] }
Column : INT { ColIndex $1 }
    | INT DOT STRING { ColIndexTable $1 $3 }

Tables : LOAD STRING { LoadTable $2 }
    | Tables TableExpr Tables { TableOp $1 $2 $3 }
    | Tables PLUS Tables { TableConc $1 $3 }
    | Tables TJoin Tables ON Comparison { TableJoin $1 $2 $3 $5 }
TableExpr : CARTESIAN { Cartesian }
    | UNION { Union }
    | INTERSECT { Intersect }
TJoin : INNER { InnerJoin }
    | LEFT { LJoin }
    | RIGHT { RJoin }
    | FULL { Join }

Optionals : Operation Optionals { ($1 : $2) }
    | OUTPUT { [Output] }
    | END { [End] }
Operation : WHEN Boolean { WhenCondition $2 }
    | STORE STRING { Store $2 }
    | AS Outputs { AsExpr $2 }
    | ORDER Order { OrderAs $2 }
    | GROUPING Comparison { GroupAs $2 }
Outputs : ColumnList { OutputCols $1 }
    | STRING { OutputString $1 }
Order : IntCalc { OrderCalc $1 }
    | IntCalc DOT Order { NestedOrder $1 }
    | UP { OrderByAsc }
    | DOWN { OrderByDesc }

Boolean : Boolean BoolOp Boolean { BoolExpr $1 $2 $3 }
    | NOT Boolean { BoolNOT $2 }
    | TRUE { BoolTrue }
    | FALSE { BoolFalse }
    | LPAREN Boolean RPAREN { $2 }
    | Comparison { BoolComp $1 }
BoolOp : AND { BoolAND }
    | OR { BoolOR }
    | XOR { BoolXOR }
Comparison : Str EQ Str { StringComp $1 $3 }
    | IntCalc EQ IntCalc { IntEq $1 $3 }
    | IntCalc GT IntCalc { IntGT $1 $3 }
    | IntCalc LT IntCalc { IntLT $1 $3 }

Str : INT { Number $1 }
    | STRING { Name $1 }
    | QUOTE Str QUOTE { $2 }
IntCalc : LENGTH Column { CountLength $2 }
    | INT { Digit $1 }
    | ORD_OF INT { CharOrdOfCol $2 }
    | IntCalc PLUS IntCalc { IntAdd $1 $3 }
    | IntCalc MINUS IntCalc { IntSub $1 $3 }
    | IntCalc TIMES IntCalc { IntMul $1 $3 }
    | IntCalc DIVIDE IntCalc { IntDiv $1 $3 }
    | IntCalc POWER IntCalc { IntPow $1 $3 }

{ 
parseError :: [Token] -> a
parseError _ = error "Parse error" 


-- | A complete parsed program
data Program = Program [Statement]
  deriving (Show, Eq)

-- | SQL-like Statements
data Statement
  = SelectStmt Selection Tables [Optional]  -- `GET ... FROM ...`
  | CommentStmt String                      -- `# Comment`
  deriving (Show, Eq)

-- | Selection of columns
data Selection
  = SelectColumns [Column]  -- List of columns
  | SelectAll               -- `*`
  deriving (Show, Eq)

-- | Column expressions
data Column
  = ColIndex Int             -- Column by index
  | ColIndexTable Int String -- Column in table (e.g., `1.name`)
  | IfStmt [Column] Boolean [Column] -- Conditional column selection (IF ... OTHERWISE ...)
  deriving (Show, Eq)

-- | Tables & joins
data Tables
  = LoadTable String                    -- `LOAD "table.csv"`
  | TableOp Tables TableExpr Tables     -- Table operation (e.g., `CARTESIAN`, `UNION`)
  | TableConc Tables Tables             -- Table concatenation (`+`)
  | TableJoin Tables TJoin Tables Comparison -- Joins (`INNER`, `LEFT`, etc.)
  deriving (Show, Eq)

data TableExpr
  = Cartesian
  | Union
  | Intersect
  deriving (Show, Eq)

data TJoin
  = InnerJoin
  | LJoin
  | RJoin
  | Join
  deriving (Show, Eq)

-- | Optional clauses like ORDER, GROUPING, WHEN, OUTPUT
data Optional
  = WhenCondition Boolean
  | Store String
  | AsExpr Outputs
  | OrderAs Order
  | GroupAs Comparison
  | Output
  | End
  deriving (Show, Eq)

-- | Outputs for `AS`
data Outputs
  = OutputCols [Column]
  | OutputString String
  deriving (Show, Eq)

-- | Ordering options
data Order
  = OrderCalc IntCalc
  | NestedOrder IntCalc
  | OrderByAsc
  | OrderByDesc
  deriving (Show, Eq)

-- | Boolean expressions
data Boolean
  = BoolExpr Boolean BoolOp Boolean
  | BoolNOT Boolean
  | BoolComp Comparison
  | BoolTrue
  | BoolFalse
  deriving (Show, Eq)

data BoolOp
  = BoolAND
  | BoolOR
  | BoolXOR
  deriving (Show, Eq)

-- | Comparisons
data Comparison
  = StringComp Str Str  -- `name = "John"`
  | IntEq IntCalc IntCalc
  | IntGT IntCalc IntCalc
  | IntLT IntCalc IntCalc
  deriving (Show, Eq)

-- | String expressions
data Str
  = Number Int
  | Name String
  | Quoted Str
  deriving (Show, Eq)

-- | Integer calculations
data IntCalc
  = CountLength Column    -- `LENGTH(col)`
  | Digit Int             -- `5`
  | CharOrdOfCol Int      -- `ORD_OF(5)`
  | IntAdd IntCalc IntCalc
  | IntSub IntCalc IntCalc
  | IntMul IntCalc IntCalc
  | IntDiv IntCalc IntCalc
  | IntPow IntCalc IntCalc
  deriving (Show, Eq)
} 

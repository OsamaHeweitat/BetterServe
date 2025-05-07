{ 
module Grammar where 
import Tokens 
}

%name parse
%tokentype { Token } 
%error { parseError }
%token 
    SEMICOLON   { TokenSemicolon _ } 
    GET         { TokenGET _ } 
    FROM        { TokenFROM _ } 
    COMMENT     { TokenComment _ $$ }

    AST         { TokenAst _ } 
    COMMA       { TokenComma _ } 
    LPAREN      { TokenLParen _ } 
    IF          { TokenIf _ } 
    OTHERWISE   { TokenOtherwise _ } 
    RPAREN      { TokenRParen _ } 
    DOT         { TokenPeriod _ } 

    LOAD        { TokenLoad _ } 
    PLUS        { TokenPlus _ } 
    ON          { TokenOn _ } 
    CARTESIAN   { TokenCartesian _ } 
    UNION       { TokenUnion _ } 
    INTERSECT   { TokenIntersect _ } 
    INNER       { TokenInner _ } 
    LEFT        { TokenLJoin _ } 
    RIGHT       { TokenRJoin _ } 
    FULL        { TokenJoin _ } 

    OUTPUT      { TokenOutput _ } 
    END         { TokenEnd _ } 

    WHEN        { TokenWhen _ } 
    STORE       { TokenStore _ } 
    AS          { TokenAs _ } 
    TRANSPOSE   { TokenTrans _ }
    ORDER       { TokenOrder _ } 
    GROUPING    { TokenGrouping _ } 
    UP          { TokenUp _ } 
    DOWN        { TokenDown _ } 

    NOT         { TokenNOT _ } 
    TRUE        { TokenTrue _ } 
    FALSE       { TokenFalse _ } 
    AND         { TokenAND _ } 
    OR          { TokenOR _ } 
    XOR         { TokenXOR _ } 

    EQ          { TokenEq _ } 
    GT          { TokenGT _ } 
    LT          { TokenLT _ } 
    QUOTE       { TokenQuote _ } 
    LENGTH      { TokenLength _ } 
    ORD_OF      { TokenOrd _ } 
    
    MINUS       { TokenMinus _ }
    DIVIDE      { TokenDiv _ }
    POWER       { TokenPow _ }
    INT         { TokenDigit _ $$ } 
    STRING      { TokenVar _ $$ }
    NUM         { TokenNumber _ }
    ANY         { TokenAny _ $$ }

%left PLUS MINUS TIMES DIVIDE
%left AND OR XOR DOT
%right COMMA
%right POWER NOT
%nonassoc EQ GT LT

%% 
-- A program is a list of statements
-- Statements mimic SQL
Program : Statement { [ $1 ] }
    | Program SEMICOLON Statement { $1 ++ [ $3 ] }
Statement : GET Selection FROM Tables End { SelectStmt $2 $4 $5 }
    | GET Selection FROM Tables Optionals End { SelectOpt $2 $4 $5 $6 }
    | COMMENT { CommentStmt $1 }
End : OUTPUT { Output }
    | END { End }

Selection : ColumnList { SelectColumns $1 }
    | AST { SelectAll }
ColumnList : Column { [$1] } -- ColumnList is a list of Columns (wow)
    | ColumnList COMMA ColumnList { $1 ++ $3 }
    | LPAREN ColumnList IF Boolean OTHERWISE ColumnList RPAREN { [IfStmt $2 $4 $6] }
Column : INT { ColIndex $1 } -- Index of column from table
    | INT DOT INT { ColIndexTable $1 $3 } -- Index of column from Indexed table
-- Table . Column

Tables : LOAD STRING { [LoadTable $2] } -- Get tables with LOAD token
    | LPAREN Tables TableExpr Tables RPAREN { [TableOp $2 $3 $4] }
    | LPAREN Tables PLUS Tables RPAREN { [TableConc $2 $4] }
    | LPAREN Tables TJoin Tables ON Comparison RPAREN { [TableJoin $2 $3 $4 $6] }
    | Tables COMMA Tables { $1 ++ $3 }
-- Table expressions, etc
TableExpr : CARTESIAN { Cartesian }
    | UNION { Union }
    | INTERSECT { Intersect }
TJoin : INNER { InnerJoin }
    | LEFT { LJoin }
    | RIGHT { RJoin }
    | FULL { Join }

-- List of optionals before end of program
Optionals : Operation Optionals { ($1 : $2) }
    | Operation { [$1] }
Operation : WHEN Boolean { WhenCondition $2 } -- WHERE
    | STORE STRING { Store $2 } -- Write to file
    | AS Outputs { AsExpr $2 } -- Output with modifications
    | TRANSPOSE { Transpose } -- I wonder
    | ORDER Order { OrderAs $2 } -- Orders output
    | GROUPING Comparison { GroupAs $2 } -- Groups lines
Outputs : Output { [ $1 ] }
    | Outputs COMMA Output { $1 ++ [$3] } -- List of modifications
Output : INT { OutputCols $1 }
    | STRING { OutputString $1 }
    | ANY { OutputQuote $1 }
Order : IntCalc { OrderCalc $1 } -- Orders
    | IntCalc COMMA Order { NestedOrder $1 $3 }
    | UP { OrderByAsc }
    | DOWN { OrderByDesc }

-- Boolean expressions are often surrounded by brackets
Boolean : LPAREN Boolean BoolOp Boolean RPAREN { BoolExpr $2 $3 $4 }
    | NOT Boolean { BoolNOT $2 }
    | TRUE { BoolTrue }
    | FALSE { BoolFalse }
    | LPAREN Boolean RPAREN { $2 }
    | Comparison { BoolComp $1 }
BoolOp : AND { BoolAND } -- Typical boolean things
    | OR { BoolOR }
    | XOR { BoolXOR }
-- String and integer comparisons
Comparison : Str EQ EQ Str { StringComp $1 $4 }
    | IntCalc EQ IntCalc { IntEq $1 $3 }
    | IntCalc GT IntCalc { IntGT $1 $3 }
    | IntCalc LT IntCalc { IntLT $1 $3 }
-- Will equate to a string
Str : INT { Number $1 } 
    | INT DOT INT { SpecNumber $1 $3 }
    | STRING { Name $1 }
    | ANY { Quote $1 }
-- Will equate to an int
IntCalc : LENGTH Str { CountLength $2 }
    | NUM INT { Digit $2 }
    | ORD_OF Str { CharOrdOfCol $2 }
    | IntCalc PLUS IntCalc { IntAdd $1 $3 }
    | IntCalc MINUS IntCalc { IntSub $1 $3 }
    | IntCalc AST IntCalc { IntMul $1 $3 }
    | IntCalc DIVIDE IntCalc { IntDiv $1 $3 }
    | IntCalc POWER IntCalc { IntPow $1 $3 }

{ 
parseError :: [Token] -> a
parseError [] = error "Parse error" 
parseError (x:xs) = error ("Parse error at: " ++ (tokenPosn x))


-- A program is a list of statements
data Program = Program [Statement]
  deriving (Show, Eq)

-- Mimics SQL statements
data Statement
  = SelectOpt Selection [Tables] [Optional] End 
  | SelectStmt Selection [Tables] End
  | CommentStmt String
  deriving (Show, Eq)

data End -- Determines how the statment will end
  = Output
  | End
  deriving (Show, Eq)

-- Selects a list of columns
data Selection
  = SelectColumns [Column]
  | SelectAll
  deriving (Show, Eq)

-- Selects an indexed colum
data Column
  = ColIndex Int
  | ColIndexTable Int Int
  | IfStmt [Column] Boolean [Column] -- Conditional column selection (IF OTHERWISE)
  deriving (Show, Eq)

-- Tables and joins
data Tables
  = LoadTable String
  | TableOp [Tables] TableExpr [Tables] 
  | TableConc [Tables] [Tables]
  | TableJoin [Tables] TJoin [Tables] Comparison
  deriving (Show, Eq)

data TableExpr -- Expressions that can be applied between 2 tables
  = Cartesian
  | Union
  | Intersect
  deriving (Show, Eq)

data TJoin -- The ways tables can be joined
  = InnerJoin
  | LJoin
  | RJoin
  | Join
  deriving (Show, Eq)

-- Optional clauses
data Optional
  = WhenCondition Boolean
  | Store String
  | AsExpr [Outputs]
  | Transpose
  | OrderAs Order
  | GroupAs Comparison
  deriving (Show, Eq)

-- Outputs for modifying the selected columns
data Outputs
  = OutputCols Int -- NOW you choose from your indexed selection
  | OutputString String
  | OutputQuote String
  deriving (Show, Eq)

-- Ordering options
data Order
  = OrderCalc IntCalc
  | NestedOrder IntCalc Order
  | OrderByAsc
  | OrderByDesc
  deriving (Show, Eq)

-- Boolean expressions
data Boolean
  = BoolExpr Boolean BoolOp Boolean
  | BoolNOT Boolean
  | BoolComp Comparison
  | BoolTrue
  | BoolFalse
  deriving (Show, Eq)

data BoolOp -- Bool operations
  = BoolAND
  | BoolOR
  | BoolXOR
  deriving (Show, Eq)

-- Comparisons between strings or integers
data Comparison
  = StringComp Str Str
  | IntEq IntCalc IntCalc
  | IntGT IntCalc IntCalc
  | IntLT IntCalc IntCalc
  deriving (Show, Eq)

-- NEEDED
data Str
  = Number Int
  | Name String
  | SpecNumber Int Int
  | Quote String -- Used
  deriving (Show, Eq)

-- Integer calculations
data IntCalc
  = CountLength Str -- Count length of string value
  | Digit Int           
  | CharOrdOfCol Str      
  | IntAdd IntCalc IntCalc
  | IntSub IntCalc IntCalc
  | IntMul IntCalc IntCalc
  | IntDiv IntCalc IntCalc
  | IntPow IntCalc IntCalc
  deriving (Show, Eq)
} 
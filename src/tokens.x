%wrapper "posn"
$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
$white+         ;
--Comparison Operators
"=="          { \p s -> PT p TokenEquals}
"!="          { \p s -> PT p TokenNotEqual}
"<"           { \p s -> PT p TokenLessThan}
">"           { \p s -> PT p TokenGreaterThan}
"<="          { \p s -> PT p TokenLessOrEqual}
">="          { \p s -> PT p TokenGreaterOrEqual}

--Logical Operators
AND           { \p s -> PT p TokenAnd}
OR            { \p s -> PT p TokenOr}

--SQL Keywords
SELECT        { \p s -> PT p TokenSelect}
FROM          { \p s -> PT p TokenFrom}
WHERE         { \p s -> PT p TokenWhere}
ORDER BY      { \p s -> PT p TokenOrderBy}
"*"           { \p s -> PT p TokenAll}

--Conditional Keywords
IF            { \p s -> PT p TokenIf}
THEN          { \p s -> PT p TokenThen}
ELSE          { \p s -> PT p TokenElse}

--Join Types
CJOIN         { \p s -> PT p TokenCJoin}
JOIN          { \p s -> PT p TokenJoin}


--Sorting Order
ASC           { \p s -> PT p TokenAscending}
DESC          { \p s -> PT p TokenDescending}

--Symbols
"."           { \p s -> PT p TokenDot}
";"           { \p s -> PT p TokenSemiColon}
","           { \p s -> PT p TokenComma}
"("           { \p s -> PT p TokenLParens}
")"           { \p s -> PT p TokenRParens}

--Miscellaneous
NULL          { \p s -> PT p TokenNull}
OF            { \p s -> PT p TokenOf}
[_$alpha] [$alpha $digit \_ \']*   { \p s -> PT p (TokenTableName s) }
[_$alpha] [$alpha $digit \_ \']* \. $digit+   { \p s -> PT p (TokenColumnName s) }
\' [^\']* \'?  { \p s -> PT p (TokenString s) }
\" [^\"]* \"?  { \p s -> PT p (TokenString s) }
$digit+                      { \p s -> PT p (TokenInt (read s)) }
$digit+ "." $digit+          { \p s -> PT p (TokenFloat (read s)) }

--Error Handling
[^ \t\n\r]+ { \p s -> PT p (TokenError s) }


{
data PosnToken = PT AlexPosn Token deriving (Show, Eq)

data Token = 
    -- Comparison Operators
    TokenEquals           |
    TokenNotEqual         |
    TokenLessThan         |
    TokenGreaterThan      |
    TokenLessOrEqual      |
    TokenGreaterOrEqual   |

    -- Logical Operators
    TokenAnd              |
    TokenOr               |

    -- SQL Keywords
    TokenSelect           |
    TokenFrom             |
    TokenWhere            |
    TokenOrderBy          |
    TokenAll              |

    -- Conditional Keywords
    TokenIf               |
    TokenThen             |
    TokenElse             |

    -- Join Types
    TokenCJoin            |
    TokenJoin             |

    -- Sorting Order
    TokenAscending        |
    TokenDescending       |

    -- Symbols
    TokenDot              |
    TokenSemiColon        |
    TokenComma            |
    TokenLParens          |
    TokenRParens          |

    -- Miscellaneous
    TokenNull             |
    TokenOf               |
    TokenTableName String |
    TokenColumnName String|
    TokenString String    |
    TokenInt Int          |
    TokenFloat Float      |

    --Error Handling
    TokenError String     
    deriving (Show, Eq)

tokenPosn :: PosnToken -> String
tokenPosn (PT (AlexPn _ l c) _) = (show l) ++ ":" ++ (show c)  
}
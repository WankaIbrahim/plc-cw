%name parser
%tokentype { PosnToken }
%error { parseError }
%token 
    --Comparison Operators
    "==" { PT p (TokenEquals)}
    "!=" { PT p (TokenNotEquals)}
    "<"  { PT p (TokenLessThan)}
    ">"  { PT p (TokenGreaterThan)}
    "<=" { PT p (TokenLessThanEquals)}
    ">=" { PT p (TokenGreaterThanEquals)}

    --Logical Operators
    AND { PT p (TokenAnd)}
    OR  { PT p (TokenOr)}

    --SQL Keywords
    SELECT { PT p (TokenSelect)}
    FROM   { PT p (TokenFrom)}
    WHERE  { PT p (TokenWhere)}
    ORDER BY { PT p (TokenOrderBy)}
    "*" { PT p (TokenAll)}

    --Conditional Keywords
    IF { PT p (TokenIf)}
    THEN { PT p (TokenThen)}
    ELSE { PT p (TokenElse)}

    --Join Types
    CJOIN { PT p (TokenCJoin)}
    JOIN { PT p (TokenJoin)}

    --Sorting Order
    ASC { PT p (TokenAsc)}
    DESC { PT p (TokenDesc)}

    --Symbols
    "." { PT p (TokenDot)}
    ";" { PT p (TokenSemiColon)}
    "," { PT p (TokenComma)}
    "(" { PT p (TokenLParen)}
    ")" { PT p (TokenRParen)}

    --Miscallaneous
    NULL { PT p (TokenNull)}
    OF { PT p (TokenOf)}
    TokenTableName { PT p (TokenTableName s) }
    TokenColumnName { PT p (TokenColumnName s) }
    '\'' [^\']* '\'' { PT p (TokenString s) }
    '\"' [^\"]* '\"' { PT p (TokenString s) }
    $digit+           { PT p (TokenInt (read s)) }
    $digit+ "." $digit+ { PT p (TokenFloat (read s)) }

    -- Error Token
    TokenError { PT p (TokenError s) }


{ 
parseError :: [PosnToken] -> a
parseError ((PT (AlexPn _ l c) _):_) = 
    error $ "Parse error at location (line " ++ show l ++ ", column " ++ show c ++ ")" 
}
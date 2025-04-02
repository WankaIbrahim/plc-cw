 { 
module Tokens where 
}

%wrapper "posn" 
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]    
-- alphabetic characters

tokens :-
$white+       ; 
  "--".*        ; 
  $digit+              { \p s -> PT p (TokenNat (read s)) } 
  true                 { \p _ -> PT p TokenTrue }
  false                { \p _ -> PT p TokenFalse }
  \<                   { \p _ -> PT p TokenLT }
  \+                   { \p _ -> PT p TokenPlus }
  x                    { \p s -> PT p (TokenVar s) }
  if                   { \p _ -> PT p TokenIf }
  then                 { \p _ -> PT p TokenThen }
  else                 { \p _ -> PT p TokenElse }
  let                  { \p _ -> PT p TokenLet }
  \=                   { \p _ -> PT p TokenEq }
  in                   { \p _ -> PT p TokenIn }
  \\                   { \p _ -> PT p TokenLam }
  "->"                 { \p _ -> PT p TokenFunction }
  \:                   { \p _ -> PT p TokenColon }
  \.                   { \p _ -> PT p TokenDot }
  \(                   { \p _ -> PT p TokenLParen }
  \)                   { \p _ -> PT p TokenRParen }
  N                     { \p _ -> PT p TokenN }
  B                     { \p _ -> PT p TokenB }    

{  
data PosnToken = PT AlexPosn Token  deriving (Eq,Show)

data Token = 
  TokenNat  Int       |
  TokenTrue          |
  TokenFalse         | 
  TokenLT             | 
  TokenPlus          |
  TokenVar  String    |
  TokenIf            |
  TokenThen          |
  TokenElse          |
  TokenLet           |
  TokenEq            |
  TokenIn            |
  TokenLam           |
  TokenFunction      |
  TokenColon         |
  TokenDot           |
  TokenFType         |  
  TokenLParen        | 
  TokenRParen        |
  TokenN             |
  TokenB 
  deriving (Eq,Show)

tokenPosn :: PosnToken -> String
tokenPosn (PT (AlexPn _ l c) _) = (show l) ++ ":" ++ (show c)
}



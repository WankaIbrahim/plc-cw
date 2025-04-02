{ 
module Grammar where 
import Tokens 
}

%name parseCalc 
%tokentype { PosnToken } 
%error { parseError }
%token 
  nat                  { PT p (TokenNat $$) }
  true                 { PT p (TokenTrue) }
  false                { PT p (TokenFalse) }
  '<'                  { PT p (TokenLT) }
  '+'                  { PT p (TokenPlus) }
  x                    { PT p (TokenVar $$) }
  if                   { PT p (TokenIf) }
  then                 { PT p (TokenThen) }
  else                 { PT p (TokenElse) }
  let                  { PT p (TokenLet) }
  '='                  { PT p (TokenEq) }
  in                   { PT p (TokenIn) }
  '\\'                 { PT p (TokenLam) }
  '->'                 { PT p (TokenFunction) }
  ':'                  { PT p (TokenColon) }
  '.'                  { PT p (TokenDot) }
  '('                  { PT p (TokenLParen) }
  ')'                  { PT p (TokenRParen) } 
  N                   { PT p (TokenN) }
  B                  { PT p (TokenB) }

%nonassoc nat
%right '<' '+' if then else let '=' in '\\' '->' ':' true false x
%right '(' ')' '.'
%left app
%% 
Exp : nat                    { Nat $1 } 
    | true                   { FTrue } 
    | false                  { FFalse } 
    | Exp '<' Exp            { LessThan $1 $3 } 
    | Exp '+' Exp            { Plus $1 $3 }
    | x                      { Var $1 } 
    | if Exp then Exp else Exp { Ite $2 $4 $6 }
    | let '(' x ':' Type ')' '=' Exp in Exp   { Let $3 $5 $8 $10 }
    | '\\' '(' x ':' Type ')' '.' Exp   { Lam $3 $5 $8 }
    | Exp Exp %prec app               { App $1 $2 }
    | '(' Exp ')'            { $2 }
    

Type : N    { NatType } 
     | B    { BoolType } 
     | Type '->' Type { FunType $1 $3 }
{ 
parseError :: [PosnToken] -> a
parseError [] = error "Parse error at end of input"
parseError ((PT (AlexPn _ l c) _):_) = error $ "Parse error at location " ++ (show l) ++ ":" ++ (show c)

data Exp = Nat Int
        | FTrue
        | FFalse
        | LessThan Exp Exp
        | Plus Exp Exp
        | Var String
        | Ite Exp Exp Exp
        | Let String Type Exp Exp 
        | Lam String Type Exp
        | App Exp Exp  
         deriving Show 

data Type = NatType
          | BoolType
          | FunType Type Type
          deriving Show
}
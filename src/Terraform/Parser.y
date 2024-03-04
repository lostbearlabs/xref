{
module Terraform.Parser(parse) where
import Terraform.Lexer
}


%name parse
%tokentype { Token }
%error {parseError}

%token
  int    { TkInt $$ }
  plus   { TkPlus }
  mult   { TkMult }
  lparen { TkLParen }
  rparen { TkRParen }

%%

Expr : Expr plus Term   { $1 + $3 }
     | Term             { $1 }
     
Term : Term mult Factor { $1 * $3 }
     | Factor           { $1 }

Factor : int            { $1 }
       | lparen Expr rparen { $2 }

{
parseError :: [Token] -> a
parseError tokens = error $ "Parse error on token: " ++ show tokens
}


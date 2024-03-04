{
module Terraform.Parser(parse) where
import Terraform.Lexer
import Terraform.ParserDependencies
}


%name parse
%tokentype { Token }
%error {parseError}
%monad { E } { thenE } { returnE }

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
parseError :: [Token] -> E a
parseError tokens = failE $ "Parse error on token: " ++ show tokens
}


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
  bool   { TkBool $$ }
  str    { TkStr $$ }
  id     { TkId $$ }

  plus   { TkPlus }
  mult   { TkMult }
  lparen { TkLParen }
  rparen { TkRParen }
  lbrace { TkBlockStart }
  rbrace { TkBlockEnd }
  lbracket { TkArrayStart }
  rbracket { TkArrayEnd }
  equals { TkEquals }
  dot { TkDot }
  comma { TkComma }

  terraform { TkTerraform }
  variable { TkVariable }
  provider { TkProvider }
  resource { TkResource }
  module { TkModule }
  data { TkData }
  output { TkOutput }
%%

Decls : Decl Decls      { $1 : $2 }
     | Decl             { [$1] }

Decl : terraform lbrace Assignments rbrace { TConfig $3 }
     | variable str lbrace Assignments rbrace { TVariable $2 $4 }
     | provider str lbrace Assignments rbrace { TProvider $2 $4 }
     | resource str str lbrace Assignments rbrace { TResource $2 $3 $5}
     | module str lbrace Assignments rbrace { TModule $2 $4 }
     | data str str lbrace Assignments rbrace { TData $2 $3 $5 }
     | output str lbrace Assignments rbrace { TOutput $2 $4 }

Assignments : Assignment Assignments { $1 : $2}
     | Assignment { [$1] }

Assignment : id equals RVal { ($1, $3) }

RVal : int { TNum $1 }
     | id { TRId $1 }
     | str {TStr $1 }
     | bool {TBool $1}
     | id dot RVal { TRef $1 $3 }
     | id lparen Args rparen { TFunc $1 $3 }
     | lbracket Args rbracket { TArray $2}
     | lbrace MapArgs rbrace { TMap $2 }

Args : RVal comma Args    { $1 : $3 }
     | RVal               { [$1] }

MapArgs : Assignment comma MapArgs { $1 : $3 }
     | Assignment { [$1] }
     |                    { [] }


{
parseError :: [Token] -> E a
parseError tokens = failE $ "Parse error on token: " ++ show tokens
}


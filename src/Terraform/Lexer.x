{
module Terraform.Lexer(module Terraform.Lexer) where
}
%wrapper "basic"

$digit = [0-9]
$white = [\ \t\n]

$alpha = [a-zA-Z]
$alnum = [a-zA-Z0-9_]

tokens :-

  $white+             ;
  $digit+             { \s -> TkInt $ read s }
  \+                  { \_ -> TkPlus }
  \*                  { \_ -> TkMult }
  \-                  { \_ -> TkMinus }
  \/                  { \_ -> TkDiv } 
  \.                  { \_ -> TkDot }
  \=                  { \_ -> TkEquals }
  \(                  { \_ -> TkLParen }
  \)                  { \_ -> TkRParen }
  \{                  { \_ -> TkBlockStart }
  \}                  { \_ -> TkBlockEnd }  
  \[                  { \_ -> TkArrayStart }
  \]                  { \_ -> TkArrayEnd }
  \?                  { \_ -> TkQuestion }
  \:                  { \_ -> TkColon }
  \"[^\n\"]*\"        { \s -> TkStr $ read s}
  -- TODO: HEREDOC STRINGS
  $alpha$alnum*       { \s -> TkId $ s}


{
data Token = TkInt Int
  | TkStr String
  | TkBool Bool
  | TkId String
  | TkPlus
  | TkMult
  | TkMinus
  | TkDiv
  | TkLParen
  | TkRParen
  | TkBlockStart
  | TkBlockEnd
  | TkArrayStart
  | TkArrayEnd
  | TkEquals
  | TkDot
  | TkQuestion
  | TkColon
  deriving(Eq, Show)

}

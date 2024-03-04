{
module Terraform.Lexer(module Terraform.Lexer) where
}
%wrapper "basic"

$digit = [0-9]
$white = [\ \t\n]

tokens :-

  $white+             ;
  $digit+             { \s -> TkInt (read s) }
  \+                  { \_ -> TkPlus }
  \*                  { \_ -> TkMult }
  \(                  { \_ -> TkLParen }
  \)                  { \_ -> TkRParen }

{
data Token = TkInt Int
           | TkPlus
           | TkMult
           | TkLParen
           | TkRParen
           deriving(Eq, Show)
}

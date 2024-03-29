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
  \,                  { \_ -> TkComma }

  \True|true          { \_ -> TkBool True }
  \False|false        { \_ -> TkBool False }

  terraform           { \_ -> TkTerraform }
  variable            { \_ -> TkVariable }
  provider            { \_ -> TkProvider }
  resource            { \_ -> TkResource }
  module              { \_ -> TkModule }
  data                { \_ -> TkData }
  output              { \_ -> TkOutput }

  \"[^\n\"]*\"        { \s -> TkStr $ read s}
  $alpha$alnum*       { \s -> TkId s}
  -- TODO: this only handles heredocts that use EOT;  generalize to allow any marker
  \<\<EOT\n[.\n]*\nEOT  { \s -> TkStr (removeHere s "EOT")}

{

removeHere :: String -> String -> String
removeHere str eot = take (length rest - length eot - 1) rest
   where
     rest = drop (length eot + 3) str

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
  | TkTerraform
  | TkVariable
  | TkProvider
  | TkResource
  | TkModule
  | TkData
  | TkOutput
  | TkComma
  deriving(Eq, Show)

}

module Terraform.Parse(parseInput, lexInput) where
import Terraform.ParserDependencies
import Terraform.Parser(parse)
import Terraform.Lexer(alexScanTokens, Token)

lexInput :: String -> [Token]
lexInput = alexScanTokens

parseInput :: String -> E [TDeclaration]
parseInput input = parse tokens
    where
      tokens :: [Token]
      tokens = alexScanTokens input

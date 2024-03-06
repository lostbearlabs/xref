module Terraform.Parse(parseInput, lexInput) where
import Terraform.ParserDependencies
import Terraform.Parser(parse)
import Terraform.Lexer(alexScanTokens, Token)

lexInput :: String -> [Token]
lexInput = alexScanTokens

parseInput :: String -> E Int
parseInput input = parse tokens
  -- Right parseResult
    where
      tokens :: [Token]
      tokens = alexScanTokens input

  --     parseResult :: Int
  --     parseResult = parse tokens

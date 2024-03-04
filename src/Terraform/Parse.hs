module Terraform.Parse(parseInput) where
import Terraform.ParserDependencies
import Terraform.Parser(parse)
import Terraform.Lexer(alexScanTokens, Token)

parseInput :: String -> E Int
parseInput input = parse tokens
  -- Right parseResult
    where
      tokens :: [Token]
      tokens = alexScanTokens input

  --     parseResult :: Int
  --     parseResult = parse tokens

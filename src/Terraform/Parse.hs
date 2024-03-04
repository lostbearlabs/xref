module Terraform.Parse(parseInput) where
import Terraform.Parser(parse)
import Terraform.Lexer(alexScanTokens, Token)

parseInput :: String -> Either String Int
parseInput input =
  Right parseResult
    where
      tokens :: [Token]
      tokens = alexScanTokens input

      parseResult :: Int
      parseResult = parse tokens

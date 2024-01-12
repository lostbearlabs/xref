{-# LANGUAGE FlexibleContexts #-}
module Terraform.TerraformLexer(module Terraform.TerraformLexer) where

import Control.Monad (liftM)
import Data.Functor.Identity (Identity)
import Data.Maybe (fromJust, isJust)
import Text.Parsec (
    Parsec, parse, SourcePos,
    many, many1, manyTill, noneOf, oneOf, optionMaybe, choice, try,
    eof, string, anyChar,
    (<?>)
  )
import Text.Parsec.Prim (ParsecT, Stream, getParserState, statePos, token)
import Prelude hiding (lines)

sourcePos :: Monad m => ParsecT s u m SourcePos
sourcePos = statePos `liftM` getParserState

data TfToken = 
    TfSep | 
    TfKProvider | 
    TfKResource | 
    TfKData | 
    TfKOutput | 
    TfKVariable | 
    TfStr String | 
    TfBool Bool | 
    TfNum String | 
    TfId String | 
    TfBlockStart | 
    TfBlockEnd | 
    TfArrayStart | 
    TfArrayEnd | 
    TfEquals
    deriving (Eq, Show)


-- TokOccur wraps a TfToken together with a SourcePos
data TokOccur = TokOccur SourcePos TfToken

instance Show TokOccur where
  show (TokOccur _ t) = show t


tfTokenizer :: Parsec String () [TokOccur]
tfTokenizer = ((map fromJust . filter isJust) <$>) $ (<* eof) $ many $ choice [
    (<?> "comment") $ try $ many sep >> string "#" >> many (noneOf "\n") >> string "\n" >> many sep >> treturn (Just TfSep),
    (<?> "newline") $ many1 sep >> treturn (Just TfSep),
    (<?> "=") $ string "=" >> treturn (Just TfEquals),
    (<?> "whitespace") $ many1 (oneOf ", ") >> treturn Nothing,
    (<?> "[") $ string "[" >> treturn (Just TfArrayStart),
    (<?> "]") $ string "]" >> treturn (Just TfArrayEnd),
    (<?> "{") $ string "{" >> treturn (Just TfBlockStart),
    (<?> "}") $ string "}" >> treturn (Just TfBlockEnd),
    (<?> "heredoc") $ do
      _ <- string "<<"
      maybeDedent <- maybe id (const dedent) <$> optionMaybe (string "-")
      endId <- identifier <* string "\n"
      lines <- (`manyTill` (try $ many (oneOf " ") >> string endId)) $
        manyTill anyChar (oneOf "\n")
      treturn . Just . TfStr . unlines $ maybeDedent lines,
        -- TODO normal join instead of unlines, if trailing \n is wrong
    (<?> "string") $ string "\"" *> (concat <$> manyTill (choice [try embed, (:[])<$>anyChar]) (string "\"")) >>= (treturn . Just . TfStr),
    (<?> "number") $ many1 (oneOf ('.':['0'..'9'])) >>= (treturn . Just . TfNum),
    (<?> "boolean") $ try $ choice [
        string "true" >> return True,
        string "false" >> return False
      ] >>= (treturn . Just . TfBool),
    (<?> "identifier") $ identifier >>= (treturn . Just . TfId)
  ]
  where
    identifier = many1 (oneOf $ concat [['a'..'z'],['A'..'Z'],"_","-"])
    embed :: Parsec String () String
    embed = do
      _ <- string "${"
      stuff <- concat <$> manyTill (concat <$> many (choice [try embed, (:[])<$>(noneOf "}")])) (oneOf "}")
      return $ "${" ++ stuff ++ "}"
    sep = choice [string "\n", string ";"]
    treturn t = do
      pos <- sourcePos
      return $ maybe Nothing (Just . TokOccur pos) t

dedent :: [String] -> [String]
dedent lines = map (drop minSpaces) lines
  where
    minSpaces = minimum $ map initSpaceCount lines
    initSpaceCount str = length $ takeWhile (==' ') str


satisfy :: (Stream s Identity TokOccur) => (TfToken -> Bool) -> Parsec s u TfToken
satisfy test = token
  (\(TokOccur _ t)-> show t)
  (\(TokOccur pos _)-> pos)
  (\(TokOccur _ t)-> if test t then Just t else Nothing)


tfParse' :: Parsec [TokOccur] () [TfToken]
tfParse' = many (satisfy (const True))

tokenizeTF :: String -> String -> [TfToken]
tokenizeTF name input
  = let toks = either (error . show) id $ parse tfTokenizer name input
            in either (error . show) id $ parse tfParse' name toks


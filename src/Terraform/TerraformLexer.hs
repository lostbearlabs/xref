{-# LANGUAGE FlexibleContexts #-}

module Terraform.TerraformLexer (module Terraform.TerraformLexer) where

import Control.Monad (liftM)
import Data.Functor.Identity (Identity)
import Data.Maybe (fromJust, isJust)
import Text.Parsec
  ( Parsec,
    SourcePos,
    anyChar,
    choice,
    eof,
    many,
    many1,
    manyTill,
    noneOf,
    oneOf,
    optionMaybe,
    parse,
    string,
    char,
    try,
    notFollowedBy,
    (<?>),
  )
import Text.Parsec.Prim (ParsecT, Stream, getParserState, statePos, token)
import Prelude hiding (lines)

-- Source position
sourcePos :: (Monad m) => ParsecT s u m SourcePos
sourcePos = statePos `liftM` getParserState

-- These are all the tokens we recognize
data TfToken
  = TokSep
  | TokStr String
  | TokBool Bool
  | TokNum String
  | TokId String
  | TokBlockStart
  | TokBlockEnd
  | TokArrayStart
  | TokArrayEnd
  | TokLeftParen
  | TokRightParen
  | TokEquals
  | TokDot
  deriving (Eq, Show)

-- Recognizers for the various tokens
isTokBool :: TfToken -> Bool
isTokBool (TokBool _) = True
isTokBool _ = False

isTokStr :: TfToken -> Bool
isTokStr (TokStr _) = True
isTokStr _ = False

isTokNum :: TfToken -> Bool
isTokNum (TokNum _) = True
isTokNum _ = False

isTokId :: TfToken -> Bool
isTokId (TokId _) = True
isTokId _ = False

-- TokOccur wraps a TfToken together with a SourcePos
data TokOccur = TokOccur SourcePos TfToken

instance Show TokOccur where
  show (TokOccur _ t) = show t

-- Here's the actual tokenizer
tfTokenizer :: Parsec String () [TokOccur]
tfTokenizer =
  ((map fromJust . filter isJust) <$>) $
    (<* eof) $
      many $
        choice
          [ (<?> "comment") $ try $ many sep >> string "#" >> many (noneOf "\n") >> string "\n" >> many sep >> treturn (Just TokSep),
            (<?> "newline") $ many1 sep >> treturn (Just TokSep),
            (<?> "=") $ string "=" >> treturn (Just TokEquals),
            (<?> "whitespace") $ many1 (oneOf ", ") >> treturn Nothing,
            (<?> "[") $ string "[" >> treturn (Just TokArrayStart),
            (<?> "]") $ string "]" >> treturn (Just TokArrayEnd),
            (<?> "{") $ string "{" >> treturn (Just TokBlockStart),
            (<?> "}") $ string "}" >> treturn (Just TokBlockEnd),
            (<?> "(") $ string "(" >> treturn (Just TokLeftParen),
            (<?> ")") $ string ")" >> treturn (Just TokRightParen),
            -- we treat * as an indentifier for use in expressions like "a = b.c.*.e"
            (<?> "*") $ string "*" >> treturn (Just $ TokId "*"),
            (<?> ".") $ do
              _ <- char '.'
              -- TODO: this doesn't work to prevent TokDot instead of TokNum when parsing ".8" -- why?
              notFollowedBy (oneOf ['0'..'9'])
              treturn (Just TokDot),
            (<?> "heredoc") $ do
              _ <- string "<<"
              maybeDedent <- maybe id (const dedent) <$> optionMaybe (string "-")
              endId <- identifier <* string "\n"
              lines <-
                (`manyTill` (try $ many (oneOf " ") >> string endId)) $
                  manyTill anyChar (oneOf "\n")
              treturn . Just . TokStr . unlines $ maybeDedent lines,
            -- TODO normal join instead of unlines, if trailing \n is wrong
            (<?> "string") $ string "\"" *> (concat <$> manyTill (choice [try embed, (: []) <$> anyChar]) (string "\"")) >>= (treturn . Just . TokStr),
            (<?> "number") $ many1 (oneOf ('.' : ['0' .. '9'])) >>= (treturn . Just . TokNum),
            (<?> "boolean") $
              try $
                choice
                  [ string "true" >> return True,
                    string "false" >> return False
                  ]
                  >>= (treturn . Just . TokBool),
            (<?> "identifier") $ identifier >>= (treturn . Just . TokId)
          ]
  where
    embed :: Parsec String () String
    embed = do
      _ <- string "${"
      stuff <- concat <$> manyTill (concat <$> many (choice [try embed, (: []) <$> noneOf "}"])) (oneOf "}")
      return $ "${" ++ stuff ++ "}"
    sep = choice [string "\n", string ";"]
    treturn t = do
      pos <- sourcePos
      return $ fmap (TokOccur pos) t

identifier :: ParsecT String u Identity [Char]
identifier = do
    firstChar <- oneOf $ concat [['a' .. 'z'], ['A' .. 'Z'], "_", "-"]
    restChars <- many (oneOf $ concat [['a' .. 'z'], ['A' .. 'Z'], "_", "-", ['0' .. '9']])
    return (firstChar:restChars)

-- Remove common indent from block of strings
dedent :: [String] -> [String]
dedent lines = map (drop minSpaces) lines
  where
    minSpaces = minimum $ map initSpaceCount lines
    initSpaceCount str = length $ takeWhile (== ' ') str

-- Returns a parser that consumes input and produces a TfToken if the test is satisfied.
satisfy :: (Stream s Identity TokOccur) => (TfToken -> Bool) -> Parsec s u TfToken
satisfy test =
  token
    -- token to string:
    (\(TokOccur _ t) -> show t)
    -- token to position:
    (\(TokOccur pos _) -> pos)
    -- does token satisfy the test?
    (\(TokOccur _ t) -> if test t then Just t else Nothing)

------------------
-- tokenizeTF can be called to tokenize without parsing

tfParse' :: Parsec [TokOccur] () [TfToken]
tfParse' = many (satisfy (const True))

tokenizeTF :: String -> String -> [TfToken]
tokenizeTF name input =
  let toks = either (error . show) id $ parse tfTokenizer name input
   in either (error . show) id $ parse tfParse' name toks

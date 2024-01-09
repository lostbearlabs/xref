-- This file is derived from:
-- > https://github.com/tsprlng/terraform-hcl-converter/blob/master/TerraformParser.hs
-- which includes the following copyright notice:
-- > ISC License
-- >
-- > Copyright (c) 2019, Tom Spurling
-- > 
-- > Permission to use, copy, modify, and/or distribute this software for any
-- > purpose with or without fee is hereby granted, provided that the above
-- > copyright notice and this permission notice appear in all copies.

{-# LANGUAGE FlexibleContexts #-}

module TerraformParser(module  TerraformParser) where

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

data TfToken = TfSep | TfKProvider | TfKResource | TfKData | TfKOutput | TfKVariable | TfStr String | TfBool Bool | TfNum String | TfId String | TfBlockStart | TfBlockEnd | TfArrayStart | TfArrayEnd | TfEquals
  deriving (Eq, Show)
data TokOccur = TokOccur SourcePos TfToken

instance Show TokOccur where
  show (TokOccur _ t) = show t

type TId = String
data TRVal = TStr String | TBool Bool | TNum String | TMap [(TId, TRVal)] | TArray [TRVal]
  deriving (Eq, Show)
data TfDeclaration = 
    TfConfig [(TId, TRVal)] |
    TfResource TId TId [(TId, TRVal)] |
    TfModule TId [(TId, TRVal)] |
    TfData TId TId [(TId, TRVal)] | 
    TfOutput TId [(TId, TRVal)] | 
    TfVariable TId [(TId, TRVal)] | 
    TfProvider TId [(TId, TRVal)]
  deriving (Eq, Show)

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

tfParse :: Parsec [TokOccur] () [TfDeclaration]
tfParse = many (choice [terraconfig, provider, resource, _data, output, variable, mmodule]) <* eof
  where
    terraconfig = do
      _ <- satisfy (==(TfId "terraform"))
      _ <- satisfy (==TfBlockStart)
      decls <- tfdecls
      _ <- satisfy (==TfBlockEnd)
      return $ TfConfig decls
    resource = do
      typ <- satisfy (==(TfId "resource")) *> tfstr
      key <- tfstr
      _ <- satisfy (==TfBlockStart)
      decls <- tfdecls
      _ <- satisfy (==TfBlockEnd)
      return $ TfResource typ key decls
    mmodule = do
      typ <- satisfy (==(TfId "module")) *> tfstr
      _ <- satisfy (==TfBlockStart)
      decls <- tfdecls
      _ <- satisfy (==TfBlockEnd)
      return $ TfModule typ decls
    _data = do
      typ <- satisfy (==(TfId "data")) *> tfstr
      key <- tfstr
      _ <- satisfy (==TfBlockStart)
      decls <- tfdecls
      _ <- satisfy (==TfBlockEnd)
      return $ TfData typ key decls
    provider = do
      key <- satisfy (==(TfId "provider")) *> tfstr
      _ <- satisfy (==TfBlockStart)
      decls <- tfdecls
      _ <- satisfy (==TfBlockEnd)
      return $ TfProvider key decls
    output = do
      key <- satisfy (==(TfId "output")) *> tfstr
      _ <- satisfy (==TfBlockStart)
      decls <- tfdecls
      _ <- satisfy (==TfBlockEnd)
      return $ TfOutput key decls
    variable = do
      key <- satisfy (==(TfId "variable")) *> tfstr
      _ <- satisfy (==TfBlockStart)
      decls <- tfdecls
      _ <- satisfy (==TfBlockEnd)
      return $ TfVariable key decls
    tfstr = do
      (TfStr s) <- satisfy isTfStr
      return $ s
      where
        isTfStr (TfStr _) = True
        isTfStr _ = False
    tfid = do
      (TfId s) <- satisfy isTfId
      return $ s
      where
        isTfId (TfId _) = True
        isTfId _ = False
    tfdecls :: Parsec [TokOccur] () [(TId, TRVal)]
    tfdecls = many (choice [try provisioner, try backend, try block, assgn])
      -- TODO limit annoying special provisioner/backend cases to the relevant contexts
    tfarray = do
      _ <- satisfy (==TfArrayStart)
      things <- many tfrval
      _ <- satisfy (==TfArrayEnd)
      return things
    provisioner = do
      _ <- satisfy (==(TfId "provisioner"))
      typ <- tfstr
      _ <- optionMaybe $ satisfy (==TfEquals)
      _ <- satisfy (==TfBlockStart)
      decls <- tfdecls
      _ <- satisfy (==TfBlockEnd)
      return $ ("__provisioner_"++typ, TMap decls)  -- TODO special-case this bullshit more nicely
    backend = do
      _ <- satisfy (==(TfId "backend"))
      typ <- tfstr
      _ <- optionMaybe $ satisfy (==TfEquals)
      _ <- satisfy (==TfBlockStart)
      decls <- tfdecls
      _ <- satisfy (==TfBlockEnd)
      return $ ("__backend_"++typ, TMap decls)  -- TODO special-case this bullshit more nicely
    block = do
      key <- tfid
      _ <- optionMaybe $ satisfy (==TfEquals)
      _ <- satisfy (==TfBlockStart)
      decls <- tfdecls
      _ <- satisfy (==TfBlockEnd)
      return $ (key, TMap decls)
    assgn = do
      key <- tfid
      _ <- satisfy (==TfEquals)
      rval <- tfrval
      return $ (key, rval)
    tfrval = choice [TArray <$> tfarray, TStr <$> tfstr, TBool <$> tfbool, TNum <$> tfnum]
    tfbool = do
      (TfBool b) <- satisfy isTfBool
      return $ b
      where
        isTfBool (TfBool _) = True
        isTfBool _ = False
    tfnum = do
      (TfNum i) <- satisfy isTfNum
      return $ i
      where
        isTfNum (TfNum _) = True
        isTfNum _ = False

tokenizeTF :: String -> String -> [TfToken]
tokenizeTF name input
  = let toks = either (error . show) id $ parse tfTokenizer name input
            in either (error . show) id $ parse tfParse' name toks

parseTF :: String -> String -> [TfDeclaration]
parseTF name input
  = let toks = either (error . show) id $ parse tfTokenizer name input
            in either (error . show) id $ parse tfParse name $ filter (\(TokOccur _ t)-> t /= TfSep) toks


{-# LANGUAGE FlexibleContexts #-}

module Terraform.TerraformParser (module Terraform.TerraformParser) where

import Terraform.TerraformLexer
  ( TfToken (..),
    TokOccur (..),
    isTokBool,
    isTokId,
    isTokNum,
    isTokStr,
    satisfy,
    tfTokenizer,
  )
import Text.Parsec
  ( Parsec,
    choice,
    eof,
    many,
    optionMaybe,
    parse,
    try,
  )
import Prelude hiding (lines)

type TId = String

-- Expressions to the right of equals signs in the various blocks
data TRVal
  = TStr String
  | TBool Bool
  | TNum String
  | TMap [(TId, TRVal)]
  | TArray [TRVal]
  | TExpr TExpr
  deriving (Eq, Show)

-- Expressions used in r-vals
-- Not intended to check semantics ... will accept legal forms like "x.y" but also illegal forms like "x.(2+3)"
data TExpr
  = ExpId TId
  | ExpRef TId TExpr
  | ExpFunc TId [TRVal]
  deriving (Eq, Show)


-- Top-level blocks in the terraform project;  our parser just returns a list of these
data TfDeclaration
  = TfConfig [(TId, TRVal)]
  | TfResource TId TId [(TId, TRVal)]
  | TfModule TId [(TId, TRVal)]
  | TfData TId TId [(TId, TRVal)]
  | TfOutput TId [(TId, TRVal)]
  | TfVariable TId [(TId, TRVal)]
  | TfProvider TId [(TId, TRVal)]
  deriving (Eq, Show)

----------------
-- Parse top-level blocks

tfParse :: Parsec [TokOccur] () [TfDeclaration]
tfParse = many (choice [terraconfig, provider, resource, _data, output, variable, mmodule]) <* eof
  where
    terraconfig = do
      _ <- satisfy (== TokId "terraform")
      _ <- satisfy (== TokBlockStart)
      decls <- tfdecls
      _ <- satisfy (== TokBlockEnd)
      return $ TfConfig decls
    resource = do
      typ <- satisfy (== TokId "resource") *> tfstr
      key <- tfstr
      _ <- satisfy (== TokBlockStart)
      decls <- tfdecls
      _ <- satisfy (== TokBlockEnd)
      return $ TfResource typ key decls
    mmodule = do
      typ <- satisfy (== TokId "module") *> tfstr
      _ <- satisfy (== TokBlockStart)
      decls <- tfdecls
      _ <- satisfy (== TokBlockEnd)
      return $ TfModule typ decls
    _data = do
      typ <- satisfy (== TokId "data") *> tfstr
      key <- tfstr
      _ <- satisfy (== TokBlockStart)
      decls <- tfdecls
      _ <- satisfy (== TokBlockEnd)
      return $ TfData typ key decls
    provider = do
      key <- satisfy (== TokId "provider") *> tfstr
      _ <- satisfy (== TokBlockStart)
      decls <- tfdecls
      _ <- satisfy (== TokBlockEnd)
      return $ TfProvider key decls
    output = do
      key <- satisfy (== TokId "output") *> tfstr
      _ <- satisfy (== TokBlockStart)
      decls <- tfdecls
      _ <- satisfy (== TokBlockEnd)
      return $ TfOutput key decls
    variable = do
      key <- satisfy (== TokId "variable") *> tfstr
      _ <- satisfy (== TokBlockStart)
      decls <- tfdecls
      _ <- satisfy (== TokBlockEnd)
      return $ TfVariable key decls

----------------
-- Parse declaration lists within blocks

tfdecls :: Parsec [TokOccur] () [(TId, TRVal)]
tfdecls = many (choice [try provisioner, try backend, try block, assgn])
  where
  -- TODO limit annoying special provisioner/backend cases to the relevant contexts
  provisioner = do
    _ <- satisfy (== TokId "provisioner")
    typ <- tfstr
    _ <- optionMaybe $ satisfy (== TokEquals)
    _ <- satisfy (== TokBlockStart)
    decls <- tfdecls
    _ <- satisfy (== TokBlockEnd)
    return ("__provisioner_" ++ typ, TMap decls) -- TODO special-case this bullshit more nicely
  backend = do
    _ <- satisfy (== TokId "backend")
    typ <- tfstr
    _ <- optionMaybe $ satisfy (== TokEquals)
    _ <- satisfy (== TokBlockStart)
    decls <- tfdecls
    _ <- satisfy (== TokBlockEnd)
    return ("__backend_" ++ typ, TMap decls) -- TODO special-case this bullshit more nicely
  block = do
    key <- tfid
    _ <- optionMaybe $ satisfy (== TokEquals)
    _ <- satisfy (== TokBlockStart)
    decls <- tfdecls
    _ <- satisfy (== TokBlockEnd)
    return (key, TMap decls)
  assgn = do
    key <- tfid
    _ <- satisfy (== TokEquals)
    rval <- tfrval
    return (key, rval)


----------------
-- Parse RVals within declarations

tfrval :: Parsec [TokOccur] () TRVal
tfrval = choice [TArray <$> tfarray, TStr <$> tfstr, TBool <$> tfbool, TNum <$> tfnum, TExpr <$> tfexpr]

tfbool :: Parsec [TokOccur] () Bool
tfbool = do
  (TokBool b) <- satisfy isTokBool
  return b

tfnum :: Parsec [TokOccur] () String
tfnum = do
  (TokNum i) <- satisfy isTokNum
  return i

tfstr :: Parsec [TokOccur] () String
tfstr = do
  (TokStr s) <- satisfy isTokStr
  return s

tfid :: Parsec [TokOccur] () String
tfid = do
  (TokId s) <- satisfy isTokId
  return s

tfarray :: Parsec [TokOccur] () [TRVal]
tfarray = do
  _ <- satisfy (== TokArrayStart)
  things <- many tfrval
  _ <- satisfy (== TokArrayEnd)
  return things

----------------
-- Parse expressions within RVals

tfexpr :: Parsec [TokOccur] () TExpr
tfexpr = choice [try tfref, try tffunc, try tfid']
  where
    tfref = do
      (TokId ident) <- satisfy isTokId
      _ <- satisfy (== TokDot)
      ExpRef ident <$> tfexpr
    tfid' = do
      (TokId ident) <- satisfy isTokId
      return $ ExpId ident
    tffunc = do
      (TokId ident) <- satisfy isTokId
      _ <- satisfy (== TokLeftParen)
      args <- many tfrval
      _ <- satisfy (== TokRightParen)
      return $ ExpFunc ident args

----------------
-- Top-level parser for a Terraform file

parseTF :: String -> String -> [TfDeclaration]
parseTF name input =
  let toks = either (error . show) id $ parse tfTokenizer name input
   in either (error . show) id $ parse tfParse name $ filter (\(TokOccur _ t) -> t /= TokSep) toks

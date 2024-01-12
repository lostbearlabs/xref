{-# LANGUAGE FlexibleContexts #-}

module Terraform.TerraformParser(module Terraform.TerraformParser) where

import Control.Monad (liftM)
import Text.Parsec (
    Parsec, parse, SourcePos,
    many, optionMaybe, choice, try,
    eof)
import Text.Parsec.Prim (ParsecT, getParserState, statePos)
import Prelude hiding (lines)
import Terraform.TerraformLexer(TfToken(..), tfTokenizer, TokOccur(..), satisfy)

sourcePos :: Monad m => ParsecT s u m SourcePos
sourcePos = statePos `liftM` getParserState


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


parseTF :: String -> String -> [TfDeclaration]
parseTF name input
  = let toks = either (error . show) id $ parse tfTokenizer name input
            in either (error . show) id $ parse tfParse name $ filter (\(TokOccur _ t)-> t /= TfSep) toks


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
      _ <- satisfy (==(TokId "terraform"))
      _ <- satisfy (==TokBlockStart)
      decls <- tfdecls
      _ <- satisfy (==TokBlockEnd)
      return $ TfConfig decls
    resource = do
      typ <- satisfy (==(TokId "resource")) *> tfstr
      key <- tfstr
      _ <- satisfy (==TokBlockStart)
      decls <- tfdecls
      _ <- satisfy (==TokBlockEnd)
      return $ TfResource typ key decls
    mmodule = do
      typ <- satisfy (==(TokId "module")) *> tfstr
      _ <- satisfy (==TokBlockStart)
      decls <- tfdecls
      _ <- satisfy (==TokBlockEnd)
      return $ TfModule typ decls
    _data = do
      typ <- satisfy (==(TokId "data")) *> tfstr
      key <- tfstr
      _ <- satisfy (==TokBlockStart)
      decls <- tfdecls
      _ <- satisfy (==TokBlockEnd)
      return $ TfData typ key decls
    provider = do
      key <- satisfy (==(TokId "provider")) *> tfstr
      _ <- satisfy (==TokBlockStart)
      decls <- tfdecls
      _ <- satisfy (==TokBlockEnd)
      return $ TfProvider key decls
    output = do
      key <- satisfy (==(TokId "output")) *> tfstr
      _ <- satisfy (==TokBlockStart)
      decls <- tfdecls
      _ <- satisfy (==TokBlockEnd)
      return $ TfOutput key decls
    variable = do
      key <- satisfy (==(TokId "variable")) *> tfstr
      _ <- satisfy (==TokBlockStart)
      decls <- tfdecls
      _ <- satisfy (==TokBlockEnd)
      return $ TfVariable key decls
    tfstr = do
      (TokStr s) <- satisfy isTfStr
      return $ s
      where
        isTfStr (TokStr _) = True
        isTfStr _ = False
    tfid = do
      (TokId s) <- satisfy isTfId
      return $ s
      where
        isTfId (TokId _) = True
        isTfId _ = False
    tfdecls :: Parsec [TokOccur] () [(TId, TRVal)]
    tfdecls = many (choice [try provisioner, try backend, try block, assgn])
      -- TODO limit annoying special provisioner/backend cases to the relevant contexts
    tfarray = do
      _ <- satisfy (==TokArrayStart)
      things <- many tfrval
      _ <- satisfy (==TokArrayEnd)
      return things
    provisioner = do
      _ <- satisfy (==(TokId "provisioner"))
      typ <- tfstr
      _ <- optionMaybe $ satisfy (==TokEquals)
      _ <- satisfy (==TokBlockStart)
      decls <- tfdecls
      _ <- satisfy (==TokBlockEnd)
      return $ ("__provisioner_"++typ, TMap decls)  -- TODO special-case this bullshit more nicely
    backend = do
      _ <- satisfy (==(TokId "backend"))
      typ <- tfstr
      _ <- optionMaybe $ satisfy (==TokEquals)
      _ <- satisfy (==TokBlockStart)
      decls <- tfdecls
      _ <- satisfy (==TokBlockEnd)
      return $ ("__backend_"++typ, TMap decls)  -- TODO special-case this bullshit more nicely
    block = do
      key <- tfid
      _ <- optionMaybe $ satisfy (==TokEquals)
      _ <- satisfy (==TokBlockStart)
      decls <- tfdecls
      _ <- satisfy (==TokBlockEnd)
      return $ (key, TMap decls)
    assgn = do
      key <- tfid
      _ <- satisfy (==TokEquals)
      rval <- tfrval
      return $ (key, rval)
    tfrval = choice [TArray <$> tfarray, TStr <$> tfstr, TBool <$> tfbool, TNum <$> tfnum]
    tfbool = do
      (TokBool b) <- satisfy isTfBool
      return $ b
      where
        isTfBool (TokBool _) = True
        isTfBool _ = False
    tfnum = do
      (TokNum i) <- satisfy isTfNum
      return $ i
      where
        isTfNum (TokNum _) = True
        isTfNum _ = False


parseTF :: String -> String -> [TfDeclaration]
parseTF name input
  = let toks = either (error . show) id $ parse tfTokenizer name input
            in either (error . show) id $ parse tfParse name $ filter (\(TokOccur _ t)-> t /= TokSep) toks


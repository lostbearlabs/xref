module Terraform.ParserDependencies(module Terraform.ParserDependencies) where

data E a = Ok a | Failed String
    deriving(Eq, Show)

------------------------------
-- Parser Monad
------------------------------

thenE :: E a -> (a -> E b) -> E b
m `thenE` k =
   case m of
       Ok a     -> k a
       Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k =
   case m of
      Ok a     -> Ok a
      Failed e -> k e


------------------------------
-- Abstract Syntax Tree
------------------------------

-- This is what our parser returns
newtype AST = AST [TDeclaration]

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

data TDeclaration
  = TConfig [(TId, TRVal)]
  | TResource TId TId [(TId, TRVal)]
  | TModule TId [(TId, TRVal)]
  | TData TId TId [(TId, TRVal)]
  | TOutput TId [(TId, TRVal)]
  | TVariable TId [(TId, TRVal)]
  | TProvider TId [(TId, TRVal)]
  deriving (Eq, Show)


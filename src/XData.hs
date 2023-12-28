module XData( Def(..) ) where

data Def = Def
    { symbol :: String
    , file :: String
} deriving(Show, Eq)

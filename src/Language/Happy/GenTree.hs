{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Happy.GenTree
    ( GenTree (..)
    , flatten
    , renderTree
    , getRules
    ) where

import           Data.Text          (Text)
import qualified Data.Text          as Text
import           GHC.Generics       (Generic)
import           Language.Happy.Ast (Sym (..))

-- | A generic Concrete Syntax Tree generated from the grammar.
data GenTree token
    = Node Sym [GenTree token]  -- ^ Rule Sym, Children
    | Leaf token                -- ^ Terminal Token
    deriving (Show, Eq, Functor, Generic)

-- | Flatten the tree into a list of tokens.
flatten :: GenTree token -> [token]
flatten (Leaf t)    = [t]
flatten (Node _ cs) = concatMap flatten cs

-- | Get all rule names used in the tree
getRules :: GenTree token -> [Text]
getRules (Leaf _)               = []
getRules (Node (Sym name _) cs) = name : concatMap getRules cs

-- | simple ASCII visualization of the tree for debugging
renderTree :: Show token => GenTree token -> String
renderTree = go 0
  where
    go indent (Leaf t) = replicate indent ' ' ++ show t ++ "\n"
    go indent (Node sym children) =
        replicate indent ' ' ++ show sym ++ "\n" ++ concatMap (go (indent + 2)) children

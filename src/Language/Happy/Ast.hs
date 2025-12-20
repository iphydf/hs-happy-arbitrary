{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Strict            #-}
module Language.Happy.Ast
    ( Node, NodeF (..)
    , Sym (..), RuleDef (..)
    ) where

import           Data.Aeson                   (FromJSON, FromJSON1, ToJSON,
                                               ToJSON1)
import           Data.Fix                     (Fix)
import           Data.Functor.Classes         (Eq1, Ord1, Read1, Show1)
import           Data.Functor.Classes.Generic (FunctorClassesDefault (..))
import           Data.Text                    (Text)
import           GHC.Generics                 (Generic, Generic1)

data NodeF lexeme a
    = Grammar [lexeme] [a] [a] [lexeme]
    | PragmaExpect lexeme
    | PragmaName lexeme lexeme
    | PragmaErrorHandlerType lexeme
    | PragmaError lexeme
    | PragmaLexer lexeme lexeme
    | PragmaMonad lexeme
    | PragmaTokenType lexeme
    | PragmaToken [a]
    | PragmaLeft [lexeme]
    | PragmaRight [lexeme]
    | Token lexeme lexeme
    | Rule (Maybe a) a
    | RuleType lexeme lexeme
    | RuleDefn lexeme [lexeme] [a] -- Name, Params, Productions
    | RuleLine [a] lexeme          -- Symbols, Code
    | Symbol lexeme [a]            -- Name, Arguments
    deriving (Show, Read, Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable)
    deriving (Show1, Read1, Eq1, Ord1) via FunctorClassesDefault (NodeF lexeme)

type Node lexeme = Fix (NodeF lexeme)

instance FromJSON lexeme => FromJSON1 (NodeF lexeme)
instance ToJSON lexeme => ToJSON1 (NodeF lexeme)

-- | Internal representation of a symbol usage (Name + Arguments)
data Sym = Sym Text [Sym] deriving (Show, Eq, Ord)

-- | Internal representation of a rule definition
data RuleDef = RuleDef
    { ruleParams :: [Text]
    , ruleProds  :: [[Sym]]
    } deriving (Show, Eq)

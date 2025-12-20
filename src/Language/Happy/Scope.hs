{-# LANGUAGE OverloadedStrings #-}
module Language.Happy.Scope
    ( genTreeScope
    ) where

import           Control.Monad.State      (evalStateT)
import qualified Data.Map                 as Map
import           Data.Text                (Text)
import           Language.Happy.Arbitrary (Config (..), Sym (..), expandFair)
import           Language.Happy.Ast       (Node)
import           Language.Happy.GenTree   (GenTree (..))
import           Language.Happy.Grammar   (Grammar, extractTokens, fromAst)
import           Language.Happy.Lexer     (Lexeme)
import           Test.QuickCheck.Gen      (Gen)
import qualified Test.QuickCheck.Gen      as Gen

genTreeScope :: Config token -> Text -> Node (Lexeme Text) -> Gen (GenTree token)
genTreeScope cfg start g = do
    let tokens = extractTokens (parseToken cfg) g
    let (grammar, _) = fromAst start g
    let startSym = Sym start []

    -- Scope Logic is not implemented, falling back to Fair expansion.
    Gen.sized $ \n -> evalStateT (expandFair cfg tokens grammar startSym 0 n) Map.empty

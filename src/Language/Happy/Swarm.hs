{-# LANGUAGE OverloadedStrings #-}
module Language.Happy.Swarm
    ( genTreeSwarm
    ) where

import           Control.Monad.State      (evalStateT)
import           Data.List                (minimumBy)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Text                (Text)
import           Language.Happy.Arbitrary (Config (..), Sym (..), expandFair)
import           Language.Happy.Ast       (Node)
import           Language.Happy.GenTree   (GenTree)
import           Language.Happy.Grammar   (Grammar, Production, extractTokens,
                                           fromAst)
import           Language.Happy.Lexer     (Lexeme)
import           Test.QuickCheck.Gen      (Gen)
import qualified Test.QuickCheck.Gen      as Gen

-- | Generates a tree using Swarm Testing (randomly disabling rules).
genTreeSwarm :: Config token -> Text -> Node (Lexeme Text) -> Gen (GenTree token)
genTreeSwarm cfg start g = do
    let tokens = extractTokens (parseToken cfg) g
    let (grammar, _) = fromAst start g
    let startSym = Sym start []

    -- Swarm: Filter grammar
    filteredGrammar <- swarmFilter grammar

    -- Use Fair expansion on the filtered grammar
    Gen.sized $ \n -> evalStateT (expandFair cfg tokens filteredGrammar startSym 0 n) Map.empty

swarmFilter :: Grammar -> Gen Grammar
swarmFilter grammar = do
    let process (sym, prods) = do
            if length prods <= 1
                then return (sym, prods)
                else do
                     -- Select subset
                     -- Keep simplest (shortest) + random others
                     let (safestIdx, _) = minimumBy (\(_, p1) (_, p2) -> compare (length p1) (length p2)) (zip [0..] prods)

                     let otherIndices = filter (/= safestIdx) [0 .. length prods - 1]
                     keptOther <- Gen.sublistOf otherIndices

                     let finalIndices = safestIdx : keptOther
                     let keptProds = [ prods !! i | i <- finalIndices ]
                     return (sym, keptProds)

    pairs <- mapM process (Map.toList grammar)
    return $ Map.fromList pairs

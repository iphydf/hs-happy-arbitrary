{-# LANGUAGE OverloadedStrings #-}
module Language.Happy.Mutation
    ( genTreeMutation
    ) where

import           Control.Monad.State      (StateT, evalStateT, get, lift,
                                           modify)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Text                (Text)
import           Language.Happy.Arbitrary (Config (..), Sym (..), expandFair,
                                           genTree)
import           Language.Happy.Ast       (Node)
import           Language.Happy.GenTree   (GenTree (..))
import           Language.Happy.Grammar   (Grammar, extractTokens, fromAst)
import           Language.Happy.Lexer     (Lexeme)
import           Test.QuickCheck.Gen      (Gen)
import qualified Test.QuickCheck.Gen      as Gen

-- | Generates a tree by first generating a base tree, then mutating a SINGLE subtree.
genTreeMutation :: Config token -> Text -> Node (Lexeme Text) -> Gen (GenTree token)
genTreeMutation cfg start g = do
    -- 1. Generate a base tree
    baseTree <- genTree cfg start g

    let tokens = extractTokens (parseToken cfg) g
    let (grammar, _) = fromAst start g

    -- Use Fair expansion for regeneration
    let regenerator sym = Gen.sized $ \n -> evalStateT (expandFair cfg tokens grammar sym 0 (n `div` 10)) Map.empty

    let count = countMutable grammar baseTree

    if count == 0
        then return baseTree
        else do
            -- Pick a random node index to mutate
            idx <- Gen.choose (0, count - 1)
            replaceAt idx regenerator grammar baseTree

countMutable :: Grammar -> GenTree token -> Int
countMutable _ (Leaf _) = 0
countMutable grammar (Node sym children) =
    let self = if isMutable grammar sym then 1 else 0
        childCounts = sum (map (countMutable grammar) children)
    in self + childCounts

isMutable :: Grammar -> Sym -> Bool
isMutable grammar sym = Map.member sym grammar

replaceAt :: Int -> (Sym -> Gen (GenTree token)) -> Grammar -> GenTree token -> Gen (GenTree token)
replaceAt targetIdx regen grammar tree = evalStateT (go tree) targetIdx
  where
    go t@(Leaf _) = return t
    go (Node sym children) = do
        let mutable = isMutable grammar sym

        shouldMutate <- if mutable
                        then do
                            k <- get
                            if k == 0
                                then return True
                                else do
                                    modify (\n -> n - 1)
                                    return False
                        else return False

        if shouldMutate
            then lift $ regen sym
            else do
                children' <- mapM go children
                return $ Node sym children'

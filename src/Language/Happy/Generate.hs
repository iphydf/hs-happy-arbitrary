{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Language.Happy.Generate
    ( Algorithm(..)
    , generateCustom
    , runStateChain
    ) where

import           Data.Semigroup           ((<>))
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Language.Alex.Ast        (AlexFile)
import           Language.Happy.Arbitrary (Config (..), History, genTree,
                                           genTreeFair, genTreeGas)
import           Language.Happy.Ast       (Node)
import           Language.Happy.GenTree   (flatten, getRules)
import qualified Language.Happy.Lexer     as HL
import           Language.Happy.Linker    (LinkConfig (..), makeConfig)
import           Language.Happy.Mutation  (genTreeMutation)
import           Language.Happy.Scope     (genTreeScope)
import           Language.Happy.Swarm     (genTreeSwarm)
import           Test.QuickCheck          (Gen)

data Algorithm = AlgDefault | AlgSwarm | AlgMutation | AlgScope | AlgFair | AlgGas
    deriving (Show, Read, Eq)

generateCustom :: Algorithm -> LinkConfig -> Double -> AlexFile -> Text -> Node (HL.Lexeme Text) -> History -> Gen (String, History, [Text])
generateCustom alg cfg z alex startSymbol happy history = do
    let config = makeConfig cfg z alex

    (tree, nextHistory) <- case alg of
            AlgDefault   -> (, history) <$> genTree config startSymbol happy
            AlgSwarm     -> (, history) <$> genTreeSwarm config startSymbol happy
            AlgMutation  -> (, history) <$> genTreeMutation config startSymbol happy
            AlgFair      -> genTreeFair config history startSymbol happy
            AlgScope     -> (, history) <$> genTreeScope config startSymbol happy
            AlgGas       -> (, history) <$> genTreeGas config startSymbol happy

    tokenGens <- return $ flatten tree
    s <- runStateChain tokenGens "0"
    return (s, nextHistory, getRules tree)

runStateChain :: [Text -> Gen (String, Text)] -> Text -> Gen String
runStateChain [] _ = return ""
runStateChain (f:fs) s = do
    (genStr, s') <- f s
    strs <- runStateChain fs s'
    if null strs
        then return genStr
        else return (genStr ++ " " ++ strs)

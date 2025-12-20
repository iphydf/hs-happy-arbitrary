{-# LANGUAGE OverloadedStrings #-}
module Language.Alex.Arbitrary
    ( genRegex
    , getFixedString
    , standardMacros
    ) where

import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Set          (Set)
import qualified Data.Set          as Set
import           Data.Text         (Text)
import qualified Data.Text         as Text
import           Language.Alex.Ast
import           Test.QuickCheck   (Gen, elements, listOf, listOf1, oneof,
                                    resize)

-- Universe of characters to sample from when generating "any character" or inverted sets.
universe :: [Char]
universe = [' ' .. '~'] ++ ['\t', '\n']

-- | Checks if the Regex produces exactly one string. Returns that string if so.
getFixedString :: Map Text Regex -> Regex -> Maybe String
getFixedString macroDefs r = case r of
    RChar c -> Just [c]
    RString t -> Just (Text.unpack t)
    RSet chars False | Set.size chars == 1 -> Just [Set.findMin chars]
    RSet _ _ -> Nothing
    RStar _ -> Nothing
    RPlus _ -> Nothing
    ROpt _ -> Nothing
    RSeq rs -> concat <$> mapM (getFixedString macroDefs) rs
    RAlt [r1] -> getFixedString macroDefs r1
    RAlt (r1:rs) -> do
        s1 <- getFixedString macroDefs r1
        let allSame = all (\rx -> getFixedString macroDefs rx == Just s1) rs
        if allSame then Just s1 else Nothing
    RAlt [] -> Nothing
    RMacro name -> case Map.lookup name macroDefs of
        Just r' -> getFixedString macroDefs r'
        Nothing -> case Map.lookup name standardMacros of
            Just r' -> getFixedString macroDefs r'
            Nothing -> Nothing

genRegex :: Map Text Regex -> Regex -> Gen String
genRegex macroDefs r = resize 5 $ case r of
        RChar c -> return [c]
        RString t -> return $ Text.unpack t
        RSet chars inverted ->
            if inverted
            then do
                let candidates = filter (`Set.notMember` chars) universe
                if null candidates
                    then return "a" -- Fallback, should not happen for reasonable sets
                    else elements candidates >>= \c -> return [c]
            else do
                let candidates = Set.toList chars
                if null candidates
                    then return "a" -- Fallback
                    else elements candidates >>= \c -> return [c]
        RStar r' -> concat <$> listOf (genRegex macroDefs r')
        RPlus r' -> concat <$> listOf1 (genRegex macroDefs r')
        ROpt r' -> oneof [return "", genRegex macroDefs r']
        RSeq rs -> concat <$> mapM (genRegex macroDefs) rs
        RAlt rs -> oneof (map (genRegex macroDefs) rs)
        RMacro name -> case Map.lookup name macroDefs of
            Just r' -> genRegex macroDefs r'
            Nothing -> case Map.lookup name standardMacros of
                Just r' -> genRegex macroDefs r'
                Nothing -> error $ "Undefined macro: " <> Text.unpack name

standardMacros :: Map Text Regex
standardMacros = Map.fromList
    [ ("white",    RSet (Set.fromList " \t\n\f\v\r") False)
    , ("digit",    RSet (Set.fromList ['0'..'9']) False)
    , ("hex",      RSet (Set.fromList (['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F'])) False)
    , ("alpha",    RSet (Set.fromList (['a'..'z'] ++ ['A'..'Z'])) False)
    , ("upper",    RSet (Set.fromList ['A'..'Z']) False)
    , ("lower",    RSet (Set.fromList ['a'..'z']) False)
    , ("alphanum", RSet (Set.fromList (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])) False)
    ]
